#include "mips2.hpp"

#include <cstddef>
#include <cstdint>
#include <vector>

#include "codegen/ssir.hpp"
#include "error_reporter.hpp"
#include "fmt/base.h"
#include "fmt/format.h"
#include "fmt/ranges.h"

#define ALIGN(addr, n) ((addr) + ((n) - 1)) & -(n)

namespace yuri::mips::v2 {

static constexpr size_t reg_args_base = 4;
static constexpr size_t reg_args_count = 4;
static constexpr size_t reg_tmp_base = 8;
static constexpr size_t reg_loc_base = 16;

static constexpr size_t reg_v0 = 2;
static constexpr size_t reg_sp = 29;
static constexpr size_t reg_ra = 31;

static constexpr size_t word_size = 4;  // 32bit words

static constexpr std::array<std::string_view, 32> regs{
    "$zero", "$at", "$v0", "$v1", "$a0", "$a1", "$a2", "$a3",
    "$t0",   "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7",
    "$s0",   "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7",
    "$t8",   "$t9", "$k0", "$k1", "$gp", "$sp", "$fp", "$ra",
};

struct Op {
    std::string op;
    std::string r;
    std::string a;
    std::string b;

    static auto init(std::string op, std::string r, std::string a = "",
                     std::string b = "") -> Op {
        return {.op = std::move(op),
                .r = std::move(r),
                .a = std::move(a),
                .b = std::move(b)};
    }
};

struct CodegenFunc {
    struct Local {
        size_t offset;
        size_t idx;
        size_t size;
        size_t align;
        size_t slot;
        bool   has_addr_taken;
    };

    struct MemRegions {
        size_t offset;
        size_t size;
    };

    void codegen(ssir::Func const& f, bool do_optimize) {
        process(f);

        codegen_preamble(f);
        codegen_body(f);
        codegen_postamble(f);

        if (do_optimize) optimize(f);
        dump_output(f);
    }

    void process(ssir::Func const& f) {
        auto const& b = f.body;

        std::span args = f.type.inner;
        args = args.subspan(0, args.size() - 1);
        for (auto const& arg : args) {
            auto l = Local{.offset = 0,
                           .idx = locals.size(),
                           .size = arg.bytesize(),
                           .align = arg.bytealign(),
                           .slot = locals.size(),
                           .has_addr_taken = false};

            locals.push_back(l);
        }

        for (size_t i = 0; i < b.text.size(); i++) {
            auto c = b.opcode_at(i);

            switch (c) {
                case ssir::Opcode::Local: {
                    auto lidx = f.body.text_at(++i);
                    if (lidx != locals.size()) {
                        er->report_bug(b.span_for(i - 1),
                                       "expected local index of {}, but got {}",
                                       locals.size(), lidx);
                    }

                    auto const& local = f.body.local_at(lidx);

                    auto l = Local{.offset = 0,
                                   .idx = locals.size(),
                                   .size = local.size,
                                   .align = local.align,
                                   .slot = locals.size(),
                                   .has_addr_taken = false};
                    locals.push_back(l);
                } break;

                case ssir::Opcode::Alloca: {
                    auto size = f.body.text_at(++i);
                    mregions.push_back({.offset = 0, .size = size});
                } break;

                case ssir::Opcode::Ref: {
                    auto idx = b.text_at(++i);
                    locals.at(idx).has_addr_taken = true;
                } break;

                case ssir::Opcode::Li:
                case ssir::Opcode::Global:
                case ssir::Opcode::GetGlobal:
                case ssir::Opcode::SetGlobal:
                case ssir::Opcode::Get:
                case ssir::Opcode::Set:
                case ssir::Opcode::Iset:
                case ssir::Opcode::DeRef:
                case ssir::Opcode::B:
                case ssir::Opcode::Bz: i++; break;

                case ssir::Opcode::Call:
                    i += 2;
                    is_not_leaf = true;
                    break;

                default: break;
            }
        }

        stack_size = 0;

        // need to save $ra
        if (is_not_leaf) {
            stack_size += word_size;
        }

        for (auto& l : locals) {
            stack_size = ALIGN(stack_size, l.align);
            l.offset = stack_size;
            stack_size += l.size;

            if (!l.has_addr_taken) l.size = word_size;
        }

        for (auto& mr : mregions) {
            // FIXME: use proper alignment, for now using `4` as alignment
            stack_size = ALIGN(stack_size, 4);
            mr.offset = stack_size;
            stack_size += mr.size;
        }

        stack_size = ALIGN(stack_size, 8);
    }

    void codegen_body(ssir::Func const& f) {
        auto const& b = f.body;

        size_t lbl{};
        size_t alloca{};

        auto binop = [&](std::string_view op) {
            auto rhs = pop_tmp();
            auto lhs = pop_tmp();
            auto dst = push_tmp();

            add_op(op, regs[dst], regs[lhs], regs[rhs]);
        };

        size_t i = 0;
        for (; i < b.text.size(); i++) {
            if (lbl < b.labels.size() && b.label_at(lbl) == i) {
                add_op("", fmt::format("{}.{}", f.name, lbl));
                lbl++;
            }

            auto c = b.opcode_at(i);

            switch (c) {
                case ssir::Opcode::Dupe: {
                    auto r = pop_tmp();
                    push_tmp();
                    auto b = push_tmp();

                    // we need this `move` to not be optimized out, so we add an
                    // extra ' '
                    add_op("move ", regs[b], regs[r]);
                } break;

                case ssir::Opcode::Local: {
                    auto slot = f.body.text_at(++i);

                    auto dst = push_local();
                    auto src = pop_tmp();

                    if (locals.at(slot).has_addr_taken) {
                        // move to memory
                        auto op = op_for_store(locals.at(slot).size);
                        add_op(op, regs[src],
                               fmt::to_string(locals.at(slot).offset),
                               regs[reg_sp]);
                        break;
                    }

                    add_op("move", regs[dst], regs[src]);
                } break;

                case ssir::Opcode::Li: {
                    auto idx = f.body.text_at(++i);
                    auto v = f.body.const_at(idx);
                    auto dst = push_tmp();

                    add_op("li", regs[dst], fmt::to_string(v));
                } break;

                case ssir::Opcode::Global: {
                    auto name = f.body.text_at(++i);
                    auto r = push_tmp();

                    add_op("la", regs[r], f.body.id_at(name));
                } break;

                case ssir::Opcode::GetGlobal: {
                    auto name = f.body.text_at(++i);
                    auto r = push_tmp();

                    auto const& global = m->globals.find(f.body.id_at(name));
                    if (global == m->globals.end()) {
                        er->report_bug(f.body.span_for(i - 1),
                                       "undefined global: {}",
                                       f.body.id_at(name));
                        break;
                    }

                    auto op = op_for_load(global->second.type);
                    add_op(op, regs[r], f.body.id_at(name), "$zero");
                } break;

                case ssir::Opcode::SetGlobal: {
                    auto name = f.body.text_at(++i);
                    auto r = pop_tmp();

                    auto const& global = m->globals.find(f.body.id_at(name));
                    if (global == m->globals.end()) {
                        er->report_bug(f.body.span_for(i - 1),
                                       "undefined global: {}",
                                       f.body.id_at(name));
                        break;
                    }

                    auto op = op_for_store(global->second.type);
                    add_op(op, regs[r], f.body.id_at(name), "$zero");
                } break;

                case ssir::Opcode::GetStatic: {
                    auto id = f.body.text_at(++i);
                    auto r = push_tmp();
                    add_op("la", regs[r], fmt::format("s_{}", id));
                } break;

                case ssir::Opcode::Get: {
                    auto slot = f.body.text_at(++i);
                    if (slot != locals.at(slot).slot) {
                        er->report_bug(b.span_for(i - 1),
                                       "expected slot {} in get, but found {}",
                                       slot, locals.at(slot).slot);
                    }

                    auto dst = push_tmp();
                    if (locals.at(slot).has_addr_taken) {
                        // get from memory
                        auto op = op_for_load(locals.at(slot).size);
                        add_op(op, regs[dst],
                               fmt::to_string(locals.at(slot).offset),
                               regs[reg_sp]);
                        break;
                    }

                    add_op("move", regs[dst], regs[reg_loc_base + slot]);
                } break;

                case ssir::Opcode::Set: {
                    auto slot = f.body.text_at(++i);
                    if (slot != locals.at(slot).slot) {
                        er->report_bug(b.span_for(i - 1),
                                       "expected slot {} in set, but found {}",
                                       slot, locals.at(slot).slot);
                    }

                    auto src = pop_tmp();
                    if (locals.at(slot).has_addr_taken) {
                        // get from memory
                        auto op = op_for_store(locals.at(slot).size);
                        add_op(op, regs[src],
                               fmt::to_string(locals.at(slot).offset),
                               regs[reg_sp]);
                        break;
                    }

                    add_op("move", regs[reg_loc_base + slot], regs[src]);
                } break;

                case ssir::Opcode::Iset: {
                    auto size = f.body.text_at(++i);

                    auto rhs = pop_tmp();
                    auto lhs = pop_tmp();

                    auto op = op_for_store(size);
                    add_op(op, regs[rhs], "", regs[lhs]);
                } break;

                case ssir::Opcode::Add: binop("addu"); break;
                case ssir::Opcode::Sub: binop("subu"); break;
                case ssir::Opcode::Mul: binop("mul"); break;
                case ssir::Opcode::Div: binop("div"); break;
                case ssir::Opcode::And: binop("and"); break;
                case ssir::Opcode::Or: binop("or"); break;
                case ssir::Opcode::Seq: binop("seq"); break;
                case ssir::Opcode::Sne: binop("sne"); break;
                case ssir::Opcode::Slt: binop("slt"); break;
                case ssir::Opcode::Sgt: binop("sgt"); break;

                case ssir::Opcode::B: {
                    auto label = f.body.text_at(++i);
                    add_op("b", fmt::format("{}.{}", f.name, label));
                } break;

                case ssir::Opcode::Bz: {
                    auto cmp = pop_tmp();

                    auto label = f.body.text_at(++i);
                    add_op("beq", regs[cmp], regs[0],
                           fmt::format("{}.{}", f.name, label));
                } break;

                case ssir::Opcode::Ref: {
                    auto slot = b.text_at(++i);
                    if (slot != locals.at(slot).slot) {
                        er->report_bug(b.span_for(i - 1),
                                       "expected slot {} in ref, but found {}",
                                       slot, locals.at(slot).slot);
                    }

                    if (!locals.at(slot).has_addr_taken) {
                        er->report_bug(b.span_for(i - 1),
                                       "local is not an l-value");
                        return;
                    }

                    auto r = push_tmp();
                    add_op("addiu", regs[r], regs[reg_sp],
                           fmt::to_string(locals.at(slot).offset));
                } break;

                case ssir::Opcode::Alloca: {
                    auto _ = f.body.text_at(++i);
                    auto idx = alloca++;
                    auto r = push_tmp();
                    add_op("addiu", regs[r], regs[reg_sp],
                           fmt::to_string(mregions.at(idx).offset));
                } break;

                case ssir::Opcode::DeRef: {
                    auto size = f.body.text_at(++i);

                    auto ptr = pop_tmp();
                    auto r = push_tmp();

                    auto op = op_for_load(size);
                    add_op(op, regs[r], "", regs[ptr]);
                } break;

                case ssir::Opcode::Call: {
                    auto id = f.body.text_at(++i);
                    auto argc = f.body.text_at(++i);
                    for (size_t i = argc; i > 0; i--) {
                        auto r = pop_tmp();
                        add_op("move", regs[reg_args_base + i - 1], regs[r]);
                    }

                    add_op("jal", f.body.id_at(id));

                    auto r = push_tmp();
                    add_op("move", regs[r], regs[reg_v0]);
                } break;

                case ssir::Opcode::Ret: {
                    auto v = pop_tmp();
                    add_op("move", regs[reg_v0], regs[v]);
                    add_op("b", fmt::format("{}.ret", f.name));
                } break;

                case ssir::Opcode::Pop: {
                    pop_tmp();
                } break;

                default:
                    er->report_bug(b.span_for(i),
                                   "invalid opcode found in codegen: {} ({})",
                                   fmt::underlying(c), c);
                    break;
            }
        }

        if (lbl < b.labels.size() && b.label_at(lbl) == i) {
            add_op("", fmt::format("{}.{}", f.name, lbl));
        }
    }

    void codegen_preamble(ssir::Func const& f) {
        add_op("subu", regs[reg_sp], regs[reg_sp], fmt::to_string(stack_size));

        auto argc = f.type.inner.size() - 1;

        if (is_not_leaf) {
            // $ra is in offset 0
            add_op("sw", regs[reg_ra], "0", regs[reg_sp]);
        }

        size_t i{};
        for (auto const& local : locals) {
            // if the local has it's address taken, we don't use a $sn register
            // for it.
            if (local.has_addr_taken) {
                i++;
                continue;
            }

            auto op = op_for_store(local.size);
            add_op(op, regs[reg_loc_base + i], fmt::to_string(local.offset),
                   regs[reg_sp]);

            if (i < argc) {
                add_op("move", regs[reg_loc_base + i], regs[reg_args_base + i]);
            }

            i++;
        }

        add_op("#", "end of preamble");
    }

    void codegen_postamble(ssir::Func const& f) {
        add_op("", fmt::format("{}.ret", f.name));

        if (is_not_leaf) {
            // $ra is in offset 0
            add_op("lw", regs[reg_ra], "0", regs[reg_sp]);
        }

        size_t i{};
        for (auto const& local : locals) {
            if (local.has_addr_taken) {
                i++;
                continue;
            }

            auto op = op_for_load(local.size);
            add_op(op, regs[reg_loc_base + i], fmt::to_string(local.offset),
                   regs[reg_sp]);

            i++;
        }

        add_op("addu", regs[reg_sp], regs[reg_sp], fmt::to_string(stack_size));
        add_op("jr", regs[reg_ra]);
    }

    // ----------------------------------------------------------------------------

    // FIXME: should not optimize when inputs are temporaries, as these may be
    // optimized by other steps
    void optimize(ssir::Func const& f) {
        fmt::println("# ====== optimizing {}", f.name);

        auto had_change = false;
        do {
            had_change = false;

            fmt::println("# running pass...");

            for (size_t i = 1; i < output.size(); i++) {
                auto const& prev = output.at(i - 1);
                auto const& curr = output.at(i);

                //    li tA, 0
                // -> move tA, $zero
                if (prev.op == "li" && prev.r.at(1) == 't' && prev.a == "0") {
                    had_change = true;
                    output.at(--i) = Op::init("move", prev.r, "$zero");
                }

                //    li tA, B
                //    addu _, _, tA
                else if (prev.op == "li" && curr.op == "addu" &&
                         prev.r.at(1) == 't' && prev.r == curr.b) {
                    had_change = true;
                    output.at(i) = Op::init("addiu", curr.r, curr.a, prev.a);
                    output.erase(output.begin() + i-- - 1);
                }

                //    li tA, B
                //    subu _, _, tA
                else if (prev.op == "li" && curr.op == "subu" &&
                         prev.r.at(1) == 't' && prev.r == curr.b) {
                    had_change = true;
                    output.at(i) = Op::init("subiu", curr.r, curr.a, prev.a);
                    output.erase(output.begin() + i-- - 1);
                }

                //    la tA, B
                //    move C, tA
                // -> la C, B
                else if (prev.op == "la" && curr.op == "move" &&
                         prev.r.at(1) == 't' && prev.r == curr.a) {
                    had_change = true;
                    output.at(i) = Op::init("la", curr.r, prev.a);
                    output.erase(output.begin() + i-- - 1);
                }

                //    move tA, B
                //    addu _, _, tA
                // -> addu _, _, B
                else if (prev.op == "move" &&
                         (curr.op == "addu" || curr.op == "subu" ||
                          curr.op == "and" || curr.op == "or") &&
                         prev.r.at(1) == 't' && prev.r == curr.b) {
                    had_change = true;
                    output.at(i) = Op::init(curr.op, curr.r, curr.a, prev.a);
                    output.erase(output.begin() + i-- - 1);
                }

                //    move tA, B
                //    addu _, tA, _
                // -> addu _, B, _
                else if (prev.op == "move" &&
                         (curr.op == "addu" || curr.op == "subu" ||
                          curr.op == "and" || curr.op == "or" ||
                          curr.op == "beq" || curr.op == "bne" ||
                          curr.op == "blt" || curr.op == "ble" ||
                          curr.op == "bgt" || curr.op == "bge" ||
                          curr.op == "addiu" || curr.op == "subiu") &&
                         prev.r.at(1) == 't' && prev.r == curr.a) {
                    had_change = true;
                    output.at(i) = Op::init(curr.op, curr.r, prev.a, curr.b);
                    output.erase(output.begin() + i-- - 1);
                }

                //    move tA, B
                //    beq tA, _, _
                // -> beq B, _, _
                else if (prev.op == "move" &&
                         (curr.op == "beq" || curr.op == "bne" ||
                          curr.op == "blt" || curr.op == "ble" ||
                          curr.op == "bgt" || curr.op == "bge") &&
                         prev.r.at(1) == 't' && prev.r == curr.r) {
                    had_change = true;
                    output.at(i) = Op::init(curr.op, prev.a, curr.a, curr.b);
                    output.erase(output.begin() + i-- - 1);
                }

                //    addu tA, _, _
                //    move B, tA
                // -> addu B, _, _
                else if ((prev.op == "addu" || prev.op == "subu" ||
                          prev.op == "and" || prev.op == "or") &&
                         curr.op == "move" && prev.r.at(1) == 't' &&
                         prev.r == curr.a) {
                    had_change = true;
                    output.at(i) = Op::init(prev.op, curr.r, prev.a, prev.b);
                    output.erase(output.begin() + i-- - 1);
                }

                //    addiu tA, _, _
                //    move C, tA
                // -> addiu C, _, _
                else if ((prev.op == "addiu" || prev.op == "subiu") &&
                         curr.op == "move" && prev.r.at(1) == 't' &&
                         prev.r == curr.a) {
                    had_change = true;
                    output.at(i) = Op::init(prev.op, curr.r, prev.a, prev.b);
                    output.erase(output.begin() + i-- - 1);
                }

                //    addu tA, B, $zero
                // -> move tA, B
                else if (prev.op == "addu" && prev.r.at(1) == 't' &&
                         prev.b == "$zero") {
                    had_change = true;
                    output.at(--i) = Op::init("move", prev.r, prev.a);
                }

                //    move tA, B
                //    move C, tA
                // -> move C, B
                else if (prev.op == "move" && curr.op == "move" &&
                         prev.r.at(1) == 't' && prev.r == curr.a) {
                    had_change = true;
                    output.at(i) = Op::init(curr.op, curr.r, prev.a);
                    output.erase(output.begin() + i-- - 1);
                }

                //    li tA, _
                //    move B, tA
                // -> li B, _
                else if (prev.op == "li" && curr.op == "move" &&
                         prev.r.at(1) == 't') {
                    had_change = true;
                    output.at(i) = Op::init(prev.op, curr.r, prev.a);
                    output.erase(output.begin() + i-- - 1);
                }

                //    slt tA, B, C
                //    beq tA, $zero, _
                // -> bgte B, C, _
                else if (prev.op == "slt" && curr.op == "beq" &&
                         prev.r.at(1) == 't' && prev.r == curr.r &&
                         curr.a == "$zero") {
                    had_change = true;
                    output.at(i) = Op::init("bge", prev.a, prev.b, curr.b);
                    output.erase(output.begin() + i-- - 1);
                }

                //    sgt tA, B, C
                //    beq tA, $zero, _
                // -> ble B, C, _
                else if (prev.op == "sgt" && curr.op == "beq" &&
                         prev.r.at(1) == 't' && prev.r == curr.r &&
                         curr.a == "$zero") {
                    had_change = true;
                    output.at(i) = Op::init("ble", prev.a, prev.b, curr.b);
                    output.erase(output.begin() + i-- - 1);
                }

                //    seq tA, B, C
                //    beq tA, $zero, _
                // -> bne B, C, _
                else if (prev.op == "seq" && curr.op == "beq" &&
                         prev.r.at(1) == 't' && prev.r == curr.r &&
                         curr.a == "$zero") {
                    had_change = true;
                    output.at(i) = Op::init("bne", prev.a, prev.b, curr.b);
                    output.erase(output.begin() + i-- - 1);
                }

                //    sne tA, B, C
                //    beq tA, $zero, _
                // -> beq B, C, _
                else if (prev.op == "sne" && curr.op == "beq" &&
                         prev.r.at(1) == 't' && prev.r == curr.r &&
                         curr.a == "$zero") {
                    had_change = true;
                    output.at(i) = Op::init("beq", prev.a, prev.b, curr.b);
                    output.erase(output.begin() + i-- - 1);
                }

                //    b A
                //    A:
                // -> A:
                else if (prev.op == "b" && curr.op == "" && prev.r == curr.r) {
                    had_change = true;
                    output.at(i) = Op::init(curr.op, curr.r);
                    output.erase(output.begin() + i-- - 1);
                }

                //    lw tA, _
                //    move B, tA
                // -> lw B, _
                else if ((prev.op == "lw" || prev.op == "lh" ||
                          prev.op == "lb") &&
                         curr.op == "move" && prev.r.at(1) == 't' &&
                         prev.r == curr.a) {
                    had_change = true;
                    output.at(i) = Op::init(prev.op, curr.r, prev.a, prev.b);
                    output.erase(output.begin() + i-- - 1);
                }

                //    move tA, B
                //    lw tC, (tA)
                // -> lw tC, (B)
                else if (prev.op == "move" &&
                         (curr.op == "lw" || curr.op == "lh" ||
                          curr.op == "lb") &&
                         prev.r.at(1) == 't' && prev.r == curr.b) {
                    had_change = true;
                    output.at(i) = Op::init(curr.op, curr.r, curr.a, prev.a);
                    output.erase(output.begin() + i-- - 1);
                }
            }
        } while (had_change);

        fmt::println("# ====== optimized {}", f.name);
    }

    void dump_output(ssir::Func const& f) {
        fmt::println("");
        fmt::println("# {}", f.type);
        fmt::println("{}:", f.name);

        for (auto const& op : output) {
            if (op.op == "lw" || op.op == "lh" || op.op == "lb" ||
                op.op == "sw" || op.op == "sh" || op.op == "sb") {
                fmt::println("    {} {}, {}({})", op.op, op.r, op.a, op.b);
            } else if (op.op == "addu" || op.op == "subu" || op.op == "mul" ||
                       op.op == "div" || op.op == "and" || op.op == "or" ||
                       op.op == "sne" || op.op == "seq" || op.op == "slt" ||
                       op.op == "sgt" || op.op == "beq" || op.op == "bne" ||
                       op.op == "blt" || op.op == "ble" || op.op == "bgt" ||
                       op.op == "bge" || op.op == "addiu" || op.op == "subiu") {
                fmt::println("    {} {}, {}, {}", op.op, op.r, op.a, op.b);
            } else if (op.op == "move" || op.op == "move " || op.op == "li" ||
                       op.op == "la") {
                fmt::println("    {} {}, {}", op.op, op.r, op.a);
            } else if (op.op == "jr" || op.op == "jal" || op.op == "b") {
                fmt::println("    {} {}", op.op, op.r);
            } else if (op.op == "" && op.r.at(0) == '.') {
                fmt::println("    {}:", op.r);
            } else if (op.op == "") {
                fmt::println("{}:", op.r);
            } else if (op.op == "#") {
                fmt::println("    # {}", op.r);
            } else {
                fmt::println("# op='{}', r='{}', a='{}', b='{}'", op.op, op.r,
                             op.a, op.b);
            }
        }
    }

    // ----------------------------------------------------------------------------

    [[nodiscard]] constexpr auto op_for_store(Type const& t) const
        -> std::string_view {
        return op_for_store(t.bytesize());
    }

    [[nodiscard]] constexpr auto op_for_store(size_t size) const
        -> std::string_view {
        std::string_view op;
        switch (size) {
            case 1: op = "sb"; break;
            case 2: op = "sh"; break;
            case 4: op = "sw"; break;
            default:
                er->report_bug({}, "invalid type size for store: {}", size);
                break;
        }

        return op;
    }

    [[nodiscard]] constexpr auto op_for_load(Type const& t) const
        -> std::string_view {
        return op_for_load(t.bytesize());
    }

    [[nodiscard]] constexpr auto op_for_load(size_t size) const
        -> std::string_view {
        // FIXME: handle signed/unsigned
        std::string_view op;
        switch (size) {
            case 1: op = "lb"; break;
            case 2: op = "lh"; break;
            case 4: op = "lw"; break;
            default:
                er->report_bug({}, "invalid type size load: {}", size);
                break;
        }

        return op;
    }

    // ----------------------------------------------------------------------------

    void add_op(std::string_view op, std::string_view r,
                std::string_view a = "", std::string_view b = "") {
        using S = std::string;
        output.push_back({S{op}, S{r}, S{a}, S{b}});
    }

    constexpr auto push_tmp() -> size_t { return stack_top++; }
    constexpr auto pop_tmp() -> size_t { return --stack_top; }

    constexpr auto push_local() -> size_t { return locals_top++; }
    constexpr auto pop_local() -> size_t { return --locals_top; }

    ErrorReporter*      er;
    ssir::Module const* m;

    std::vector<Op>         output;
    std::vector<Local>      locals;
    std::vector<MemRegions> mregions;

    size_t stack_top{reg_tmp_base};
    size_t locals_top{reg_loc_base};
    size_t stack_size{};

    bool is_not_leaf{};
};

struct Codegen {
    void codegen(ssir::Module const& m) {
        fmt::println(".set noreorder");
        fmt::println("");
        fmt::println(".data");

        for (auto const& [id, v] : m.statics) {
            if (v.align != 1) {
                er->report_bug(
                    {}, "alignment different from 1 not implemented, found {}",
                    v.align);
            }

            fmt::print("s_{}: .byte ", v.id);
            fmt::print("{}", fmt::join(v.data, ", "));
        }

        fmt::println("");

        for (auto const& [k, v] : m.globals) {
            // TODO: handle things that are not integers
            std::string_view ty = ".err";
            switch (v.type.bytesize()) {
                case 4: ty = ".word"; break;
                case 2: ty = ".half"; break;
                case 1: ty = ".byte"; break;
            }

            fmt::println("{}: {} {}", v.name, ty, v.initial_value);
        }

        fmt::println("");
        fmt::println(".text");
        fmt::println(".global _start");
        fmt::println("_start:");
        fmt::println("    jal main");
        fmt::println("    move $a0, $v0");
        fmt::println("    li $v0, 17");
        fmt::println("    syscall");

        for (auto const& [name, f] : m.asm_entries) {
            fmt::println("");
            fmt::println("# {}", f.type);
            fmt::println("{}:", f.name);

            for (auto const& line : f.body) {
                fmt::println("    {}", line);
            }
        }

        for (auto const& [name, f] : m.entries) {
            codegen_func(m, f);
        }
    }

    void codegen_func(ssir::Module const& m, ssir::Func const& f) {
        fmt::println(
            "# --------------------------------------------------------------");

        auto c = CodegenFunc{
            .er = er, .m = &m, .output = {}, .locals = {}, .mregions = {}};
        c.codegen(f, true);
    }

    ErrorReporter* er;
};

void codegen_stdout(ssir::Module const& m, ErrorReporter& er) {
    auto c = Codegen{.er = &er};

    c.codegen(m);
}

}  // namespace yuri::mips::v2
