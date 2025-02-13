#include "mips2.hpp"

#include <cstddef>
#include <vector>

#include "codegen/ssir.hpp"
#include "error_reporter.hpp"
#include "fmt/base.h"
#include "fmt/format.h"

// round to 8bytes
#define ALIGN(addr) ((addr) + (8 - 1)) & -8

namespace yuri::mips::v2 {

static constexpr size_t reg_args_base = 4;
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

struct CodegenFunc {
    struct Local {
        size_t offset;
        size_t idx;
        size_t size;
    };

    void codegen(ssir::Func const& f) {
        process(f);

        codegen_pramble(f);
        codegen_body(f);
        codegen_postamble(f);
    }

    void process(ssir::Func const& f) {
        auto const& b = f.body;

        size_t count{};

        std::span args = f.type.inner;
        args = args.subspan(0, args.size() - 1);
        for (auto const& arg : args) {
            auto l = Local{.offset = count * arg.bytesize(),
                           .idx = count,
                           .size = arg.bytesize()};

            locals.push_back(l);
            count++;
        }

        for (size_t i = 0; i < b.text.size(); i++) {
            auto c = b.opcode_at(i);

            switch (c) {
                case ssir::Opcode::Local: {
                    auto sz = f.body.text_at(++i);
                    auto l =
                        Local{.offset = count * sz, .idx = count, .size = sz};

                    // er->report_note(b.span_for(i), "found local: {},
                    // offset={}", count, l.offset);
                    locals.push_back(l);

                    count++;
                } break;
                case ssir::Opcode::Li:
                case ssir::Opcode::Get: i++; break;
                default: break;
            }
        }
    }

    void codegen_body(ssir::Func const& f) {
        auto const& b = f.body;

        auto binop = [&](std::string_view op) {
            auto rhs = pop_tmp();
            auto lhs = pop_tmp();
            auto dst = push_tmp();

            fmt ::println("    {} {}, {}, {}", op, regs[dst], regs[lhs],
                          regs[rhs]);
        };

        for (size_t i = 0; i < b.text.size(); i++) {
            auto c = b.opcode_at(i);

            switch (c) {
                case ssir::Opcode::Local: {
                    auto sz = f.body.text_at(++i);

                    auto dst = push_local();
                    auto src = pop_tmp();

                    fmt::println("    move {}, {}", regs[dst], regs[src]);
                } break;

                case ssir::Opcode::Li: {
                    auto idx = f.body.text_at(++i);
                    auto v = f.body.const_at(idx);
                    auto dst = push_tmp();

                    fmt::println("    li {}, {}", regs[dst], v);
                } break;

                case ssir::Opcode::Get: {
                    auto slot = f.body.text_at(++i);
                    auto dst = push_tmp();

                    fmt ::println("    move {}, {}", regs[dst],
                                  regs[reg_loc_base + slot]);
                } break;

                case ssir::Opcode::Add: binop("addu"); break;
                case ssir::Opcode::Sub: binop("subu"); break;

                case ssir::Opcode::Ret: {
                    auto v = pop_tmp();
                    fmt::println("    move $v0, {}", regs[v]);
                    fmt::println("    b .ret");
                } break;

                default:
                    er->report_bug(b.span_for(i),
                                   "invalid opcode found in codegen: {}",
                                   fmt::underlying(c));
                    break;
            }
        }
    }

    void codegen_pramble(ssir::Func const& f) {
        fmt::println("");
        fmt::println("# {}", f.type);
        fmt::println("{}:", f.name);

        fmt::println("    subu $sp, $sp, {}", ALIGN(locals.size() * word_size));

        auto argc = f.type.inner.size() - 1;

        size_t i{};
        for (auto const& local : locals) {
            fmt::println("    sw {}, {}($sp)", regs[reg_loc_base + i],
                         local.offset);

            if (i < argc) {
                fmt::println("    move {}, {}", regs[reg_loc_base + i],
                             regs[reg_args_base + i]);
            }

            i++;
        }
    }

    void codegen_postamble(ssir::Func const& f) {
        fmt::println("    .ret:");

        size_t i{};
        for (auto const& local : locals) {
            fmt::println("    lw {}, {}($sp)", regs[reg_loc_base + i],
                         local.offset);

            i++;
        }

        fmt::println("    addu $sp, $sp, {}", ALIGN(locals.size() * word_size));

        fmt::println("    jr $ra");
        fmt::println("    # end of {}", f.name);
    }

    constexpr auto push_tmp() -> size_t { return stack_top++; }
    constexpr auto pop_tmp() -> size_t { return --stack_top; }

    constexpr auto push_local() -> size_t { return locals_top++; }
    constexpr auto pop_local() -> size_t { return --locals_top; }

    ErrorReporter* er;

    std::vector<Local> locals{};

    size_t stack_top{reg_tmp_base};
    size_t locals_top{reg_loc_base};
};

struct Codegen {
    void codegen(ssir::Module const& m) {
        for (auto const& [name, f] : m.entries) {
            codegen_func(f);
        }
    }

    void codegen_func(ssir::Func const& f) {
        auto c = CodegenFunc{.er = er};
        c.codegen(f);
    }

    ErrorReporter* er;
};

void codegen_stdout(ssir::Module const& m, ErrorReporter& er) {
    auto c = Codegen{.er = &er};
    c.codegen(m);
}

}  // namespace yuri::mips::v2
