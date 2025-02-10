#include "mips.hpp"

#include <array>
#include <cstdint>
#include <limits>
#include <span>
#include <string_view>
#include <vector>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "fmt/base.h"

// round to 8bytes
#define ALIGN(addr) ((addr) + (8 - 1)) & -8

namespace yuri::mips {

enum class Opcode : uint8_t {
    Err,
    Li,
    Move,
    Addiu,
    Addu,
    Subu,
    Seq,
    Slt,
    Sltu,
    Sgt,
    Sgtu,
    Lw,
    Sw,
    J,
    Jal,
    Jr,
    B,
    Beq,
    Bne,
};

struct Instr {
    Opcode      op;
    uint8_t     r;
    uint8_t     a{};
    uint8_t     b{};
    int32_t     value{};
    std::string name{};
};

struct Label {
    uint32_t id;
    uint32_t offset;
};

}  // namespace yuri::mips

template <>
struct fmt::formatter<yuri::mips::Opcode> : formatter<string_view> {
    auto format(yuri::mips::Opcode c, format_context& ctx) const
        -> format_context::iterator {
        using T = yuri::mips::Opcode;

        string_view name = "unknown";
        switch (c) {
            case T::Err: name = "Err"; break;
            case T::Li: name = "li"; break;
            case T::Move: name = "move"; break;
            case T::Addiu: name = "addiu"; break;
            case T::Addu: name = "addu"; break;
            case T::Subu: name = "subu"; break;
            case T::Seq: name = "seq"; break;
            case T::Slt: name = "slt"; break;
            case T::Sltu: name = "sltu"; break;
            case T::Sgt: name = "sgt"; break;
            case T::Sgtu: name = "sgtu"; break;
            case T::Lw: name = "lw"; break;
            case T::Sw: name = "sw"; break;
            case T::J: name = "j"; break;
            case T::Jal: name = "jal"; break;
            case T::Jr: name = "jr"; break;
            case T::B: name = "b"; break;
            case T::Beq: name = "beq"; break;
            case T::Bne: name = "bne"; break;
        }
        return formatter<string_view>::format(name, ctx);
    }
};

template <>
struct fmt::formatter<yuri::mips::Instr> {
    static constexpr std::array<std::string_view, 32> regs{
        "$zero", "$at", "$v0", "$v1", "$a0", "$a1", "$a2", "$a3",
        "$t0",   "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7",
        "$s0",   "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7",
        "$t8",   "$t9", "$k0", "$k1", "$gp", "$sp", "$fp", "$ra",
    };

    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yuri::mips::Instr t, format_context& ctx) const
        -> format_context::iterator {
        switch (t.op) {
            case yuri::mips::Opcode::Err:
                return fmt::format_to(ctx.out(), "{{Err}}");

            case yuri::mips::Opcode::Li:
                return fmt::format_to(ctx.out(), "{} {}, {}", t.op, regs[t.r],
                                      t.value);

            case yuri::mips::Opcode::Move:
                return fmt::format_to(ctx.out(), "{} {}, {}", t.op, regs[t.r],
                                      regs[t.a]);

            case yuri::mips::Opcode::Addiu:
                return fmt::format_to(ctx.out(), "{} {}, {}, {}", t.op,
                                      regs[t.r], regs[t.a], t.value);

            case yuri::mips::Opcode::Addu:
            case yuri::mips::Opcode::Subu:
            case yuri::mips::Opcode::Seq:
            case yuri::mips::Opcode::Slt:
            case yuri::mips::Opcode::Sltu:
            case yuri::mips::Opcode::Sgt:
            case yuri::mips::Opcode::Sgtu:
                return fmt::format_to(ctx.out(), "{} {}, {}, {}", t.op,
                                      regs[t.r], regs[t.a], regs[t.b]);

            case yuri::mips::Opcode::Lw:
            case yuri::mips::Opcode::Sw:
                return fmt::format_to(ctx.out(), "{} {}, {}({})", t.op,
                                      regs[t.r], t.value, regs[t.a]);

            case yuri::mips::Opcode::J:
            case yuri::mips::Opcode::Jal:
                return fmt::format_to(ctx.out(), "{} {}", t.op, t.name);
            case yuri::mips::Opcode::Jr:
                return fmt::format_to(ctx.out(), "{} {}", t.op, regs[t.r]);

            case yuri::mips::Opcode::B:
                return fmt::format_to(ctx.out(), "{} .label_{}", t.op, t.value);

            case yuri::mips::Opcode::Beq:
            case yuri::mips::Opcode::Bne:
                return fmt::format_to(ctx.out(), "{} {}, {}, .label_{}", t.op,
                                      regs[t.a], regs[t.b], t.value);
        }

        return fmt::format_to(ctx.out(), "{{{}}}", t.op);
    }
};

namespace yuri::mips {

struct Local {
    std::string name;
    uint32_t    offset;
    uint32_t    size;
};

struct Func {
    std::string name;
    uint32_t    stack_size;

    std::vector<Instr> body;
    std::vector<Label> labels;
};

struct CodegenFunc {
    static constexpr size_t reg_tmp_base = 8;
    static constexpr size_t reg_v0 = 2;
    static constexpr size_t reg_sp = 29;
    static constexpr size_t reg_ra = 31;

    static constexpr size_t reg_args_base = 4;

    static constexpr size_t word_size = 4;  // 32bit words

    // ------------------------------------------------------------------------

    auto codegen() -> Func {
        gen_local_offsets();
        codegen_body(func->last());

        auto const& name = func->value_string();
        return {.name = name,
                .stack_size = stack_top,
                .body = body,
                .labels = labels};
    }

    void codegen_body(AstNode const& node) {
        auto stack_frame_size = ALIGN(stack_top);
        ret_label = gen_label();

        out({
            .op = Opcode::Addiu,
            .r = reg_sp,
            .a = reg_sp,
            .b = 0,  // unused
            .value = -static_cast<uint16_t>(stack_frame_size),
        });

        auto ra_var = locals.at(0);
        if (ra_var.name != "") {
            er->report_error(node.span,
                             "something is very wrong, `$ra` is not in the "
                             "implicit first local");
            return;
        }

        out({
            .op = Opcode::Sw,
            .r = reg_ra,
            .a = reg_sp,
            .value = sp_offset(ra_var.offset),
        });

        std::span args = func->children;
        args = args.subspan(0, args.size() - 2);

        size_t i{};
        for (auto const& arg : args) {
            if (arg.kind != AstNodeKind::FuncDeclArg) {
                er->report_error(arg.span, "expected function argument, got {}",
                                 arg);
                continue;
            }

            auto l = lookup_local(arg.value_string());

            out({
                .op = Opcode::Sw,
                .r = static_cast<uint8_t>(reg_args_base + i),
                .a = reg_sp,
                .value = sp_offset(l->offset),
            });

            i++;
        }

        codegen_block(node);

        add_label(ret_label);

        out({
            .op = Opcode::Lw,
            .r = reg_ra,
            .a = reg_sp,
            .value = sp_offset(ra_var.offset),
        });

        out({
            .op = Opcode::Addiu,
            .r = reg_sp,
            .a = reg_sp,
            .b = 0,  // unused
            .value = static_cast<uint16_t>(stack_frame_size),
        });

        out({.op = Opcode::Jr, .r = reg_ra});
    }

    void codegen_block(AstNode const& node) {
        // er->report_note(node.span, "codegen_block({})", node);

        if (node.kind != AstNodeKind::Block) {
            er->report_error(node.span,
                             "expected block for function body, got {}", node);
            return;
        }

        for (auto const& stmt : node.children) {
            codegen_stmt(stmt);
        }
    }

    void codegen_stmt(AstNode const& node) {
        // er->report_note(node.span, "codegen_stmt({})", node);

        switch (node.kind) {
            case AstNodeKind::VarDecl: {
                auto const& name = node.value_string();
                auto        local = lookup_local(name);
                if (!local) {
                    er->report_error(node.span, "undefined name: {}", name);
                    return;
                }

                codegen_expr(node.second());
                auto e = pop_tmp();

                if (node.type.bytesize() > 0) {
                    out({.op = Opcode::Sw,
                         .r = e,
                         .a = reg_sp,
                         .value = sp_offset(local->offset)});
                }
            } break;

            case AstNodeKind::Block: {
                codegen_block(node);
            } break;
            case AstNodeKind::ExprStmt: {
                codegen_expr(node.first());
                pop_tmp();
            } break;
            case AstNodeKind::ReturnStmt: {
                codegen_expr(node.first());
                out({.op = Opcode::Move, .r = reg_v0, .a = pop_tmp()});

                out({.op = Opcode::B,
                     .r = 0,  // unused
                     .value = static_cast<int32_t>(ret_label)});
            } break;

            case AstNodeKind::IfStmt:
                er->report_error(node.span, "not implemented: {}", node);
                break;

            case AstNodeKind::WhileStmt: {
                auto cond_label = gen_label();
                auto end_label = gen_label();
                add_label(cond_label);

                codegen_expr(node.first());
                auto cond = pop_tmp();
                out({.op = Opcode::Beq,
                     .r = 0,
                     .a = cond,
                     .b = 0,
                     .value = static_cast<int32_t>(end_label)});

                codegen_block(node.second());
                out({.op = Opcode::B,
                     .r = 0,  // unused
                     .value = static_cast<int32_t>(cond_label)});

                add_label(end_label);
            } break;

            case AstNodeKind::Assign: {
                auto lhs = node.first();
                if (lhs.kind != AstNodeKind::Id) {
                    er->report_error(node.span, "left is not an lvalue: {}",
                                     lhs);
                    return;
                }

                auto const& name = lhs.value_string();
                auto        local = lookup_local(name);
                if (!local) {
                    er->report_error(node.span, "undefined name: {}", name);
                    return;
                }

                codegen_expr(node.second());
                auto e = pop_tmp();

                if (lhs.type.bytesize() == 0) {
                    er->report_error(node.span, "lhs has zero-width type: {}",
                                     lhs.type);
                    return;
                }

                out({.op = Opcode::Sw,
                     .r = e,
                     .a = reg_sp,
                     .value = sp_offset(local->offset)});
            } break;

            case AstNodeKind::Err:
            case AstNodeKind::Nil:
            case AstNodeKind::SourceFile:
            case AstNodeKind::Func:
            case AstNodeKind::FuncDeclArg:
            case AstNodeKind::Add:
            case AstNodeKind::Sub:
            case AstNodeKind::Mul:
            case AstNodeKind::Div:
            case AstNodeKind::LessThan:
            case AstNodeKind::LessThanEqual:
            case AstNodeKind::GreaterThan:
            case AstNodeKind::GreaterThanEqual:
            case AstNodeKind::Equal:
            case AstNodeKind::Call:
            case AstNodeKind::Id:
            case AstNodeKind::Int:
                er->report_error(node.span,
                                 "found unexpected node in codegen stmt: {}",
                                 node);
        }
    }

    void codegen_expr(AstNode const& node) {
        // er->report_note(node.span, "codegen_expr({})", node);

        auto const binop = [&](auto&& out) {
            codegen_expr(node.first());
            codegen_expr(node.second());

            auto rhs = pop_tmp();
            auto lhs = pop_tmp();
            auto ret = push_tmp();

            out(ret, lhs, rhs);
        };

        switch (node.kind) {
            case AstNodeKind::Add:
                binop([&](auto r, auto a, auto b) {
                    out({.op = Opcode::Addu, .r = r, .a = a, .b = b});
                });
                break;
            case AstNodeKind::Sub:
                binop([&](auto r, auto a, auto b) {
                    out({.op = Opcode::Subu, .r = r, .a = a, .b = b});
                });
                break;

            case AstNodeKind::Mul:
            case AstNodeKind::Div:
                er->report_error(node.span, "not implemented: {}", node);
                break;

            case AstNodeKind::LessThan:
                // TODO: signed vs unsigned?
                binop([&](auto r, auto a, auto b) {
                    out({.op = Opcode::Slt, .r = r, .a = a, .b = b});
                });
                break;

            case AstNodeKind::LessThanEqual:
                er->report_error(node.span, "not implemented: {}", node);
                break;

            case AstNodeKind::GreaterThan:
                // TODO: signed vs unsigned?
                binop([&](auto r, auto a, auto b) {
                    out({.op = Opcode::Sgt, .r = r, .a = a, .b = b});
                });
                break;

            case AstNodeKind::GreaterThanEqual:
                er->report_error(node.span, "not implemented: {}", node);
                break;

            case AstNodeKind::Equal:
                binop([&](auto r, auto a, auto b) {
                    out({.op = Opcode::Seq, .r = r, .a = a, .b = b});
                });
                break;

            case AstNodeKind::Call: {
                if (node.first().kind != AstNodeKind::Id) {
                    er->report_error(node.span, "can't call: {}", node.first());
                    return;
                }

                auto const& name = node.first().value_string();

                Func const* found = nullptr;
                for (auto const& f : all_funcs) {
                    if (f.name == name) {
                        found = &f;
                        break;
                    }
                }

                if (!found) {
                    er->report_error(node.span, "undefined function: {}", name);
                    return;
                }

                std::span args = node.children;
                args = args.subspan(1);
                if (args.size() > 4) {
                    er->report_error(node.span,
                                     "can't call functions with more than 4 "
                                     "arguments (got {})",
                                     args.size());
                    return;
                }

                for (auto const& arg : args) {
                    codegen_expr(arg);
                }

                for (size_t i = 0; i < args.size(); i++) {
                    auto v = pop_tmp();
                    out({.op = Opcode::Move,
                         .r = static_cast<uint8_t>(reg_args_base + i),
                         .a = v});
                }

                if (reg_top) {
                    er->report_error(
                        node.span,
                        "have registers that could be lost: reg_top={}",
                        reg_top);
                }

                out({.op = Opcode::Jal, .r = 0, .name = found->name});

                auto ret = push_tmp();  // the return value
                out({.op = Opcode::Move, .r = ret, .a = reg_v0});
            } break;

            case AstNodeKind::Id: {
                auto const& name = node.value_string();
                auto        local = lookup_local(name);
                if (!local) {
                    er->report_error(node.span, "undefined name: {}", name);
                    return;
                }

                auto r = push_tmp();

                out({.op = Opcode::Lw,
                     .r = r,
                     .a = reg_sp,
                     .value = sp_offset(local->offset)});
            } break;

            case AstNodeKind::Int: {
                auto const& v = node.value_int();
                if (v > std::numeric_limits<uint16_t>::max()) {
                    er->report_error(
                        node.span,
                        "literal '{}' is to large for codegen to handle", v);
                    break;
                }

                auto ret = push_tmp();
                out({.op = Opcode::Li,
                     .r = ret,
                     .value = static_cast<uint16_t>(v)});
            } break;

            case AstNodeKind::Err:
            case AstNodeKind::Nil:
            case AstNodeKind::SourceFile:
            case AstNodeKind::Func:
            case AstNodeKind::FuncDeclArg:
            case AstNodeKind::VarDecl:
            case AstNodeKind::Block:
            case AstNodeKind::ExprStmt:
            case AstNodeKind::ReturnStmt:
            case AstNodeKind::IfStmt:
            case AstNodeKind::WhileStmt:
            case AstNodeKind::Assign:
                er->report_error(node.span,
                                 "found unexpected node in codegen expr: {}",
                                 node);
        }
    }

    // ------------------------------------------------------------------------

    void add_local(std::string const& name, uint32_t size) {
        auto const offset = stack_top;
        stack_top += size;
        locals.push_back({.name = name, .offset = offset, .size = size});
    }

    void gen_local_offsets() {
        // add a place to put our return address. The name is empty, so it can
        // never be matched by real variables
        add_local("", word_size);

        std::span args = func->children;
        args = args.subspan(0, args.size() - 2);
        for (auto const& arg : args) {
            if (arg.kind != AstNodeKind::FuncDeclArg) {
                er->report_error(arg.span, "expected function argument, got {}",
                                 arg);
                continue;
            }

            add_local(arg.value_string(), arg.type.bytesize());
        }

        gen_block_offsets(func->last());
    }

    void gen_block_offsets(AstNode const& block) {
        if (block.kind != AstNodeKind::Block) {
            er->report_error(block.span, "expected block, got {}", block);
            return;
        }

        for (auto const& stmt : block.children) {
            gen_stmt_offsets(stmt);
        }
    };

    void gen_stmt_offsets(AstNode const& node) {
        switch (node.kind) {
            case AstNodeKind::VarDecl: {
                auto const& name = node.value_string();
                auto const  size = node.type.bytesize();
                add_local(name, size);
            } break;

            case AstNodeKind::Block: {
                codegen_block(node);
            } break;

            default: break;
        }
    }

    // ------------------------------------------------------------------------

    auto lookup_local(std::string_view name) const -> Local const* {
        for (auto const& l : locals) {
            if (l.name == name) return &l;
        }

        return nullptr;
    }

    // NOTE: this is here just as a hook to allow easy change of how we locate
    // stack variables
    constexpr auto sp_offset(uint32_t offset) const -> int32_t {
        return offset;
    }

    constexpr auto pop_tmp() -> uint8_t { return --reg_top + reg_tmp_base; }
    constexpr auto push_tmp() -> uint8_t { return reg_top++ + reg_tmp_base; }
    void           out(Instr const i) { body.push_back(i); }

    constexpr auto gen_label() -> uint32_t { return next_label++; }
    void           add_label(uint32_t id) {
        labels.push_back(
            {.id = id, .offset = static_cast<uint32_t>(body.size())});
    }

    // ------------------------------------------------------------------------

    AstNode const*        func;
    std::span<Func const> all_funcs;

    ErrorReporter*     er;
    std::vector<Instr> body{};
    std::vector<Label> labels{};
    std::vector<Local> locals{};

    uint32_t ret_label{};
    uint32_t next_label{};
    uint32_t stack_top{};
    uint8_t  reg_top{};
};

struct Codegen {
    void codegen_source_file(AstNode const& node) {
        if (node.kind != AstNodeKind::SourceFile) {
            er->report_error(node.span, "expected source file, got {}", node);
            return;
        }

        std::vector<Func> funcs;

        for (auto const& decl : node.children) {
            auto c = CodegenFunc{.func = &decl, .all_funcs = funcs, .er = er};
            funcs.push_back(c.codegen());
        }

        for (auto const& f : funcs) {
            fmt::println("{}:", f.name);

            uint32_t i{};
            uint32_t j{};
            for (auto const& o : f.body) {
                if (j < f.labels.size() && f.labels.at(j).offset == i) {
                    fmt::println("  .label_{}:", f.labels.at(j).id);
                    j++;
                }

                fmt::println("  {}", o);

                i++;
            }

            fmt::println("# end of {}", f.name);
        }
    }

    ErrorReporter* er;

    uint8_t            top{};
    std::vector<Instr> output{};
};

void codegen(AstNode const& node, FILE* out, ErrorReporter& er) {
    auto c = Codegen{.er = &er};
    c.codegen_source_file(node);
}

}  // namespace yuri::mips
