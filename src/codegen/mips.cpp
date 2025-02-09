#include "mips.hpp"

#include <array>
#include <cstdint>
#include <limits>
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
    Addiu,
    Addu,
    Subu,
    Slt,
    Sltu,
    Lw,
    Sw,
    Jr,
};

struct Instr {
    Opcode  op;
    uint8_t r;
    uint8_t a{};
    uint8_t b{};
    int32_t value{};
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
            case T::Addiu: name = "addiu"; break;
            case T::Addu: name = "addu"; break;
            case T::Subu: name = "subu"; break;
            case T::Slt: name = "slt"; break;
            case T::Sltu: name = "sltu"; break;
            case T::Lw: name = "lw"; break;
            case T::Sw: name = "sw"; break;
            case T::Jr: name = "jr"; break;
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

            case yuri::mips::Opcode::Addiu:
                return fmt::format_to(ctx.out(), "{} {}, {}, {}", t.op,
                                      regs[t.r], regs[t.a], t.value);

            case yuri::mips::Opcode::Addu:
            case yuri::mips::Opcode::Subu:
            case yuri::mips::Opcode::Slt:
            case yuri::mips::Opcode::Sltu:
                return fmt::format_to(ctx.out(), "{} {}, {}, {}", t.op,
                                      regs[t.r], regs[t.a], regs[t.b]);

            case yuri::mips::Opcode::Lw:
            case yuri::mips::Opcode::Sw:
                return fmt::format_to(ctx.out(), "{} {}, {}({})", t.op,
                                      regs[t.r], t.value, regs[t.a]);

            case yuri::mips::Opcode::Jr:
                return fmt::format_to(ctx.out(), "{} {}", t.op, regs[t.r]);
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

struct CodegenFunc {
    static constexpr size_t reg_tmp_base = 8;
    static constexpr size_t reg_sp = 29;
    static constexpr size_t reg_ra = 31;

    // ------------------------------------------------------------------------

    void gen_local_offsets() { gen_block_offsets(func->last()); }

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
                auto const& name = std::get<std::string>(node.value);
                auto const  size = node.type.bytesize();

                auto const offset = stack_top;
                stack_top += size;

                locals.push_back(
                    {.name = name, .offset = offset, .size = size});
            } break;

            case AstNodeKind::Block: {
                codegen_block(node);
            } break;

            default: break;
        }
    }

    // ------------------------------------------------------------------------

    void codegen() {
        gen_local_offsets();

        auto const& body = func->last();
        codegen_body(body);
    }

    void codegen_body(AstNode const& node) {
        auto stack_frame_size = ALIGN(stack_top);

        out({
            .op = Opcode::Addiu,
            .r = reg_sp,
            .a = reg_sp,
            .b = 0,  // unused
            .value = -static_cast<uint16_t>(stack_frame_size),
        });

        codegen_block(node);

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
        switch (node.kind) {
            case AstNodeKind::VarDecl: {
                auto const& name = std::get<std::string>(node.value);
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

            case AstNodeKind::ExprStmt:
            case AstNodeKind::ReturnStmt:
            case AstNodeKind::IfStmt:
            case AstNodeKind::WhileStmt:
                er->report_error(node.span, "not implemented: {}", node);
                break;

            case AstNodeKind::Assign: {
                auto lhs = node.first();
                if (lhs.kind != AstNodeKind::Id) {
                    er->report_error(node.span, "left is not an lvalue: {}",
                                     lhs);
                    return;
                }

                auto const& name = std::get<std::string>(lhs.value);
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
                    out({.op = Opcode::Slt, .r = r, .a = b, .b = a});
                });
                break;

            case AstNodeKind::GreaterThanEqual:
                er->report_error(node.span, "not implemented: {}", node);
                break;

            case AstNodeKind::Equal:
            case AstNodeKind::Call:
                er->report_error(node.span, "not implemented: {}", node);
                break;

            case AstNodeKind::Id: {
                auto const& name = std::get<std::string>(node.value);
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
                auto const& v = std::get<uint64_t>(node.value);
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

    auto lookup_local(std::string_view name) const -> Local const* {
        for (auto const& l : locals) {
            if (l.name == name) return &l;
        }

        return nullptr;
    }

    constexpr auto sp_offset(uint32_t offset) const -> int32_t {
        auto word_size = 4;  // 32bit words
        return stack_top - word_size - offset;
    }

    constexpr auto pop_tmp() -> uint8_t { return --reg_top + reg_tmp_base; }
    constexpr auto push_tmp() -> uint8_t { return reg_top++ + reg_tmp_base; }
    void           out(Instr const i) { body.push_back(i); }

    // ------------------------------------------------------------------------

    AstNode const* func;

    ErrorReporter*     er;
    std::vector<Instr> body{};
    std::vector<Local> locals{};

    uint32_t stack_top{};
    uint8_t  reg_top{};
};

struct Codegen {
    void codegen_source_file(AstNode const& node) {
        if (node.kind != AstNodeKind::SourceFile) {
            er->report_error(node.span, "expected source file, got {}", node);
            return;
        }

        for (auto const& decl : node.children) {
            auto c = CodegenFunc{.func = &decl, .er = er};
            c.codegen();

            for (auto const& o : c.body) {
                fmt::println("  {}", o);
            }
        }
    }

    auto pop() -> uint8_t { return --top; }
    auto push() -> uint8_t { return top++; }

    void out(Instr const i) { output.push_back(i); }

    ErrorReporter* er;

    uint8_t            top{};
    std::vector<Instr> output{};
};

void codegen(AstNode const& node, FILE* out, ErrorReporter& er) {
    auto c = Codegen{.er = &er};
    c.codegen_source_file(node);
}

}  // namespace yuri::mips
