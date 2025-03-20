#include "codegen.hpp"

#include <array>
#include <cstdint>
#include <cstdio>
#include <string_view>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "fmt/base.h"

using fmt::println;

namespace yuri {

struct Reg {
    uint8_t n;
};

static constexpr std::array regs{
    "zero", "at", "v0", "v1", "a0", "a1", "a2", "a3", "t0", "t1", "t2",
    "t3",   "t4", "t5", "t6", "t7", "s0", "s1", "s2", "s3", "s4", "s5",
    "s6",   "s7", "t8", "t9", "k0", "k1", "gp", "sp", "fp", "ra",
};

}  // namespace yuri

template <>
struct fmt::formatter<yuri::Reg> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yuri::Reg n, format_context& ctx) const
        -> format_context::iterator {
        return fmt::format_to(ctx.out(), "${}", yuri::regs[n.n]);
    }
};

namespace yuri {

struct Codegen {
    void preamble() const {
        println(out, ".set noreorder");
        println(out, "");
    }

    void codegen_func(AstNode const& n) {
        println(out, ".global _start");
        println(out, "_start:");

        codegen_stmt(n);

        println(out, "_start.end:");
        println(out, "    move $a0, $v0");
        println(out, "    li $v0, 10");
        println(out, "    syscall");
    }

    void codegen_stmt(AstNode const& n) {
        switch (n.kind) {
            case AstNodeKind::ReturnStmt: {
                codegen_expr(n.children.at(0));
                auto r = pop_reg();

                println(out, "    move $v0, {}", r);
                println(out, "    b _start.end");
            } break;

            default:
                throw std::runtime_error{
                    fmt::format("invalid node in codegen_stmt: {}", n.kind)};
        }
    }

    void codegen_expr(AstNode const& n) {
        switch (n.kind) {
            case AstNodeKind::Add:
            case AstNodeKind::Sub:
            case AstNodeKind::Mul:
            case AstNodeKind::Div: {
                codegen_expr(n.children.at(0));
                codegen_expr(n.children.at(1));

                auto rhs = pop_reg();
                auto lhs = pop_reg();

                std::string_view op;
                switch (n.kind) {
                    case AstNodeKind::Add: op = "add"; break;
                    case AstNodeKind::Sub: op = "sub"; break;
                    case AstNodeKind::Mul: op = "mul"; break;
                    case AstNodeKind::Div: op = "div"; break;
                    default: __builtin_unreachable();
                }

                auto o = push_reg();
                println(out, "    {} {}, {}, {}", op, o, lhs, rhs);
            } break;

            case AstNodeKind::Neg: {
                codegen_expr(n.children.at(0));
                auto r = pop_reg();
                auto o = push_reg();
                println(out, "    sub {}, $zero, {}", o, r);
            } break;

            case AstNodeKind::Int: {
                auto r = push_reg();
                println(out, "    li {}, {}", r, n.value_uint64());
            } break;

            case AstNodeKind::Id:

            default:
                throw std::runtime_error{
                    fmt::format("invalid node in codegen_expr: {}", n.kind)};
        }
    }

    // ========================================================================

    auto push_reg() -> Reg { return {stack_top++}; }
    auto pop_reg() -> Reg { return {--stack_top}; }

    // ========================================================================

    uint8_t stack_top = 8;

    FILE*          out;
    ErrorReporter* er;
};

void codegen(AstNode const& n, FILE* out, ErrorReporter& er) {
    auto codegen = Codegen{.out = out, .er = &er};

    codegen.preamble();
    codegen.codegen_func(n);
}

}  // namespace yuri
