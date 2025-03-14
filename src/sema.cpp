#include "sema.hpp"

#include <cstdint>
#include <cstdio>
#include <ranges>
#include <string_view>
#include <vector>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "fmt/base.h"
#include "fmt/ranges.h"
#include "ir.hpp"
#include "libassert/assert.hpp"
#include "ssa/cfold.hpp"
#include "ssa/check.hpp"
#include "ssa/dcelim.hpp"

namespace yuri {

struct Decl {
    std::string name;
    ir::Inst*   val;

    uint16_t block;
    uint16_t generation;
};

struct Env {
    constexpr auto child() -> Env { return {.parent = this, .decls = {}}; }

    void define_assign(Decl const& decl, ir::Inst* val, uint16_t block) {
        define(decl.name, val, block, decl.generation + 1);
    }

    void define(std::string name, ir::Inst* val, uint16_t block, uint16_t gen) {
        decls.push_back({
            .name = name,
            .val = val,
            .block = block,
            .generation = gen,
        });
    }

    [[nodiscard]] auto lookup(std::string_view name) -> Decl* {
        for (auto& d : decls | std::ranges::views::reverse) {
            if (d.name == name) return &d;
        }

        return parent ? parent->lookup(name) : nullptr;
    }

    Env*              parent{};
    std::vector<Decl> decls;
};

struct Context {};

struct Sema {
    void sema_func(Context const& ctx, Env& env, AstNode const& node) {
        // NOTE: do not get this from the error reporter
        fn.name = er->get_source_path();
        fn.blocks.emplace_back();

        sema_block(ctx, env, node);
    }

    void sema_stmt(Context const& ctx, Env& env, AstNode const& node) {
        switch (node.kind) {
            case AstNodeKind::Err:
                er->report_error(node.span, "found error node in sema");
                break;

            case AstNodeKind::Empty:
                er->report_error(node.span, "found error node in sema");
                break;

            case AstNodeKind::VarDecl: sema_var_decl(ctx, env, node); break;
            case AstNodeKind::ReturnStmt:
                sema_return_stmt(ctx, env, node);
                break;
            case AstNodeKind::Assign: sema_assign(ctx, env, node); break;

            case AstNodeKind::ExprStmt:
            case AstNodeKind::Block:

            default: PANIC("invalid node for `sema_stmt`", node.kind);
        }
    }

    auto sema_expr(Context const& ctx, Env& env, AstNode const& node)
        -> ir::Inst* {
        switch (node.kind) {
            case AstNodeKind::Int:
                return push_inst_const(ir::InstType::Word, node.value_uint64());

            case AstNodeKind::Add:
            case AstNodeKind::Sub:
            case AstNodeKind::Mul:
            case AstNodeKind::Div: {
                auto lhs = sema_expr(ctx, env, *node.first());
                auto rhs = sema_expr(ctx, env, *node.second());

                auto k = ir::InstKind::Add;
                switch (node.kind) {
                    case AstNodeKind::Add: k = ir::InstKind::Add; break;
                    case AstNodeKind::Sub: k = ir::InstKind::Sub; break;
                    case AstNodeKind::Mul: k = ir::InstKind::Imul; break;
                    case AstNodeKind::Div: k = ir::InstKind::Idiv; break;
                    default:
                        UNREACHABLE("invalid node in sema_expr", node.kind);
                }

                return push_inst(k, lhs->type, lhs, rhs);
            }

            case AstNodeKind::Neg: {
                auto child = sema_expr(ctx, env, *node.first());
                auto zero = push_inst_const(child->type, 0);
                return push_inst(ir::InstKind::Sub, child->type, zero, child);
            }

            case AstNodeKind::Id: {
                auto v = env.lookup(node.value_string());
                if (!v) {
                    er->report_error(node.span, "undefined identifier: '{}'",
                                     node.value_string());
                    return push_inst(ir::InstKind::Err, ir::InstType::Err);
                }

                return v->val;
            }

            default: PANIC("invalid node for `sema_expr`", node.kind);
        }
    }

    // ------------------------------------------------------------------------

    void sema_block(Context const& ctx, Env& env_, AstNode const& node) {
        ASSERT(node.kind == AstNodeKind::Block);
        auto env = env_.child();

        for (auto const& stmt : node.children) {
            sema_stmt(ctx, env_, stmt);
        }
    }

    // NOLINTNEXTLINE(readability-function-cognitive-complexity)
    void sema_var_decl(Context const& ctx, Env& env, AstNode const& node) {
        ASSERT(node.kind == AstNodeKind::VarDecl);
        ASSERT(node.children.at(0).kind == AstNodeKind::Empty,
               "explicit type in decl has not been implemented");

        auto init = sema_expr(ctx, env, node.children.at(1));
        env.define(std::string{node.value_string()}, init, current_block, 0);
    }

    // NOLINTNEXTLINE(readability-function-cognitive-complexity)
    void sema_assign(Context const& ctx, Env& env, AstNode const& node) {
        ASSERT(node.kind == AstNodeKind::Assign);
        ASSERT(node.first()->is_lvalue());

        if (node.first()->kind == AstNodeKind::Id) {
            auto decl = env.lookup(node.first()->value_string());
            if (!decl) {
                er->report_error(node.span, "undefined identifier: '{}'",
                                 node.value_string());
                return;
            }

            // FIXME: this is where we might need to handle phis
            ASSERT(decl->block == current_block);

            auto rhs = sema_expr(ctx, env, *node.second());

            // an assign just shadows the previous declaration
            env.define_assign(*decl, rhs, current_block);
            return;
        }

        UNREACHABLE("node kind not handled as lhs", node.first()->kind);
    }

    void sema_return_stmt(Context const& ctx, Env& env, AstNode const& node) {
        ASSERT(node.kind == AstNodeKind::ReturnStmt);

        auto child = sema_expr(ctx, env, node.children.at(0));
        push_inst(ir::InstKind::Ret, child->type, child);
    }

    // ------------------------------------------------------------------------

    auto push_inst_const(ir::InstType ty, uint64_t v) -> ir::Inst* {
        auto i = fn.alloc_inst_const(ty, v);
        get_current_block()->body.push_back(i);
        return i;
    }

    auto push_inst(ir::InstKind kind, ir::InstType ty, auto&&... v)
        -> ir::Inst* {
        auto i = fn.alloc_inst(kind, ty, 0, std::vector<ir::Inst*>{v...});
        get_current_block()->body.push_back(i);
        return i;
    }

    auto get_current_block() -> ir::Block* {
        ASSERT(fn.blocks.size() > 0);
        return &fn.blocks.at(current_block);
    }

    // ------------------------------------------------------------------------

    ir::Func       fn{};
    uint16_t       current_block{};
    ErrorReporter* er;
};

auto sema(ErrorReporter& er, AstNode const& ast) -> ir::Func {
    auto s = Sema{.er = &er};

    Env env;
    s.sema_func({}, env, ast);

    // FIXME: return an actual thing
    if (er.had_error()) return {};

    ssa::check_valid(s.fn);

    auto had_changes = false;
    do {
        had_changes = false;

        fmt::println(stderr, "{0:=<10} loop start {0:=<10}", "");
        s.fn.disasm(stderr);

        had_changes |= ssa::constant_fold(s.fn);
        had_changes |= ssa::dead_code_elim(s.fn);

        ssa::check_valid(s.fn);
    } while (had_changes);

    ssa::check_valid(s.fn);

    fmt::println(stderr, "{0:=<10} sema done {0:=<10}", "");

    return s.fn;
}

}  // namespace yuri
