#include "sema.hpp"

#include <cstdint>
#include <cstdio>
#include <queue>
#include <ranges>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "fmt/base.h"
#include "fmt/ranges.h"
#include "ir.hpp"
#include "libassert/assert.hpp"
#include "ssa/branch_fold.hpp"
#include "ssa/cfold.hpp"
#include "ssa/check.hpp"
#include "ssa/dcelim.hpp"

namespace yuri {

struct Decl {
    std::string name;
    ir::Inst*   val;

    ir::Block* block;
    uint16_t   id;
    uint16_t   generation;
};

struct Env {
    constexpr auto child() -> Env { return {.parent = this, .decls = {}}; }

    auto define_assign(Decl const& decl, ir::Inst* val, ir::Block* block)
        -> uint16_t {
        return define(decl.name, val, block, decl.generation + 1, decl.id);
    }

    auto define(std::string name, ir::Inst* val, ir::Block* blk, uint16_t gen,
                uint16_t id) -> uint16_t {
        decls.push_back({
            .name = name,
            .val = val,
            .block = blk,
            .id = id,
            .generation = gen,
        });

        return id;
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
        current_block = alloc_block();

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
            case AstNodeKind::IfStmt: sema_if_stmt(ctx, env, node); break;

            case AstNodeKind::ExprStmt: PANIC("not implemented", node.kind);
            case AstNodeKind::Block: sema_block(ctx, env, node); break;

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

                if (v->block != current_block) {
                    if (requires_phi(v->id)) {
                        mark_definition_in_block(v->id);
                        return push_inst_phi(ir::InstKind::Phi, v->val->type,
                                             v->id);
                    }
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
            sema_stmt(ctx, env, stmt);
        }
    }

    // NOLINTNEXTLINE(readability-function-cognitive-complexity)
    void sema_var_decl(Context const& ctx, Env& env, AstNode const& node) {
        ASSERT(node.kind == AstNodeKind::VarDecl);
        ASSERT(node.children.at(0).kind == AstNodeKind::Empty,
               "explicit type in decl has not been implemented");

        auto init = sema_expr(ctx, env, node.children.at(1));
        auto id = env.define(std::string{node.value_string()}, init,
                             current_block, 0, next_decl_id++);
        mark_definition_in_block(id);
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

            auto rhs = sema_expr(ctx, env, *node.second());

            // an assign just shadows the previous declaration
            auto id = env.define_assign(*decl, rhs, current_block);
            mark_definition_in_block(id);

            if (decl->block != current_block) {
                push_inst_phi(ir::InstKind::Upsilon, decl->val->type, decl->id,
                              rhs);
            }

            return;
        }

        UNREACHABLE("node kind not handled as lhs", node.first()->kind);
    }

    void sema_return_stmt(Context const& ctx, Env& env, AstNode const& node) {
        ASSERT(node.kind == AstNodeKind::ReturnStmt);

        auto child = sema_expr(ctx, env, node.children.at(0));
        push_inst(ir::InstKind::Ret, child->type, child);
        seal_block(current_block);
    }

    // NOLINTNEXTLINE(readability-function-cognitive-complexity)
    void sema_if_stmt(Context const& ctx, Env& env, AstNode const& node) {
        ASSERT(node.kind == AstNodeKind::IfStmt);

        auto has_else = node.third()->kind != AstNodeKind::Empty;

        auto cond = sema_expr(ctx, env, *node.first());

        auto wt = alloc_block();
        auto wf = alloc_block();
        auto after = wf;

        push_inst_branch(cond);
        seal_block(current_block, wt, wf);

        // sema the then branch
        set_current_block(wt);
        sema_block(ctx, env, *node.second());

        // in case we have an else, there is a need to jump over it
        if (has_else) {
            after = alloc_block();
        }

        push_inst_jump();
        seal_block(current_block, after);

        if (has_else) {
            set_current_block(wf);
            sema_block(ctx, env, *node.third());

            push_inst_jump();
            seal_block(current_block, after);
        }

        set_current_block(after);
    }

    // ------------------------------------------------------------------------

    void mark_definition_in_block(uint16_t id) {
        definitions[current_block].insert(id);
    }

    [[nodiscard]] auto requires_phi(uint16_t id) const -> bool {
        std::queue<ir::Block*> worklist;

        auto add = [&](std::span<ir::Block* const> blks) {
            for (auto blk : blks) worklist.push(blk);
        };

        std::unordered_set<ir::Block*> checked;
        size_t                         ndefs{};

        add(predecessors.at(current_block));
        while (!worklist.empty()) {
            auto blk = worklist.front();
            worklist.pop();

            if (checked.contains(blk)) continue;

            checked.insert(blk);

            auto pit = predecessors.find(blk);
            if (pit != predecessors.end()) add(pit->second);

            auto it = definitions.find(blk);
            if (it == definitions.end()) continue;

            ndefs += it->second.contains(id);
        }

        ASSERT(ndefs > 0);
        return ndefs > 1;
    }

    // ------------------------------------------------------------------------

    auto push_inst_const(ir::InstType ty, uint64_t v) -> ir::Inst* {
        auto i = fn.alloc_inst_const(ty, v);
        get_current_block()->body.push_back(i);
        return i;
    }

    auto push_inst_branch(auto&&... v) -> ir::Inst* {
        auto i = fn.alloc_inst(ir::InstKind::Branch, ir::InstType::Err, 0,
                               std::vector<ir::Inst*>{v...});
        get_current_block()->body.push_back(i);
        return i;
    }

    auto push_inst_jump() -> ir::Inst* {
        auto i = fn.alloc_inst(ir::InstKind::Jump, ir::InstType::Err, 0,
                               std::vector<ir::Inst*>{});
        get_current_block()->body.push_back(i);
        return i;
    }

    auto push_inst_phi(ir::InstKind kind, ir::InstType ty, uint16_t shadow,
                       auto&&... v) -> ir::Inst* {
        auto i = fn.alloc_inst(kind, ty, shadow, std::vector<ir::Inst*>{v...});
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
        ASSERT(current_block != nullptr);
        return current_block;
    }

    void set_current_block(ir::Block* blk) { current_block = blk; }

    auto alloc_block() -> ir::Block* { return fn.alloc_block(); }

    void seal_block(ir::Block* blk, auto&&... v) {
        blk->successors = {std::forward<decltype(v)>(v)...};
        for (auto const& s : blk->successors) {
            predecessors[s].push_back(blk);
        }
    }

    // ------------------------------------------------------------------------

    // TODO: use a better map impl
    std::unordered_map<ir::Block*, std::vector<ir::Block*>>      predecessors;
    std::unordered_map<ir::Block*, std::unordered_set<uint16_t>> definitions;

    ir::Block* current_block{};
    uint16_t   next_decl_id{};

    ir::Func       fn{};
    ErrorReporter* er;
};

auto sema(ErrorReporter& er, AstNode const& ast) -> ir::Func {
    auto s = Sema{.predecessors = {}, .definitions = {}, .er = &er};

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
        had_changes |= ssa::branch_fold(s.fn);
        had_changes |= ssa::branch_elim(s.fn);

        ssa::check_valid(s.fn);
    } while (had_changes);

    ssa::check_valid(s.fn);

    fmt::println(stderr, "{0:=<10} sema done {0:=<10}", "");

    return s.fn;
}

}  // namespace yuri
