#include "ssir.hpp"

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <limits>
#include <ranges>
#include <string_view>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "fmt/base.h"
#include "fmt/format.h"
#include "span.hpp"

namespace yuri::ssir {

struct CodegenFunc {
    struct Local {
        std::string name;
        size_t      slot;
        size_t      size;
    };

    void codegen(AstNode const& node) {
        if (!node.is_func()) {
            er->report_bug(node.span,
                           "expected func for function codegen, found {}",
                           node.kind);
            return;
        }

        f.name = node.value_string();
        f.type = node.type;

        codegen_args(node);

        codegen_body(node.last());
    }

    void codegen_args(AstNode const& func) {
        std::span args = func.children;
        args = args.subspan(0, args.size() - 2);

        for (auto const& arg : args) {
            if (!arg.is_func_arg()) {
                er->report_bug(
                    arg.span,
                    "expected function argument in argument list, got {}",
                    arg.kind);
                continue;
            }

            add_local(arg.value_string(), arg.type.bytesize());
        }
    }

    void codegen_body(AstNode const& node) {
        if (!node.is_block()) {
            er->report_bug(node.span,
                           "expected block for body codegen, found {}",
                           node.kind);
            return;
        }

        for (auto const& stmt : node.children) {
            codegen_stmt(stmt);
        }
    }

    void codegen_stmt(AstNode const& node) {
        switch (node.kind) {
            case AstNodeKind::Block: {
                auto locals_start = locals.size();

                for (auto const& stmt : node.children) {
                    codegen_stmt(stmt);
                }

                for (size_t i = 0; i < locals.size() - locals_start; i++) {
                    append_op(node.span, Opcode::Pop);
                }
            } break;

            case AstNodeKind::VarDecl:
                // TODO: type coercion
                codegen_expr(node.second());
                append_op(node.span, Opcode::Local);
                append_idx(node.span, locals.size());
                append_idx(node.span, node.type.bytesize());
                add_local(node.value_string(), node.type.bytesize());
                break;

            case AstNodeKind::Assign: {
                auto const& lhs = node.first();
                if (lhs.is_id()) {
                    codegen_expr(node.second());

                    auto local = find_local(lhs.value_string());
                    if (!local) {
                        auto global = find_global(lhs.value_string());
                        if (!global) {
                            er->report_bug(lhs.span,
                                           "undefined identifier: '{}'",
                                           lhs.value_string());
                            return;
                        }

                        append_op(lhs.span, Opcode::SetGlobal);
                        append_id(lhs.span, global->name);
                        break;
                    }

                    append_op(lhs.span, Opcode::Set);
                    append_idx(node.span, local->slot);
                    break;
                }

                if (lhs.is_deref() || lhs.is_index()) {
                    codegen_expr_addr(lhs);
                    codegen_expr(node.second());
                    append_op(node.span, Opcode::Iset);
                    append_idx(node.span, lhs.type.bytesize());

                    break;
                }

                er->report_error(lhs.span,
                                 "can't use {} as left hand side in assignment",
                                 lhs.kind);
            } break;

            case AstNodeKind::WhileStmt: {
                auto start = append_label();
                auto end = append_label();

                codegen_expr(node.first());

                append_op(node.span, Opcode::Bz);
                append_idx(node.span, end);

                codegen_stmt(node.second());

                append_op(node.span, Opcode::B);
                append_idx(node.span, start);

                update_label_offest(end);
            } break;

            case AstNodeKind::IfStmt: {
                codegen_expr(node.first());

                auto wf = append_label();
                auto end = wf;

                append_op(node.span, Opcode::Bz);
                append_idx(node.span, wf);

                codegen_stmt(node.second());

                if (!node.last().is_nil()) {
                    end = append_label();
                    append_op(node.span, Opcode::B);
                    append_idx(node.span, end);
                }

                update_label_offest(wf);

                if (!node.last().is_nil()) {
                    codegen_stmt(node.last());
                    update_label_offest(end);
                }
            } break;

            case AstNodeKind::ExprStmt:
                codegen_expr(node.first());
                append_op(node.span, Opcode::Pop);
                break;

            case AstNodeKind::ReturnStmt:
                codegen_expr(node.first());
                append_op(node.span, Opcode::Ret);
                break;

            default:
                er->report_bug(node.span, "invalid node for statement: {}",
                               node.kind);
                break;
        }
    }

    void codegen_expr(AstNode const& node) {
        auto binop = [&](Opcode op) {
            codegen_expr(node.first());
            codegen_expr(node.second());
            append_op(node.span, op);
        };

        switch (node.kind) {
            case AstNodeKind::Int:
                append_op(node.span, Opcode::Li);
                append_const(node.span, node.value_int());
                break;
            case AstNodeKind::Id: {
                auto local = find_local(node.value_string());
                if (!local) {
                    auto global = find_global(node.value_string());
                    if (!global) {
                        er->report_bug(node.span, "undefined identifier: '{}'",
                                       node.value_string());
                        return;
                    }

                    append_op(node.span, Opcode::GetGlobal);
                    append_id(node.span, global->name);
                    break;
                }

                append_op(node.span, Opcode::Get);
                append_idx(node.span, local->slot);
            } break;

            case AstNodeKind::Array: {
                append_op(node.span, Opcode::Alloca);
                append_idx(node.span, node.type.bytesize());

                std::span items = node.children;
                items = items.subspan(2);

                size_t offset{};

                for (auto const& item : items) {
                    append_op(node.span, Opcode::Dupe);
                    append_op(node.span, Opcode::Li);
                    append_const(node.span, offset);
                    offset += item.type.bytesize();
                    append_op(node.span, Opcode::Add);

                    codegen_expr(item);
                    append_op(item.span, Opcode::Iset);
                    append_idx(node.span, item.type.bytesize());
                }
            } break;

            case AstNodeKind::Add: binop(Opcode::Add); break;
            case AstNodeKind::Sub: binop(Opcode::Sub); break;
            case AstNodeKind::Mul: binop(Opcode::Mul); break;
            case AstNodeKind::Div: binop(Opcode::Div); break;
            case AstNodeKind::LessThan: binop(Opcode::Slt); break;
            case AstNodeKind::GreaterThan: binop(Opcode::Sgt); break;
            case AstNodeKind::Equal: binop(Opcode::Seq); break;
            case AstNodeKind::NotEqual: binop(Opcode::Sne); break;

            case AstNodeKind::Ref: {
                if (!node.first().is_id()) {
                    er->report_bug(node.span, "child is not an id");
                    return;
                }

                auto local = find_local(node.first().value_string());
                if (!local) {
                    auto global = find_global(node.first().value_string());
                    if (!global) {
                        er->report_bug(node.first().span,
                                       "undefined identifier: '{}'",
                                       node.first().value_string());
                        return;
                    }

                    append_op(node.span, Opcode::Global);
                    append_id(node.span, global->name);

                    break;
                }

                append_op(node.span, Opcode::Ref);
                append_idx(node.span, local->slot);
            } break;

            case AstNodeKind::DeRef: {
                codegen_expr(node.first());
                append_op(node.span, Opcode::DeRef);
            } break;

            case AstNodeKind::Index: {
                auto const& t = node.first().type.inner.at(0);

                codegen_expr(node.first());
                codegen_expr(node.second());

                append_op(node.second().span, Opcode::Li);
                append_const(node.second().span, t.bytesize());
                append_op(node.second().span, Opcode::Mul);
                append_op(node.second().span, Opcode::Add);

                append_op(node.span, Opcode::DeRef);
            } break;

            case AstNodeKind::Cast: {
                codegen_expr(node.first());

                auto const& lhs = node.first().type;
                auto const& rhs = node.type;

                if (lhs.is_integral()) {
                    if (rhs.is_integral()) {
                        // do nothing
                    } else {
                        er->report_error(
                            node.span, "can't cast {} to non-integral type {}",
                            lhs, rhs);
                    }

                } else if (lhs.is_array()) {
                    if (rhs.is_ptr() && rhs.inner.at(0) == lhs.inner.at(0)) {
                        // do nothing
                    } else {
                        er->report_error(node.span,
                                         "can't cast {} to {}, inner type must "
                                         "match: {} not equal to {}",
                                         lhs, rhs, lhs.inner.at(0),
                                         rhs.inner.at(0));
                    }
                } else {
                    er->report_error(node.span,
                                     "can't cast non-integral type {}", lhs);
                }
            } break;

            case AstNodeKind::Call: {
                std::span args = node.children;
                args = args.subspan(1, args.size() - 1);

                auto const& callee = node.first();
                if (!callee.is_id()) {
                    er->report_error(callee.span,
                                     "can't call {}, indirect calls have not "
                                     "been implemented",
                                     callee);
                    return;
                }

                // find the function
                auto f = find_function_name(callee.value_string());
                if (!f) {
                    er->report_error(callee.span,
                                     "undefined function (ssir): {}",
                                     callee.value_string(), callee);
                    return;
                }

                for (auto const& arg : args) {
                    codegen_expr(arg);
                }

                append_op(node.span, Opcode::Call);
                append_id(node.span, *f);
                append_idx(node.span, args.size());
            } break;

            default:
                er->report_bug(node.span, "invalid node for expression: {}",
                               node.kind);
                break;
        }
    }

    void codegen_expr_addr(AstNode const& node) {
        if (node.is_id()) {
            auto local = find_local(node.value_string());
            if (!local) {
                er->report_bug(node.span, "undefined identifier: '{}'",
                               node.value_string());
                return;
            }

            append_op(node.span, Opcode::Get);
            append_idx(node.span, local->slot);
            return;
        }

        if (node.is_deref()) {
            codegen_expr_addr(node.first());
            return;
        }

        if (node.is_index()) {
            auto const& t = node.first().type.inner.at(0);

            codegen_expr(node.first());
            codegen_expr(node.second());

            append_op(node.second().span, Opcode::Li);
            append_const(node.second().span, t.bytesize());
            append_op(node.second().span, Opcode::Mul);
            append_op(node.second().span, Opcode::Add);
            return;
        }

        codegen_expr(node);
    }

    [[nodiscard]] auto find_function_name(std::string const& name) const
        -> std::optional<std::string> {
        if (auto const& f = m->entries.find(name); f != m->entries.end()) {
            return f->first;
        }

        if (auto const& f = m->asm_entries.find(name);
            f != m->asm_entries.end()) {
            return f->first;
        }

        return std::nullopt;
    }

    void add_local(std::string const& name, size_t size) {
        locals.push_back({.name = name, .slot = locals.size(), .size = size});
    }

    constexpr auto find_local(std::string_view name) -> Local* {
        for (auto& l : locals | std::ranges::views::reverse) {
            if (l.name == name) return &l;
        }

        return nullptr;
    }

    [[nodiscard]] auto find_global(std::string const& name) const -> Global* {
        auto it = m->globals.find(name);
        if (it == m->globals.end()) return nullptr;

        return &it->second;
    }

    void append_op(Span s, Opcode op) {
        f.body.append_op(op);
        f.body.append_span(s);
    }

    void append_id(Span s, std::string const& id) {
        auto idx = f.body.append_id(id);
        if (idx > std::numeric_limits<uint8_t>::max()) {
            er->report_bug({}, "maximum number of local identifiers reached");
        }

        append_idx(s, idx);
    }

    void append_const(Span s, uint64_t v) {
        auto idx = f.body.append_const(v);
        if (idx > std::numeric_limits<uint8_t>::max()) {
            er->report_bug({}, "maximum number of local constants reached");
        }

        append_idx(s, idx);
    }

    void append_idx(Span s, size_t idx) {
        if (idx > std::numeric_limits<uint8_t>::max()) {
            er->report_bug({}, "maximum stack size reached");
        }

        f.body.append_text(idx);
        f.body.append_span(s);
    }

    auto append_label() -> size_t { return f.body.append_label(); }
    void update_label_offest(size_t label) {
        f.body.labels.at(label) = f.body.text.size();
    }

    ErrorReporter*     er;
    Module*            m;
    Func               f{};
    std::vector<Local> locals;
};

struct Codegen {
    void codegen(AstNode const& node) {
        if (!node.is_source_file()) {
            er->report_bug(
                node.span,
                "expected source file for top-level codegen, found {}",
                node.kind);
            return;
        }

        for (auto const& decl : node.children) {
            if (decl.kind == AstNodeKind::Func) {
                auto c = CodegenFunc{.er = er, .m = &m, .locals = {}};
                c.codegen(decl);

                m.entries[c.f.name] = std::move(c.f);
            } else if (decl.kind == AstNodeKind::AsmFunc) {
                std::vector<std::string> body;
                for (auto const& line :
                     decl.children.at(decl.children.size() - 1).children) {
                    body.push_back(line.value_string());
                }

                m.asm_entries[decl.value_string()] = {
                    .name = decl.value_string(),
                    .type = decl.type,
                    .body = body,
                };
            } else if (decl.kind == AstNodeKind::VarDecl) {
                auto init = decl.second();
                if (init.kind != AstNodeKind::Int) {
                    er->report_error(
                        decl.span,
                        "globals can only be initialized to integer constants");
                    return;
                }

                m.globals[decl.value_string()] = {
                    .name = decl.value_string(),
                    .initial_value = init.value_int()};
            } else {
                er->report_bug(decl.span, "unexpected node in top-level: {}",
                               decl);
            }
        }
    }

    ErrorReporter* er;
    Module         m{};
};

auto codegen(AstNode const& ast, ErrorReporter& er) -> Module {
    auto c = Codegen{.er = &er};
    c.codegen(ast);

    return c.m;
}

void dump_module(Module const& m) {
    for (auto const& [key, func] : m.entries) {
        fmt::println(stderr, "{}:", key);

        size_t lbl{};
        size_t i{};
        for (; i < func.body.text.size(); i++) {
            if (lbl < func.body.labels.size() && func.body.label_at(lbl) == i) {
                fmt::println(stderr, "{}:", lbl);
                lbl++;
            }

            auto c = func.body.opcode_at(i);
            fmt::print(stderr, "{:04d} | {}", i, c);

            switch (c) {
                case Opcode::Li: {
                    auto idx = func.body.text_at(++i);
                    auto v = func.body.const_at(idx);

                    fmt::println(stderr, "[{}], {}", idx, v);
                } break;

                case Opcode::Iset:
                case Opcode::Alloca: {
                    auto size = func.body.text_at(++i);
                    fmt::println(stderr, ", {}B", size);
                } break;

                case Opcode::Global:
                case Opcode::GetGlobal:
                case Opcode::SetGlobal: {
                    auto name = func.body.text_at(++i);
                    fmt::println(stderr, ", {}", func.body.id_at(name));
                } break;

                case Opcode::Ref:
                case Opcode::Get:
                case Opcode::Set: {
                    auto slot = func.body.text_at(++i);
                    fmt::println(stderr, ", {}", slot);
                } break;

                case Opcode::Local: {
                    auto slot = func.body.text_at(++i);
                    auto sz = func.body.text_at(++i);
                    fmt::println(stderr, ", {}, {}B", slot, sz);
                } break;

                case Opcode::B:
                case Opcode::Bz: {
                    auto label = func.body.text_at(++i);
                    fmt::println(stderr, ", {}", label);
                } break;

                case Opcode::Call: {
                    auto id = func.body.text_at(++i);
                    auto argc = func.body.text_at(++i);
                    fmt::println(stderr, ", {}, {}", func.body.id_at(id), argc);
                } break;

                case Opcode::Dupe:
                case Opcode::DeRef:
                case Opcode::Pop:
                case Opcode::Add:
                case Opcode::Sub:
                case Opcode::Mul:
                case Opcode::Div:
                case Opcode::Seq:
                case Opcode::Sne:
                case Opcode::Slt:
                case Opcode::Sgt:
                case Opcode::Ret: fmt::println(stderr, ""); break;

                case Opcode::Invalid:
                default:
                    fmt::println(stderr, "invalid opcode: {}",
                                 fmt::underlying(c));
                    break;
            }
        }

        if (lbl < func.body.labels.size() && func.body.label_at(lbl) == i) {
            fmt::println(stderr, "{}:", lbl);
            lbl++;
        }
    }
}

}  // namespace yuri::ssir

auto fmt::formatter<yuri::ssir::Opcode>::format(yuri::ssir::Opcode c,
                                                format_context&    ctx) const
    -> format_context::iterator {
    string_view name = "unknown";
    switch (c) {
        case yuri::ssir::Opcode::Invalid: name = "Invalid"; break;
        case yuri::ssir::Opcode::Dupe: name = "Dupe"; break;
        case yuri::ssir::Opcode::Local: name = "Local"; break;
        case yuri::ssir::Opcode::Li: name = "Li"; break;
        case yuri::ssir::Opcode::Pop: name = "Pop"; break;
        case yuri::ssir::Opcode::Ref: name = "Ref"; break;
        case yuri::ssir::Opcode::DeRef: name = "DeRef"; break;
        case yuri::ssir::Opcode::Alloca: name = "Alloca"; break;
        case yuri::ssir::Opcode::Global: name = "Global"; break;
        case yuri::ssir::Opcode::GetGlobal: name = "GetGlobal"; break;
        case yuri::ssir::Opcode::SetGlobal: name = "SetGlobal"; break;
        case yuri::ssir::Opcode::Get: name = "Get"; break;
        case yuri::ssir::Opcode::Set: name = "Set"; break;
        case yuri::ssir::Opcode::Iset: name = "Iset"; break;
        case yuri::ssir::Opcode::Add: name = "Add"; break;
        case yuri::ssir::Opcode::Sub: name = "Sub"; break;
        case yuri::ssir::Opcode::Mul: name = "Mul"; break;
        case yuri::ssir::Opcode::Div: name = "Div"; break;
        case yuri::ssir::Opcode::Seq: name = "Seq"; break;
        case yuri::ssir::Opcode::Sne: name = "Sne"; break;
        case yuri::ssir::Opcode::Slt: name = "Slt"; break;
        case yuri::ssir::Opcode::Sgt: name = "Sgt"; break;
        case yuri::ssir::Opcode::B: name = "B"; break;
        case yuri::ssir::Opcode::Bz: name = "Bz"; break;
        case yuri::ssir::Opcode::Call: name = "Call"; break;
        case yuri::ssir::Opcode::Ret: name = "Ret"; break;
    }

    return formatter<string_view>::format(name, ctx);
}
