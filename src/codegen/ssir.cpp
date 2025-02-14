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
                codegen_expr(node.second());
                append_op(node.span, Opcode::Local);
                append_idx(node.span, node.type.bytesize());
                add_local(node.value_string(), node.type.bytesize());
                break;

            case AstNodeKind::Assign: {
                auto const& lhs = node.first();
                if (!lhs.is_id()) {
                    er->report_error(
                        lhs.span,
                        "can't use {} as left hand side in assignment",
                        lhs.kind);
                    return;
                }

                auto local = find_local(lhs.value_string());
                if (!local) {
                    er->report_bug(lhs.span, "undefined identifier: '{}'",
                                   lhs.value_string());
                    return;
                }

                codegen_expr(node.second());

                append_op(lhs.span, Opcode::Set);
                append_idx(node.span, local->slot);
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
                    er->report_bug(node.span, "undefined identifier: '{}'",
                                   node.value_string());
                    return;
                }

                append_op(node.span, Opcode::Get);
                append_idx(node.span, local->slot);
            } break;

            case AstNodeKind::Add: binop(Opcode::Add); break;
            case AstNodeKind::Sub: binop(Opcode::Sub); break;
            case AstNodeKind::LessThan: binop(Opcode::Slt); break;
            case AstNodeKind::GreaterThan: binop(Opcode::Sgt); break;
            case AstNodeKind::Equal: binop(Opcode::Seq); break;
            case AstNodeKind::NotEqual: binop(Opcode::Sne); break;

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
                auto const& f = m->entries.find(callee.value_string());
                if (f == m->entries.end()) {
                    er->report_error(callee.span, "undefined function: {}",
                                     callee.value_string(), callee);
                    return;
                }

                for (auto const& arg : args) {
                    codegen_expr(arg);
                }

                append_op(node.span, Opcode::Call);
                append_id(node.span, f->first);
                append_idx(node.span, args.size());
            } break;

            default:
                er->report_bug(node.span, "invalid node for expression: {}",
                               node.kind);
                break;
        }
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
    std::vector<Local> locals{};
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
            auto c = CodegenFunc{.er = er, .m = &m};
            c.codegen(decl);

            m.entries[c.f.name] = std::move(c.f);
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

                case Opcode::Get: {
                    auto slot = func.body.text_at(++i);
                    fmt::println(stderr, ", {}", slot);
                } break;

                case Opcode::Set: {
                    auto slot = func.body.text_at(++i);
                    fmt::println(stderr, ", {}", slot);
                } break;

                case Opcode::Local: {
                    auto sz = func.body.text_at(++i);
                    fmt::println(stderr, ", {}B", sz);
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

                case Opcode::Pop:
                case Opcode::Add:
                case Opcode::Sub:
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
            fmt::println("{}:", lbl);
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
        case yuri::ssir::Opcode::Local: name = "Local"; break;
        case yuri::ssir::Opcode::Li: name = "Li"; break;
        case yuri::ssir::Opcode::Pop: name = "Pop"; break;
        case yuri::ssir::Opcode::Get: name = "Get"; break;
        case yuri::ssir::Opcode::Set: name = "Set"; break;
        case yuri::ssir::Opcode::Add: name = "Add"; break;
        case yuri::ssir::Opcode::Sub: name = "Sub"; break;
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
