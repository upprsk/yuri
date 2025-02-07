#include "ast.hpp"

#include <stdexcept>
#include <variant>

#include "error_reporter.hpp"
#include "fmt/ranges.h"  // IWYU pragma: keep
#include "fmt/std.h"     // IWYU pragma: keep
#include "types.hpp"

namespace yuri {

auto AstNode::add_types(Env& env, ErrorReporter& er) -> Type {
    switch (kind) {
        case AstNodeKind::Nil: return set_type(Type::Void());
        case AstNodeKind::SourceFile: {
            auto e = env.child();
            for (auto& node : children) node.add_types(e, er);

            return set_type(Type::Void());
        }
        case AstNodeKind::Func: {
            auto        e = env.child();
            auto const& name = std::get<std::string>(value);

            // TODO: args
            // auto args = [&] {
            //     std::span nodes = children;
            //     return nodes.subspan(0, children.size() - 2);
            // }();

            auto ret_anon = children.at(children.size() - 2).add_types(e, er);
            if (!ret_anon.is_type()) {
                er.report_error(children.at(children.size() - 2).span,
                                "expected type for return type, got {}",
                                ret_anon);
            }

            auto ret_span = children.at(children.size() - 2).span;
            auto ret = children.at(children.size() - 2).eval_to_type(e, er);

            // function body
            auto be = e.with_return_type(&ret, ret_span);
            children.at(children.size() - 1).add_types(be, er);

            env.define(name, Type::Func(ret));

            return set_type(Type::Void());
        }
        case AstNodeKind::VarDecl: {
            auto        e = env.child();
            auto const& name = std::get<std::string>(value);

            auto has_type_anon = !children.at(0).is_nil();
            auto type_anon = children.at(0).add_types(e, er);
            if (has_type_anon && !type_anon.is_type()) {
                er.report_error(span, "expected type for declaration, got {}",
                                type_anon);
                er.report_note(children.at(0).span, "this has type {}",
                               type_anon);
            }

            auto type = has_type_anon ? children.at(0).eval_to_type(e, er)
                                      : Type::Void();
            auto init = children.at(1).add_types(e, er);

            if (has_type_anon && type != init) {
                er.report_error(
                    span,
                    "incompatible types in declaration, expected {}, got {}",
                    type, init);
            }

            if (!has_type_anon) type = init;
            if (!type.is_type()) {
                env.define(name, type);
            } else {
                // in case the variable is a type, then we want to get the
                // underlying evaluated type from it
                env.define(name, type, children.at(1).eval_to_type(e, er));
            }

            return set_type(Type::Void());
        }
        case AstNodeKind::Block: {
            auto e = env.child();
            for (auto& node : children) node.add_types(e, er);

            return set_type(Type::Void());
        }
        case AstNodeKind::ExprStmt: {
            auto e = env.child();
            auto inner = children.at(0).add_types(e, er);

            if (!inner.is_void()) {
                er.report_error(span, "discarting expression result");
                er.report_note(children.at(0).span, "expressing has type {}",
                               inner);
            }

            return set_type(Type::Void());
        }
        case AstNodeKind::ReturnStmt: {
            auto e = env.child();
            auto inner = children.at(0).add_types(e, er);

            auto [ret_type, ret_span] = env.lookup_return_type();
            if (!ret_type) {
                er.report_error(span, "return statement outside of function");
                return set_type(Type::Void());
            }

            if (*ret_type != inner) {
                er.report_error(span, "incompatible types in return {}", kind);
                er.report_note(ret_span, "function expects type {}", *ret_type);
                er.report_note(children.at(0).span, "this has type {}", inner);
            }

            return set_type(Type::Void());
        }
        case AstNodeKind::WhileStmt: {
            auto e = env.child();
            auto cond = children.at(0).add_types(e, er);

            // TODO: add boolean types
            if (!cond.is_integral()) {
                er.report_error(children.at(0).span,
                                "can't use non-integral {} in condition", cond);
            }

            auto be = e.child();
            children.at(1).add_types(be, er);

            return set_type(Type::Void());
        }
        case AstNodeKind::Assign: {
            auto lhs = children.at(0).add_types(env, er);
            auto rhs = children.at(1).add_types(env, er);

            if (!children.at(0).is_lvalue()) {
                er.report_error(span, "can't assign to non-lvalue");
            }

            if (lhs != rhs) {
                er.report_error(span, "incompatible types in assignment");
                er.report_note(children.at(0).span, "this has type {}", lhs);
                er.report_note(children.at(1).span, "this has type {}", rhs);
            }

            return set_type(Type::Void());
        }
        case AstNodeKind::Add:
        case AstNodeKind::Sub:
        case AstNodeKind::Mul:
        case AstNodeKind::Div: {
            auto lhs = children.at(0).add_types(env, er);
            auto rhs = children.at(1).add_types(env, er);

            if (lhs != rhs) {
                er.report_error(span, "incompatible types in {}", kind);
                er.report_note(children.at(0).span, "this has type {}", lhs);
                er.report_note(children.at(1).span, "this has type {}", rhs);
            }

            return set_type(lhs);
        }
        case AstNodeKind::Id: {
            auto const& name = std::get<std::string>(value);
            auto        type = env.lookup(name);
            if (!type) {
                er.report_error(span, "undefined identifier '{}'", name);
                return set_type(Type::Err());
            }

            return set_type(*type);
        }
        case AstNodeKind::Int: {
            return set_type(Type::Int());
        }
        case AstNodeKind::Err: break;
    }

    throw std::runtime_error(fmt::format("not implemented for {}", *this));
}

auto AstNode::eval_to_type(Env& env, ErrorReporter& er) -> Type {
    switch (kind) {
        case AstNodeKind::Id: {
            auto const& name = std::get<std::string>(value);
            auto        type = env.lookup_underlying(name);
            if (!type) {
                er.report_error(span, "undefined identifier '{}'", name);
                return Type::Err();
            }

            return *type;
        }
        default: {
            er.report_error(span, "can't evaluate to a type");
            return Type::Err();
        }
    }
}

}  // namespace yuri

auto fmt::formatter<yuri::AstNodeKind>::format(yuri::AstNodeKind c,
                                               format_context&   ctx) const
    -> format_context::iterator {
    using T = yuri::AstNodeKind;

    string_view name = "unknown";
    switch (c) {
        case T::Nil: name = "Nil"; break;
        case T::SourceFile: name = "SourceFile"; break;
        case T::Func: name = "Func"; break;
        case T::VarDecl: name = "VarDecl"; break;
        case T::Block: name = "Block"; break;
        case T::ExprStmt: name = "ExprStmt"; break;
        case T::ReturnStmt: name = "ReturnStmt"; break;
        case T::WhileStmt: name = "WhileStmt"; break;
        case T::Assign: name = "Assign"; break;
        case T::Add: name = "Add"; break;
        case T::Sub: name = "Sub"; break;
        case T::Mul: name = "Mul"; break;
        case T::Div: name = "Div"; break;
        case T::Id: name = "Id"; break;
        case T::Int: name = "Int"; break;
        case T::Err: name = "Err"; break;
    }

    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yuri::AstNode>::format(yuri::AstNode   t,
                                           format_context& ctx) const
    -> format_context::iterator {
    if (std::holds_alternative<std::monostate>(t.value)) {
        if (t.children.empty())
            return fmt::format_to(ctx.out(), "{{{}, {}, {}}}", t.kind, t.type,
                                  t.span);

        return fmt::format_to(ctx.out(), "{{{}, {}, {}, {}}}", t.kind, t.type,
                              t.span, t.children);
    }

    if (t.children.empty())
        return fmt::format_to(ctx.out(), "{{{}, {}, {}, {}}}", t.kind, t.type,
                              t.span, t.value);

    return fmt::format_to(ctx.out(), "{{{}, {}, {}, {}, {}}}", t.kind, t.type,
                          t.span, t.value, t.children);
}
