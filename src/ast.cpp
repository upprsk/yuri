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
        case AstNodeKind::Func: return add_types_to_func(env, er);
        case AstNodeKind::AsmFunc: return add_types_to_asm_func(env, er);
        case AstNodeKind::FuncDeclArg: {
            auto const& name = std::get<std::string>(value);

            auto type_anon = children.at(0).add_types(env, er);
            if (!type_anon.is_type()) {
                er.report_error(children.at(0).span,
                                "expected type for argument, got {}",
                                type_anon);
            }

            auto type = children.at(0).eval_to_type(env, er);
            env.define(name, type);

            return set_type(type);
        }
        case AstNodeKind::VarDecl: return add_types_to_var_decl(env, er);
        case AstNodeKind::Block: {
            auto e = env.child();
            for (auto& node : children) node.add_types(e, er);

            return set_type(Type::Void());
        }
        case AstNodeKind::ExprStmt: {
            auto e = env.child();
            auto inner = children.at(0).add_types(e, er);

            if (!inner.is_void() && !inner.is_err()) {
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
        case AstNodeKind::IfStmt: {
            auto e = env.child();
            auto cond = children.at(0).add_types(e, er);

            if (!cond.is_bool()) {
                er.report_error(children.at(0).span,
                                "can't use non-boolean {} in condition", cond);
            }

            auto be = e.child();
            children.at(1).add_types(be, er);

            if (!children.at(2).is_nil()) {
                auto be = e.child();
                children.at(2).add_types(be, er);
            }

            return set_type(Type::Void());
        }
        case AstNodeKind::WhileStmt: {
            auto e = env.child();
            auto cond = children.at(0).add_types(e, er);

            if (!cond.is_bool()) {
                er.report_error(children.at(0).span,
                                "can't use non-boolean {} in condition", cond);
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
        case AstNodeKind::Array: return add_types_to_array(env, er);
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
        case AstNodeKind::LessThan:
        case AstNodeKind::LessThanEqual:
        case AstNodeKind::GreaterThan:
        case AstNodeKind::GreaterThanEqual:
        case AstNodeKind::Equal:
        case AstNodeKind::NotEqual: {
            auto lhs = children.at(0).add_types(env, er);
            auto rhs = children.at(1).add_types(env, er);

            if (lhs != rhs) {
                er.report_error(span, "incompatible types in comparison");
                er.report_note(children.at(0).span, "this has type {}", lhs);
                er.report_note(children.at(1).span, "this has type {}", rhs);
            }

            return set_type(Type::Bool());
        }
        case AstNodeKind::Index: {
            auto lhs = children.at(0).add_types(env, er);
            if (!lhs.is_array()) {
                er.report_error(span, "can't index non-array {}", lhs);
                return set_type(Type::Err());
            }

            auto rhs = children.at(1).add_types(env, er);
            if (!rhs.is_integral()) {
                er.report_error(
                    span, "can't index array with non integer type {}", rhs);
            }

            return set_type(lhs.inner.at(0));
        }
        case AstNodeKind::Cast: {
            auto lhs = children.at(0).add_types(env, er);
            auto rhs = children.at(1).add_types(env, er);

            if (!rhs.is_type()) {
                er.report_error(span, "expected type in cast, got {}", rhs);
                return set_type(Type::Err());
            }

            auto ty = children.at(1).eval_to_type(env, er);

            return set_type(ty);
        }
        case AstNodeKind::Call: return add_types_to_call(env, er);
        case AstNodeKind::Ref: {
            if (!children.at(0).is_lvalue()) {
                er.report_error(span, "can't take address of non l-value: {}",
                                children.at(0).kind);
                return set_type(Type::Err());
            }

            auto inner = children.at(0).add_types(env, er);
            return set_type(Type::Ptr(inner));
        } break;
        case AstNodeKind::DeRef: {
            auto inner = children.at(0).add_types(env, er);
            if (!inner.is_ptr()) {
                er.report_error(span, "can't de-reference non-pointer {}",
                                inner);
                return set_type(Type::Err());
            }

            return set_type(inner.inner.at(0));
        } break;
        case AstNodeKind::Id: {
            auto const& name = std::get<std::string>(value);
            auto        type = env.lookup(name);
            if (!type) {
                er.report_error(span, "undefined identifier '{}'", name);
                return set_type(Type::Err());
            }

            return set_type(*type);
        }
        case AstNodeKind::Ptr: {
            auto r = children.at(0).add_types(env, er);

            return set_type(Type::make_type());
        }
        case AstNodeKind::Int: {
            return set_type(Type::Int());
        }
        case AstNodeKind::Err: break;
    }

    er.report_error(span, "not implemented for: {}", *this);
    return set_type(Type::Err());
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
        case AstNodeKind::Ptr: {
            auto const inner = children.at(0).eval_to_type(env, er);
            return Type::Ptr(inner);
        }
        default: {
            er.report_error(span, "can't evaluate to a type");
            return Type::Err();
        }
    }
}

auto AstNode::add_types_to_func(Env& env, ErrorReporter& er) -> Type {
    auto        e = env.child();
    auto const& name = std::get<std::string>(value);

    auto args = [&] {
        std::span nodes = children;
        return nodes.subspan(0, children.size() - 2);
    }();

    for (auto& arg : args) {
        arg.add_types(e, er);
    }

    auto ret_anon = children.at(children.size() - 2).add_types(e, er);
    if (!children.at(children.size() - 2).is_nil() && !ret_anon.is_type()) {
        er.report_error(children.at(children.size() - 2).span,
                        "expected type for return type, got {}", ret_anon);
    }

    auto ret_span = children.at(children.size() - 2).is_nil()
                        ? span
                        : children.at(children.size() - 2).span;
    auto ret = children.at(children.size() - 2).is_nil()
                   ? Type::Void()
                   : children.at(children.size() - 2).eval_to_type(e, er);

    // function body
    auto be = e.with_return_type(&ret, ret_span);
    children.at(children.size() - 1).add_types(be, er);

    std::vector<Type> arg_types;
    for (auto const& arg : args) {
        arg_types.push_back(arg.type);
    }

    auto ty = Type::Func(arg_types, ret);
    env.define(name, ty);

    // TODO: check if value was returned

    return set_type(ty);
}

auto AstNode::add_types_to_asm_func(Env& env, ErrorReporter& er) -> Type {
    auto        e = env.child();
    auto const& name = std::get<std::string>(value);

    auto args = [&] {
        std::span nodes = children;
        return nodes.subspan(0, children.size() - 2);
    }();

    for (auto& arg : args) {
        arg.add_types(e, er);
    }

    auto ret_anon = children.at(children.size() - 2).add_types(e, er);
    if (!children.at(children.size() - 2).is_nil() && !ret_anon.is_type()) {
        er.report_error(children.at(children.size() - 2).span,
                        "expected type for return type, got {}", ret_anon);
    }

    auto ret = children.at(children.size() - 2).is_nil()
                   ? Type::Void()
                   : children.at(children.size() - 2).eval_to_type(e, er);

    // function body
    for (auto const& s : children.at(children.size() - 1).children) {
        if (s.kind != AstNodeKind::Str) {
            er.report_bug(s.span, "got non-str node in asm function body: {}",
                          s);
            continue;
        }
    }

    std::vector<Type> arg_types;
    for (auto const& arg : args) {
        arg_types.push_back(arg.type);
    }

    auto ty = Type::Func(arg_types, ret);
    env.define(name, ty);

    return set_type(ty);
}

auto AstNode::add_types_to_var_decl(Env& env, ErrorReporter& er) -> Type {
    auto        e = env.child();
    auto const& name = std::get<std::string>(value);

    auto has_type_anon = !children.at(0).is_nil();
    auto type_anon = children.at(0).add_types(e, er);
    if (has_type_anon && !type_anon.is_type()) {
        er.report_error(span, "expected type for declaration, got {}",
                        type_anon);
        er.report_note(children.at(0).span, "this has type {}", type_anon);
    }

    auto type =
        has_type_anon ? children.at(0).eval_to_type(e, er) : Type::Void();
    auto init = children.at(1).add_types(e, er);

    if (has_type_anon && type != init) {
        er.report_error(
            span, "incompatible types in declaration, expected {}, got {}",
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

    return set_type(type);
}

auto AstNode::add_types_to_array(Env& env, ErrorReporter& er) -> Type {
    auto ty_node = children.at(1).add_types(env, er);
    if (!ty_node.is_type()) {
        er.report_error(children.at(1).span,
                        "expected type of array, but found {}", ty_node);
        // FIXME: use an array type here
        return set_type(Type::Err());
    }

    auto ty = children.at(1).eval_to_type(env, er);

    std::span items = children;
    items = items.subspan(2);

    size_t count{};
    if (!children.at(0).is_nil()) {
        auto n = children.at(0).add_types(env, er);
        if (!n.is_integral()) {
            er.report_error(
                children.at(0).span,
                "explicit size of array must be an integer, found {}", n);
        }

        if (children.at(0).is_int()) {
            count = children.at(0).value_int();
        } else {
            er.report_error(children.at(0).span,
                            "the compiler is limited, use an integer "
                            "here or auto-detect");
        }
    } else {
        count = items.size();
    }

    if (count < items.size()) {
        er.report_error(
            span, "array can hava at most {} items, found {} in initializer",
            count, items.size());
    }

    for (auto& item : items) {
        auto item_type = item.add_types(env, er);
        if (ty != item_type) {
            er.report_error(item.span,
                            "expected {}, but found {} in array initialization",
                            ty, item_type);
        }
    }

    return set_type(Type::Array(ty, count));
}

auto AstNode::add_types_to_call(Env& env, ErrorReporter& er) -> Type {
    auto callee = children.at(0).add_types(env, er);
    if (!callee.is_func()) {
        er.report_error(children.at(0).span, "can't call non-function {}",
                        callee);
        return set_type(Type::Err());
    }

    std::span args = children;
    args = args.subspan(1);
    for (auto& arg : args) {
        arg.add_types(env, er);
    }

    std::span expected_args = callee.inner;
    expected_args = expected_args.subspan(0, expected_args.size() - 1);

    if (args.size() != expected_args.size()) {
        er.report_error(children.at(0).span,
                        "wrong number of arguments, expected {}, got {}",
                        expected_args.size(), args.size());
    }

    for (size_t i = 0; i < std::min(args.size(), expected_args.size()); i++) {
        if (args[i].type != expected_args[i]) {
            er.report_error(args[i].span,
                            "incompatible argument type, expected {}",
                            expected_args[i]);
            er.report_note(args[i].span, "this expression has type {}",
                           args[i].type);
        }
    }

    return set_type(callee.inner.at(callee.inner.size() - 1));
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
        case T::AsmFunc: name = "AsmFunc"; break;
        case T::FuncDeclArg: name = "FuncDeclArg"; break;
        case T::VarDecl: name = "VarDecl"; break;
        case T::Block: name = "Block"; break;
        case T::ExprStmt: name = "ExprStmt"; break;
        case T::ReturnStmt: name = "ReturnStmt"; break;
        case T::IfStmt: name = "IfStmt"; break;
        case T::WhileStmt: name = "WhileStmt"; break;
        case T::Assign: name = "Assign"; break;
        case T::Array: name = "Array"; break;
        case T::Cast: name = "Cast"; break;
        case T::Add: name = "Add"; break;
        case T::Sub: name = "Sub"; break;
        case T::Mul: name = "Mul"; break;
        case T::Div: name = "Div"; break;
        case T::LessThan: name = "LessThan"; break;
        case T::LessThanEqual: name = "LessThanEqual"; break;
        case T::GreaterThan: name = "GreaterThan"; break;
        case T::GreaterThanEqual: name = "GreaterThanEqual"; break;
        case T::Equal: name = "Equal"; break;
        case T::NotEqual: name = "NotEqual"; break;
        case T::Index: name = "Index"; break;
        case T::Call: name = "Call"; break;
        case T::Ref: name = "Ref"; break;
        case T::DeRef: name = "DeRef"; break;
        case T::Ptr: name = "Ptr"; break;
        case T::Id: name = "Id"; break;
        case T::Int: name = "Int"; break;
        case T::Str: name = "Str"; break;
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
