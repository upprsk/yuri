#pragma once

#include <cstdint>
#include <string>
#include <variant>
#include <vector>

#include "env.hpp"
#include "error_reporter.hpp"
#include "span.hpp"
#include "types.hpp"

namespace yuri {

enum class AstNodeKind {
    Nil,
    SourceFile,
    Func,
    FuncDeclArg,
    VarDecl,
    Block,
    ExprStmt,
    ReturnStmt,
    IfStmt,
    WhileStmt,
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    Call,
    Id,
    Int,
    Err,
};

struct AstNode {
    std::variant<std::monostate, std::string, uint64_t> value{};
    std::vector<AstNode>                                children;
    Span                                                span;
    AstNodeKind                                         kind;
    Type                                                type{};

    static auto Nil() -> AstNode {
        return {.children = {}, .span = {}, .kind = AstNodeKind::Nil};
    }

    static auto SourceFile(Span span, std::vector<AstNode> const& children)
        -> AstNode {
        return {.children = children,
                .span = span,
                .kind = AstNodeKind::SourceFile};
    }

    static auto Func(Span span, std::string const& name,
                     std::vector<AstNode> const& args, AstNode const& ret,
                     AstNode const& body) -> AstNode {
        auto children = args;
        children.push_back(ret);
        children.push_back(body);

        return {
            .value = name,
            .children = std::move(children),
            .span = span,
            .kind = AstNodeKind::Func,
        };
    }

    static auto FuncArg(Span span, std::string const& name, AstNode const& type)
        -> AstNode {
        return {
            .value = name,
            .children = {type},
            .span = span,
            .kind = AstNodeKind::FuncDeclArg,
        };
    }

    static auto VarDecl(Span span, std::string const& name, AstNode const& type,
                        AstNode const& init) -> AstNode {
        return {
            .value = name,
            .children = {type, init},
            .span = span,
            .kind = AstNodeKind::VarDecl,
        };
    }

    static auto Block(Span span, std::vector<AstNode> const& children)
        -> AstNode {
        return {
            .children = children,
            .span = span,
            .kind = AstNodeKind::Block,
        };
    }

    static auto IfStmt(Span span, AstNode const& cond, AstNode const& wt,
                       AstNode const& wf) -> AstNode {
        return {
            .children = {cond, wt, wf},
            .span = span,
            .kind = AstNodeKind::IfStmt,
        };
    }

    static auto Unary(Span span, AstNodeKind kind, AstNode const& child)
        -> AstNode {
        return {
            .children = {child},
            .span = span,
            .kind = kind,
        };
    }

    static auto Binary(Span span, AstNodeKind kind, AstNode const& left,
                       AstNode const& right) -> AstNode {
        return {
            .children = {left, right},
            .span = span,
            .kind = kind,
        };
    }

    static auto Call(Span span, AstNode const& callee,
                     std::vector<AstNode> const& args) -> AstNode {
        std::vector children = {callee};
        for (auto const& n : args) children.push_back(n);

        return {
            .children = children,
            .span = span,
            .kind = AstNodeKind::Call,
        };
    }

    static auto Id(Span span, std::string const& value) -> AstNode {
        return {
            .value = value,
            .children = {},
            .span = span,
            .kind = AstNodeKind::Id,
        };
    }

    static auto Int(Span span, uint64_t value) -> AstNode {
        return {
            .value = value,
            .children = {},
            .span = span,
            .kind = AstNodeKind::Int,
        };
    }

    static auto Err(Span span, std::string const& message) -> AstNode {
        return {
            .value = message,
            .children = {},
            .span = span,
            .kind = AstNodeKind::Err,
        };
    }

    constexpr auto first() const -> AstNode const& { return children.at(0); }
    constexpr auto second() const -> AstNode const& { return children.at(1); }
    constexpr auto last() const -> AstNode const& {
        return children.at(children.size() - 1);
    }

    constexpr auto is_lvalue() const -> bool { return kind == AstNodeKind::Id; }
    constexpr auto is_nil() const -> bool { return kind == AstNodeKind::Nil; }

    constexpr auto set_type(Type const& t) -> Type { return type = t; }

    auto add_types(Env& env, ErrorReporter& er) -> Type;
    auto eval_to_type(Env& env, ErrorReporter& er) -> Type;
};

}  // namespace yuri

template <>
struct fmt::formatter<yuri::AstNodeKind> : formatter<string_view> {
    auto format(yuri::AstNodeKind c, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yuri::AstNode> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yuri::AstNode t, format_context& ctx) const
        -> format_context::iterator;
};
