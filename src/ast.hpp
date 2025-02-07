#pragma once

#include <cstdint>
#include <string>
#include <variant>
#include <vector>

#include "span.hpp"

namespace yuri {

enum class AstNodeKind {
    Nil,
    SourceFile,
    Func,
    VarDecl,
    Block,
    ExprStmt,
    ReturnStmt,
    WhileStmt,
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Id,
    Int,
    Err,
};

struct AstNode {
    std::variant<std::monostate, std::string, uint64_t> value{};
    std::vector<AstNode>                                children;
    Span                                                span;
    AstNodeKind                                         kind;

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

    constexpr auto left() const -> AstNode const& { return children.at(0); }
    constexpr auto right() const -> AstNode const& { return children.at(1); }
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
