#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "span.hpp"

namespace yuri {

enum class AstNodeKind {
    Err,
    Empty,

    VarDecl,

    ReturnStmt,
    ExprStmt,

    Block,
    SourceFile,

    Add,
    Sub,
    Mul,
    Div,

    Neg,

    Int,
    Id,
};

struct AstNode {
    AstNodeKind                                         kind;
    Span                                                span;
    std::vector<AstNode>                                children;
    std::variant<std::monostate, uint64_t, std::string> value;

    [[nodiscard]] constexpr auto value_string() const -> std::string_view {
        return std::get<std::string>(value);
    }

    [[nodiscard]] constexpr auto value_uint64() const -> uint64_t {
        return std::get<std::uint64_t>(value);
    }

    [[nodiscard]] constexpr auto first() const -> AstNode const* {
        return &children.at(0);
    }

    [[nodiscard]] constexpr auto second() const -> AstNode const* {
        return &children.at(1);
    }

    static auto Error(Span s) -> AstNode {
        return {
            .kind = AstNodeKind::Err, .span = s, .children = {}, .value = {}};
    }

    static auto Empty(Span s = {}) -> AstNode {
        return {
            .kind = AstNodeKind::Empty, .span = s, .children = {}, .value = {}};
    }

    static auto VarDecl(Span s, std::string name, AstNode type, AstNode init)
        -> AstNode {
        return {
            .kind = AstNodeKind::VarDecl,
            .span = s,
            .children = {type, init},
            .value = {name},
        };
    }

    static auto ReturnStmt(Span s, AstNode child) -> AstNode {
        return {
            .kind = AstNodeKind::ReturnStmt,
            .span = s,
            .children = {child},
            .value = {},
        };
    }

    static auto ExprStmt(Span s, AstNode child) -> AstNode {
        return {
            .kind = AstNodeKind::ExprStmt,
            .span = s,
            .children = {child},
            .value = {},
        };
    }

    static auto Block(Span s, std::vector<AstNode> children) -> AstNode {
        return {
            .kind = AstNodeKind::Block,
            .span = s,
            .children = children,
            .value = {},
        };
    }

    static auto SourceFile(Span s, std::vector<AstNode> children) -> AstNode {
        return {
            .kind = AstNodeKind::SourceFile,
            .span = s,
            .children = children,
            .value = {},
        };
    }

    static auto Unary(Span s, AstNodeKind kind, AstNode lhs) -> AstNode {
        return {.kind = kind, .span = s, .children = {lhs}, .value = {}};
    }

    static auto Binary(Span s, AstNodeKind kind, AstNode lhs, AstNode rhs)
        -> AstNode {
        return {
            .kind = kind,
            .span = s,
            .children = {lhs, rhs},
            .value = {},
        };
    }

    static auto Int(Span s, uint64_t value) -> AstNode {
        return {
            .kind = AstNodeKind::Int,
            .span = s,
            .children = {},
            .value = value,
        };
    }

    static auto Id(Span s, std::string value) -> AstNode {
        return {
            .kind = AstNodeKind::Id,
            .span = s,
            .children = {},
            .value = value,
        };
    }
};

}  // namespace yuri

template <>
struct fmt::formatter<yuri::AstNodeKind> : formatter<string_view> {
    auto format(yuri::AstNodeKind n, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yuri::AstNode> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yuri::AstNode n, format_context& ctx) const
        -> format_context::iterator;
};
