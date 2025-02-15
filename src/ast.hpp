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
    AsmFunc,
    FuncDeclArg,
    VarDecl,
    Block,
    ExprStmt,
    ReturnStmt,
    IfStmt,
    WhileStmt,
    Assign,
    Array,
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,
    Call,
    Index,
    Ref,
    DeRef,
    Ptr,
    Id,
    Int,
    Str,
    Err,
};

struct AstNode {
    std::variant<std::monostate, std::string, uint64_t> value;
    std::vector<AstNode>                                children;
    Span                                                span;
    AstNodeKind                                         kind;
    Type                                                type{};

    static auto Nil() -> AstNode {
        return {
            .value = {}, .children = {}, .span = {}, .kind = AstNodeKind::Nil};
    }

    static auto SourceFile(Span span, std::vector<AstNode> const& children)
        -> AstNode {
        return {
            .value = {},
            .children = children,
            .span = span,
            .kind = AstNodeKind::SourceFile,
        };
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

    static auto AsmFunc(Span span, std::string const& name,
                        std::vector<AstNode> const& args, AstNode const& ret,
                        AstNode const& body) -> AstNode {
        auto children = args;
        children.push_back(ret);
        children.push_back(body);

        return {
            .value = name,
            .children = std::move(children),
            .span = span,
            .kind = AstNodeKind::AsmFunc,
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
            .value = {},
            .children = children,
            .span = span,
            .kind = AstNodeKind::Block,
        };
    }

    static auto IfStmt(Span span, AstNode const& cond, AstNode const& wt,
                       AstNode const& wf) -> AstNode {
        return {
            .value = {},
            .children = {cond, wt, wf},
            .span = span,
            .kind = AstNodeKind::IfStmt,
        };
    }

    static auto Array(Span span, AstNode const& n, AstNode const& ty,
                      std::vector<AstNode> const& items) -> AstNode {
        std::vector children{n, ty};
        for (auto const& n : items) children.push_back(n);

        return {
            .value = {},
            .children = children,
            .span = span,
            .kind = AstNodeKind::Array,
        };
    }

    static auto Unary(Span span, AstNodeKind kind, AstNode const& child)
        -> AstNode {
        return {
            .value = {},
            .children = {child},
            .span = span,
            .kind = kind,
        };
    }

    static auto Binary(Span span, AstNodeKind kind, AstNode const& left,
                       AstNode const& right) -> AstNode {
        return {
            .value = {},
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
            .value = {},
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

    static auto Str(Span span, std::string const& value) -> AstNode {
        if (value.at(0) != '"')
            return AstNode::Err(span, "invalid string start character");
        if (value.at(value.size() - 1) != '"')
            return AstNode::Err(span, "invalid string end character");

        auto v = escape_string(value.substr(1, value.size() - 2));

        return {
            .value = v,
            .children = {},
            .span = span,
            .kind = AstNodeKind::Str,
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

    [[nodiscard]] constexpr auto first() const -> AstNode const& {
        return children.at(0);
    }

    [[nodiscard]] constexpr auto second() const -> AstNode const& {
        return children.at(1);
    }

    [[nodiscard]] constexpr auto last() const -> AstNode const& {
        return children.at(children.size() - 1);
    }

    [[nodiscard]] constexpr auto value_string() const -> std::string const& {
        return std::get<std::string>(value);
    }

    [[nodiscard]] constexpr auto value_int() const -> uint64_t {
        return std::get<uint64_t>(value);
    }

    [[nodiscard]] constexpr auto is_lvalue() const -> bool {
        return kind == AstNodeKind::Id || kind == AstNodeKind::DeRef ||
               kind == AstNodeKind::Index;
    }

    [[nodiscard]] constexpr auto is_nil() const -> bool {
        return kind == AstNodeKind::Nil;
    }
    [[nodiscard]] constexpr auto is_id() const -> bool {
        return kind == AstNodeKind::Id;
    }
    [[nodiscard]] constexpr auto is_int() const -> bool {
        return kind == AstNodeKind::Int;
    }
    [[nodiscard]] constexpr auto is_deref() const -> bool {
        return kind == AstNodeKind::DeRef;
    }
    [[nodiscard]] constexpr auto is_index() const -> bool {
        return kind == AstNodeKind::Index;
    }
    [[nodiscard]] constexpr auto is_func() const -> bool {
        return kind == AstNodeKind::Func;
    }
    [[nodiscard]] constexpr auto is_block() const -> bool {
        return kind == AstNodeKind::Block;
    }
    [[nodiscard]] constexpr auto is_source_file() const -> bool {
        return kind == AstNodeKind::SourceFile;
    }
    [[nodiscard]] constexpr auto is_func_arg() const -> bool {
        return kind == AstNodeKind::FuncDeclArg;
    }

    constexpr auto set_type(Type const& t) -> Type { return type = t; }

    auto add_types(Env& env, ErrorReporter& er) -> Type;
    auto eval_to_type(Env& env, ErrorReporter& er) -> Type;

    static constexpr auto escape_string(std::string_view s) -> std::string {
        std::string out;
        while (!s.empty()) {
            if (s.at(0) == '\\') {
                s = s.substr(1);
                switch (s.at(0)) {
                    case 'n': out.push_back('\n'); break;
                    case 't': out.push_back('\t'); break;
                    case 'v': out.push_back('\v'); break;
                    case 'r': out.push_back('\r'); break;
                    default: out.push_back(s.at(0)); break;
                }

                s = s.substr(1);
            } else {
                out.push_back(s.at(0));
                s = s.substr(1);
            }
        }

        return out;
    }
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
