#include "parser.hpp"

#include <charconv>
#include <cstdint>
#include <span>
#include <string_view>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "span.hpp"
#include "tokenizer.hpp"

namespace yuri {

struct Parser {
    [[nodiscard]] constexpr auto peek() const -> Token {
        return tokens[current];
    }

    [[nodiscard]] constexpr auto peek_prev() const -> Token {
        return tokens[current - 1];
    }

    [[nodiscard]] constexpr auto span() const -> Span { return peek().span; }

    [[nodiscard]] constexpr auto prev_span() const -> Span {
        return peek_prev().span;
    }

    [[nodiscard]] constexpr auto is_at_end() const -> bool {
        return peek().type == TokenType::Eof;
    }

    constexpr void advance() {
        if (!is_at_end()) current++;
    }

    [[nodiscard]] constexpr auto peek_and_advance() -> Token {
        auto t = peek();
        advance();
        return t;
    }

    [[nodiscard]] constexpr auto check(TokenType t) const -> bool {
        return peek().type == t;
    }

    [[nodiscard]] constexpr auto check(std::string_view kw) const -> bool {
        return peek().type == TokenType::Id && peek().span.src(source) == kw;
    }

    [[nodiscard]] constexpr auto match(TokenType t) -> bool {
        if (!check(t)) return false;

        advance();
        return true;
    }

    [[nodiscard]] constexpr auto match(std::string_view kw) -> bool {
        if (!check(kw)) return false;

        advance();
        return true;
    }

    // ------------------------------------------------------------------------

    [[nodiscard]] auto consume(TokenType t) -> bool {
        if (match(t)) return true;

        er->report_error(span(), "expected {}, but got {}", t, peek().type);
        advance();

        return false;
    }

    [[nodiscard]] auto consume(std::string_view kw) -> bool {
        if (match(kw)) return true;

        er->report_error(span(), "expected `{}`, but got {}", kw, peek().type);
        advance();

        return false;
    }

    // ------------------------------------------------------------------------

    auto parse_source_file() -> AstNode {
        std::vector<AstNode> stmts;

        while (!is_at_end()) {
            stmts.push_back(parse_stmt());
        }

        // no need to actually abort here, just report an error
        (void)consume(TokenType::Eof);

        auto span =
            stmts.empty()
                ? Span{}
                : stmts.at(0).span.extend(stmts.at(stmts.size() - 1).span);

        return AstNode::Block(span, stmts);
    }

    auto parse_stmt() -> AstNode {
        if (check("var")) return parse_var_decl();
        if (check("return")) return parse_return_stmt();

        auto lhs = parse_expr();
        if (match(TokenType::Equal)) {
            auto rhs = parse_expr();

            if (!consume(TokenType::Semi)) return AstNode::Error(prev_span());

            return AstNode::Assign(lhs.span.extend(prev_span()), lhs, rhs);
        }

        if (!consume(TokenType::Semi)) return AstNode::Error(prev_span());

        return AstNode::ExprStmt(lhs.span.extend(prev_span()), lhs);
    }

    auto parse_var_decl() -> AstNode {
        auto s = span();
        if (!consume("var")) return AstNode::Error(s);

        auto id = peek();
        if (!consume(TokenType::Id))
            return AstNode::Error(s.extend(prev_span()));

        if (!consume(TokenType::Equal))
            return AstNode::Error(s.extend(prev_span()));

        auto init = parse_expr();

        if (!consume(TokenType::Semi))
            return AstNode::Error(s.extend(prev_span()));

        return AstNode::VarDecl(s.extend(prev_span()),
                                std::string{id.span.src(source)}, init);
    }

    auto parse_return_stmt() -> AstNode {
        auto s = span();
        if (!consume("return")) return AstNode::Error(s);

        auto child = parse_expr();

        if (!consume(TokenType::Semi))
            return AstNode::Error(s.extend(prev_span()));

        return AstNode::ReturnStmt(s.extend(prev_span()), child);
    }

    // ------------------------------------------------------------------------

    auto parse_expr() -> AstNode { return parse_comp(); }

    auto parse_comp() -> AstNode {
        auto lhs = parse_term();
        while (check(TokenType::EqualEqual) || check(TokenType::BangEqual) ||
               check(TokenType::Less) || check(TokenType::LessEqual) ||
               check(TokenType::Greater) || check(TokenType::GreaterEqual)) {
            auto t = peek_and_advance();

            auto kind = AstNodeKind::Err;
            switch (t.type) {
                case TokenType::EqualEqual: kind = AstNodeKind::Eq; break;
                case TokenType::BangEqual: kind = AstNodeKind::Neq; break;
                case TokenType::Less: kind = AstNodeKind::Lt; break;
                case TokenType::LessEqual: kind = AstNodeKind::Lte; break;
                case TokenType::Greater: kind = AstNodeKind::Gt; break;
                case TokenType::GreaterEqual: kind = AstNodeKind::Gte; break;
                default: __builtin_unreachable();
            }

            auto rhs = parse_term();
            lhs = AstNode::Binary(lhs.span.extend(rhs.span), kind, lhs, rhs);
        }

        return lhs;
    }

    auto parse_term() -> AstNode {
        auto lhs = parse_factor();
        while (check(TokenType::Plus) || check(TokenType::Minus)) {
            auto t = peek_and_advance();

            auto kind = AstNodeKind::Err;
            switch (t.type) {
                case TokenType::Plus: kind = AstNodeKind::Add; break;
                case TokenType::Minus: kind = AstNodeKind::Sub; break;
                default: __builtin_unreachable();
            }

            auto rhs = parse_factor();
            lhs = AstNode::Binary(lhs.span.extend(rhs.span), kind, lhs, rhs);
        }

        return lhs;
    }

    auto parse_factor() -> AstNode {
        auto lhs = parse_unary();
        while (check(TokenType::Star) || check(TokenType::Slash)) {
            auto t = peek_and_advance();

            auto kind = AstNodeKind::Err;
            switch (t.type) {
                case TokenType::Star: kind = AstNodeKind::Mul; break;
                case TokenType::Slash: kind = AstNodeKind::Div; break;
                default: __builtin_unreachable();
            }

            auto rhs = parse_unary();
            lhs = AstNode::Binary(lhs.span.extend(rhs.span), kind, lhs, rhs);
        }

        return lhs;
    }

    auto parse_unary() -> AstNode {
        auto s = span();

        if (match(TokenType::Minus)) {
            auto child = parse_unary();
            return AstNode::Unary(s.extend(child.span), AstNodeKind::Neg,
                                  child);
        }

        return parse_primary();
    }

    auto parse_primary() -> AstNode {
        if (match(TokenType::Lparen)) {
            auto s = prev_span();
            auto child = parse_expr();

            if (!consume(TokenType::Rparen))
                return AstNode::Error(s.extend(child.span));

            return child;
        }

        if (match(TokenType::Int)) {
            auto s = prev_span();
            auto str = s.src(source);

            uint64_t value;
            std::from_chars(str.data(), str.data() + str.length(), value);

            return AstNode::Int(s, value);
        }

        if (match(TokenType::Id)) {
            auto s = prev_span();
            auto str = s.src(source);

            return AstNode::Id(s, std::string{str});
        }

        er->report_error(span(), "expected expression, found {}", peek().type);
        advance();

        return AstNode::Error(prev_span());
    }

    // ------------------------------------------------------------------------

    std::span<Token const> tokens;
    std::string_view       source;
    ErrorReporter*         er;
    size_t                 current{};
};

auto parse(std::span<Token const> tokens, std::string_view src,
           ErrorReporter& er) -> AstNode {
    auto p = Parser{.tokens = tokens, .source = src, .er = &er};
    return p.parse_source_file();
}

}  // namespace yuri
