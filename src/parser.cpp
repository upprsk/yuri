#include "parser.hpp"

#include <charconv>
#include <cstdint>
#include <span>
#include <string_view>
#include <vector>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "fmt/color.h"
#include "span.hpp"
#include "tokenizer.hpp"

namespace yuri {

struct Parser {
    constexpr auto peek() const -> Token { return tokens[current]; }
    constexpr auto peek_prev() const -> Token {
        if (current == 0) return {.span = {}, .type = TokenType::Err};
        return tokens[current - 1];
    }

    constexpr auto span() const -> Span { return peek().span; }
    constexpr auto prev_span() const -> Span { return peek_prev().span; }

    constexpr auto peek_and_advance() -> Token {
        auto t = peek();
        advance();

        return t;
    }

    constexpr void advance() {
        if (!is_at_end()) current++;
    }

    constexpr auto is_at_end() const -> bool { return peek().is_eof(); }

    constexpr auto span_from(std::span<AstNode const> nodes,
                             Span initial = {}) const -> Span {
        if (nodes.empty()) return initial;

        return {.begin = nodes[0].span.begin,
                .end = nodes[nodes.size() - 1].span.end};
    }

    constexpr auto consume(TokenType expected) -> bool {
        auto t = peek_and_advance();
        if (t.type == expected) return true;

        er->report_error(t.span, "expected {}, but got {}", expected, t.type);

        return false;
    }

    constexpr auto consume_id(std::string_view expected) -> bool {
        auto t = peek_and_advance();
        if (t.is_kw(source, expected)) return true;

        er->report_error(t.span, "expected {}, but got {}", expected,
                         t.span.str(source));

        return false;
    }

#define try_consume(T, span, errmsg) \
    if (!consume(T)) return AstNode::Err(span, errmsg)

    // ------------------------------------------------------------------------

    auto parse_source_file() -> AstNode {
        std::vector<AstNode> nodes;

        while (!is_at_end()) {
            nodes.push_back(parse_top_decl());
        }

        return AstNode::SourceFile(span_from(nodes), std::move(nodes));
    }

    // decls ------------------------------------------------------------------

    auto parse_top_decl() -> AstNode { return parse_func_decl(); }

    auto parse_func_decl() -> AstNode {
        auto start = span();
        if (!consume_id("func")) return AstNode::Err(start, "expected 'func'");

        auto name = peek();
        try_consume(TokenType::Id, name.span, "expected function name");

        try_consume(TokenType::Lparen, prev_span(), "expected '('");
        try_consume(TokenType::Rparen, prev_span(), "expected ')'");

        auto ret = AstNode::Nil();
        if (peek().type != TokenType::Lbrace) {
            ret = parse_expr();
        }

        auto body = parse_block();

        return AstNode::Func(start.extend(body.span),
                             std::string{name.span.str(source)}, {}, ret, body);
    }

    auto parse_var_decl() -> AstNode {
        auto start = span();
        if (!consume_id("var")) return AstNode::Err(start, "expected 'var'");

        auto name = peek();
        try_consume(TokenType::Id, name.span, "expected variable name");

        AstNode type = AstNode::Nil();
        if (peek().type == TokenType::Colon) {
            advance();
            type = parse_expr();
        }

        try_consume(TokenType::Equal, prev_span(), "expected '='");

        auto init = parse_expr();
        auto end = span();
        try_consume(TokenType::Semi, prev_span(), "expected ';'");

        return AstNode::VarDecl(start.extend(end),
                                std::string{name.span.str(source)}, type, init);
    }

    // stmts ------------------------------------------------------------------

    auto parse_stmt() -> AstNode {
        auto t = peek();
        if (t.is_kw(source, "return")) return parse_return_stmt();
        if (t.is_kw(source, "if")) return parse_if_stmt();
        if (t.is_kw(source, "while")) return parse_while_stmt();
        if (t.is_kw(source, "var")) return parse_var_decl();

        return parse_expr_stmt();
    }

    auto parse_block() -> AstNode {
        auto start = span();
        try_consume(TokenType::Lbrace, start, "expected '{'");

        std::vector<AstNode> children;

        while (!is_at_end() && peek().type != TokenType::Rbrace) {
            children.push_back(parse_stmt());
        }

        auto end = span();
        try_consume(TokenType::Rbrace, end, "expected '}'");

        return AstNode::Block(start.extend(end), children);
    }

    auto parse_expr_stmt() -> AstNode {
        auto expr = parse_expr();
        auto end = span();

        // assign
        //
        // > assign does not have a dedicated function, we use this one for that
        if (peek().type == TokenType::Equal) {
            advance();
            auto rhs = parse_expr();

            auto end = span();
            try_consume(TokenType::Semi, end, "expected ';'");

            return AstNode::Binary(expr.span.extend(end), AstNodeKind::Assign,
                                   expr, rhs);
        }

        try_consume(TokenType::Semi, prev_span(), "expected ';'");

        return AstNode::Unary(expr.span.extend(end), AstNodeKind::ExprStmt,
                              expr);
    }

    auto parse_return_stmt() -> AstNode {
        auto start = span();
        if (!consume_id("return"))
            return AstNode::Err(start, "expected 'return'");

        auto expr = parse_expr();

        auto end = span();
        try_consume(TokenType::Semi, prev_span(), "expected ';'");

        return AstNode::Unary(start.extend(end), AstNodeKind::ReturnStmt, expr);
    }

    auto parse_if_stmt() -> AstNode {
        auto start = span();
        if (!consume_id("if")) return AstNode::Err(start, "expected 'if'");

        auto expr = parse_expr();
        auto wt = parse_block();
        auto wf = AstNode::Nil();

        if (peek().is_kw(source, "else")) {
            advance();

            if (peek().is_kw(source, "if")) {
                wf = parse_if_stmt();
            } else {
                wf = parse_block();
            }
        }

        return AstNode::IfStmt(start.extend(wf.is_nil() ? wt.span : wf.span),
                               expr, wt, wf);
    }

    auto parse_while_stmt() -> AstNode {
        auto start = span();
        if (!consume_id("while"))
            return AstNode::Err(start, "expected 'while'");

        auto expr = parse_expr();
        auto body = parse_block();

        return AstNode::Binary(start.extend(body.span), AstNodeKind::WhileStmt,
                               expr, body);
    }

    // exprs ------------------------------------------------------------------

    auto parse_expr() -> AstNode { return parse_comparison(); }

    auto parse_comparison() -> AstNode {
        auto left = parse_additive();

        auto        t = peek();
        AstNodeKind kind;
        if (t.type == TokenType::Less)
            kind = AstNodeKind::LessThan;
        else if (t.type == TokenType::LessEqual)
            kind = AstNodeKind::LessThanEqual;
        else if (t.type == TokenType::Greater)
            kind = AstNodeKind::GreaterThan;
        else if (t.type == TokenType::GreaterEqual)
            kind = AstNodeKind::GreaterThanEqual;
        else if (t.type == TokenType::EqualEqual)
            kind = AstNodeKind::Equal;
        else
            return left;

        advance();
        AstNode right = parse_comparison();

        return AstNode::Binary(left.span.extend(right.span), kind, left, right);
    }

    auto parse_additive() -> AstNode {
        auto left = parse_multiplicative();

        auto        t = peek();
        AstNodeKind kind;
        if (t.type == TokenType::Plus)
            kind = AstNodeKind::Add;
        else if (t.type == TokenType::Minus)
            kind = AstNodeKind::Sub;
        else
            return left;

        advance();
        AstNode right = parse_additive();

        return AstNode::Binary(left.span.extend(right.span), kind, left, right);
    }

    auto parse_multiplicative() -> AstNode {
        auto left = parse_call();

        auto        t = peek();
        AstNodeKind kind;
        if (t.type == TokenType::Star)
            kind = AstNodeKind::Mul;
        else if (t.type == TokenType::Slash)
            kind = AstNodeKind::Div;
        else
            return left;

        advance();
        AstNode right = parse_multiplicative();

        return AstNode::Binary(left.span.extend(right.span), kind, left, right);
    }

    auto parse_call() -> AstNode {
        auto expr = parse_primary();

        if (peek().type != TokenType::Lparen) return expr;
        advance();

        auto end = span();
        try_consume(TokenType::Rparen, end, "expected ')'");

        return AstNode::Call(expr.span.extend(end), expr, {});
    }

    auto parse_primary() -> AstNode {
        auto t = peek_and_advance();
        if (t.type == TokenType::Id) {
            return AstNode::Id(t.span, std::string{t.span.str(source)});
        }

        if (t.type == TokenType::Lparen) {
            auto expr = parse_expr();
            try_consume(TokenType::Rparen, prev_span(), "expected ')'");

            return expr;
        }

        if (t.type == TokenType::Int) {
            auto     s = t.span.str(source);
            uint64_t value = 0xDEAD'BEEF;

            // NOTE: ignoring errors
            std::from_chars(s.data(), s.data() + s.length(), value);

            return AstNode::Int(t.span, value);
        }

        er->report_error(t.span,
                         "invalid syntax, expected expression, found {}",
                         t.span.str(source));
        return AstNode::Err(t.span, "expected expression");
    }

    // ------------------------------------------------------------------------

    ErrorReporter         *er;
    std::string_view       source;
    std::span<Token const> tokens;
    uint32_t               current{};
};

auto parse(ErrorReporter *er, std::string_view source,
           std::span<Token const> tokens) -> AstNode {
    auto p = Parser{.er = er, .source = source, .tokens = tokens};

    return p.parse_source_file();
}

}  // namespace yuri
