#include "tokenizer.hpp"

#include <cstdint>
#include <string_view>
#include <vector>

#include "error_reporter.hpp"

namespace yuri {

struct Tokenizer {
    constexpr auto span() const -> Span { return {start, current}; }
    constexpr auto peek() const -> uint8_t {
        if (is_at_end()) return 0;

        return source.at(current);
    }

    constexpr auto peek_and_advance() -> uint8_t {
        auto c = peek();
        advance();

        return c;
    }

    constexpr auto match(uint8_t c) -> bool {
        if (peek() == c) {
            advance();
            return true;
        }

        return false;
    }

    constexpr void advance() {
        if (!is_at_end()) current++;
    }

    constexpr auto is_at_end() const -> bool {
        return current == source.length();
    }

    constexpr static auto is_digit(uint8_t c) -> bool {
        return c >= '0' && c <= '9';
    }

    constexpr static auto is_alpha(uint8_t c) -> bool {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
    }

    constexpr static auto is_whitespace(uint8_t c) -> bool {
        return c == '\n' || c == '\r' || c == '\t' || c == ' ';
    }

    // ------------------------------------------------------------------------

    constexpr auto tokenize() -> std::vector<Token> {
        std::vector<Token> tokens;

        while (!is_at_end()) {
            tokens.push_back(tokenize_one());
        }

        return tokens;
    }

    constexpr auto tokenize_one() -> Token {
        consume_whitespace();
        if (is_at_end()) return Token::Eof(span());

        start = current;

        auto c = peek_and_advance();

        switch (c) {
            case '=':
                if (match('=')) return Token::EqualEqual(span());
                return Token::Equal(span());
            case '<':
                if (match('=')) return Token::LessEqual(span());
                if (match('<')) return Token::LessLess(span());
                return Token::Less(span());
            case '>':
                if (match('=')) return Token::GreaterEqual(span());
                if (match('>')) return Token::GreaterGreater(span());
                return Token::Greater(span());
            case '+':
                if (match('=')) return Token::PlusEqual(span());
                if (match('+')) return Token::PlusPlus(span());
                return Token::Plus(span());
            case '-':
                if (match('=')) return Token::MinusEqual(span());
                if (match('+')) return Token::MinusMinus(span());
                return Token::Minus(span());
            case '*':
                if (match('=')) return Token::StarEqual(span());
                if (match('*')) return Token::StarStar(span());
                return Token::Star(span());
            case '/':
                if (match('=')) return Token::SlashEqual(span());
                if (match('/')) return tokenize_comment();
                return Token::Slash(span());
            case '!':
                if (match('=')) return Token::BangEqual(span());
                return Token::Bang(span());
            case ':': return Token::Colon(span());
            case ';': return Token::Semi(span());
            case ',': return Token::Comma(span());
            case '(': return Token::Lparen(span());
            case ')': return Token::Rparen(span());
            case '{': return Token::Lbrace(span());
            case '}': return Token::Rbrace(span());
            case '[': return Token::Lbracket(span());
            case ']': return Token::Rbracket(span());
            case 'a' ... 'z':
            case 'A' ... 'Z':
            case '_': return tokenize_id();
            case '0' ... '9': return tokenize_number();
            case '"': return tokenize_string();
            default: {
                er->report_error(span(), "unexpected character: {:?}",
                                 static_cast<char>(c));
                return Token::Err(span());
            } break;
        }
    }

    constexpr auto tokenize_number() -> Token {
        while (is_digit(peek()) || peek() == '_') advance();

        return Token::Int(span());
    }

    constexpr auto tokenize_id() -> Token {
        while (is_alpha(peek()) || is_digit(peek()) || peek() == '_') advance();

        return Token::Id(span());
    }

    auto tokenize_string() -> Token {
        while (!is_at_end() && peek() != '"') {
            if (peek() == '\\') advance();
            advance();
        }

        if (!match('"')) {
            er->report_error(span(), "unterminated string");
        }

        return Token::Str(span());
    }

    constexpr auto tokenize_comment() -> Token {
        while (!is_at_end() && peek() != '\n') advance();

        return Token::Comment(span());
    }

    constexpr void consume_whitespace() {
        while (is_whitespace(peek())) advance();
    }

    //-------------------------------------------------------------------------

    ErrorReporter* er;

    std::string_view source;

    uint32_t start{};
    uint32_t current{};
};

auto tokenize(ErrorReporter* er, std::string_view source)
    -> std::vector<Token> {
    auto t = Tokenizer{.er = er, .source = source};

    return t.tokenize();
}

}  // namespace yuri

auto fmt::formatter<yuri::TokenType>::format(yuri::TokenType t,
                                             format_context& ctx) const
    -> format_context::iterator {
    using T = yuri::TokenType;

    string_view name = "unknown";
    switch (t) {
        case T::Equal: name = "Equal"; break;
        case T::EqualEqual: name = "EqualEqual"; break;
        case T::Less: name = "Less"; break;
        case T::LessLess: name = "LessLess"; break;
        case T::LessEqual: name = "LessEqual"; break;
        case T::Greater: name = "Greater"; break;
        case T::GreaterGreater: name = "GreaterGreater"; break;
        case T::GreaterEqual: name = "GreaterEqual"; break;
        case T::Plus: name = "Plus"; break;
        case T::PlusPlus: name = "PlusPlus"; break;
        case T::PlusEqual: name = "PlusEqual"; break;
        case T::Minus: name = "Minus"; break;
        case T::MinusMinus: name = "MinusMinus"; break;
        case T::MinusEqual: name = "MinusEqual"; break;
        case T::Star: name = "Star"; break;
        case T::StarStar: name = "StarStar"; break;
        case T::StarEqual: name = "StarEqual"; break;
        case T::Slash: name = "Slash"; break;
        case T::SlashEqual: name = "SlashEqual"; break;
        case T::Bang: name = "Bang"; break;
        case T::BangEqual: name = "BangEqual"; break;
        case T::Semi: name = "Semi"; break;
        case T::Colon: name = "Colon"; break;
        case T::Comma: name = "Comma"; break;
        case T::Lparen: name = "Lparen"; break;
        case T::Rparen: name = "Rparen"; break;
        case T::Lbrace: name = "Lbrace"; break;
        case T::Rbrace: name = "Rbrace"; break;
        case T::Lbracket: name = "Lbracket"; break;
        case T::Rbracket: name = "Rbracket"; break;
        case T::Id: name = "Id"; break;
        case T::Int: name = "Int"; break;
        case T::Str: name = "Str"; break;
        case T::Comment: name = "Comment"; break;
        case T::Eof: name = "Eof"; break;
        case T::Err: name = "Err"; break;
    }
    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yuri::Token>::format(yuri::Token     t,
                                         format_context& ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "{{{}, {}}}", t.span, t.type);
}
