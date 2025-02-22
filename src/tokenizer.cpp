#include "tokenizer.hpp"

#include <cstdint>
#include <vector>

#include "span.hpp"

namespace yuri {

struct Tokenizer {
    [[nodiscard]] constexpr auto is_at_end() const -> bool {
        return current == source.length();
    }

    constexpr void advance() {
        if (!is_at_end()) current++;
    }

    [[nodiscard]] constexpr auto peek() const -> uint8_t {
        if (is_at_end()) return 0;
        return source.at(current);
    }

    constexpr auto peek_and_advance() -> uint8_t {
        auto c = peek();
        advance();
        return c;
    }

    [[nodiscard]] constexpr auto span() const -> Span {
        return {.begin = start, .end = current};
    }

    [[nodiscard]] constexpr auto mkt(TokenType t) const -> Token {
        return {.type = t, .span = span()};
    }

    // ------------------------------------------------------------------------

    auto tokenize_all() -> std::vector<Token> {
        std::vector<Token> tokens;

        while (!is_at_end()) {
            tokens.push_back(tokenize_one());
        }

        return tokens;
    }

    auto tokenize_one() -> Token {
        skip_whitespace();

        start = current;
        if (is_at_end()) return mkt(TokenType::Eof);

        auto c = peek_and_advance();
        switch (c) {
            case '+': return mkt(TokenType::Plus);
            case '-': return mkt(TokenType::Minus);
            case '*': return mkt(TokenType::Star);
            case '/': return mkt(TokenType::Slash);
            case '0' ... '9': return tokenize_number();
            case 'a' ... 'z':
            case 'A' ... 'Z':
            case '_': return tokenize_id();
            default:
                er->report_error(span(), "invalid character found '{}'", c);
                return mkt(TokenType::Err);
        }
    }

    constexpr auto tokenize_number() -> Token {
        while (!is_at_end() && is_digit(peek())) advance();

        return mkt(TokenType::Int);
    }

    constexpr auto tokenize_id() -> Token {
        while (is_alpha(peek()) || is_digit(peek()) || peek() == '_') advance();

        return mkt(TokenType::Id);
    }

    constexpr void skip_whitespace() {
        while (is_whitespace(peek())) advance();
    }

    // ------------------------------------------------------------------------

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

    std::string_view source;
    ErrorReporter*   er;
    uint32_t         start{};
    uint32_t         current{};
};

auto tokenize(std::string_view source, ErrorReporter& er)
    -> std::vector<Token> {
    auto tokenizer = Tokenizer{.source = source, .er = &er};
    return tokenizer.tokenize_all();
}

}  // namespace yuri

auto fmt::formatter<yuri::TokenType>::format(yuri::TokenType t,
                                             format_context& ctx) const
    -> format_context::iterator {
    string_view name = "unknown";
    switch (t) {
        case yuri::TokenType::Err: name = "ERROR"; break;
        case yuri::TokenType::Plus: name = "Plus"; break;
        case yuri::TokenType::Minus: name = "Minus"; break;
        case yuri::TokenType::Star: name = "Star"; break;
        case yuri::TokenType::Slash: name = "Slash"; break;
        case yuri::TokenType::Int: name = "Int"; break;
        case yuri::TokenType::Eof: name = "EOF"; break;
    }
    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yuri::Token>::format(yuri::Token     t,
                                         format_context& ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "{{{}, {}}}", t.span, t.type);
}
