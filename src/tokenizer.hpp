#pragma once

#include <string_view>
#include <vector>

#include "error_reporter.hpp"
#include "span.hpp"

namespace yuri {

enum class TokenType {
    Err,
    Plus,
    Minus,
    Star,
    Slash,
    Eof,
};

struct Token {
    TokenType type;
    Span      span;
};

auto tokenize(std::string_view source, ErrorReporter& er) -> std::vector<Token>;

}  // namespace yuri

template <>
struct fmt::formatter<yuri::TokenType> : formatter<string_view> {
    auto format(yuri::TokenType t, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yuri::Token> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yuri::Token t, format_context& ctx) const
        -> format_context::iterator;
};
