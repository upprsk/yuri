#pragma once

#include <cstdint>
#include <string_view>
#include <vector>

#include "error_reporter.hpp"
#include "fmt/base.h"
#include "span.hpp"

namespace yuri {

enum class TokenType : uint8_t {
    Equal,
    EqualEqual,
    Less,
    LessLess,
    LessEqual,
    Greater,
    GreaterGreater,
    GreaterEqual,
    Plus,
    PlusPlus,
    PlusEqual,
    Minus,
    MinusMinus,
    MinusEqual,
    Star,
    StarStar,
    StarEqual,
    Slash,
    SlashEqual,
    Bang,
    BangEqual,
    Ampersand,
    Semi,
    Colon,
    Comma,
    Dot,
    DotDot,
    DotStar,
    DotEqual,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,
    Id,
    Int,
    Str,
    Comment,
    Eof,
    Err,
};

struct Token {
    Span      span;
    TokenType type;

#define define_constructor(T)                        \
    static constexpr auto T(Span span) -> Token {    \
        return {.span = span, .type = TokenType::T}; \
    }

    define_constructor(Equal);
    define_constructor(EqualEqual);
    define_constructor(Less);
    define_constructor(LessLess);
    define_constructor(LessEqual);
    define_constructor(Greater);
    define_constructor(GreaterGreater);
    define_constructor(GreaterEqual);
    define_constructor(Plus);
    define_constructor(PlusPlus);
    define_constructor(PlusEqual);
    define_constructor(Minus);
    define_constructor(MinusMinus);
    define_constructor(MinusEqual);
    define_constructor(Star);
    define_constructor(StarStar);
    define_constructor(StarEqual);
    define_constructor(Slash);
    define_constructor(SlashEqual);
    define_constructor(Bang);
    define_constructor(BangEqual);
    define_constructor(Ampersand);
    define_constructor(Semi);
    define_constructor(Colon);
    define_constructor(Comma);
    define_constructor(Dot);
    define_constructor(DotDot);
    define_constructor(DotStar);
    define_constructor(DotEqual);
    define_constructor(Lparen);
    define_constructor(Rparen);
    define_constructor(Lbrace);
    define_constructor(Rbrace);
    define_constructor(Lbracket);
    define_constructor(Rbracket);
    define_constructor(Id);
    define_constructor(Int);
    define_constructor(Str);
    define_constructor(Comment);
    define_constructor(Eof);
    define_constructor(Err);

#undef define_constructor

    [[nodiscard]] constexpr auto is_eof() const -> bool {
        return type == TokenType::Eof;
    }

    [[nodiscard]] constexpr auto is_id() const -> bool {
        return type == TokenType::Id;
    }

    [[nodiscard]] constexpr auto is_comment() const -> bool {
        return type == TokenType::Comment;
    }

    [[nodiscard]] constexpr auto is_kw(std::string_view source,
                                       std::string_view s) const -> bool {
        return type == TokenType::Id && span.str(source) == s;
    }
};

auto tokenize(ErrorReporter* er, std::string_view source) -> std::vector<Token>;

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
