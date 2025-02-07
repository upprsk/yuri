#pragma once

#include "fmt/base.h"

namespace yuri {

enum class TypeKind {
    Err,
    Void,
    Func,
    Int,
};

struct Type {
    TypeKind kind;

    static constexpr auto Void() -> Type { return {.kind = TypeKind::Void}; }
    static constexpr auto Func() -> Type { return {.kind = TypeKind::Func}; }
};

}  // namespace yuri

template <>
struct fmt::formatter<yuri::TypeKind> : formatter<string_view> {
    auto format(yuri::TypeKind c, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yuri::Type> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yuri::Type t, format_context& ctx) const
        -> format_context::iterator;
};
