#pragma once

#include <vector>

#include "fmt/base.h"

namespace yuri {

enum class TypeKind {
    Err,
    Void,
    Type,
    Func,
    Int,
};

struct Type {
    TypeKind          kind;
    std::vector<Type> inner{};

    constexpr auto is_void() const -> bool { return kind == TypeKind::Void; }
    constexpr auto is_type() const -> bool { return kind == TypeKind::Type; }

    // NOTE: remember to add other integer types here in the future
    constexpr auto is_integral() const -> bool { return kind == TypeKind::Int; }

    constexpr auto operator==(Type const& o) const -> bool {
        return kind == o.kind && inner == o.inner;
    }

    static auto Void() -> Type { return {.kind = TypeKind::Void}; }

    static auto Func(Type const& return_type) -> Type {
        return {.kind = TypeKind::Func, .inner = {return_type}};
    }

    static auto Int() -> Type { return {.kind = TypeKind::Int}; }
    static auto Err() -> Type { return {.kind = TypeKind::Err}; }
    static auto make_type() -> Type { return {.kind = TypeKind::Type}; }
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
