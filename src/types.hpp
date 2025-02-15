#pragma once

#include <cstdint>
#include <vector>

#include "fmt/base.h"

namespace yuri {

enum class TypeKind {
    Err,
    Void,
    Type,
    Func,
    Int,
    Bool,
    Ptr,
};

struct Type {
    TypeKind          kind;
    std::vector<Type> inner;

    [[nodiscard]] constexpr auto is_err() const -> bool {
        return kind == TypeKind::Err;
    }
    [[nodiscard]] constexpr auto is_void() const -> bool {
        return kind == TypeKind::Void;
    }
    [[nodiscard]] constexpr auto is_type() const -> bool {
        return kind == TypeKind::Type;
    }
    [[nodiscard]] constexpr auto is_bool() const -> bool {
        return kind == TypeKind::Bool;
    }
    [[nodiscard]] constexpr auto is_func() const -> bool {
        return kind == TypeKind::Func;
    }
    [[nodiscard]] constexpr auto is_ptr() const -> bool {
        return kind == TypeKind::Ptr;
    }

    // NOTE: remember to add other integer types here in the future
    [[nodiscard]] constexpr auto is_integral() const -> bool {
        return kind == TypeKind::Int;
    }

    [[nodiscard]] constexpr auto bytesize() const -> uint32_t {
        switch (kind) {
            case TypeKind::Err:
            case TypeKind::Void:
            case TypeKind::Type:
            case TypeKind::Func: return 0;
            case TypeKind::Int:
            case TypeKind::Bool:
            case TypeKind::Ptr: return 4;
        }

        return 0;
    }

    constexpr auto operator==(Type const& o) const -> bool {
        return kind == o.kind && inner == o.inner;
    }

    static auto Void() -> Type { return {.kind = TypeKind::Void, .inner = {}}; }

    static auto Func(std::vector<Type> const& args, Type const& return_type)
        -> Type {
        auto inner = args;
        inner.push_back(return_type);

        return {.kind = TypeKind::Func, .inner = inner};
    }

    static auto Int() -> Type { return {.kind = TypeKind::Int, .inner = {}}; }
    static auto Bool() -> Type { return {.kind = TypeKind::Bool, .inner = {}}; }
    static auto Ptr(Type const& t) -> Type {
        return {.kind = TypeKind::Ptr, .inner = {t}};
    }
    static auto Err() -> Type { return {.kind = TypeKind::Err, .inner = {}}; }
    static auto make_type() -> Type {
        return {.kind = TypeKind::Type, .inner = {}};
    }
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
