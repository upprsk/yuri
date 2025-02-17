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
    Half,
    Byte,
    Bool,
    Ptr,
    Array,
};

struct Type {
    TypeKind          kind;
    std::vector<Type> inner;
    size_t            length;

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
    [[nodiscard]] constexpr auto is_array() const -> bool {
        return kind == TypeKind::Array;
    }

    [[nodiscard]] constexpr auto is_integral() const -> bool {
        return kind == TypeKind::Int || kind == TypeKind::Half ||
               kind == TypeKind::Byte;
    }

    [[nodiscard]] constexpr auto bytesize() const -> uint32_t {
        switch (kind) {
            case TypeKind::Err:
            case TypeKind::Void:
            case TypeKind::Type:
            case TypeKind::Func: return 0;
            case TypeKind::Int:
            case TypeKind::Ptr: return 4;
            case TypeKind::Half: return 2;
            case TypeKind::Bool:
            case TypeKind::Byte: return 1;
            case TypeKind::Array: return inner.at(0).bytesize() * length;
        }

        return 0;
    }

    [[nodiscard]] constexpr auto bytealign() const -> uint32_t {
        switch (kind) {
            case TypeKind::Err:
            case TypeKind::Void:
            case TypeKind::Type:
            case TypeKind::Func: return 0;
            case TypeKind::Int:
            case TypeKind::Ptr: return 4;
            case TypeKind::Half: return 2;
            case TypeKind::Byte:
            case TypeKind::Bool: return 1;
            case TypeKind::Array: return inner.at(0).bytealign();
        }

        return 0;
    }

    constexpr auto operator==(Type const& o) const -> bool {
        return kind == o.kind && inner == o.inner;
    }

    static auto Void() -> Type {
        return {.kind = TypeKind::Void, .inner = {}, .length = 0};
    }

    static auto Func(std::vector<Type> const& args, Type const& return_type)
        -> Type {
        auto inner = args;
        inner.push_back(return_type);

        return {.kind = TypeKind::Func, .inner = inner, .length = 1};
    }

    static auto Int() -> Type {
        return {.kind = TypeKind::Int, .inner = {}, .length = 1};
    }
    static auto Half() -> Type {
        return {.kind = TypeKind::Half, .inner = {}, .length = 1};
    }
    static auto Byte() -> Type {
        return {.kind = TypeKind::Byte, .inner = {}, .length = 1};
    }
    static auto Bool() -> Type {
        return {.kind = TypeKind::Bool, .inner = {}, .length = 1};
    }
    static auto Ptr(Type const& t) -> Type {
        return {.kind = TypeKind::Ptr, .inner = {t}, .length = 1};
    }
    static auto Array(Type const& t, size_t length) -> Type {
        return {.kind = TypeKind::Array, .inner = {t}, .length = length};
    }
    static auto Err() -> Type {
        return {.kind = TypeKind::Err, .inner = {}, .length = 1};
    }
    static auto make_type() -> Type {
        return {.kind = TypeKind::Type, .inner = {}, .length = 1};
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
