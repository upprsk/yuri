#include "types.hpp"

#include "fmt/ranges.h"  // IWYU pragma: keep

auto fmt::formatter<yuri::TypeKind>::format(yuri::TypeKind  c,
                                            format_context& ctx) const
    -> format_context::iterator {
    using T = yuri::TypeKind;

    string_view name = "unknown";
    switch (c) {
        case T::Err: name = "Err"; break;
        case T::Type: name = "Type"; break;
        case T::Void: name = "Void"; break;
        case T::Func: name = "Func"; break;
        case T::Int: name = "Int"; break;
    }

    // return formatter<string_view>::format(name, ctx);
    return fmt::format_to(ctx.out(), "{}", name);
}

auto fmt::formatter<yuri::Type>::format(yuri::Type t, format_context& ctx) const
    -> format_context::iterator {
    if (t.inner.empty()) return fmt::format_to(ctx.out(), "{{{}}}", t.kind);
    return fmt::format_to(ctx.out(), "{{{}, {}}}", t.kind, t.inner);
}
