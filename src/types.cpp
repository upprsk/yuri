#include "types.hpp"

auto fmt::formatter<yuri::TypeKind>::format(yuri::TypeKind  c,
                                            format_context& ctx) const
    -> format_context::iterator {
    using T = yuri::TypeKind;

    string_view name = "unknown";
    switch (c) {
        case T::Err: name = "Err"; break;
        case T::Void: name = "Void"; break;
        case T::Func: name = "Func"; break;
        case T::Int: name = "Int"; break;
    }

    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yuri::Type>::format(yuri::Type t, format_context& ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "{{{}}}", t.kind);
}
