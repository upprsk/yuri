#include "types.hpp"

#include <span>

#include "fmt/base.h"
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
        case T::Bool: name = "Bool"; break;
        case T::Ptr: name = "Ptr"; break;
        case T::Array: name = "Array"; break;
        case T::Half: name = "Half"; break;
        case T::Byte: name = "Byte"; break;
    }

    // return formatter<string_view>::format(name, ctx);
    return fmt::format_to(ctx.out(), "{}", name);
}

auto fmt::formatter<yuri::Type>::format(yuri::Type t, format_context& ctx) const
    -> format_context::iterator {
    switch (t.kind) {
        case yuri::TypeKind::Err: return fmt::format_to(ctx.out(), "Type::Err");
        case yuri::TypeKind::Void: return fmt::format_to(ctx.out(), "void");
        case yuri::TypeKind::Type: return fmt::format_to(ctx.out(), "type");
        case yuri::TypeKind::Func: {
            std::span args = t.inner;
            args = args.subspan(0, args.size() - 1);

            return fmt::format_to(ctx.out(), "func({}) {}",
                                  fmt::join(args, ", "),
                                  t.inner.at(t.inner.size() - 1));
        }
        case yuri::TypeKind::Int: return fmt::format_to(ctx.out(), "int");
        case yuri::TypeKind::Half: return fmt::format_to(ctx.out(), "half");
        case yuri::TypeKind::Byte: return fmt::format_to(ctx.out(), "byte");
        case yuri::TypeKind::Bool: return fmt::format_to(ctx.out(), "bool");
        case yuri::TypeKind::Ptr:
            return fmt::format_to(ctx.out(), "*{}", t.inner.at(0));
        case yuri::TypeKind::Array:
            return fmt::format_to(ctx.out(), "[{}]{}", t.length, t.inner.at(0));
    }

    return fmt::format_to(ctx.out(), "{{{}, {}}}", t.kind, t.inner);
}
