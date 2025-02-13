#pragma once

#include <cstdint>
#include <string_view>

#include "fmt/base.h"

namespace yuri {

struct Span {
    uint32_t begin;
    uint32_t end;

    constexpr auto size() const -> uint32_t { return end - begin; }
    constexpr auto str(std::string_view source) const -> std::string_view {
        return source.substr(begin, size());
    }

    constexpr auto extend(Span o) const -> Span {
        return {.begin = begin, .end = o.end};
    }

    constexpr auto operator==(Span const& o) const -> bool = default;
};

}  // namespace yuri

template <>
struct fmt::formatter<yuri::Span> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yuri::Span s, format_context& ctx) const
        -> format_context::iterator;
};
