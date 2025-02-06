#pragma once

#include <cstdint>

#include "fmt/base.h"

namespace yuri {

struct Span {
    uint32_t begin;
    uint32_t end;

    constexpr auto size() const -> uint32_t { return end - begin; }
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
