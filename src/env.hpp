#pragma once

#include <optional>
#include <string>
#include <unordered_map>

#include "span.hpp"
#include "types.hpp"

namespace yuri {

struct Env {
    auto child() -> Env { return {.parent = this}; }

    auto with_return_type(Type const* ty, Span span) -> Env {
        return {.current_return_type = ty,
                .current_return_span = span,
                .parent = this};
    }

    auto lookup_return_type() const -> std::pair<Type const*, Span>;

    // underlying is used for typedefs, and is the actual type for the name. So
    // that lookup returns `Type`.
    void define(std::string const& k, Type const& t,
                Type const& underlying = Type::Err()) {
        entries[k] = {t, underlying};
    }

    auto lookup(std::string const& k) const -> std::optional<Type>;
    auto lookup_underlying(std::string const& k) const -> std::optional<Type>;

    auto get(std::string const& k) const
        -> std::optional<std::pair<Type, Type>>;

    Type const* current_return_type = nullptr;
    Span        current_return_span{};
    std::unordered_map<std::string, std::pair<Type, Type>> entries{};
    Env*                                                   parent{};
};

}  // namespace yuri
