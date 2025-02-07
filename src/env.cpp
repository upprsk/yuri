#include "env.hpp"

namespace yuri {

auto Env::lookup_return_type() const -> std::pair<Type const*, Span> {
    if (current_return_type) return {current_return_type, current_return_span};
    if (parent) return parent->lookup_return_type();

    return {nullptr, {}};
}

auto Env::lookup(std::string const& k) const -> std::optional<Type> {
    auto v = get(k);
    if (v) return v->first;

    if (parent) return parent->lookup(k);
    return std::nullopt;
}

auto Env::lookup_underlying(std::string const& k) const -> std::optional<Type> {
    auto v = get(k);
    if (v) return v->second;

    if (parent) return parent->lookup_underlying(k);
    return std::nullopt;
}

auto Env::get(std::string const& k) const
    -> std::optional<std::pair<Type, Type>> {
    auto v = entries.find(k);
    if (v == entries.end()) return std::nullopt;

    return v->second;
}
}  // namespace yuri
