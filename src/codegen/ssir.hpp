#pragma once

// Simple Stack Intermediate Representation

#include <cstddef>
#include <cstdint>
#include <span>
#include <vector>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "types.hpp"

namespace yuri::ssir {

enum class Opcode : uint8_t {
    Invalid,
    Local,
    Li,
    Pop,
    Get,
    Add,
    Sub,
    Ret,
};

struct Block {
    constexpr auto opcode_at(size_t i) const -> Opcode {
        return static_cast<Opcode>(text_at(i));
    }

    constexpr auto text_at(size_t i) const -> uint8_t { return text.at(i); }
    constexpr auto const_at(size_t i) const -> uint64_t { return consts.at(i); }
    constexpr auto data_span(size_t ptr, size_t size) const
        -> std::span<uint8_t const> {
        std::span s = data;
        return s.subspan(ptr, size);
    }

    constexpr auto span_for(size_t i) const -> Span {
        size_t off{};
        for (auto const& [cnt, span] : spans) {
            off += cnt;
            if (i < off) return span;
        }

        return {};
    }

    void append_op(Opcode op) { append_text(fmt::underlying(op)); }
    void append_text(uint8_t b) { text.push_back(b); }
    auto append_const(uint64_t c) -> size_t {
        auto sz = consts.size();
        consts.push_back(c);
        return sz;
    }

    void append_span(Span s) {
        if (spans.size() == 0) {
            spans.push_back({1, s});
            return;
        }

        if (spans.at(spans.size() - 1).second == s) {
            spans.at(spans.size() - 1).first++;
            return;
        }

        spans.push_back({1, s});
    }

    std::vector<uint8_t>                 text;
    std::vector<uint64_t>                consts;
    std::vector<uint8_t>                 data;
    std::vector<std::pair<size_t, Span>> spans;
};

struct Func {
    std::string name;
    Type        type;
    Block       body;
};

struct Module {
    std::unordered_map<std::string, Func> entries;
};

auto codegen(AstNode const& ast, ErrorReporter& er) -> Module;

void dump_module(Module const& m);

}  // namespace yuri::ssir

template <>
struct fmt::formatter<yuri::ssir::Opcode> : formatter<string_view> {
    auto format(yuri::ssir::Opcode c, format_context& ctx) const
        -> format_context::iterator;
};
