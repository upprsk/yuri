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
    Dupe,
    Local,
    Li,
    Pop,
    Ref,
    DeRef,
    Alloca,
    Global,
    GetGlobal,
    SetGlobal,
    GetStatic,
    Get,
    Set,
    Iset,
    Add,
    Sub,
    Mul,
    Div,
    Or,
    And,
    Seq,
    Sne,
    Slt,
    Sgt,
    B,
    Bz,
    Call,
    Ret,
};

struct Block {
    struct Local {
        uint32_t size;
        uint32_t align;
    };

    [[nodiscard]] constexpr auto opcode_at(size_t i) const -> Opcode {
        return static_cast<Opcode>(text_at(i));
    }

    [[nodiscard]] constexpr auto text_at(size_t i) const -> uint8_t {
        return text.at(i);
    }

    [[nodiscard]] constexpr auto const_at(size_t i) const -> uint64_t {
        return consts.at(i);
    }

    [[nodiscard]] constexpr auto data_span(size_t ptr, size_t size) const
        -> std::span<uint8_t const> {
        std::span s = data;
        return s.subspan(ptr, size);
    }

    [[nodiscard]] constexpr auto label_at(size_t i) const -> size_t {
        return labels.at(i);
    }

    [[nodiscard]] constexpr auto id_at(size_t i) const -> std::string const& {
        return ids.at(i);
    }

    [[nodiscard]] constexpr auto local_at(size_t i) const -> Local const& {
        return locals.at(i);
    }

    [[nodiscard]] constexpr auto span_for(size_t i) const -> Span {
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

    auto append_id(std::string const& s) -> size_t {
        auto sz = ids.size();
        ids.push_back(s);
        return sz;
    }

    auto append_label() -> size_t {
        auto sz = labels.size();
        labels.push_back(text.size());
        return sz;
    }

    auto append_local(uint32_t size, uint32_t align) -> size_t {
        auto sz = locals.size();
        locals.push_back({.size = size, .align = align});
        return sz;
    }

    void append_span(Span s) {
        if (spans.size() == 0) {
            spans.emplace_back(1, s);
            return;
        }

        if (spans.at(spans.size() - 1).second == s) {
            spans.at(spans.size() - 1).first++;
            return;
        }

        spans.emplace_back(1, s);
    }

    std::vector<uint8_t>                 text;
    std::vector<uint64_t>                consts;
    std::vector<uint8_t>                 data;
    std::vector<size_t>                  labels;
    std::vector<Local>                   locals;
    std::vector<std::string>             ids;
    std::vector<std::pair<size_t, Span>> spans;
};

struct Func {
    std::string name;
    Type        type;
    Block       body;
};

struct AsmFunc {
    std::string              name;
    Type                     type;
    std::vector<std::string> body;
};

struct Global {
    std::string name;
    Type        type;
    uint64_t    initial_value;
};

struct Static {
    size_t               id;
    size_t               size;
    size_t               align;
    std::vector<uint8_t> data;
};

struct Module {
    std::unordered_map<std::string, Func>    entries;
    std::unordered_map<std::string, AsmFunc> asm_entries;
    std::unordered_map<std::string, Global>  globals;
    std::unordered_map<size_t, Static>       statics;
};

auto codegen(AstNode const& ast, ErrorReporter& er) -> Module;

void dump_module(Module const& m);

}  // namespace yuri::ssir

template <>
struct fmt::formatter<yuri::ssir::Opcode> : formatter<string_view> {
    auto format(yuri::ssir::Opcode c, format_context& ctx) const
        -> format_context::iterator;
};
