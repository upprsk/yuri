#pragma once

#include <algorithm>
#include <cstdint>
#include <string>
#include <vector>

#include "fmt/base.h"

namespace yuri::ir {

enum class InstKind : uint16_t {
    Err,
    Nop,
    Const,
    Fwd,

    Add,
    Sub,
    Imul,
    Idiv,
    Umul,
    Udiv,

    Ret,
};

enum class InstType : uint16_t {
    Err,
    Byte,
    Half,
    Word,
    Long,
};

struct InstId {
    uint32_t id;
};

struct Inst {
    InstId             id;
    InstKind           kind{};
    InstType           type{};
    uint32_t           offset;
    std::vector<Inst*> args;

    [[nodiscard]] constexpr auto is_oneof(auto&&... kinds) const -> bool {
        return ((kind == kinds) || ...);
    }

    constexpr void transmute_to_nop() {
        *this = {
            .id = id,
            .kind = InstKind::Nop,
            .type = type,
            .offset = offset,
            .args = {},
        };
    }

    constexpr void transmute_to_const(uint32_t offset) {
        *this = {
            .id = id,
            .kind = InstKind::Const,
            .type = type,
            .offset = offset,
            .args = {},
        };
    }

    constexpr void transmute_to_fwd(Inst* fwd) {
        *this = {
            .id = id,
            .kind = InstKind::Fwd,
            .type = type,
            .offset = 0,
            .args = {fwd},
        };
    }

    [[nodiscard]] constexpr auto first() const -> Inst* { return args.at(0); }
    [[nodiscard]] constexpr auto second() const -> Inst* { return args.at(1); }

    [[nodiscard]] constexpr auto unwrap_fwd() -> Inst* {
        auto inst = this;
        while (inst->is_fwd()) inst = args.at(0);

        return inst;
    }

    [[nodiscard]] constexpr auto is_branch() const -> bool {
        return kind == InstKind::Ret;
    }

    [[nodiscard]] constexpr auto is_const() const -> bool {
        return kind == InstKind::Const;
    }

    [[nodiscard]] constexpr auto is_fwd() const -> bool {
        return kind == InstKind::Fwd;
    }

    [[nodiscard]] constexpr auto has_side_effect() const -> bool {
        return is_branch();
    }
};

struct Block {
    std::vector<Inst*> body;
};

struct Const {
    InstType ty;
    uint64_t value;
};

struct Func {
    [[nodiscard]] auto alloc_const(InstType ty, uint64_t v) -> uint32_t;

    [[nodiscard]] constexpr auto get_const_value(uint32_t offset) const
        -> uint64_t {
        return consts.at(offset).value;
    }

    [[nodiscard]] constexpr auto get_const(uint32_t offset) const -> Const {
        return consts.at(offset);
    }

    [[nodiscard]] constexpr auto const_eq_to(Inst const* inst,
                                             uint64_t    rhs) const -> bool {
        return inst->is_const() && get_const_value(inst->offset) == rhs;
    }

    auto alloc_inst_const(InstType ty, uint64_t v) -> Inst*;

    auto alloc_inst(auto&&... args) -> Inst* {
        return new Inst({next_inst_id++},
                        std::forward<decltype(args)>(args)...);
    }

    void free_inst(Inst* i) { delete i; }
    void free();

    void disasm(FILE* out) const;

    uint32_t next_inst_id{};

    std::string        name;
    std::vector<Block> blocks;

    // store constants, we store both the type and value. The type is there just
    // for debug.
    std::vector<Const> consts;
};

}  // namespace yuri::ir

template <>
struct fmt::formatter<yuri::ir::InstKind> : formatter<string_view> {
    auto format(yuri::ir::InstKind t, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yuri::ir::InstType> : formatter<string_view> {
    auto format(yuri::ir::InstType t, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yuri::ir::InstId> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yuri::ir::InstId i, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yuri::ir::Inst> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yuri::ir::Inst i, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yuri::ir::Const> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yuri::ir::Const i, format_context& ctx) const
        -> format_context::iterator;
};
