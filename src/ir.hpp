#pragma once

#include <algorithm>
#include <cstdint>
#include <string>
#include <vector>

#include "fmt/base.h"
#include "fmt/format.h"

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

    Upsilon,
    Phi,

    Branch,
    Jump,
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
    uint16_t           extra_a{};
    uint16_t           extra_b{};
    std::vector<Inst*> args;

    [[nodiscard]] constexpr auto offset() const -> uint16_t { return extra_a; }

    [[nodiscard]] constexpr auto shadow() const -> uint16_t { return extra_a; }

    [[nodiscard]] constexpr auto branch_wt() const -> uint16_t {
        return extra_a;
    }

    [[nodiscard]] constexpr auto branch_wf() const -> uint16_t {
        return extra_b;
    }

    constexpr void branch_wt_set(uint16_t v) { extra_a = v; }
    constexpr void branch_wf_set(uint16_t v) { extra_b = v; }

    [[nodiscard]] constexpr auto is_oneof(auto&&... kinds) const -> bool {
        return ((kind == kinds) || ...);
    }

    constexpr void transmute_to_nop() {
        *this = {
            .id = id,
            .kind = InstKind::Nop,
            .args = {},
        };
    }

    constexpr void transmute_to_const(uint16_t offset) {
        *this = {
            .id = id,
            .kind = InstKind::Const,
            .type = type,
            .extra_a = offset,
            .args = {},
        };
    }

    constexpr void transmute_to_jump(uint16_t target) {
        *this = {
            .id = id,
            .kind = InstKind::Jump,
            .type = type,
            .extra_a = target,
            .args = {},
        };
    }

    constexpr void transmute_to_fwd(Inst* fwd) {
        *this = {
            .id = id,
            .kind = InstKind::Fwd,
            .type = type,
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
        return kind == InstKind::Ret || kind == InstKind::Jump ||
               kind == InstKind::Branch;
    }

    [[nodiscard]] constexpr auto is_const() const -> bool {
        return kind == InstKind::Const;
    }

    [[nodiscard]] constexpr auto is_fwd() const -> bool {
        return kind == InstKind::Fwd;
    }

    [[nodiscard]] constexpr auto has_side_effect() const -> bool {
        return is_branch() || kind == InstKind::Upsilon;
    }
};

struct Block {
    [[nodiscard]] auto successors() const -> std::vector<uint16_t>;
    [[nodiscard]] auto control() const -> Inst* {
        return body.at(body.size() - 1);
    }

    // this stores the index of the block. It should be update to match the
    // actual index when blocks are moved.
    uint16_t           id;
    std::vector<Inst*> body;
};

struct Const {
    InstType ty;
    uint64_t value;
};

struct Func {
    [[nodiscard]] auto alloc_const(InstType ty, uint64_t v) -> uint32_t;

    [[nodiscard]] constexpr auto get_const_value(uint16_t offset) const
        -> uint64_t {
        return consts.at(offset).value;
    }

    [[nodiscard]] constexpr auto get_const(uint16_t offset) const -> Const {
        return consts.at(offset);
    }

    [[nodiscard]] constexpr auto const_eq_to(Inst const* inst,
                                             uint64_t    rhs) const -> bool {
        return inst->is_const() && get_const_value(inst->offset()) == rhs;
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
    enum Flags {
        FlagNone = 0,
        FlagSigned,
        FlagUnsigned,
    };

    Flags flag = FlagNone;

    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        auto it = ctx.begin();
        if (*it == 'd') {
            flag = FlagSigned;
            it++;
        } else if (*it == 'u') {
            flag = FlagUnsigned;
            it++;
        }

        return it;
    }

    auto format(yuri::ir::Const i, format_context& ctx) const
        -> format_context::iterator;
};
