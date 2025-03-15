#include "ir.hpp"

#include <algorithm>
#include <cstdint>
#include <ranges>
#include <string_view>

#include "fmt/base.h"
#include "fmt/ranges.h"
#include "libassert/assert.hpp"

namespace yuri::ir {

auto Block::successors() const -> std::vector<uint16_t> {
    auto last_inst = body.at(body.size() - 1);

    if (last_inst->kind == InstKind::Jump) return {last_inst->branch_wt()};
    if (last_inst->kind == InstKind::Branch)
        return {last_inst->branch_wt(), last_inst->branch_wf()};
    if (last_inst->kind == InstKind::Ret) return {};

    UNREACHABLE("invalid inst kind in `successors`", last_inst->kind);
}

auto Func::alloc_const(InstType ty, uint64_t v) -> uint32_t {
    // de-dup constants
    for (size_t i{}; auto const& [cty, c] : consts) {
        if (cty == ty && c == v) return i;
        i++;
    }

    auto sz = consts.size();
    consts.emplace_back(ty, v);
    return sz;
}

auto Func::alloc_inst_const(InstType ty, uint64_t v) -> Inst* {
    auto idx = alloc_const(ty, v);
    return alloc_inst(InstKind::Const, ty, idx, 0, std::vector<Inst*>{});
}

void Func::free() {
    for (auto const& bb : blocks) {
        for (auto const& inst : bb.body) free_inst(inst);
    }
}

void Func::disasm(FILE* out) const {
    fmt::println(out, "func @{}()", name);
    fmt::println(out, "; consts: [{:d}]", fmt::join(consts, ", "));

    for (size_t i{}; auto const& bb : blocks) {
        fmt::println(out, "@b{}:", i);

        for (auto const& inst : bb.body) {
            if (inst->kind == yuri::ir::InstKind::Const) {
                fmt::println(out, "{} = {} {} {:d} [{}]", inst->id, inst->type,
                             inst->kind,
                             Const{.ty = inst->type,
                                   .value = get_const_value(inst->offset())},
                             inst->offset());
            } else if (inst->kind == InstKind::Jump) {
                fmt::println(out, "{} = {} {}", inst->id, inst->kind,
                             inst->branch_wt());
            } else if (inst->kind == InstKind::Branch) {
                fmt::println(
                    out, "{} = {} {}, {}, {}", inst->id, inst->kind,
                    fmt::join(inst->args | std::ranges::views::transform(
                                               [](auto i) { return i->id; }),
                              ", "),
                    inst->branch_wt(), inst->branch_wf());
            } else if (inst->kind == InstKind::Phi) {
                fmt::println(out, "{} = {} {} ^{}", inst->id, inst->type,
                             inst->kind, inst->shadow());
            } else if (inst->kind == InstKind::Upsilon) {
                fmt::println(
                    out, "{} = {} {} {}, ^{}", inst->id, inst->type, inst->kind,
                    fmt::join(inst->args | std::ranges::views::transform(
                                               [](auto i) { return i->id; }),
                              ", "),
                    inst->shadow());
            } else {
                fmt::println(
                    out, "{} = {} {} {}", inst->id, inst->type, inst->kind,
                    fmt::join(inst->args | std::ranges::views::transform(
                                               [](auto i) { return i->id; }),
                              ", "));
            }
        }

        i++;
    }
}

}  // namespace yuri::ir

auto fmt::formatter<yuri::ir::InstKind>::format(yuri::ir::InstKind t,
                                                format_context&    ctx) const
    -> format_context::iterator {
    std::string_view name = "unknown";
    switch (t) {
        case yuri::ir::InstKind::Err: name = "Err"; break;
        case yuri::ir::InstKind::Nop: name = "Nop"; break;
        case yuri::ir::InstKind::Const: name = "Const"; break;
        case yuri::ir::InstKind::Fwd: name = "Fwd"; break;
        case yuri::ir::InstKind::Add: name = "Add"; break;
        case yuri::ir::InstKind::Sub: name = "Sub"; break;
        case yuri::ir::InstKind::Imul: name = "Imul"; break;
        case yuri::ir::InstKind::Idiv: name = "Idiv"; break;
        case yuri::ir::InstKind::Umul: name = "Umul"; break;
        case yuri::ir::InstKind::Udiv: name = "Udiv"; break;
        case yuri::ir::InstKind::Upsilon: name = "Upsilon"; break;
        case yuri::ir::InstKind::Phi: name = "Phi"; break;
        case yuri::ir::InstKind::Branch: name = "Branch"; break;
        case yuri::ir::InstKind::Jump: name = "Jump"; break;
        case yuri::ir::InstKind::Ret: name = "Ret"; break;
    }
    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yuri::ir::InstType>::format(yuri::ir::InstType t,
                                                format_context&    ctx) const
    -> format_context::iterator {
    std::string_view name = "unknown";
    switch (t) {
        case yuri::ir::InstType::Err: name = "Err"; break;
        case yuri::ir::InstType::Byte: name = "b"; break;
        case yuri::ir::InstType::Half: name = "h"; break;
        case yuri::ir::InstType::Word: name = "w"; break;
        case yuri::ir::InstType::Long: name = "l"; break;
    }
    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yuri::ir::InstId>::format(yuri::ir::InstId i,
                                              format_context&  ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "v_{}", i.id);
}

auto fmt::formatter<yuri::ir::Inst>::format(yuri::ir::Inst  i,
                                            format_context& ctx) const
    -> format_context::iterator {
    if (i.kind == yuri::ir::InstKind::Const)
        return fmt::format_to(ctx.out(), "{} = {} {} [{}]", i.id, i.type,
                              i.kind, i.offset());
    if (i.kind == yuri::ir::InstKind::Jump)
        return fmt::format_to(ctx.out(), "{} {}", i.kind, i.offset());
    if (i.kind == yuri::ir::InstKind::Branch)
        return fmt::format_to(ctx.out(), "{} {}, {}", i.kind, i.branch_wt(),
                              i.branch_wt());
    if (i.kind == yuri::ir::InstKind::Phi)
        fmt::format_to(ctx.out(), "{} = {} {} ^{}", i.id, i.type, i.kind,
                       i.shadow());
    if (i.kind == yuri::ir::InstKind::Upsilon)
        fmt::format_to(ctx.out(), "{} = {} {} {}, ^{}", i.id, i.type, i.kind,
                       fmt::join(i.args | std::ranges::views::transform(
                                              [](auto i) { return i->id; }),
                                 ", "),
                       i.shadow());

    return fmt::format_to(ctx.out(), "{} = {} {} {}", i.id, i.type, i.kind,
                          fmt::join(i.args | std::ranges::views::transform(
                                                 [](auto i) { return i->id; }),
                                    ", "));
}

auto fmt::formatter<yuri::ir::Const>::format(yuri::ir::Const i,
                                             format_context& ctx) const
    -> format_context::iterator {
    auto [ty, v] = i;

    if (flag == FlagSigned) {
        switch (ty) {
            case yuri::ir::InstType::Byte:
                return fmt::format_to(ctx.out(), "{}", static_cast<int8_t>(v));
            case yuri::ir::InstType::Half:
                return fmt::format_to(ctx.out(), "{}", static_cast<int16_t>(v));
            case yuri::ir::InstType::Word:
                return fmt::format_to(ctx.out(), "{}", static_cast<int32_t>(v));
            case yuri::ir::InstType::Long:
                return fmt::format_to(ctx.out(), "{}", static_cast<int64_t>(v));
            default: UNREACHABLE("invalid type for constant");
        }
    }

    if (flag == FlagUnsigned) {
        switch (ty) {
            case yuri::ir::InstType::Byte:
                return fmt::format_to(ctx.out(), "{}", static_cast<uint8_t>(v));
            case yuri::ir::InstType::Half:
                return fmt::format_to(ctx.out(), "{}",
                                      static_cast<uint16_t>(v));
            case yuri::ir::InstType::Word:
                return fmt::format_to(ctx.out(), "{}",
                                      static_cast<uint32_t>(v));
            case yuri::ir::InstType::Long:
                return fmt::format_to(ctx.out(), "{}", v);
            default: UNREACHABLE("invalid type for constant");
        }
    }

    switch (ty) {
        case yuri::ir::InstType::Byte:
            return fmt::format_to(ctx.out(), "{}/{}", static_cast<uint8_t>(v),
                                  static_cast<int8_t>(v));
        case yuri::ir::InstType::Half:
            return fmt::format_to(ctx.out(), "{}/{}", static_cast<uint16_t>(v),
                                  static_cast<int16_t>(v));
        case yuri::ir::InstType::Word:
            return fmt::format_to(ctx.out(), "{}/{}", static_cast<uint32_t>(v),
                                  static_cast<int32_t>(v));
        case yuri::ir::InstType::Long:
            return fmt::format_to(ctx.out(), "{}/{}", v,
                                  static_cast<int64_t>(v));
        default: UNREACHABLE("invalid type for constant");
    }
}
