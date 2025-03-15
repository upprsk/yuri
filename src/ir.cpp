#include "ir.hpp"

#include <algorithm>
#include <cstdint>
#include <ranges>
#include <string_view>

#include "fmt/base.h"
#include "fmt/ranges.h"
#include "libassert/assert.hpp"

namespace yuri::ir {

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
    return alloc_inst(InstKind::Const, ty, idx, std::vector<Inst*>{});
}

void Func::free_block(Block* bb) {
    for (auto const& inst : bb->body) free_inst(inst);
    delete bb;
}

void Func::free() {
    for (auto const& bb : blocks) {
        free_block(bb);
    }
}

void Func::disasm(FILE* out) const {
    fmt::println(out, "func @{}()", name);
    fmt::println(out, "; consts: [{:d}]", fmt::join(consts, ", "));

    for (auto const& bb : blocks) {
        fmt::println(out, "@b{}:", bb->id);

        for (auto const& inst : bb->body) {
            if (inst->kind == yuri::ir::InstKind::Const) {
                fmt::println(out, "{} = {} {} {:d} [{}]", inst->id, inst->type,
                             inst->kind,
                             Const{.ty = inst->type,
                                   .value = get_const_value(inst->offset())},
                             inst->offset());
            } else if (inst->kind == InstKind::Jump) {
                fmt::println(out, "{} = {}", inst->id, inst->kind);
            } else if (inst->kind == InstKind::Branch) {
                fmt::println(
                    out, "{} = {} {}", inst->id, inst->kind,
                    fmt::join(inst->args | std::ranges::views::transform(
                                               [](auto i) { return i->id; }),
                              ", "));
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

        fmt::println(
            out, "successors: [{}]",
            fmt::join(bb->successors | std::ranges::views::transform(
                                           [](auto b) { return b->id; }),
                      ", "));
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
        return fmt::format_to(ctx.out(), "{}", i.kind);
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
