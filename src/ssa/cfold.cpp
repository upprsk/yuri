#include "cfold.hpp"

#include <cstdint>

#include "fmt/base.h"
#include "ir.hpp"
#include "libassert/assert.hpp"

namespace yuri::ssa {

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto constant_fold(ir::Func& fn) -> bool {
    auto had_changes = false;

    for (auto const& bb : fn.blocks) {
        for (auto const& inst : bb.body) {
            // v_1 = T Div v_0, 0
            // ---
            // ERROR
            if (inst->is_oneof(ir::InstKind::Idiv, ir::InstKind::Udiv) &&
                fn.const_eq_to(inst->second(), 0)) {
                PANIC("Division by zero in compile-time");
            }

            // v_2 = T <Op> v_0, v_1
            // ---
            // v_2 = T Const [v_0 <Op> v_1]
            else if (inst->is_oneof(ir::InstKind::Add, ir::InstKind::Sub,
                                    ir::InstKind::Imul, ir::InstKind::Idiv,
                                    ir::InstKind::Umul, ir::InstKind::Udiv) &&
                     inst->first()->is_const() && inst->second()->is_const()) {
                auto lhs = fn.get_const_value(inst->first()->offset());
                auto rhs = fn.get_const_value(inst->second()->offset());

                uint64_t v;
                switch (inst->kind) {
                    case ir::InstKind::Add: v = lhs + rhs; break;
                    case ir::InstKind::Sub: v = lhs - rhs; break;
                    case ir::InstKind::Umul: v = lhs * rhs; break;
                    case ir::InstKind::Udiv: v = lhs / rhs; break;
                    case ir::InstKind::Imul:
                        v = static_cast<int64_t>(lhs) *
                            static_cast<int64_t>(rhs);
                        break;
                    case ir::InstKind::Idiv:
                        v = static_cast<int64_t>(lhs) /
                            static_cast<int64_t>(rhs);
                        break;
                    default: UNREACHABLE("invalid inst kind cfold", inst->kind);
                }

                // FIXME: figure out if this is even correct
                switch (inst->type) {
                    case ir::InstType::Byte: v &= 0xFF; break;
                    case ir::InstType::Half: v &= 0xFFFF; break;
                    case ir::InstType::Word: v &= 0xFFFF'FFFF; break;
                    case ir::InstType::Long: v &= 0xFFFF'FFFF'FFFF'FFFF; break;
                    default:
                        UNREACHABLE("invalid inst type in cfold", inst->type);
                }

                inst->transmute_to_const(fn.alloc_const(inst->type, v));
                had_changes = true;
            }

            // v_1 = T Add|Sub v_0, 0
            // ---
            // v_1 = T Fwd v_1
            else if (inst->is_oneof(ir::InstKind::Add, ir::InstKind::Sub) &&
                     fn.const_eq_to(inst->second(), 0)) {
                inst->transmute_to_fwd(inst->first());
                had_changes = true;
            }

            // v_1 = T Mul v_0, 0
            // ---
            // v_1 = T Const 0
            else if (inst->is_oneof(ir::InstKind::Imul, ir::InstKind::Umul) &&
                     fn.const_eq_to(inst->second(), 0)) {
                inst->transmute_to_const(fn.alloc_const(inst->type, 0));
                had_changes = true;
            }

            // v_1 = T Mul|Div v_0, 1
            // ---
            // v_1 = T Fwd v_0
            else if (inst->is_oneof(ir::InstKind::Imul, ir::InstKind::Umul,
                                    ir::InstKind::Idiv, ir::InstKind::Udiv) &&
                     fn.const_eq_to(inst->second(), 1)) {
                inst->transmute_to_fwd(inst->first());
                had_changes = true;
            }
        }
    }

    for (auto const& bb : fn.blocks) {
        for (auto const& inst : bb.body) {
            for (auto& arg : inst->args) {
                if (arg->is_fwd()) {
                    arg = arg->unwrap_fwd();
                    had_changes = true;
                }
            }
        }
    }

    return had_changes;
}

}  // namespace yuri::ssa
