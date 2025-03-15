#include "branch_fold.hpp"

#include <algorithm>
#include <cstdint>
#include <unordered_map>
#include <unordered_set>

#include "fmt/base.h"
#include "ir.hpp"
#include "libassert/assert.hpp"

namespace yuri::ssa {

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto branch_fold(ir::Func& fn) -> bool {
    auto had_changes = false;

    for (auto const& bb : fn.blocks) {
        for (auto const& inst : bb.body) {
            if (inst->is_oneof(ir::InstKind::Branch) &&
                inst->first()->is_const()) {
                if (fn.const_eq_to(inst->first(), 0)) {
                    // always false, transmute to a jump to the else branch
                    inst->transmute_to_jump(inst->branch_wt());
                } else {
                    // always true, transmute to a jump to the then branch
                    inst->transmute_to_jump(inst->branch_wf());
                }

                had_changes = true;
            }
        }
    }

    return had_changes;
}

auto branch_elim(ir::Func& fn) -> bool {
    std::unordered_set<uint16_t> reached{0};
    for (auto const& bb : fn.blocks) {
        for (auto s : bb.successors()) reached.insert(s);
    }

    std::unordered_set<uint16_t> to_delete;

    for (auto const& bb : fn.blocks) {
        if (!reached.contains(bb.id)) {
            to_delete.insert(bb.id);
        }
    }

    fn.blocks.erase(
        begin(std::ranges::remove_if(
            fn.blocks, [&](auto b) { return to_delete.contains(b.id); })),
        fn.blocks.end());

    // now fix the jump offsets
    for (auto const& bb : fn.blocks) {
        auto ctrl = bb.control();

        if (ctrl->is_oneof(ir::InstKind::Branch)) {
            for (size_t i = 0; i < fn.blocks.size(); i++) {
                if (fn.blocks.at(i).id == ctrl->branch_wt()) {
                    ctrl->branch_wt_set(i);
                }

                if (fn.blocks.at(i).id == ctrl->branch_wf()) {
                    ctrl->branch_wf_set(i);
                }
            }
        } else if (ctrl->is_oneof(ir::InstKind::Jump)) {
            for (size_t i = 0; i < fn.blocks.size(); i++) {
                if (fn.blocks.at(i).id == ctrl->branch_wt()) {
                    ctrl->branch_wt_set(i);
                }
            }
        } else if (ctrl->is_oneof(ir::InstKind::Ret)) {
        } else {
            UNREACHABLE("invalid kind in branch_elim", ctrl->kind);
        }
    }

    // fix the ids
    for (size_t i = 0; i < fn.blocks.size(); i++) {
        fn.blocks.at(i).id = i;
    }

    return !to_delete.empty();
}

}  // namespace yuri::ssa
