#include "branch_fold.hpp"

#include <algorithm>
#include <cstdint>
#include <ranges>
#include <unordered_map>
#include <unordered_set>

#include "fmt/base.h"
#include "fmt/ranges.h"
#include "ir.hpp"
#include "libassert/assert.hpp"

namespace yuri::ssa {

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto branch_fold(ir::Func& fn) -> bool {
    auto had_changes = false;

    for (auto const& bb : fn.blocks) {
        auto ctrl = bb->control();
        if (ctrl->is_oneof(ir::InstKind::Branch) && ctrl->first()->is_const()) {
            if (fn.const_eq_to(ctrl->first(), 0)) {
                // always false, transmute to a jump to the else branch
                bb->successors = {bb->successors.at(1)};
            } else {
                // always true, transmute to a jump to the then branch
                bb->successors = {bb->successors.at(0)};
            }

            // need to transmute after so we don't loose the `first` arguemnt
            ctrl->transmute_to_jump();
            had_changes = true;
        }
    }

    return had_changes;
}

auto branch_elim(ir::Func& fn) -> bool {
    fmt::println(stderr, "{0:+<10} branch_elim start {0:+<10}", "");

    std::unordered_set<ir::Block*> reached{fn.blocks.at(0)};
    for (auto const& bb : fn.blocks) {
        for (auto s : bb->successors) reached.insert(s);
    }

    auto it = std::ranges::remove_if(fn.blocks, [&](auto b) {
        auto r = !reached.contains(b);
        if (r) fn.free_block(b);
        return r;
    });

    fn.blocks.erase(begin(it), fn.blocks.end());

    fmt::println(stderr, "{0:+<10} branch_elim end {0:+<10}", "");

    //
    // std::unordered_set<uint16_t> to_delete;
    //
    // for (auto const& bb : fn.blocks) {
    //     if (!reached.contains(bb.id)) {
    //         to_delete.insert(bb.id);
    //     }
    // }
    //
    // fn.blocks.erase(
    //     begin(std::ranges::remove_if(
    //         fn.blocks, [&](auto b) { return to_delete.contains(b.id); })),
    //     fn.blocks.end());
    //
    // // now fix the jump offsets
    // for (auto const& bb : fn.blocks) {
    //     auto ctrl = bb.control();
    //
    //     if (ctrl->is_oneof(ir::InstKind::Branch)) {
    //         for (size_t i = 0; i < fn.blocks.size(); i++) {
    //             if (fn.blocks.at(i).id == ctrl->branch_wt()) {
    //                 ctrl->branch_wt_set(i);
    //             }
    //
    //             if (fn.blocks.at(i).id == ctrl->branch_wf()) {
    //                 ctrl->branch_wf_set(i);
    //             }
    //         }
    //     } else if (ctrl->is_oneof(ir::InstKind::Jump)) {
    //         for (size_t i = 0; i < fn.blocks.size(); i++) {
    //             if (fn.blocks.at(i).id == ctrl->branch_wt()) {
    //                 ctrl->branch_wt_set(i);
    //             }
    //         }
    //     } else if (ctrl->is_oneof(ir::InstKind::Ret)) {
    //     } else {
    //         UNREACHABLE("invalid kind in branch_elim", ctrl->kind);
    //     }
    // }
    //
    // // fix the ids
    // for (size_t i = 0; i < fn.blocks.size(); i++) {
    //     fn.blocks.at(i).id = i;
    // }
    //
    // return !to_delete.empty();
    return false;
}

}  // namespace yuri::ssa
