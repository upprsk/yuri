#include "dcelim.hpp"

#include <algorithm>
#include <ranges>
#include <unordered_set>

#include "fmt/base.h"
#include "ir.hpp"
#include "libassert/assert.hpp"

namespace yuri::ssa {

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto dead_code_elim(ir::Func& fn) -> bool {
    auto had_changes = false;

    // TODO: use a better set implementation
    std::unordered_set<ir::Inst*> uses;

    for (auto const& bb : fn.blocks) {
        for (auto const& inst : bb.body) {
            for (auto const& o : inst->args) uses.insert(o);
        }
    }

    for (auto& bb : fn.blocks) {
        for (auto const& inst : bb.body) {
            if (!inst->has_side_effect() && uses.find(inst) == uses.end()) {
                inst->transmute_to_nop();
                had_changes = true;
            }
        }

        auto it = std::ranges::remove_if(bb.body, [&](auto i) {
            if (i->kind != ir::InstKind::Nop) return false;
            fn.free_inst(i);
            return true;
        });
        bb.body.erase(begin(it), end(bb.body));
    }

    return had_changes;
}

}  // namespace yuri::ssa
