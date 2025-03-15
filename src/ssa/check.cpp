#include "check.hpp"

#include <unordered_set>

#include "ir.hpp"
#include "libassert/assert.hpp"

namespace yuri::ssa {

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void check_valid(ir::Func& fn) {
    ASSERT(fn.blocks.size() > 0);

    for (auto const& bb : fn.blocks) {
        ASSERT(bb->body.size() > 0);
        ASSERT(bb->body.at(bb->body.size() - 1)->is_branch());

        // TODO: check that every inst has arguments of the correct type
    }
}

}  // namespace yuri::ssa
