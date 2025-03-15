#pragma once

#include "ir.hpp"

namespace yuri::ssa {

auto branch_fold(ir::Func& fn) -> bool;
auto branch_elim(ir::Func& fn) -> bool;

}  // namespace yuri::ssa
