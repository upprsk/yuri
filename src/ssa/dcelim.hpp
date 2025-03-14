#pragma once

#include "ir.hpp"

namespace yuri::ssa {

auto dead_code_elim(ir::Func& fn) -> bool;

}  // namespace yuri::ssa
