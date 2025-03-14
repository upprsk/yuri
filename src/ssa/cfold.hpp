#pragma once

#include "ir.hpp"

namespace yuri::ssa {

auto constant_fold(ir::Func& fn) -> bool;

}  // namespace yuri::ssa
