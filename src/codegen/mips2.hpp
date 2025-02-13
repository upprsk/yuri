#pragma once

#include "codegen/ssir.hpp"
#include "error_reporter.hpp"

// Implementation of MIPS code generation from SSIR

namespace yuri::mips::v2 {

void codegen_stdout(ssir::Module const& m, ErrorReporter& er);

}  // namespace yuri::mips::v2
