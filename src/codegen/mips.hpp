#pragma once

#include <cstdio>

#include "ast.hpp"
#include "error_reporter.hpp"

namespace yuri::mips {

void codegen(AstNode const& node, FILE* out, ErrorReporter& er);

}  // namespace yuri::mips
