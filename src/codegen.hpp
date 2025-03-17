#pragma once

#include "ast.hpp"
#include "error_reporter.hpp"

namespace yuri {

void codegen(AstNode const& n, FILE* out, ErrorReporter& er);

}  // namespace yuri
