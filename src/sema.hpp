#pragma once

#include "ast.hpp"
#include "error_reporter.hpp"
#include "ir.hpp"

namespace yuri {

auto sema(ErrorReporter& er, AstNode const& ast) -> ir::Func;

}  // namespace yuri
