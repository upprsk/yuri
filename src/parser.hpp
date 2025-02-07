#pragma once

#include <span>

#include "ast.hpp"
#include "tokenizer.hpp"

namespace yuri {

auto parse(ErrorReporter* er, std::string_view source,
           std::span<Token const> tokens) -> AstNode;

}  // namespace yuri
