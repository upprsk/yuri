#pragma once

#include <span>
#include <string_view>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "tokenizer.hpp"

namespace yuri {

auto parse(std::span<Token const> token, std::string_view src, ErrorReporter& er)
    -> AstNode;

}  // namespace yuri
