#include "error_reporter.hpp"

#include <unistd.h>

#include <string_view>

#include "fmt/base.h"
#include "fmt/core.h"

namespace yuri {

void ErrorReporter::report(Span s, std::string_view prefix,
                           fmt::text_style color, fmt::string_view fmt,
                           fmt::format_args args) {
    auto [row, col] = find_rowcol(s);

    fmt::print(stderr, "{}:{}:{}: ", source_path, row, col, prefix);

    if (isatty(STDERR_FILENO)) {
        fmt::print(stderr, color, "{}", prefix);
        fmt::print(stderr, ": ");
    } else {
        fmt::print(stderr, "{}: ", prefix);
    }

    fmt::vprint(stderr, fmt, args);
    fmt::println(stderr, "");

    std::string_view source = this->source;
    auto [ls, le] = find_linestart(s);
    fmt::println(stderr, "{:04} | {}", row, source.substr(ls, le - ls));

    if (isatty(STDERR_FILENO)) {
        fmt::print(stderr, color, "{0: <{1}}{0:^<{2}}", "",
                   4 + 3 + s.begin - ls, std::min(s.end, le) - s.begin);
    } else {
        fmt::print(stderr, "       {:<{}}", "^", le - ls);
    }

    fmt::println(stderr, "");
}
}  // namespace yuri
