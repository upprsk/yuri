#pragma once

#include <unistd.h>

#include <cstdint>
#include <string_view>
#include <utility>

#include "fmt/color.h"
#include "fmt/core.h"
#include "span.hpp"

namespace yuri {

class ErrorReporter {
    static constexpr auto const error_style = fmt::fg(fmt::color::red);
    static constexpr auto const note_style = fmt::fg(fmt::color::cyan);
    static constexpr auto const bug_style =
        fmt::fg(fmt::color::crimson) | fmt::emphasis::bold;

public:
    constexpr ErrorReporter(std::string source, std::string source_path)
        : source{source}, source_path{source_path} {}

    template <typename... T>
    void report_error(Span s, fmt::format_string<T...> fmt, T&&... args) {
        error_count++;
        report(s, "error", error_style, fmt, fmt::make_format_args(args...));
    }

    template <typename... T>
    void report_note(Span s, fmt::format_string<T...> fmt, T&&... args) {
        report(s, "note", note_style, fmt, fmt::make_format_args(args...));
    }

    template <typename... T>
    void report_bug(Span s, fmt::format_string<T...> fmt, T&&... args) {
        error_count++;
        report(s, "BUG", bug_style, fmt, fmt::make_format_args(args...));
    }

    void report(Span s, std::string_view prefix, fmt::text_style color,
                fmt::string_view fmt, fmt::format_args args);

    // -----------------------------------------------------------------------

private:
    constexpr auto find_rowcol(Span s) const -> std::pair<uint32_t, uint32_t> {
        uint32_t col{};
        uint32_t row{};

        for (size_t i = 0; i < s.begin; i++, col++) {
            if (source.at(i) == '\n') {
                row++;
                col = 0;
            }
        }

        return {row, col};
    }

    constexpr auto find_linestart(Span s) const -> Span {
        uint32_t line_start{};
        uint32_t line_end{};

        for (ssize_t i = s.begin; i >= 0; i--) {
            if (source[i] == '\n') {
                line_start = i + 1;
                break;
            }
        }

        for (size_t i = s.begin; i < source.length(); i++) {
            if (source[i] == '\n') {
                line_end = i;
                break;
            }
        }

        return {.begin = line_start, .end = line_end};
    }

    // -----------------------------------------------------------------------

private:
    std::string source;
    std::string source_path;
    uint32_t    error_count{};
};

}  // namespace yuri
