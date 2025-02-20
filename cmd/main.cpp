#include <cstdio>

#include "cpptrace/from_current.hpp"
#include "fmt/format.h"

auto main() -> int {
    CPPTRACE_TRY {
        fmt::println("Hello, World!");

        return 0;
    }
    CPPTRACE_CATCH(std::exception const& e) {
        fmt::println(stderr, "exception: {}", e.what());
        cpptrace::from_current_exception().print();
    }
}
