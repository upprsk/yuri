add_library(yuri
    error_reporter.cpp
    error_reporter.hpp
    span.cpp
    span.hpp
    tokenizer.cpp
    tokenizer.hpp
)
target_include_directories(yuri PUBLIC .)
target_compile_options(yuri PRIVATE -Wall -Wextra)
target_link_libraries(yuri PUBLIC fmt::fmt cpptrace::cpptrace)
