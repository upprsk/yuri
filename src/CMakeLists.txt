add_library(yuri
    ast.cpp
    ast.hpp
    codegen/mips.cpp
    codegen/mips.hpp
    codegen/mips2.cpp
    codegen/mips2.hpp
    codegen/ssir.cpp
    codegen/ssir.hpp
    env.cpp
    env.hpp
    error_reporter.cpp
    error_reporter.hpp
    parser.cpp
    parser.hpp
    span.cpp
    span.hpp
    tokenizer.cpp
    tokenizer.hpp
    types.cpp
    types.hpp
)
target_include_directories(yuri PUBLIC .)
target_compile_options(yuri PRIVATE -Wall -Wextra)
target_link_libraries(yuri PUBLIC fmt::fmt cpptrace::cpptrace)
