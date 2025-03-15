add_library(yuri
    ast.cpp
    ast.hpp
    error_reporter.cpp
    error_reporter.hpp
    ir.cpp
    ir.hpp
    parser.cpp
    parser.hpp
    sema.cpp
    sema.hpp
    span.cpp
    span.hpp
    ssa/branch_fold.cpp
    ssa/branch_fold.hpp
    ssa/cfold.cpp
    ssa/cfold.hpp
    ssa/check.cpp
    ssa/check.hpp
    ssa/dcelim.cpp
    ssa/dcelim.hpp
    tokenizer.cpp
    tokenizer.hpp
)
target_include_directories(yuri PUBLIC .)
target_compile_options(yuri PRIVATE -Wall -Wextra)
target_link_libraries(yuri PUBLIC fmt::fmt libassert::assert)
target_compile_definitions(yuri PUBLIC LIBASSERT_USE_FMT=1)

# On windows copy libassert.dll to the same directory as the executable for yuri
if(WIN32)
  add_custom_command(
    TARGET yuri POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
    $<TARGET_FILE:libassert::assert>
    $<TARGET_FILE_DIR:yuri>
  )
endif()
