add_library(yuri
    ast.cpp
    ast.hpp
    error_reporter.cpp
    error_reporter.hpp
    parser.cpp
    parser.hpp
    span.cpp
    span.hpp
    tokenizer.cpp
    tokenizer.hpp
)
target_include_directories(yuri PUBLIC .)
target_compile_options(yuri PRIVATE -Wall -Wextra)
target_link_libraries(yuri PUBLIC fmt::fmt libassert::assert)

# On windows copy libassert.dll to the same directory as the executable for yuri
if(WIN32)
  add_custom_command(
    TARGET yuri POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
    $<TARGET_FILE:libassert::assert>
    $<TARGET_FILE_DIR:yuri>
  )
endif()
