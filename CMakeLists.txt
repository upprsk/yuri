cmake_minimum_required(VERSION 3.11...3.30)

project(
  yuri
  VERSION 1.0
  LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_COLOR_DIAGNOSTICS ON)

include(FetchContent)

FetchContent_Declare(
  fmt
  GIT_REPOSITORY https://github.com/fmtlib/fmt.git
  GIT_TAG        11.1.3)
FetchContent_MakeAvailable(fmt)

include(FetchContent)
FetchContent_Declare(
  libassert
  GIT_REPOSITORY https://github.com/jeremy-rifkin/libassert.git
  GIT_TAG        v2.1.5 # <HASH or TAG>
)
FetchContent_MakeAvailable(libassert)

add_subdirectory(src)
add_subdirectory(cmd)
