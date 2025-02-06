add_executable(yuric
    main.cpp
)
target_include_directories(yuric PRIVATE .)
target_link_libraries(yuric PUBLIC yuri)
target_compile_options(yuric PRIVATE -Wall -Wextra)
