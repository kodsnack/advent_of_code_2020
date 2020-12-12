cmake_minimum_required(VERSION 3.15)
project(aoc20_f00ale)

set(CMAKE_CXX_STANDARD 17)

add_compile_options($<$<BOOL:${SANITIZE}>:-fsanitize=address,leak>)
add_link_options($<$<BOOL:${SANITIZE}>:-fsanitize=address,leak>)

add_compile_options(-Wall -Wextra -march=native)

add_executable(aoc20 src/main.cpp
        src/p01.cpp
        src/p02.cpp
        src/p03.cpp
        src/p04.cpp
        src/p05.cpp
        src/p06.cpp
        src/p07.cpp
        src/p08.cpp
        src/p09.cpp
        src/p10.cpp
        src/p11.cpp
        src/p12.cpp
        )
