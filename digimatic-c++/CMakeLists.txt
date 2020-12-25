cmake_minimum_required(VERSION 3.15)

if( "${CMAKE_VERSION}" VERSION_GREATER "3.19")
	cmake_policy(SET CMP0114 NEW)
endif()
set (CMAKE_CXX_STANDARD 20)
set (CMAKE_CXX_STANDARD_REQUIRED TRUE)
set (CMAKE_XCODE_ATTRIBUTE_COMPILER_INDEX_STORE_ENABLE NO)

Include(FetchContent)
FetchContent_Declare(
  Catch2
  GIT_REPOSITORY https://github.com/catchorg/Catch2.git
  GIT_TAG        v2.13.1)
FetchContent_MakeAvailable(Catch2)

project(AdventOfCode2020-digimatic)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_subdirectory(day01)
add_subdirectory(day02)
add_subdirectory(day03)
add_subdirectory(day04)
add_subdirectory(day05)
add_subdirectory(day06)
add_subdirectory(day07)
add_subdirectory(day08)
add_subdirectory(day09)
add_subdirectory(day10)
add_subdirectory(day11)
add_subdirectory(day12)
add_subdirectory(day13)
add_subdirectory(day14)
add_subdirectory(day15)
add_subdirectory(day16)
add_subdirectory(day17)
add_subdirectory(day18)
add_subdirectory(day19)
add_subdirectory(day20)
add_subdirectory(day21)
add_subdirectory(day22)
add_subdirectory(day23)
add_subdirectory(day24)
add_subdirectory(day25)

