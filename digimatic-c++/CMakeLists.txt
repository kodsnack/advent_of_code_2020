cmake_minimum_required(VERSION 3.15)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_CXX_STANDARD_REQUIRED TRUE)
set (CMAKE_XCODE_ATTRIBUTE_COMPILER_INDEX_STORE_ENABLE NO)

project(AdventOfCode2020-digimatic)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_subdirectory(day01)
