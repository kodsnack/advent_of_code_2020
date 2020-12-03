get_filename_component(ProjectId ${CMAKE_CURRENT_SOURCE_DIR} NAME)
string(REPLACE " " "_" ProjectId ${ProjectId})
project(${ProjectId})

include_directories(${CMAKE_CURRENT_BINARY_DIR})

configure_file(config.h.in config.h)

add_executable(${PROJECT_NAME} "main.cpp" input.txt inputExample1.txt config.h.in)

target_link_libraries(${PROJECT_NAME} Catch2::Catch2)
