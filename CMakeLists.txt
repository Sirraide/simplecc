cmake_minimum_required(VERSION 3.20)
project(simplecc C)

set(CMAKE_C_STANDARD 23)

file(GLOB_RECURSE sources src/*.c)
file(GLOB_RECURSE headers src/*.h)
add_executable(simplecc ${sources})
target_sources(simplecc PRIVATE FILE_SET HEADERS FILES ${headers})
target_compile_options(simplecc PRIVATE
	-Wall
	-Wextra
	-Wundef
	-Wconversion
	-Werror=return-type
	-Wshadow
)

set_target_properties(simplecc PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR})
