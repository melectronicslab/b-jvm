cmake_minimum_required(VERSION 3.29)
project(bjvm)

set(CMAKE_C_STANDARD 23)
Include(FetchContent)

FetchContent_Declare(
        Catch2
        GIT_REPOSITORY https://github.com/catchorg/Catch2.git
        GIT_TAG        v3.4.0
)

FetchContent_MakeAvailable(Catch2)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "-O0 -fexceptions -fwasm-exceptions -Wall")
set(EmscriptenFlags "-g -s EXPORTED_FUNCTIONS=\"['_malloc','_main']\" -s TOTAL_MEMORY=1024MB")

add_library(bjvm OBJECT src/bjvm.c)

add_executable(bjvm_main src/bjvm.h src/main.c)
target_link_libraries(bjvm_main PRIVATE bjvm)
set_target_properties(bjvm_main PROPERTIES LINK_FLAGS "${EmscriptenFlags}")

add_executable(tests test/tests.cc src/bjvm.h src/main.c)
target_link_libraries(tests PRIVATE Catch2::Catch2WithMain)
target_link_libraries(tests PRIVATE bjvm)
set_target_properties(tests PROPERTIES LINK_FLAGS "${EmscriptenFlags}")
