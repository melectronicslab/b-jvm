cmake_minimum_required(VERSION 3.28)
project(bjvm)

set(CMAKE_C_STANDARD 23)
set(CMAKE_CXX_STANDARD 17)
Include(FetchContent)

FetchContent_Declare(
        Catch2
        GIT_REPOSITORY https://github.com/catchorg/Catch2.git
        GIT_TAG        v3.4.0
)

FetchContent_MakeAvailable(Catch2)

# If doing emscripten
if (EMSCRIPTEN)
    set(CMAKE_C_FLAGS "-O3 -fexceptions -fwasm-exceptions -Wall -Wextra -g -Wpedantic -fno-strict-aliasing")
    set(CMAKE_CXX_FLAGS "-O3 -fexceptions -fwasm-exceptions -Wall -g -Wpedantic -fno-strict-aliasing")
    set(EmscriptenFlags "-s EXPORTED_FUNCTIONS=\"['_malloc','_main']\" -s TOTAL_MEMORY=1024MB")
elseif(APPLE)
    set(CMAKE_C_FLAGS "-O3 -Wall -Wextra -Wimplicit-fallthrough -g -Werror-incompatible-pointer-types -fno-strict-aliasing -fsanitize=address,undefined")
    set(CMAKE_CXX_FLAGS "-O3 -Wall -Wextra -Wimplicit-fallthrough -g -Werror-incompatible-pointer-types -fno-strict-aliasing -fsanitize=address,undefined") # -openmp -Xclang -fopenmp -L/opt/homebrew/opt/libomp/lib -I/opt/homebrew/opt/libomp/include -lomp -g")
    set(EmscriptenFlags "")
else()
    set(CMAKE_C_FLAGS "-O3 -Wall -Wextra -g -fsanitize=address,undefined -march=native -fno-strict-aliasing")
    set(CMAKE_CXX_FLAGS "-O3 -Wall -g -fsanitize=address,undefined -fopenmp -march=native -fno-strict-aliasing")
    set(EmscriptenFlags "")
endif()

add_library(bjvm OBJECT src/bjvm.h src/bjvm.c src/natives.h)

add_executable(bjvm_main src/main.c)
target_link_libraries(bjvm_main PRIVATE bjvm)
set_target_properties(bjvm_main PROPERTIES LINK_FLAGS "${EmscriptenFlags}")

add_executable(gen_natives codegen/gen_natives.c)
target_link_libraries(gen_natives PRIVATE bjvm)
set_target_properties(gen_natives PROPERTIES LINK_FLAGS "${EmscriptenFlags}")

add_executable(tests test/tests.cc)
target_link_libraries(tests PRIVATE bjvm)
target_link_libraries(tests PRIVATE Catch2::Catch2WithMain)
set_target_properties(tests PROPERTIES LINK_FLAGS "${EmscriptenFlags}")
