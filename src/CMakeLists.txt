# CMakeList.txt : CMake project for RbxCppProject, include source and define
# project specific logic here.
#

# Add source to this project's executable.
file(GLOB_RECURSE SOURCES "*.cpp" "*.c" "*.h" "*.hpp")
add_executable (RbxCppProject ${SOURCES})

if (CMAKE_VERSION VERSION_GREATER 3.12)
  set_property(TARGET RbxCppProject PROPERTY CXX_STANDARD 20)
endif()

# TODO: Add tests and install targets if needed.

# set(CMAKE_CXX_FLAGS "-Wall -Wextra")
# set(CMAKE_CXX_FLAGS_DEBUG "-g -O3")
set(CMAKE_CXX_FLAGS_RELEASE "-g -O3 -Wno-limited-postlink-optimizations")
# We need -g so emscripten preserves the wasm name section that our
# post-processor regexes against. The price is binaryen's "limited
# postlink optimizations" notice; the -Wno flag silences it on both
# compile and link invocations.
add_compile_options("-Wno-limited-postlink-optimizations")
add_link_options("-Wno-limited-postlink-optimizations")

set(EMS_LINK
	"SHELL:-s EXPORTED_FUNCTIONS=['_malloc','_lua_call','_strlen','_main'] -s EXPORTED_RUNTIME_METHODS=['ccall','cwrap']" # -s LINKABLE=1 -s EXPORT_ALL=1
)

target_link_options(RbxCppProject PRIVATE ${EMS_LINK})
