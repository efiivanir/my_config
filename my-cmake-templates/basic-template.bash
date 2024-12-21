function cmake-create-tests-file {
    file=$1
    cat >>  ${file}/CMakeLists.txt << EOC
include(Catch)

set(TEST_MAIN "unit_tests")
set(TEST_SOURCES "main.cc")
set(TEST_INCLUDES "./")

add_executable(\${TEST_MAIN} \${TEST_SOURCES})
target_include_directories(\${TEST_MAIN} PUBLIC \${TEST_INCLUDES})
target_link_libraries(\${TEST_MAIN} PUBLIC \${LIBRARY_NAME} Catch2::Catch2WithMain)

catch_discover_tests(\${TEST_MAIN})
EOC

    touch ${file}/main.cc

}

function cmake-create-project {
    prj=$1
    if test -d ${prj}; then
	echo "Err: $prj exists. Please remove it."
	return 1
    fi
    mkdir -p ${prj}/{src,app,tests,doc,build}
    touch ${prj}/{src,app,tests,doc}/CMakeLists.txt
    touch ${prj}/CMakeLists.txt
    cat >>  ${prj}/CMakeLists.txt << EOM
cmake_minimum_required(VERSION 3.22)

project(
    ${prj}
    VERSION 1.0.0
    LANGUAGES C CXX)

set(CMAKE_BUILD_TYPE Debug CACHE PATH "" FORCE)
set(CMAKE_GENERATOR "Unix Makefiles" CACHE PATH "" FORCE)
set(CMAKE_CXX_COMPILER /usr/bin/g++-14 CACHE PATH "" FORCE) 
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wpedantic -Wshadow -Wformat=2 -Wcast-align -Wconversion -Wsign-conversion -Wnull-dereference" CACHE PATH "" FORCE)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

option(ENABLE_TESTING "Enable a Unit Testing Build" ON)

set(LIBRARY_NAME "Library")
set(EXECUTABLE_NAME "${prj}")

set(CMAKE_MODULE_PATH "\${PROJECT_SOURCE_DIR}/cmake/")
#include(AddGitSubmodule)
include(FetchContent)
#include(Docs)

message(STATUS "--------------------------------------")
message(STATUS "Importing nlohmann_json")
message(STATUS "--------------------------------------")
FetchContent_Declare(
    nlohmann_json
    GIT_REPOSITORY https://github.com/nlohmann/json
    GIT_TAG v3.11.3
    GIT_SHALLOW TRUE)
FetchContent_MakeAvailable(nlohmann_json)

message(STATUS "--------------------------------------")
message(STATUS "Importing fmt")
message(STATUS "--------------------------------------")
FetchContent_Declare(
    fmt
    GIT_REPOSITORY https://github.com/fmtlib/fmt
    GIT_TAG 9.1.0
    GIT_SHALLOW TRUE)
FetchContent_MakeAvailable(fmt)

message(STATUS "--------------------------------------")
message(STATUS "Importing spdlog")
message(STATUS "--------------------------------------")
FetchContent_Declare(
    spdlog
    GIT_REPOSITORY https://github.com/gabime/spdlog
    GIT_TAG v1.13.0
    GIT_SHALLOW TRUE)
FetchContent_MakeAvailable(spdlog)

message(STATUS "--------------------------------------")
message(STATUS "Importing cxxopts")
message(STATUS "--------------------------------------")
FetchContent_Declare(
    cxxopts
    GIT_REPOSITORY https://github.com/jarro2783/cxxopts
    GIT_TAG v3.1.1
    GIT_SHALLOW TRUE)
FetchContent_MakeAvailable(cxxopts)

if(ENABLE_TESTING)
    message(STATUS "--------------------------------------")
    message(STATUS "Importing Catch2")
    message(STATUS "--------------------------------------")
    FetchContent_Declare(
        Catch2
        GIT_REPOSITORY https://github.com/catchorg/Catch2
        GIT_TAG v3.5.3
        GIT_SHALLOW TRUE)
    FetchContent_MakeAvailable(Catch2)
    list(APPEND CMAKE_MODULE_PATH \${catch2_SOURCE_DIR}/extras)
endif()

#add_subdirectory(configured)
#add_subdirectory(external)
add_subdirectory(src)
add_subdirectory(app)
if(ENABLE_TESTING)
    include(CTest)
    enable_testing()
    add_subdirectory(tests)
endif()

EOM

    cmake-create-tests-file ${prj}/tests
}
