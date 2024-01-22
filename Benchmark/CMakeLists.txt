# Google Benchmark requires CMake v3.10 or above
cmake_minimum_required(VERSION 3.10.0)

# Include our own cmake module path
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

project(OpenCatalogBenchmark # OCB for short
  DESCRIPTION "Benchmarking framework for the Open Catalog"
  VERSION     0.0.1
)

# Define some useful variables
set(OCB_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")
set(OCB_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}")
set(OCB_CHECKS_DIR "${OCB_SOURCE_DIR}/../Checks")

# Configure the project
include(Config)

# Set up a macro to simplify benchmark creation
include(Benchmark)

add_subdirectory(src)
