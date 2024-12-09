# Checks that all "example codes" in the `Checks/` folders are compilable to
# avoid committing incorrect examples. It's done here, separately and in bulk,
# so that no overhead is added by default when submitting a new check. You'll
# hopefully only have to know this exists if your example fails to compile

cmake_minimum_required(VERSION 3.25)
project("Test that the check examples compile")

enable_language(Fortran)
# Build them as libraries, not executables
set (CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)

set (BASE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../../Checks)
message(STATUS "Looking for files in '${BASE_DIR}'")

# The non-recursive glob only includes files in the first level directory of the
# Checks which is where the examples are usually located. This way we avoid
# benchmark programs which may be harder to compile and will be checked anyways
# by the benchmark running script
file(GLOB check_files
  ${BASE_DIR}/*/*.c
  ${BASE_DIR}/*/*.cpp
  ${BASE_DIR}/*/*.f
  ${BASE_DIR}/*/*.F
  ${BASE_DIR}/*/*.f90
  ${BASE_DIR}/*/*.F90
)

# PWR068: Has a module file that has to be compiled in order, and a compilation error on purpose
# PWD003, PWD006: GFortran catches the data transfer error
set (excluded_checks_regex "PWR068|PWD003|PWD006")

list(FILTER check_files EXCLUDE REGEX ${excluded_checks_regex})

foreach(check_file ${check_files})
  message(CHECK_START "Trying to compile ${check_file}")
  set (extra_flags "-fopenmp -fopenacc")
  try_compile(
    TRY_RESULT ${CMAKE_BINARY_DIR}
    SOURCES ${check_file}
    CMAKE_FLAGS -DCOMPILE_DEFINITIONS:STRING=${extra_flags}
    OUTPUT_VARIABLE TRY_RESULT_OUTPUT
  )

  if (TRY_RESULT)
    message(CHECK_PASS "Good")
  else()
    message(CHECK_FAIL "Bad:\n ${TRY_RESULT_OUTPUT}")
  endif()
endforeach()
