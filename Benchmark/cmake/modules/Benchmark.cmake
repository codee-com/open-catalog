# Custom target to run all benchmarks
add_custom_target(run
  COMMENT "Running all the available benchmarks"
)

# Helper function to scan the files in the check directories
function(scan_check_files VAR CHECKID EXPR)
  file(GLOB files "${OCB_CHECKS_DIR}/${CHECKID}/${OCB_CHECKS_BENCH_CODES_DIR}/${EXPR}")
  if(NOT files)
    file(GLOB files "${OCB_CHECKS_DIR}/${CHECKID}/${EXPR}")
  endif()
  # Propagate the result to the calling function
  set(${VAR} "${files}" PARENT_SCOPE)
endfunction()

# Utility function to automatically create the build target for the given check
function(add_benchmark CHECKID)
  set(files)

  # Auto-compile all the codes in the check directory; in particular:
  #
  #   - Prioritize benchmark-specific codes for the check, if available
  #   - Otherwise, compile the usual code examples created for the README
  if (OCB_ENABLE_C)
    scan_check_files(c_files "${CHECKID}" "*.c")
    list(APPEND files ${c_files})
  endif()
  if (OCB_ENABLE_Fortran)
    scan_check_files(fortran_files "${CHECKID}" "*.f90")
    list(APPEND files ${fortran_files})
  endif()

  # Skip empty benchmarks
  if (NOT files)
    return()
  endif()

  # Create the benchmarking binary with all the files
  add_executable(${CHECKID}
    ${CHECKID}.cpp
    ${files}
    "${OCB_SOURCE_DIR}/src/main.cpp"
  )

  target_link_libraries(${CHECKID}
    PUBLIC
      benchmark::benchmark
  )

  find_library(MATH_LIBRARY m)
  if(MATH_LIBRARY)
    target_link_libraries(${CHECKID}
      PUBLIC
        ${MATH_LIBRARY}
    )
  endif()

  target_include_directories(${CHECKID}
    PRIVATE "${OCB_SOURCE_DIR}/external/google_benchmark/include"
  )

  target_compile_definitions(${CHECKID}
    PRIVATE OCB_ENABLE_C=$<BOOL:${OCB_ENABLE_C}>
    PRIVATE OCB_ENABLE_Fortran=$<BOOL:${OCB_ENABLE_Fortran}>
  )

  add_custom_target(run-${CHECKID}
    COMMAND $<TARGET_FILE:${CHECKID}>
    DEPENDS ${CHECKID}
    COMMENT "Running the ${CHECKID} benchmark"
    USES_TERMINAL
  )

  add_dependencies(run run-${CHECKID})
endfunction()

# Find a systems google benchmark library or build it from source
# We require version 1.5.3 or above because we change the name of the benchmarks
# through the `::Name()` method
find_package(benchmark 1.5.3 QUIET)
if (NOT benchmark_FOUND)
  note("No compatible Benchmark library was found. Building it from source...")

  # Disable benchmark testing to avoid depending on GTest
  set(BENCHMARK_ENABLE_TESTING OFF CACHE BOOL "")
  add_subdirectory("${OCB_SOURCE_DIR}/external/google_benchmark")
endif()
