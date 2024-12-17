# Custom target to run all benchmarks
add_custom_target(run
  COMMENT "Running all the available benchmarks"
)

# Utility function to automatically create the build target for the given check 
function(add_benchmark CHECKID)
  set(files)

  # Auto-compile all the codes in the check directory; in particular:
  #
  #   - Prioritize benchmark-specific codes for the check, if available
  #   - Otherwise, compile the usual code examples created for the README
  if (OCB_ENABLE_C)
    file(GLOB c_files "${OCB_CHECKS_DIR}/${CHECKID}/${OCB_CHECKS_BENCH_CODES_DIR}/*.c")
    if(NOT c_files)
      file(GLOB c_files "${OCB_CHECKS_DIR}/${CHECKID}/*.c")
    endif()
    list(APPEND files ${c_files})
  endif()
  if (OCB_ENABLE_Fortran)
    file(GLOB fortran_files "${OCB_CHECKS_DIR}/${CHECKID}/${OCB_CHECKS_BENCH_CODES_DIR}/*.f90")
    if(NOT fortran_files)
      file(GLOB fortran_files "${OCB_CHECKS_DIR}/${CHECKID}/*.f90")
    endif()
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
      m
  )

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
