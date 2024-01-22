# Custom target to run all benchmarks
add_custom_target(run
  COMMENT "Running all the available benchmarks"
)

# Utility function to automatically create the build target for the given check 
macro(add_benchmark CHECKID)
  set(files)

  # Auto-compile all the codes in the check directory
  if (OCB_ENABLE_C)
    file(GLOB c_files "${OCB_CHECKS_DIR}/${CHECKID}/*.c")
    list(APPEND files ${c_files})
  endif()
  if (OCB_ENABLE_Fortran)
    file(GLOB fortran_files "${OCB_CHECKS_DIR}/${CHECKID}/*.f90")
    list(APPEND files ${fortran_files})
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
endmacro()

# Find a systems google benchmark library
find_package(benchmark REQUIRED)
