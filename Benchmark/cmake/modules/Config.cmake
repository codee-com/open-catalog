include (Utils)

# Build in release mode by default
if (NOT CMAKE_BUILD_TYPE OR CMAKE_BUILD_TYPE STREQUAL "")
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "" FORCE)
endif()

# Default to C11 and ++11 standards without extensions
set(CMAKE_CXX_STANDARD          11)
set(CMAKE_CXX_EXTENSIONS        OFF)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(OCB_ENABLE_C "Enable benchmarking on C codes" ON)

# Auto-enable the Fortran support based on exising compilers
set(OCB_ENABLE_Fortran_default ON)
enable_language(Fortran OPTIONAL)
if(NOT CMAKE_Fortran_COMPILER)
  set(OCB_ENABLE_Fortran_default OFF)
  note("Disabling Fortran benchmarks as no compiler was found")
endif()
option(OCB_ENABLE_Fortran "Enable benchmarking on Fortran codes" ${OCB_ENABLE_Fortran_default})

# Enable all compiler warning
if(MSVC)
  add_compiler_flags_if_supported("/W4")
else()
  add_compiler_flags_if_supported("-Wall" "-Wextra" "-Wpedantic")
endif()

# Build all the binaries in the `bin` build directory
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${OCB_BINARY_DIR}/bin")

# Create the compilation commands by default
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
