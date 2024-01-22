include(CheckCCompilerFlag)
include(CheckCXXCompilerFlag)
include(CheckFortranCompilerFlag)

# Append the the `value` if the `condition` is true to the extra arguments
function(append_if condition value)
  if (${condition})
    foreach(variable ${ARGN})
      set(${variable} "${${variable}} ${value}" PARENT_SCOPE)
    endforeach(variable)
  endif()
endfunction()

# Add a compiler flags only if the compiler doesn't fail with it
function(add_compiler_flags_if_supported)
  set(EXTRA_FLAGS)
  if (NOT MSVC)
    # Some compilers may just emit a warning, so force them to error
    list(APPEND EXTRA_FLAGS "-Werror")
  endif()

  foreach(flag ${ARGN})
    check_c_compiler_flag("${EXTRA_FLAGS} ${flag}" C_SUPPORTS_FLAG)
    append_if(C_SUPPORTS_FLAG "${flag}" CMAKE_C_FLAGS)
    check_cxx_compiler_flag("${EXTRA_FLAGS} ${flag}" CXX_SUPPORTS_FLAG)
    append_if(CXX_SUPPORTS_FLAG "${flag}" CMAKE_CXX_FLAGS)
    check_fortran_compiler_flag("${EXTRA_FLAGS} ${flag}" Fortran_SUPPORTS_FLAG)
    append_if(Fortran_SUPPORTS_FLAG "${flag}" CMAKE_Fortran_FLAGS)
  endforeach()
endfunction()

# As CMake outputs lots of status messages in the output, is hard to locate out
# own messages
function(note)
  message("
================================================================================
${ARGN}
================================================================================
")
endfunction()
