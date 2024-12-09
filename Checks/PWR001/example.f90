! PWR001: Declare global variables as function parameters

module globalsMod
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32) :: global_a
end module globalsMod

pure function example()
  use iso_fortran_env, only: real32
  ! NOT-PWR069: The `only` is ommited to trigger PWR001
  use globalsMod
  implicit none
  real(kind=real32) :: example
  example = global_a
end function
