! PWR089: Remove unexpected arguments from procedure calls

! NOT-PWR068: External procedure used to demonstrate how compilers can't catch
!             the argument count mismatch
program correct_call
  use iso_fortran_env, only: real32
  implicit none

  external :: logField
  character(len=20) :: name
  integer :: step
  real(kind=real32) :: val

  name = "field"
  step = 10
  val = 3.14

  call logField(name, step, val)
end program correct_call
