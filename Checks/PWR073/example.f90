! PWR073: Transform common block into a module for better data encapsulation

program test_common_block
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32) :: var1
  integer :: var2
  common /my_common/ var1, var2

  var1 = 3.14
  var2 = 20

  call printVar1
  call printVar2

contains

subroutine printVar1
  implicit none
  real(kind=real32) :: var1
  common /my_common/ var1

  print *, "Var1: ", var1
end subroutine printVar1

subroutine printVar2
  implicit none
  integer :: var2
  common /my_common/ var2

  print *, "Var2: ", var2

  ! Did you spot the bug?
  ! 
  ! In the common block re-definition, `var1` is missing. As a result, `var2`
  ! unintentionally references the memory location of `var1`.
  !
  ! This lack of naming consistency, as well as type safety (note that `var1`
  ! also contains real data), make this error go easily unnoticed, as the code
  ! still compiles.
end subroutine printVar2
end program test_common_block
