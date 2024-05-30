! PWR073: Transform common block into a module for better data encapsulation

module my_module
  implicit none
  public
  real    :: var1
  integer :: var2
end module my_module

program test_module
  use my_module
  implicit none

  var1 = 3.14
  var2 = 20

  call printVar1
  call printVar2

contains

subroutine printVar1
  use my_module, only: var1
  implicit none

  print *, "Var1: ", var1
end subroutine printVar1

subroutine printVar2
  use my_module, only: var2
  implicit none

  print *, "Var2: ", var2
end subroutine printVar2
end program test_module
