! PWR073: Transform common block into a module for better data encapsulation

module my_module
  use iso_fortran_env, only: real32
  implicit none
  private
  real(kind=real32) :: var1
  integer :: var2
  public :: getVar1, setVar1, getVar2, setVar2

contains

  real(kind=real32) function getVar1()
    getVar1 = var1
  end function getVar1

  subroutine setVar1(value)
    real(kind=real32), intent(in) :: value
    var1 = value
  end subroutine setVar1

  integer function getVar2()
    getVar2 = var2
  end function getVar2

  subroutine setVar2(value)
    integer, intent(in) :: value
    var2 = value
  end subroutine setVar2
end module my_module

program test_module
  use my_module, only: setVar1, setVar2
  implicit none

  call setVar1(3.14)
  call setVar2(20)

  call printVar1
  call printVar2

contains

subroutine printVar1
  use my_module, only: getVar1
  implicit none

  print *, "Var1: ", getVar1()
end subroutine printVar1

subroutine printVar2
  use my_module, only: getVar2
  implicit none

  print *, "Var2: ", getVar2()
end subroutine printVar2
end program test_module
