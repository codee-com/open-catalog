! PWR068: Call procedures through explicit interfaces, preferably as module
!         procedures

program test_implicit_interface
  use iso_fortran_env, only: real32
  implicit none
  external :: factorial
  real(kind=real32) :: number, result

  number = 5
  call factorial(number, result)
  print *, "Factorial of", number, "is", result
end program test_implicit_interface
