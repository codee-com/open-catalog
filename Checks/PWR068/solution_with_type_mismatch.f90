! PWR068: Encapsulate procedures within modules to avoid the risks of calling
!         implicit interfaces

program test_explicit_interface
  use iso_fortran_env, only: real32
  use mod_factorial, only: factorial
  implicit none
  real(kind=real32) :: number, result

  number = 5
  call factorial(number, result)
  print *, "Factorial of", number, "is", result
end program test_explicit_interface
