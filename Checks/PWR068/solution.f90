! PWR068: Call procedures through explicit interfaces, preferably as module
!         procedures

program test_explicit_interface
  use mod_factorial, only: factorial
  implicit none
  integer :: number, result

  number = 5
  call factorial(number, result)
  print *, "Factorial of", number, "is", result
end program test_explicit_interface
