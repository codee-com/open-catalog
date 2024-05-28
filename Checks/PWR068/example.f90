! PWR068: Encapsulate external procedures within modules to avoid the risks of
!         calling implicit interfaces

program test_implicit_interface
  implicit none
  external :: factorial
  real :: number, result

  number = 5
  call factorial(number, result)
  print *, "Factorial of", number, "is", result
end program test_implicit_interface
