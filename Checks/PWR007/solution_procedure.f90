! PWR007: Disable the implicit declaration of variables and procedures

program solution
  use mod_factorial, only: factorial
  implicit none(type, external)
  integer :: number, result

  number = 5
  call factorial(number, result)
  print *, "Factorial of", number, "is", result
end program solution
