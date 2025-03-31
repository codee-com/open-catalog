! PWR007: Disable the implicit declaration of variables and procedures

program example
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32) :: number, result

  number = 5
  call factorial(number, result)
  print *, "Factorial of", number, "is", result
end program example
