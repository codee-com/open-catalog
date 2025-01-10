! PWR007: Disable implicit declaration of variables

program solution
  use iso_fortran_env, only: real32
  implicit none
  integer :: num1
  real(kind=real32) :: num2, res

  num1 = 7
  num2 = 2.5
  res = num1 / num2
  print *, res
end program solution
