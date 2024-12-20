! PWR046: Replace two divisions with a division and a multiplication

program main
  implicit none

  print *, "Result:", example(1.23, 1.41, 1.89)

  contains

  function example(a, b, c)
    use iso_fortran_env, only: real32
    implicit none

    real(kind=real32), intent(in) :: a, b, c
    real(kind=real32) :: example

    example = a / b / c
  end function example

end program main
