! PWR031: Replace pow by multiplication, division and/or square root

program main
  use iso_fortran_env, only : real64
  implicit none
  !
  print '(A, F0.15)', '2 raised to the power of 1.5 is: ', &
      raise_x_to_the_power_of_1_point_5(2.0_real64)
contains
  pure function raise_x_to_the_power_of_1_point_5(x)
    implicit none
    ! function return type
    real(kind=real64) :: raise_x_to_the_power_of_1_point_5
    ! dummy args
    real(kind=real64), intent(in) :: x
    !
    raise_x_to_the_power_of_1_point_5 = x * sqrt(x)
  end function raise_x_to_the_power_of_1_point_5
end program main
