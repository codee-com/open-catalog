! PWR079: Avoid undefined behavior due to uninitialized variables

program main
  use iso_fortran_env, only: real32
  implicit none

  real(kind=real32) :: array(5)
  array = [0.24, 0.33, 0.17, 0.89, 0.05]

  print *, "Sum is:", sum_array(array)

contains

  pure real(kind=real32) function sum_array(array)
    implicit none
    real(kind=real32), intent(in) :: array(:)
    real(kind=real32) :: sum
    integer :: i

    sum = 0

    do i = 1, size(array, 1)
      sum = sum + array(i)
    end do

    sum_array = sum
  end function sum_array

end program main
