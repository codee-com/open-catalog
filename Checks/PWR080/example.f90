! PWR080: Conditionally initialized variables can lead to undefined behavior

program main
  use iso_fortran_env, only: real32
  implicit none

  real(kind=real32) :: array(4)
  array = [0.25, 0.25, 0.25, 0.25]

  print *, "Sum is:", transform_and_sum(array, "unknownOption")

contains

  real(kind=real32) function transform_and_sum(array, option)
    implicit none

    real(kind=real32), intent(in) :: array(:)
    character(len=*), intent(in) :: option

    real(kind=real32) :: sum
    real(kind=real32) :: factor
    integer :: i

    sum = 0.0

    if (option == "half") then
      factor = 0.5
    else if (option == "double") then
      factor = 2.0
    end if

    do i = 1, size(array, 1)
      sum = sum + array(i) * factor
    end do

    transform_and_sum = sum
  end function transform_and_sum

end program main
