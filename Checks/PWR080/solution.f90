! PWR080: Conditionally initialized variables can lead to undefined behavior

module options
  integer, parameter :: OPTION_HALF   = 1
  integer, parameter :: OPTION_DOUBLE = 2
  integer, parameter :: OPTION_UNKNOWN = 3
end module options

program main
  use iso_fortran_env, only: real32
  use options, only: OPTION_HALF, OPTION_DOUBLE, OPTION_UNKNOWN
  implicit none

  real(kind=real32) :: array(4)
  array = [0.25, 0.25, 0.25, 0.25]

  print *, "Sum is:", transform_and_sum(array, OPTION_UNKNOWN)

contains

  real(kind=real32) function transform_and_sum(array, option)
    implicit none

    real(kind=real32), intent(in) :: array(:)
    integer, intent(in) :: option

    real(kind=real32) :: sum
    real(kind=real32) :: factor
    integer :: i

    sum = 0.0
    ! Identity transformation by default
    factor = 1.0

    if (option == OPTION_HALF) then
      factor = 0.5
    else if (option == OPTION_DOUBLE) then
      factor = 2.0
    end if

    do i = 1, size(array, 1)
      sum = sum + array(i) * factor
    end do

    transform_and_sum = sum
  end function transform_and_sum

end program main
