! PWR016: Use separate arrays instead of an Array-of-Structs

program main
  implicit none

  type point
    integer :: x
    integer :: y
    integer :: z
  end type point

contains

  subroutine foo()
    implicit none
    type(point) :: points(1000)
    integer :: i

    do i = 1, 1000
      points(i)%x = 1
      points(i)%y = 1
    end do
  end subroutine foo

end program main
