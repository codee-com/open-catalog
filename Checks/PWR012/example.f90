! PWR012: Pass only required fields from derived type as arguments to minimize data movements

program example

  implicit none

  type data
    integer :: a(10)
    integer :: b(10)
  end type data

contains

  pure subroutine foo(d)
    implicit none
    type(data), intent(in) :: d
    integer :: i, sum

    sum = 0
    do i = 1, 10
      sum = sum + d%a(i)
    end do
  end subroutine foo

  pure subroutine bar()
    implicit none
    type(data) :: d
    integer :: i

    do i = 1, 10
      d%a(i) = 1
      d%b(i) = 1
    end do

    call foo(d)
  end subroutine bar

end program example
