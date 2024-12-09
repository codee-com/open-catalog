! PWR021: temporary computation can be extracted to a vectorizable loop

subroutine example()
  implicit none
  integer :: a(1000), c(1000), i, t

  do i = 1, 1000
    t = expensive_computation(c, i)
    a(c(i)) = a(c(i)) + t
  end do

contains

  pure integer function expensive_computation(c, i)
    implicit none
    integer, intent(in) :: i, c(:)

    expensive_computation = c(i) * 2
  end function expensive_computation
end subroutine example
