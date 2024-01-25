! PWR035: Avoid non-consecutive array access to improve performance

subroutine example()
  implicit none
  integer :: a(100, 100), i, j

  do i = 1, 100
    do j = 1, 100
      a(j, 1) = 0
    end do
  end do
end subroutine example
