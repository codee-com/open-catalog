! PWR035: Avoid non-consecutive array access to improve performance

subroutine example(a)
  implicit none
  integer, intent(out) :: a(:, :)
  integer :: i, j

  do j = 1, size(a, 2)
    do i = 1, size(a, 1)
      a(i, 1) = 0
    end do
  end do
end subroutine example
