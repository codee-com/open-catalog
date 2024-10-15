! PWR049: Move iterator-dependent condition outside of the loop

subroutine example(a, b)
  implicit none
  integer, intent(inout) :: a(:, :)
  integer, intent(in) :: b(:, :)
  integer :: i, j

  do j = 1, size(a, 2)
    do i = 1, size(a, 1)
      if (i == 1) then
        a(i, j) = 0
      else
        a(i, j) = a(i - 1, j) + b(i, j)
      end if
    end do
  end do
end subroutine example
