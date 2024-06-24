! PWR040: Consider loop tiling to improve the locality of reference

subroutine example(a, b)
  implicit none
  real, dimension(:, :), intent(out) :: a
  real, dimension(:, :), intent(in) :: b
  integer :: i, j

  do j = 1, size(a, 2)
    do i = 1, size(a, 1)
      a(i, j) = b(j, i)
    end do
  end do
end subroutine example
