! PWR040: Consider loop tiling to improve the locality of reference

subroutine example(a, b, n)
  implicit none
  integer, intent(in) :: n
  real, dimension(1:n, 1:n), intent(inout) :: a, b
  integer :: i, j

  do i = 1, n
    do j = 1, n
      a(j, i) = b(i, j)
    end do
  end do
end subroutine example
