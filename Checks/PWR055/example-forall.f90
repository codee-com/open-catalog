! PWR055: consider applying offloading parallelism to forall loop

subroutine example(D, X, Y, n, a)
  implicit none
  integer, intent(in) :: n
  real(kind=8), intent(in) :: a
  real(kind=8), dimension(1:n), intent(in) :: X, Y
  real(kind=8), dimension(1:n), intent(out) :: D
  integer :: i

  do i = 1, n
    D(i) = a * X(i) + Y(i)
  end do
end subroutine example
