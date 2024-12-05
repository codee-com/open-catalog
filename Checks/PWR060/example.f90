! PWR060 : consider loop fission to separate gather memory access pattern

pure subroutine example(a, D, X, Y, index, n)
  implicit none
  real(kind=8), intent(in) :: a
  real(kind=8), dimension(n), intent(out) :: D
  real(kind=8), dimension(n), intent(in) :: X, Y
  integer, dimension(n), intent(in) :: index
  integer, intent(in) :: n
  integer :: i

  do i = 1, n
    D(i) = a * X(index(i)) + Y(i)
  end do
end subroutine example
