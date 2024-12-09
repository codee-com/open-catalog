! PWR060 : consider loop fission to separate gather memory access pattern

pure subroutine example(a, D, X, Y, index)
  implicit none
  real(kind=8), intent(in) :: a
  real(kind=8), dimension(:), intent(out) :: D
  real(kind=8), dimension(:), intent(in) :: X, Y
  integer, dimension(:), intent(in) :: index
  integer :: i

  do i = 1, size(D, 1)
    D(i) = a * X(index(i)) + Y(i)
  end do
end subroutine example
