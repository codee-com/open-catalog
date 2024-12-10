! PWR060 : consider loop fission to separate gather memory access pattern

pure subroutine example(a, D, X, Y, index)
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32), intent(in) :: a
  real(kind=real32), dimension(:), intent(out) :: D
  real(kind=real32), dimension(:), intent(in) :: X, Y
  integer, dimension(:), intent(in) :: index
  integer :: i

  do i = 1, size(D, 1)
    D(i) = a * X(index(i)) + Y(i)
  end do
end subroutine example
