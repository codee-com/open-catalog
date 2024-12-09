! PWR053: consider applying vectorization to forall loop

pure subroutine example(D, X, Y, a)
use iso_fortran_env, only: real32
  implicit none
  real(kind=real32), intent(out) :: D(:)
  real(kind=real32), intent(in) :: X(:), Y(:)
  real(kind=real32), intent(in) :: a
  integer :: i

  do i = 1, size(D, 1)
    D(i) = a * X(i) + Y(i)
  end do
end subroutine example
