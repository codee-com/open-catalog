! PWR040: Consider loop tiling to improve the locality of reference

pure subroutine example(a, b)
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32), dimension(:, :), intent(out) :: a
  real(kind=real32), dimension(:, :), intent(in) :: b
  integer :: i, j

  do j = 1, size(a, 2)
    do i = 1, size(a, 1)
      a(i, j) = b(j, i)
    end do
  end do
end subroutine example
