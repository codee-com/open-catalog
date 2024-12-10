! PWR042: Loop nest can benefit from loop interchange, but reduction variable
! initialization prevents loop interchange

pure subroutine example(A, B)
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32), intent(in) :: A(:, :)
  real(kind=real32), intent(out) :: B(:)
  real(kind=real32) :: s
  integer :: i, j

  do i = 1, size(A, 1)
    s = 0.0

    do j = 1, size(A, 2)
      s = s + A(i, j)
    end do

    B(i) = 0.1 * s
  end do
end subroutine example
