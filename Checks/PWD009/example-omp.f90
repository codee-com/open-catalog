! PWD009: Incorrect privatization in parallel region

subroutine example(A, B, C)
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32), intent(in) :: A(:), B(:)
  real(kind=real32), intent(inout) :: C(:)
  real(kind=real32) :: temp
  integer :: i

  !$omp parallel do default(none) private(i, temp, C) shared(A, B)
  do i = 1, size(C, 1)
    temp = A(i) * B(i)
    C(i) = C(i) + temp
  end do
end subroutine example
