! PWD009: Incorrect privatization in parallel region

subroutine example(A, B, C)
  implicit none
  real, intent(in) :: A(:), B(:)
  real, intent(inout) :: C(:)
  real :: temp
  integer :: i

  !$omp parallel do default(none) private(i, temp, C) shared(A, B)
  do i = 1, size(C, 1)
    temp = A(i) * B(i)
    C(i) = C(i) + temp
  end do
end subroutine example
