! PWR013: Avoid copying unused variables to the GPU

subroutine example(A, B, C)
  implicit none
  integer, intent(in) :: A(:), B(:)
  integer, intent(inout) :: C(:)
  integer :: i

  !$omp target teams distribute parallel do schedule(auto) default(none) &
  !$omp& shared(A, B, C) private(i) map(to: A, B) map(tofrom: C)
  do i = 1, size(C, 1)
    C(i) = C(i) + A(i)
  end do
  !$omp end target teams distribute parallel do
end subroutine example
