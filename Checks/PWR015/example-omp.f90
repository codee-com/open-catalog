! PWR015: Avoid copying unnecessary array elements to the GPU

subroutine example(A, B, sum)
  implicit none
  integer, intent(in) :: A(:), B(:)
  integer, intent(out) :: sum(:)
  integer :: i

  !$omp target parallel do default(none) shared(A, B, sum) &
  !$omp& map(to: a, b) map(from: sum)
  do i = 1, size(sum, 1) / 2
    sum(i) = A(i) + B(i)
  end do
  !$omp end target parallel do
end subroutine example
