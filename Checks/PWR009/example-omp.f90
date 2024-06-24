! PWR009: Use OpenMP teams to offload work to GPU

subroutine example(A, B, C)
  implicit none
  real, dimension(:, :), intent(in) :: A, B
  real, dimension(:, :), intent(inout) :: C
  integer :: i, j, k

  !$omp target map(to: A, B) map(tofrom: C)
  !$omp parallel default(none) private(i, j, k) shared(A, B, C)
  !$omp do
  do j = 1, size(C, 2)
    do k = 1, size(C, 2)
      do i = 1, size(C, 1)
        C(i, j) = C(i, j) + A(i, k) * B(k, j)
      end do
    end do
  end do
  !$omp end do
  !$omp end parallel
  !$omp end target
end subroutine example
