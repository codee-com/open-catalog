! PWR009: Use OpenMP teams to offload work to GPU

subroutine example()
  implicit none
  real, dimension(100,200) :: y
  integer(8) :: i, j

  !$omp target map(from: y(1:100, 1:200))
  !$omp parallel default(none) private(i, j) shared(y)
  !$omp do private(j) schedule(static)
  do i = 1, 100
    do j = 1, 200
      y(i, j) = 0
    end do
  end do
  !$omp end parallel
  !$omp end target
end subroutine example
