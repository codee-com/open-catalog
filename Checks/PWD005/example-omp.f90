! PWD005: Array range copied to the GPU does not cover the used range

subroutine example()
  implicit none
  integer, dimension(100) :: A, B, sum
  integer :: i

  !$omp target map(to: A(1:50), B(1:50)) map(from: sum(1:50))
  !$omp parallel do private(i) shared(A, B, sum)
  do i = 1, size(sum, 1)
    sum(i) = A(i) + B(i)
  end do
  !$omp end target
end subroutine example
