! PWR051: Consider applying multithreading parallelism to scalar reduction loop

subroutine example(A, n, sum)
  implicit none
  integer, intent(in) :: n
  real(kind=8), dimension(1:n), intent(in) :: A
  real(kind=8), intent(out) :: sum
  integer :: i

  sum = 0
  do i = 1, n
    sum = sum + A(i)
  end do
end subroutine example
