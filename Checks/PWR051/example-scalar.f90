! PWR051: Consider applying multithreading parallelism to scalar reduction loop

function example(A) result(sum)
  implicit none
  real(kind=8), intent(in) :: A(:)
  real(kind=8) :: sum
  integer :: i

  sum = 0.0
  do i = 1, size(A, 1)
    sum = sum + A(i)
  end do
end function example
