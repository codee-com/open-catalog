! PWR085: Favor iterative implementations over recursion to prevent stack overflows

subroutine solution(times)
  implicit none
  integer, intent(in) :: times
  integer :: i, sum
  integer :: fib_0, fib_1, fib

  sum = 0
  fib_0 = 0
  fib_1 = 1

  do i = 2, times - 1
    fib = fib_0 + fib_1
    sum = sum + fib
    fib_0 = fib_1
    fib_1 = fib
  end do
end subroutine solution
