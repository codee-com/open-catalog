! PWR018: Call to recursive function within a loop may inhibit vectorization

module mod_fibonacci
  contains
  recursive function fibonacci(n) result(fibo)
    implicit none
    integer, intent(in) :: n
    integer :: fibo

    if (n == 0) then
      fibo = 0
    else if (n == 1) then
      fibo = 1
    else
      fibo = fibonacci(n - 1) + fibonacci(n - 2)
    end if
  end function fibonacci
end module mod_fibonacci

subroutine example(times)
  use mod_fibonacci, only : fibonacci

  implicit none
  integer, intent(in) :: times
  integer :: i, sum

  do i = 1, times
    sum = sum + fibonacci(i)
  end do
end subroutine example
