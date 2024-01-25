! PWR036: Avoid indirect array access to improve performance

subroutine example()
  implicit none
  integer :: a(100), b(100), i

  do i = 1, 100
    a(b(i)) = 0
  end do
end subroutine example
