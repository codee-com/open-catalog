! PWR022: move invariant conditional out of the loop to facilitate vectorization

subroutine example()
  implicit none
  integer :: n
  integer :: a(1000), i, total

  n = 100

  do i = 1, n
    if (n .lt. 10) then
      total = total + 1
    end if
    a(i) = total
  end do
end subroutine example
