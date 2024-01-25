! PWR049: Move iterator-dependent condition outside of the loop

subroutine example(n)
  implicit none
  integer, intent(in) :: n
  integer :: i, j, a(n, n), b(n, n)

  do i = 1, n
    do j = 1, n
      if (j .eq. 1) then
        a(j, i) = 0
      else
        a(j, i) = a(j - 1, i) + b(j, i)
      end if
    end do
  end do
end subroutine example
