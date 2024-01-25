! PWD007: Unprotected multithreading recurrence

subroutine example()
  implicit none
  integer :: i, x(5), y(5)

  y(1) = 0
  !$omp parallel do
  do i = 2, 5
    y(i) = y(i - 1) + x(i - 1)
  end do
end subroutine
