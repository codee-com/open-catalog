! PWD007: Unprotected multithreading recurrence

subroutine example(x, y)
  implicit none
  integer, intent(in) :: x(:)
  integer, intent(inout) :: y(:)
  integer :: i

  y(1) = 0

  !$omp parallel do
  do i = 2, size(y, 1)
    y(i) = y(i - 1) + x(i - 1)
  end do
  !$omp end parallel do
end subroutine example
