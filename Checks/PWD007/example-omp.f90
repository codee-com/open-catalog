! PWD007: Unprotected multithreading recurrence

subroutine example(x, y)
  implicit none
  integer, intent(in) :: x(:)
  integer, intent(inout) :: y(:)
  integer :: i

  y(1) = 0

  !$omp parallel do private(i) shared(x, y)
  do i = 2, 10
    y(i) = y(i - 1) + x(i - 1)
  end do
  !$omp end parallel do
end subroutine example
