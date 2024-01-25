! PWR015: Avoid copying unnecessary array elements to the GPU

subroutine example(a, c)
  implicit none
  integer, intent(inout) :: a(100), c(100)
  integer :: i
  !$omp target teams distribute parallel do schedule(auto) shared(a, b) &
  !$omp map(to: a(0:100)) map(tofrom: c(0:100))
  do i = 1, 50
    c(i) = c(i) + a(i)
  end do
end subroutine example
