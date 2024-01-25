! PWR006: Avoid privatization of read-only variables

program example
  implicit none
  integer :: i
  integer :: sum(10)
  integer :: a(10), b(10)

  !$omp parallel do firstprivate(i, a, b) shared(sum)
  do i = 1, 10
    sum(i) = a(i) + b(i);
  end do
end program example
