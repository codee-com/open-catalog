! PWR006: Avoid privatization of read-only variables

subroutine example()
  implicit none
  integer :: i
  integer :: a(5)
  integer :: b(5) 
  integer :: sum(5)

  a = [1, 2, 3, 4, 5]
  b = [6, 7, 8, 9, 10]

  !$omp parallel do default(none) firstprivate(a, b) shared(sum) private(i)
  do i = 1, 5
    sum(i) = a(i) + b(i)
  end do
  !$omp end parallel do
end subroutine example
