! PWD003: Copy of pointer value instead of pointed-to data to an accelerator
! device

subroutine example(a, b, sum, size)
  implicit none
  integer, dimension(*), intent(in) :: a, b
  integer, dimension(*), intent(out) :: sum
  integer, intent(in) :: size
  integer :: i

  ! Array bounds should be specified
  !$omp target map(to: a, b) map(from: sum)
  !$omp parallel do default(none) shared(a, b, sum)
  do i = 1, size
    sum(i) = a(i) + b(i)
  end do
  !$omp end parallel do
  !$omp end target
end subroutine example
