! PWD003: Copy of pointer value instead of pointed-to data to an accelerator
! device

subroutine example(a, b, result, size)
  implicit none
  integer, intent(in) :: size
  integer, dimension(1:size), intent(in) :: a, b
  integer, dimension(1:size), intent(out) :: result
  integer :: i

  ! array bounds should be specified
  !$omp target map(to: a, b) map(from: result)
  !$omp parallel do default(none) shared(a, b, result)
  do i = 1, size
    result(i) = a(i) + b(i)
  end do
  !$omp end target
end subroutine example
