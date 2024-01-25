! PWR005: Disable default OpenMP scoping

subroutine example(result)
  implicit none
  integer, intent(out) :: result(:)
  integer :: i

  ! default data scoping is used which may not be correct
  !$omp parallel do
  do i = 1, size(result, 1)
    result(i) = i
  end do
end subroutine example
