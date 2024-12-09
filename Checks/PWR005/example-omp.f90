! PWR005: Disable default OpenMP scoping

subroutine example(result)
  implicit none
  integer, intent(out) :: result(:)
  integer :: i, t

  ! Default data scoping is used, making `t` shared instead of private
  ! NOT-PWR004: omitting scoping specifiers to highlight dangers of PWR005
  !$omp parallel do
  do i = 1, size(result, 1)
    t = i + 1
    result(i) = t
  end do
  !$omp end parallel do
end subroutine example
