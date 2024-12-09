! PWR004: Declare OpenMP scoping for all variables

subroutine example(result)
  implicit none
  integer, intent(out) :: result(:)
  integer :: i, factor
  
  factor = 42

  ! No data scoping is specified
  !$omp parallel do
  do i = 1, size(result, 1)
    result(i) = factor * i
  end do
end subroutine example
