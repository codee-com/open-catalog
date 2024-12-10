! PWD010: Incorrect sharing in parallel region

subroutine example(result)
  implicit none
  integer, intent(out) :: result(:, :)
  integer :: i, j

  ! NOT-PWR004: `i` and `j` aren't set to `private` to showcase the issue
  !$omp parallel do shared(result)
  do j = 1, size(result, 2)
    do i = 1, size(result, 1)
      result(i, j) = 0
    end do
  end do
end subroutine example
