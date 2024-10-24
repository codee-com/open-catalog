! PWD010: Incorrect sharing in parallel region

subroutine example(result)
  integer, intent(out) :: result(:, :)
  integer :: i, j

  !$omp parallel do shared(result)
  do j = 1, size(result, 2)
    do i = 1, size(result, 1)
      result(i, j) = 0
    end do
  end do
end subroutine example
