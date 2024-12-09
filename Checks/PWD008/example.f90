! PWD008: Unprotected multithreading recurrence due to out-of-dimension-bounds
! array access

subroutine example(A)
  integer, intent(inout) :: A(:, :)
  integer :: i, j

  !$omp parallel do private(i, j) shared(A)
  do j = 2, size(A, 2)
    do i = 1, size(A, 1)
      A(i, j) = A(i, j) + A(i - 1, j)
    end do
  end do 
end subroutine example
