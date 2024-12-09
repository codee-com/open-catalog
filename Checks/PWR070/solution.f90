! PWR070: Declare array dummy arguments as assumed-shape arrays

program test_assumed_shape
  use iso_fortran_env, only: real32
  implicit none
  integer, parameter :: rows = 2, cols = 3
  ! Each row contains "1, 2, 3"
  real(kind=real32) :: matrix(rows, cols) = reshape([1.0, 2.0, 3.0, 1.0, 2.0, 3.0], [rows, cols])

  ! Should print "6" (1 + 2 + 3) for each row
  call sum_rows_assumed_shape(matrix)
  
contains

subroutine sum_rows_assumed_shape(arr)
  real(kind=real32), intent(in) :: arr(:, :)
  integer :: i, j
  real(kind=real32) :: sum

  do i = 1, size(arr, 1)
    sum = 0.0
    do j = 1, size(arr, 2)
      sum = sum + arr(i, j)
    end do
    print *, 'Row', i, 'Sum:', sum
  end do
end subroutine sum_rows_assumed_shape
end program test_assumed_shape
