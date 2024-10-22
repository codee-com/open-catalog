! PWR042: Loop nest can benefit from loop interchange, but reduction variable
! initialization prevents loop interchange

subroutine example(A, B)
  real, intent(in) :: A(:, :)
  real, intent(out) :: B(:)
  real :: s
  integer :: i, j

  do i = 1, size(A, 1)
    s = 0.0

    do j = 1, size(A, 2)
      s = s + A(i, j)
    end do

    B(i) = 0.1 * s
  end do
end subroutine example
