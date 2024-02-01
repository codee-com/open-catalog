! PWR022: move invariant conditional out of the loop to facilitate vectorization

subroutine solution_f(n, A, B, C, D)
  implicit none
  integer i, j, k
  integer, intent(in) :: n
  real :: val
  real, dimension(1:n), intent(in) :: A
  real, dimension(1:n, 1:n), intent(in) :: B
  real, dimension(1:n, 1:n, 1:n), intent(in) :: C
  real, dimension(1:n, 1:n), intent(out) :: D

  do k = 1, n
    do j = 1, n
      D(j, k) = B(j, k) + A(1) * C(j, k, 1)
    end do
  end do
  do i = 2, n
    do k = 1, n
      do j = 1, n
        D(j, k) = D(j, k) + A(i) * C(j, k, i)
      end do
    end do
  end do
end subroutine solution_f
