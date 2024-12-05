! PWR022: move invariant conditional out of the loop to facilitate vectorization

pure subroutine solution_f(n, A, B, C, D) bind(c)
  use iso_c_binding, only : c_int, c_double

  implicit none
  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(1:n), intent(in) :: A
  real(kind=c_double), dimension(1:n, 1:n), intent(in) :: B
  real(kind=c_double), dimension(1:n, 1:n, 1:n), intent(in) :: C
  real(kind=c_double), dimension(1:n, 1:n), intent(inout) :: D
  integer(kind=c_int) :: i, j, k

  do j = 1, n
    do i = 1, n
      D(i, j) = B(i, j) + A(1) * C(i, j, 1)
    end do
  end do
  do k = 2, n
    do j = 1, n
      do i = 1, n
        D(i, j) = D(i, j) + A(k) * C(i, j, k)
      end do
    end do
  end do
end subroutine solution_f
