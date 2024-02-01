! PWR022: move invariant conditional out of the loop to facilitate vectorization

subroutine solution_f(n, A, B, C, D) bind(c)
  use iso_c_binding, only : c_int, c_double

  implicit none
  integer(kind=c_int) i, j, k
  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double) :: val
  real(kind=c_double), dimension(1:n), intent(in) :: A
  real(kind=c_double), dimension(1:n, 1:n), intent(in) :: B
  real(kind=c_double), dimension(1:n, 1:n, 1:n), intent(in) :: C
  real(kind=c_double), dimension(1:n, 1:n), intent(out) :: D

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
