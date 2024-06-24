! PWR062: Consider loop interchange by removing accumulation on array value

subroutine matmul_f(n, A, B, C) bind(c)
  use iso_c_binding, only : c_int, c_double

  implicit none
  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(1:n, 1:n), intent(in) :: A, B
  real(kind=c_double), dimension(1:n, 1:n), intent(inout) :: C
  integer(kind=c_int) :: i, j, k

  do j = 1, n
    do i = 1, n
      C(i, j) = 0
      do k = 1, n
        C(i, j) = C(i, j) + A(i, k) * B(k, j)
      end do
    end do
  end do
end subroutine matmul_f
