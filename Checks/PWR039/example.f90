! PWR039: Consider loop interchange to improve the locality of reference and
! enable vectorization

! NOT-PWR070: Explicit-shape arrays used for C-interoperability
pure subroutine matmul_f(n, A, B, C) bind(c)
  use iso_c_binding, only : c_int, c_double

  implicit none
  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(1:n, 1:n), intent(in) :: A, B
  real(kind=c_double), dimension(1:n, 1:n), intent(inout) :: C
  integer(kind=c_int) :: i, j, k

  do j = 1, n
    do i = 1, n
      do k = 1, n
        C(i, j) = C(i, j) + A(i, k) * B(k, j)
      end do
    end do
  end do
end subroutine matmul_f
