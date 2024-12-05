! PWR043: Loop nest can benefit from loop interchange, but reduction variable
! initialization prevents loop interchange

pure subroutine matmul_f(n, A, B, C) bind(c)
  use iso_c_binding, only : c_int, c_double

  implicit none
  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(1:n, 1:n), intent(in) :: A, B
  real(kind=c_double), dimension(1:n, 1:n), intent(out) :: C
  integer(kind=c_int) :: i, j, k
  real(kind=c_double) :: c_aux

  do j = 1, n
    do i = 1, n
      c_aux = 0.0
      do k = 1, n
        c_aux = c_aux + A(i, k) * B(k, j)
      end do
      C(i, j) = c_aux
    end do
  end do
end subroutine matmul_f
