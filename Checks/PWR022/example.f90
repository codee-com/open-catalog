! PWR022: move invariant conditional out of the loop to facilitate vectorization

subroutine example_f(n, A, B, C, D) bind(c)
  use iso_c_binding, only : c_int, c_double

  implicit none
  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(1:n), intent(in) :: A
  real(kind=c_double), dimension(1:n, 1:n), intent(in) :: B
  real(kind=c_double), dimension(1:n, 1:n, 1:n), intent(in) :: C
  real(kind=c_double), dimension(1:n, 1:n), intent(inout) :: D
  integer(kind=c_int) :: i, j, k
  real(kind=c_double) :: val

  do k = 1, n
    do j = 1, n
      do i = 1, n
        if (i == 1) then
          val = B(j, k)
        else
          val = D(j, k)
        end if
        D(j, k) = val + A(i) * C(i, j, k)
      end do
    end do
  end do
end subroutine example_f
