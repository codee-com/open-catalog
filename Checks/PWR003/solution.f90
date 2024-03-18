! PWR003: Explicitly declare pure functions

! Computes the weight of each object in a vector
subroutine solution_f(n, M, W) bind(c)
  use iso_c_binding, only : c_int, c_double
  use gravity_getters, only : gravity_pure

  implicit none
  integer(kind=c_int) :: i
  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(1:n), intent(in) :: M
  real(kind=c_double), dimension(1:n), intent(out) :: W

  do i = 1, n
    W(i) = M(i) * gravity_pure(3)
  end do
end subroutine solution_f
