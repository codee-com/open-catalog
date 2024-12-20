! PWR046: Replace two divisions with a division and a multiplication

! Computes the harmonic means for each pair {x_i, y_i}
!
! https://en.wikipedia.org/wiki/Harmonic_mean
subroutine compute_harmonic_mean_between_pairs_f(n, x, y, result) bind(c)
  use iso_c_binding, only: c_double, c_int
  implicit none

  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(n), intent(in) :: x, y
  real(kind=c_double), dimension(n), intent(out) :: result

  integer(kind=c_int) :: i

  do i = 1, n
    result(i) = 2 / (1 / x(i) + 1 / y(i))
  end do
end subroutine compute_harmonic_mean_between_pairs_f
