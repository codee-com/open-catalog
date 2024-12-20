! PWR031: Replace pow by multiplication, division and/or square root

! Computes the integral of $f(x) = x^{1.5} \sin(x)$ over the interval $\{a, b\}$
! using the midpoint rule with $n$ samples
pure function midpoint_rule_x_pow_1_5_sin_x_improved_f(a, b, n) bind(c)
  use iso_c_binding, only : c_double, c_int
  implicit none
  ! function return type
  real(kind=c_double) :: midpoint_rule_x_pow_1_5_sin_x_improved_f
  ! dummy args
  real(kind=c_double), intent(in), value :: a, b
  integer(kind=c_int), intent(in), value :: n
  ! local vars
  real(kind=c_double) :: integral, dx, x
  integer(kind=c_int) :: i
  !
  integral = 0.0_c_double
  dx = (b - a) / n
  do i = 0_c_int, n
    x = (real(i, kind=c_double) + 0.5_c_double) * dx + a
    integral = integral + x * sqrt(x) * sin(x) * dx
  end do
  midpoint_rule_x_pow_1_5_sin_x_improved_f = integral
end function midpoint_rule_x_pow_1_5_sin_x_improved_f
