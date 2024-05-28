! PWR070: Declare array dummy arguments as assumed-shape arrays

module mod_explicit_shape
  implicit none
contains
subroutine clamp_values_f(n, X, min_value, max_value)
  use iso_c_binding, only : c_double, c_int

  implicit none
  integer(kind=c_int), intent(in) :: n
  real(kind=c_double), dimension(n), intent(inout) :: X
  real(kind=c_double), intent(in) :: min_value, max_value
  
  integer(kind=c_int) :: i

  do i = 1, n
    if(X(i) < min_value) then
      X(i) = min_value
    else if(X(i) > max_value) then
      X(i) = max_value
    end if
  end do
end subroutine clamp_values_f
end module mod_explicit_shape

subroutine clamp_even_data_points_f(n, X, min_value, max_value) bind(c)
  use iso_c_binding, only : c_double, c_int
  use mod_explicit_shape, only : clamp_values_f

  implicit none
  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(n), intent(inout) :: X
  real(kind=c_double), intent(in), value :: min_value, max_value

  call clamp_values_f(n/2, X(2:n:2), min_value, max_value)
end subroutine clamp_even_data_points_f
