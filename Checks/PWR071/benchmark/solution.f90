! PWR071: Prefer real(kind=kind_value) for declaring consistent floating types

subroutine calculate_euclidean_distances_improved_f(n, X1, Y1, X2, Y2, &
                                                    distances) bind(c)
  use iso_c_binding, only : c_double, c_int

  implicit none
  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(n), intent(in) :: X1, Y1, X2, Y2
  real(kind=c_double), dimension(n), intent(out) :: distances

  integer(kind=c_int) :: i
  real(kind=c_double) :: distance

  do i = 1, n
    distance = sqrt((Y1(i) - X1(i))**2 + (Y2(i) - X2(i))**2)
    distances(i) = distance
  end do
end subroutine calculate_euclidean_distances_improved_f
