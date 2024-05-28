! PWR069: Use the keyword only to explicitly state what to import from a module

subroutine calculate_euclidean_distances_f(n, X1, Y1, X2, Y2, distances) bind(c)
  use iso_c_binding, only : c_double, c_int
  use utils

  implicit none
  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(n), intent(in) :: X1, Y1, X2, Y2
  real(kind=c_double), dimension(n), intent(out) :: distances

  integer(kind=c_int) :: i

  do i = 1, n
    distances(i) = euclidean_distance_f(X1(i), Y1(i), X2(i), Y2(i))
  end do
end subroutine calculate_euclidean_distances_f
