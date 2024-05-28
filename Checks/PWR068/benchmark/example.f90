! PWR068: Encapsulate external procedures within modules to avoid the risks of
!         calling implicit interfaces

function euclidean_distance_f(x1, y1, x2, y2)
  use iso_c_binding, only : c_double

  implicit none
  real(kind=c_double) :: euclidean_distance_f
  real(kind=c_double), intent(in) :: x1, y1, x2, y2

  euclidean_distance_f = sqrt((y1 - x1)**2 + (y2 - x2)**2)
end function euclidean_distance_f

subroutine calculate_euclidean_distances_f(n, X1, Y1, X2, Y2, distances) bind(c)
  use iso_c_binding, only : c_double, c_int

  implicit none

  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(n), intent(in) :: X1, Y1, X2, Y2
  real(kind=c_double), dimension(n), intent(out) :: distances
  
  integer(kind=c_int) :: i
  real(kind=c_double), external :: euclidean_distance_f

  do i = 1, n
    distances(i) = euclidean_distance_f(X1(i), Y1(i), X2(i), Y2(i))
  end do
end subroutine calculate_euclidean_distances_f
