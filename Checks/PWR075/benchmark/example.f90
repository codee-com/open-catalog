! PWR075: Avoid using GNU Fortran extensions
!
! `dcosd` and `dsind` compute the cosine and sine of an angle given in degrees,
! using real(8) types. As of August, 2024, they are GNU Fortran extensions.

! Computes the distance between `n` pairs of points (P1(n), P2(n)) in 
! the circunferences given by radiuses(n)
subroutine calculate_distances_f(n, P1, P2, radiuses, distances) bind(c)
  use iso_c_binding, only : c_double, c_int
  
  implicit none
  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(n), intent(in) :: P1, P2, radiuses
  real(kind=c_double), dimension(n), intent(out) :: distances

  integer(kind=c_int) :: i
  real(kind=c_double) :: p1_x, p1_y, p2_x, p2_y

  do i = 1, n
    ! From polar coordinates (angle, radius) to Cartesian coordinates (x, y)
    p1_x = dcosd(P1(i)) * radiuses(i)
    p1_y = dsind(P1(i)) * radiuses(i)
    p2_x = dcosd(P2(i)) * radiuses(i)
    p2_y = dsind(P2(i)) * radiuses(i)
 
    ! Euclidean distance between the two points
    distances(i) = sqrt((p2_x - p1_x)**2 + (p2_y - p1_y)**2)
  end do
end subroutine calculate_distances_f
