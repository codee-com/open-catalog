! PWR075: Avoid using compiler-specific Fortran extensions
!
! The Fortran standard specifies the availability of:
!
!   - `cosd` and `sind`, which operate on degrees and real(4) types (Fortran 23).
!   - `dcos` and `dsin`, which operate on radians and real(8) types (Fortran 77).
!
! To replace `dcosd` and `dsind`, let's convert from degrees to radians
! and then call `dcos` and `dsin`. This ensures that all computations are still
! performed in real(8).

module trigonometric_utils
  use iso_c_binding, only : c_double
  implicit none

  real(kind=c_double), parameter :: pi = 3.141592653589793_c_double

contains

  real(kind=c_double) function dcosd(degrees)
    real(kind=c_double), intent(in) :: degrees
    ! `dcos` expects the angle to be in radians, not degrees
    dcosd = dcos(degrees * pi / 180.0)
  end function dcosd

  real(kind=c_double) function dsind(degrees)
    real(kind=c_double), intent(in) :: degrees
    ! `dsin` expects the angle to be in radians, not degrees
    dsind = dsin(degrees * pi / 180.0)
  end function dsind
end module trigonometric_utils

! Computes the distance between `n` pairs of points (P1(n), P2(n)) in 
! the circunferences given by radiuses(n)
subroutine calculate_distances_improved_f(n, P1, P2, radiuses, distances) &
                                                                        bind(c)
  use iso_c_binding, only : c_double, c_int
  use trigonometric_utils, only : dcosd, dsind

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
end subroutine calculate_distances_improved_f
