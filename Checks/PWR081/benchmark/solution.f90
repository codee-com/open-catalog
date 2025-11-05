! PWR081: Uninitialized output arguments can lead to undefined behavior

! NOT-PWR070: Explicit-shape arrays used for C-interoperability
pure subroutine compute_stats_improved_f(n, data, mean, stddev) bind(c)
  use iso_c_binding, only: c_double, c_int
  implicit none

  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(n), intent(in) :: data
  real(kind=c_double), intent(out) :: mean
  real(kind=c_double), intent(out) :: stddev

  integer(kind=c_int) :: i

  mean = 0.0
  stddev = 0.0

  ! We can't compute stddev from a single data point
  if (n <= 1) then
    return
  end if

  ! Also abort if there are invalid data points
  do i = 1, n
    if (data(i) < -900.0) then
      return
    end if
  end do

  mean = sum(data) / n
  stddev = sqrt(sum((data - mean)**2) / (n - 1))
end subroutine compute_stats_improved_f
