! PWR080: Conditionally initialized variables can lead to undefined behavior

! NOT-PWR070: Explicit-shape arrays used for C-interoperability
pure function transform_and_sum_improved_f(n, array, option) bind(c)
  use iso_c_binding, only: c_double, c_int
  implicit none

  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(n), intent(in) :: array
  integer(kind=c_int), intent(in), value :: option
  real(kind=c_double) :: transform_and_sum_improved_f

  real(kind=c_double) :: sum
  real(kind=c_double) :: factor
  integer(kind=c_int) :: i

  sum = 0.0
  ! Identity transformation by default
  factor = 1.0

  if (option == 1) then
    factor = 0.5
  else if (option == 2) then
    factor = 2.0
  end if

  do i = 1, n
    sum = sum + array(i) * factor
  end do

  transform_and_sum_improved_f = sum
end function transform_and_sum_improved_f
