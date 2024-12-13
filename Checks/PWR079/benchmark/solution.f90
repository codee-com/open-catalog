! PWR079: Avoid undefined behavior due to uninitialized variables

! NOT-PWR070: Explicit-shape arrays used for C-interoperability
pure function sum_array_improved_f(n, array) bind(c)
  use iso_c_binding, only: c_double, c_int

  implicit none
  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(n), intent(in) :: array
  real(kind=c_double) :: sum_array_improved_f

  integer(kind=c_int) :: i
  real(kind=c_double) :: sum

  sum = 0

  do i = 1, n
    sum = sum + array(i)
  end do

  sum_array_improved_f = sum
end function sum_array_improved_f
