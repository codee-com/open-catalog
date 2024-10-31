! PWR072: Split the variable initialization from the declaration to prevent the
!         implicit 'save' behavior

module utils_improved
  implicit none
contains
function compute_next_moving_average_improved_f(xi) result(new_average)
  use iso_c_binding, only : c_double, c_int

  implicit none
  real(kind=c_double) :: new_average
  real(kind=c_double), intent(in) :: xi

  integer(kind=c_int), save :: num_processed_elements = 0
  real(kind=c_double), save :: moving_average = 0.0
  
  real(kind=c_double) :: alpha

  num_processed_elements = num_processed_elements + 1
  alpha = 1.0 / num_processed_elements
  moving_average = (1.0 - alpha) * moving_average + alpha * xi

  new_average = moving_average
end function compute_next_moving_average_improved_f
end module utils_improved

function compute_final_moving_average_improved_f(n, X) bind(c)
  use iso_c_binding, only : c_double, c_int
  use utils_improved, only : compute_next_moving_average_improved_f

  implicit none

  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double) :: compute_final_moving_average_improved_f
  real(kind=c_double), dimension(n), intent(in) :: X

  integer(kind=c_int) :: i
  real(kind=c_double) :: moving_average

  do i = 1, n
    moving_average = compute_next_moving_average_improved_f(X(i))
  end do

  compute_final_moving_average_improved_f = moving_average
end function compute_final_moving_average_improved_f
