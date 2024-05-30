! PWR073: Transform common block into a module for better data encapsulation

module linear_transformation_improved
  use iso_c_binding, only : c_double
  implicit none

  private
  real(kind=c_double) :: a, b

  public :: set_coefficients_improved_f, transform_element_improved_f

contains

  subroutine set_coefficients_improved_f(coeff_a, coeff_b)
    implicit none
    real(kind=c_double), intent(in) :: coeff_a, coeff_b

    a = coeff_a
    b = coeff_b
  end subroutine set_coefficients_improved_f

  function transform_element_improved_f(xi)
    implicit none
    real(kind=c_double) :: transform_element_improved_f
    real(kind=c_double), intent(in) :: xi
    
    transform_element_improved_f = a * xi + b
  end function transform_element_improved_f
end module linear_transformation_improved

subroutine transform_vector_improved_f(n, X, result, coeff_a, coeff_b) bind(c)
  use iso_c_binding, only : c_double, c_int
  use linear_transformation_improved, only : set_coefficients_improved_f, &
                                             transform_element_improved_f

  implicit none

  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(n), intent(in) :: X
  real(kind=c_double), dimension(n), intent(out) :: result
  real(kind=c_double), intent(in), value :: coeff_a, coeff_b

  integer(kind=c_int) :: i

  call set_coefficients_improved_f(coeff_a, coeff_b)

  do i = 1, n
    result(i) = transform_element_improved_f(X(i))
  end do
end subroutine transform_vector_improved_f
