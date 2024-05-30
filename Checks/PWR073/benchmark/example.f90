! PWR073: Transform common block into a module for better data encapsulation

module linear_transformation
  implicit none
contains
  function transform_element_f(xi)
    use iso_c_binding, only : c_double

    implicit none
    real(kind=c_double) :: transform_element_f
    real(kind=c_double), intent(in) :: xi

    real(kind=c_double) :: a, b
    common /coefficients/ a, b
    
    transform_element_f = a * xi + b
  end function transform_element_f
end module linear_transformation

subroutine transform_vector_f(n, X, result, coeff_a, coeff_b) bind(c)
  use iso_c_binding, only : c_double, c_int
  use linear_transformation, only : transform_element_f

  implicit none

  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(n), intent(in) :: X
  real(kind=c_double), dimension(n), intent(out) :: result
  real(kind=c_double), intent(in), value :: coeff_a, coeff_b

  integer(kind=c_int) :: i
  real(kind=c_double) :: a, b
  common /coefficients/ a, b

  a = coeff_a
  b = coeff_b

  do i = 1, n
    result(i) = transform_element_f(X(i))
  end do
end subroutine transform_vector_f
