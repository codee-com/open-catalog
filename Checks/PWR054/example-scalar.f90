! PWR054: consider applying vectorization to scalar reduction loop

pure function example(A) result(sum)
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32), intent(in) :: A(:)
  real(kind=real32) :: sum
  integer :: i

  sum = 0.0
  do i = 1, size(A, 1)
    sum = sum + A(i)
  end do
end function example
