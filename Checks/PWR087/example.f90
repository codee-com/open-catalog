! PWR087: Declare array dummy arguments as assumed-shape arrays to favor
!         compiler optimizations

module example
  implicit none
contains
subroutine clamp_values(n, X, min_value, max_value)
  use iso_fortran_env, only: real32

  implicit none
  integer, intent(in) :: n
  real(kind=real32), dimension(n), intent(inout) :: X
  real(kind=real32), intent(in) :: min_value, max_value
  
  integer :: i

  do i = 1, n
    if(X(i) < min_value) then
      X(i) = min_value
    else if(X(i) > max_value) then
      X(i) = max_value
    end if
  end do
end subroutine clamp_values
end module example
