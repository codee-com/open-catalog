! PWR087: Declare array dummy arguments as assumed-shape arrays to favor
!         compiler optimizations

module solution
  implicit none
contains
subroutine clamp_values(X, min_value, max_value)
  use iso_fortran_env, only: real32

  implicit none
  real(kind=real32), dimension(:), intent(inout) :: X
  real(kind=real32), intent(in) :: min_value, max_value
  
  integer :: i

  do i = 1, size(X, 1)
    if(X(i) < min_value) then
      X(i) = min_value
    else if(X(i) > max_value) then
      X(i) = max_value
    end if
  end do
end subroutine clamp_values
end module solution
