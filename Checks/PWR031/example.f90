! PWR031: Replace pow by multiplication, division and/or square root

subroutine example(a, x)
  use iso_fortran_env, only : real32, int32
  implicit none
  ! dummy args
  real(kind=real32), intent(out) :: a(:)
  real(kind=real32), intent(in) :: x
  ! local vars
  integer(kind=int32) :: i
  !
  do i = 1_int32, 10_int32
    a(i) = x ** 1.5_real32
  end do
end subroutine example
