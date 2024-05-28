! PWR068: Encapsulate external procedures within modules to avoid the risks of
!         calling implicit interfaces

subroutine factorial(number, result)
  implicit none
  integer, intent(in) :: number
  integer, intent(out) :: result
  integer :: i

  result = 1
  do i = 1, number
    result = result * i
  end do
end subroutine factorial
