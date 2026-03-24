! PWR068: Call procedures through explicit interfaces, preferably as module
!         procedures

pure subroutine factorial(number, result)
  implicit none
  integer, intent(in) :: number
  integer, intent(out) :: result
  integer :: i

  result = 1
  do i = 1, number
    result = result * i
  end do
end subroutine factorial
