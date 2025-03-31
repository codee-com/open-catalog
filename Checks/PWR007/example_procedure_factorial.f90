! PWR007: Disable the implicit declaration of variables and procedures

! NOT-PWR068: The subroutine doesn't provide an explicit interface on purpose
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
