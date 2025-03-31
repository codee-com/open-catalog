! PWR007: Disable the implicit declaration of variables and procedures

module mod_factorial
  implicit none
contains
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
end module mod_factorial
