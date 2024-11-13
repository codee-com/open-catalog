! PWR068: Encapsulate procedures within modules to avoid the risks of calling
!         implicit interfaces

module mod_factorial
  implicit none
contains
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
end module mod_factorial
