! PWR035: Avoid non-consecutive array access to improve performance

pure subroutine reverseArray(array)
  implicit none
  integer, intent(inout) :: array(:)
  integer :: i, length, temp

  length = size(array)

  do i = 1, length / 2
    temp = array(i)
    array(i) = array(length - i + 1)
    array(length - i + 1) = temp
  end do
end subroutine reverseArray
