! PWR082: Remove unused variables

pure function sumArray(array) result(sum)
  implicit none
  integer, intent(in) :: array(:)
  integer :: sum, i

  sum = 0

  do i = 1, size(array)
    sum = sum + array(i)
  end do
end function sumArray
