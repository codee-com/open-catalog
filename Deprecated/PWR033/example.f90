! PWR033: Move invariant conditional out of the loop to avoid redundant
! computations

subroutine example(addTwo)
  implicit none
  logical, intent(in) :: addTwo
  integer :: i, sum

  do i = 1, 1000
    if (addTwo .eqv. .true.) then
      sum = sum + 2
    else
      sum = sum + 1
    end if
  end do
end subroutine
