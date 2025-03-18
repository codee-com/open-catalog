! PWR008: Declare the intent for each procedure argument

subroutine example(a, b)
  implicit none
  integer :: a
  integer :: b
  a = 5
  b = a * 2 ! this is allowed because no intent has been declared
end subroutine example
