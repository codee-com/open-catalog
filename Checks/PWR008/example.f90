! PWR008: Declare the intent for each procedure parameter

SUBROUTINE example(a, b)
  IMPLICIT NONE
  INTEGER :: a
  INTEGER :: b

  a = 5
  b = a * 2 ! This is allowed because no intent has been declared
END SUBROUTINE example
