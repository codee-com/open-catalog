! PWR034: Avoid strided array access to improve performance

SUBROUTINE example()
  IMPLICIT NONE
  INTEGER :: a(100), i

  DO i = 1, 100, 2
    a(i) = 0
  END DO
END SUBROUTINE example
