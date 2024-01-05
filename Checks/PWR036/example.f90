! PWR036: Avoid indirect array access to improve performance

SUBROUTINE example()
  IMPLICIT NONE
  INTEGER :: a(100), b(100), i

  DO i = 1, 100
    a(b(i)) = 0
  END DO
END SUBROUTINE example
