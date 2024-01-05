! PWR035: Avoid non-consecutive array access to improve performance

SUBROUTINE example()
  IMPLICIT NONE
  INTEGER :: a(100, 100), i, j

  DO i = 1, 100
    DO j = 1, 100
      a(j, 1) = 0
    END DO
  END DO
END SUBROUTINE example
