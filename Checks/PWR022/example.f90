! PWR022: move invariant conditional out of the loop to facilitate vectorization

SUBROUTINE example()
  IMPLICIT NONE
  INTEGER :: n
  INTEGER :: a(1000), i, total

  n = 100

  DO i = 1, n
    IF (n .lt. 10) THEN
      total = total + 1
    END IF
    a(i) = total
  END DO
END SUBROUTINE example
