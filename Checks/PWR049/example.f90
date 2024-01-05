! PWR049: Move iterator-dependent condition outside of the loop

SUBROUTINE example(n)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  INTEGER :: i, j, a(n, n), b(n, n)

  DO i = 1, n
    DO j = 1, n
      IF (j .eq. 1) THEN
        a(j, i) = 0
      ELSE
        a(j, i) = a(j - 1, i) + b(j, i)
      END IF
    END DO
  END DO
END SUBROUTINE example
