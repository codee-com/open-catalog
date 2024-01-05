! PWR006: Avoid privatization of read-only variables

PROGRAM example
  IMPLICIT NONE
  INTEGER :: i
  INTEGER :: sum(19)
  INTEGER :: a(10), b(10)

  !$omp parallel do firstprivate(i, a, b) shared(sum)
  DO i = 1, 10
    sum(i) = A(i) + B(i);
  END DO
END PROGRAM example
