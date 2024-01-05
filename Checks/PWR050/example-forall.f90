! PWR050: Consider applying multithreading parallelism to forall loop

SUBROUTINE example(D, X, Y, n, a)
  INTEGER, INTENT(IN) :: n
  DOUBLE PRECISION, INTENT(IN) :: a
  DOUBLE PRECISION, DIMENSION(1:n), INTENT(IN) :: X, Y
  DOUBLE PRECISION, DIMENSION(1:n), INTENT(OUT) :: D
  INTEGER :: i

  DO i = 1, n
    D(i) = a * X(i) + Y(i)
  END DO
END SUBROUTINE example
