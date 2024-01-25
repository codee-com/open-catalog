! PWR055: consider applying offloading parallelism to forall loop

SUBROUTINE example(D, X, Y, n, a)
  INTEGER, INTENT(IN) :: n
  REAL(KIND=8), INTENT(IN) :: a
  REAL(KIND=8), DIMENSION(1:n), INTENT(IN) :: X, Y
  REAL(KIND=8), DIMENSION(1:n), INTENT(OUT) :: D
  INTEGER :: i

  DO i = 1, n
    D(i) = a * X(i) + Y(i)
  END DO
END SUBROUTINE example
