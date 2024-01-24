! PWR051: Consider applying multithreading parallelism to scalar reduction loop

SUBROUTINE example(A, n, sum)
  INTEGER, INTENT(IN) :: n
  DOUBLE PRECISION, DIMENSION(1:n), INTENT(IN) :: A
  DOUBLE PRECISION, INTENT(OUT) :: sum
  INTEGER :: i

  sum = 0
  DO i = 1, n
    sum = sum + A(i)
  END DO
END SUBROUTINE example
