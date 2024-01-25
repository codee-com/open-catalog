! PWR054: consider applying vectorization to scalar reduction loop

SUBROUTINE example(A, n, sum)
  INTEGER, INTENT(IN) :: n
  REAL(KIND=8), DIMENSION(1:n), INTENT(IN) :: A
  REAL(KIND=8), INTENT(OUT) :: sum
  INTEGER :: i

  sum = 0
  DO i = 1, n
    sum = sum + A(i)
  END DO
END SUBROUTINE example
