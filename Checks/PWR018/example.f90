! PWR018: Call to recursive function within a loop may inhibit vectorization

RECURSIVE FUNCTION fibonacci(n) RESULT(fibo)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  INTEGER :: fibo
  IF (n <= 2) THEN
    fibo = 1
  ELSE
    fibo = fibonacci(n - 1) + fibonacci(n - 2)
  END IF
END FUNCTION fibonacci

SUBROUTINE example(times)
  IMPLICIT NONE
  INTEGER :: sum, fibonacci
  INTEGER :: i, times
  DO i = 1, times
    sum = sum + fibonacci(i)
  END DO
END SUBROUTINE example
