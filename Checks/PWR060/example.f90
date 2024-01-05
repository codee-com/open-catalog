! PWR060 : consider loop fission to separate gather memory access pattern

SUBROUTINE example(a, D, X, Y, index, n)
  IMPLICIT NONE
  REAL(KIND=8), INTENT(IN) :: a
  REAL(KIND=8), DIMENSION(n), INTENT(OUT) :: D
  REAL(KIND=8), DIMENSION(n), INTENT(IN) :: X, Y
  INTEGER, DIMENSION(n), INTENT(IN) :: index
  INTEGER, INTENT(in) :: n
  INTEGER :: i

  DO i = 1, n
    D(i) = a * X(index(i)) + Y(i)
  END DO
END SUBROUTINE example
