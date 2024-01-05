! PWR040: Consider loop tiling to improve the locality of reference

SUBROUTINE example(a, b, n)
  INTEGER, INTENT(IN) :: n
  REAL, DIMENSION(1:n, 1:n), INTENT(INOUT) :: a, b
  INTEGER :: i, j

  DO i = 1, n
    DO j = 1, n
      a(j, i) = b(i, j)
    END DO
  END DO
END SUBROUTINE example
