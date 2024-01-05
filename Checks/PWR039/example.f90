! PWR039: Consider loop interchange to improve the locality of reference and
! enable vectorization

SUBROUTINE example(a, n)
  INTEGER, INTENT(IN) :: n
  REAL, DIMENSION(1:n, 1:n), INTENT(INOUT) :: a
  INTEGER :: i, j

  DO i = 1, n
    DO j = 1, n
      a(i, j) = 0
    END DO
  END DO
END SUBROUTINE example
