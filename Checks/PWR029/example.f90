! PWR029: Remove integer increment preventing performance optimization

SUBROUTINE example()
  IMPLICIT NONE
  INTEGER :: a(100), b(100), i, k

  k = 1
  DO i = 1, 100
    b(i) = a(k) + 1
    k = k + 1
  END DO
END SUBROUTINE example
