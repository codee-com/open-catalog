! PWR020: consider loop fission to enable vectorization

SUBROUTINE example()
  INTEGER :: a(100), b(100), c(100), i

  DO i = 1, 100
    a(i) = i
    b(i) = i
    c(i) = i
  END DO

  DO i = 1, 100
    a(i) = a(i) + i
    b(c(i)) = b(c(i)) + i
  END DO
END SUBROUTINE example
