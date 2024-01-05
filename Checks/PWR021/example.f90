! PWR021: temporary computation can be extracted to a vectorizable loop

INTEGER FUNCTION expensive_computation(c, i)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: i, c(1000)

  expensive_computation = c(i) * 2
END FUNCTION expensive_computation

SUBROUTINE example()
  IMPLICIT NONE
  INTEGER :: a(1000), c(1000), i, t, expensive_computation

  DO i = 1, 1000
    t = expensive_computation(c, i)
    a(c(i)) = a(c(i)) + t
  END DO
END SUBROUTINE example
