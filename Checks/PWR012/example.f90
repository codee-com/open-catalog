! PWR012: Pass only required fields from derived type as parameters

PROGRAM example
  TYPE data
    INTEGER :: a(10)
    INTEGER :: b(10)
  END TYPE data
CONTAINS
  SUBROUTINE foo(d)
    TYPE(data), INTENT(IN) :: d
    INTEGER :: i, sum
    DO i = 1, 10
      sum = sum + d%a(i)
    END DO
  END SUBROUTINE foo
  SUBROUTINE bar()
    TYPE(data) :: d
    CALL foo(d)
  END SUBROUTINE bar
END PROGRAM example
