! PWR016: Use separate arrays instead of an Array-of-Structs

PROGRAM main
  TYPE point
    INTEGER :: x
    INTEGER :: y
    INTEGER :: z
  END TYPE point
CONTAINS
  SUBROUTINE foo()
    TYPE(point) :: points(100)
    INTEGER :: i
    DO i = 1, 100
      points(i)%x = 1
      points(i)%y = 1
    END DO
  END SUBROUTINE foo
END PROGRAM main
