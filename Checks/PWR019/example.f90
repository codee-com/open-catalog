! PWR019: Consider interchanging loops to favor vectorization by maximizing
! inner loop's trip count

SUBROUTINE example()
  INTEGER :: a(200, 10), i, j
  DO i = 1, 200
    DO j = 1, 10
      a(j, i) = 0
    END DO
  END DO
END SUBROUTINE example
