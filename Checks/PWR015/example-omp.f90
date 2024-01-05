! PWR015: Avoid copying unnecessary array elements to the GPU

SUBROUTINE example(a, c)
  INTEGER, INTENT(INOUT) :: a(100), c(100)
  INTEGER :: i
  !$omp target teams distribute parallel do schedule(auto) shared(A, B) &
  !$omp map(to: a(0:100)) map(tofrom: c(0:100))
  DO i = 1, 50
    c(i) = c(i) + a(i)
  END DO
END SUBROUTINE example
