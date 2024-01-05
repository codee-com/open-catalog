! PWR013: Avoid copying unused variables to the GPU

SUBROUTINE example(a, b, c)
  INTEGER, INTENT(INOUT) :: a(100), b(100), c(100)
  INTEGER :: i
  !$omp target teams distribute parallel do schedule(auto) shared(A, B) &
  !$omp map(to: a(1:100), b(1:100)) map(tofrom: c(1:100))
  DO i = 1, 100
    c(i) = c(i) + a(i)
  END DO
END SUBROUTINE example
