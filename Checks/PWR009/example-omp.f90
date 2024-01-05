! PWR009: Use OpenMP teams to offload work to GPU

SUBROUTINE example()
  REAL, DIMENSION(100,200) :: y
  INTEGER(8) :: i, j

  !$omp target map(from: y(1:100, 1:200))
  !$omp parallel default(none) private(i, j) shared(y)
  !$omp do private(j) schedule(static)
  DO i = 1, 100
    DO j = 1, 200
      y(i, j) = 0
    END DO
  END DO
  !$omp end parallel
  !$omp end target
END SUBROUTINE example
