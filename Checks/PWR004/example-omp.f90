! PWR004: Declare OpenMP scoping for all variables

SUBROUTINE example(result)
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: result(:)
  INTEGER :: i, factor = 42

  ! No data scoping is specified
  !$omp parallel do
  DO i = 1, size(result, 1)
    result(i) = factor * i
  END DO
END SUBROUTINE example
