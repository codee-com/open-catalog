! PWR005: Disable default OpenMP scoping

SUBROUTINE example(result)
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: result(:)
  INTEGER :: i

  ! Default data scoping is used which may not be correct
  !$omp parallel do
  DO i = 1, size(result, 1)
    result(i) = i
  END DO
END SUBROUTINE example
