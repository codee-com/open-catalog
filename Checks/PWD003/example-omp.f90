! PWD003: Copy of pointer value instead of pointed-to data to an accelerator
! device

SUBROUTINE example(a, b, result, size)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: size
  INTEGER, DIMENSION(1:size), INTENT(IN) :: a, b
  INTEGER, DIMENSION(1:size), INTENT(OUT) :: result
  INTEGER :: i

  ! Array bounds should be specified
  !$omp target map(to: a, b) map(from: result)
  !$omp parallel do default(none) shared(a, b, result)
  DO i = 1, size
    result(i) = a(i) + b(i)
  END DO
  !$omp end target
END SUBROUTINE example
