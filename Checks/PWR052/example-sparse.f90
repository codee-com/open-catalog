! PWR052: Consider applying multithreading parallelism to sparse reduction loop

SUBROUTINE example(A, nodes, n)
  INTEGER, INTENT(IN) :: n
  INTEGER, DIMENSION(1:n), INTENT(IN) :: nodes
  REAL(KIND=8), DIMENSION(1:n), INTENT(OUT) :: A
  INTEGER :: nel

  DO nel = 1, n
    A(nodes(nel)) = A(nodes(nel)) + (nel * 1)
  END DO
END SUBROUTINE example
