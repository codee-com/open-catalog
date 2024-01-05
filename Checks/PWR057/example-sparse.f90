! PWR057: consider applying offloading parallelism to sparse reduction loop

SUBROUTINE example(A, nodes, n)
  INTEGER, INTENT(IN) :: n
  INTEGER, DIMENSION(1:n), INTENT(IN) :: nodes
  DOUBLE PRECISION, DIMENSION(1:n), INTENT(OUT) :: A
  INTEGER :: nel

  DO nel = 1, n
    A(nodes(nel)) = A(nodes(nel)) + (nel * 1)
  END DO
END SUBROUTINE example
