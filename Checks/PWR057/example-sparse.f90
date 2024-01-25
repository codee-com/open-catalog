! PWR057: consider applying offloading parallelism to sparse reduction loop

subroutine example(A, nodes, n)
  implicit none
  integer, intent(in) :: n
  integer, dimension(1:n), intent(in) :: nodes
  real(kind=8), dimension(1:n), intent(out) :: A
  integer :: nel

  do nel = 1, n
    A(nodes(nel)) = A(nodes(nel)) + (nel * 1)
  end do
end subroutine example
