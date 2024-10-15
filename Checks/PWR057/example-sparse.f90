! PWR057: consider applying offloading parallelism to sparse reduction loop

subroutine example(A, nodes)
  implicit none
  real(kind=8), intent(inout) :: A(:)
  integer, intent(in) :: nodes(:)
  integer :: nel

  do nel = 1, size(nodes, 1)
    A(nodes(nel)) = A(nodes(nel)) + (nel * 1)
  end do
end subroutine example
