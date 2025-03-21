! PWR052: Consider applying multithreading parallelism to sparse reduction loop

! NOT-PWR003: `pure` prevents OpenMP parallelization
subroutine example(A, nodes)
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32), intent(inout) :: A(:)
  integer, intent(in) :: nodes(:)
  integer :: nel

  do nel = 1, size(nodes, 1)
    A(nodes(nel)) = A(nodes(nel)) + (nel * 1)
  end do
end subroutine example
