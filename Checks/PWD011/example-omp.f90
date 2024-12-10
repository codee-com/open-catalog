! PWD011: Missing OpenMP lastprivate clause

real function example(A, B, C)
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32), intent(in) :: A(:), B(:)
  real(kind=real32), intent(inout) :: C(:)
  real(kind=real32) :: liveOut
  integer :: i

  !$omp parallel do private(i, liveOut) shared(A, B, C)
  do i = 1, size(C, 1)
    liveOut = A(i) * B(i)
    C(i) = C(i) + liveOut
  end do

  liveOut = liveOut + 5
  example = liveOut
end function example
