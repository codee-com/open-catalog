! PWD011: Missing OpenMP lastprivate clause

real function example(A, B, C)
  implicit none
  real, intent(in) :: A(:), B(:)
  real, intent(inout) :: C(:)
  real :: liveOut
  integer :: i

  !$omp parallel do private(i, liveOut) shared(A, B, C)
  do i = 1, size(C, 1)
    liveOut = A(i) * B(i)
    C(i) = C(i) + liveOut
  end do

  liveOut = liveOut + 5
  example = liveOut
end function example
