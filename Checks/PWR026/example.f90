! PWR026: Annotate function for OpenMP offload

pure integer function foo(a)
  implicit none
  integer, intent(in) :: a
  foo = 2 * a
end function foo

subroutine example(A)
  implicit none
  integer, external :: foo
  integer, intent(out) :: A(:)
  integer :: i

  !$omp target teams distribute parallel do default(none) private(i) shared(A)
  do i = 1, size(A, 1)
    A(i) = foo(i)
  end do
end subroutine example
