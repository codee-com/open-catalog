! PWR027: Annotate function for OpenACC offload

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

  !$acc kernels
  do i = 1, size(A, 1)
    A(i) = foo(i)
  end do
  !$acc end kernels
end subroutine example
