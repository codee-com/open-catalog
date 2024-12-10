! PWR027: Annotate function for OpenACC offload

subroutine example(A)
  implicit none
  integer, intent(out) :: A(:)
  integer :: i

  !$acc kernels
  do i = 1, size(A, 1)
    A(i) = foo(i)
  end do
  !$acc end kernels

contains

  pure integer function foo(a)
    implicit none
    integer, intent(in) :: a
    foo = 2 * a
  end function foo
end subroutine example
