! PWR025: Consider annotating pure function with OpenMP `declare simd`

subroutine example(A)
  implicit none
  integer, intent(out) :: A(:)
  integer :: i

  do i = 1, size(A, 1)
    A(i) = foo(i)
  end do

contains

  pure integer function foo(a)
    implicit none
    integer, intent(in) :: a
    foo = 2 * a
  end function foo
end subroutine example
