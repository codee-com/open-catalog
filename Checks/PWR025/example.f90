! PWR025: Consider annotating pure function with OpenMP `declare simd`

integer function foo(a)
  integer, intent(in) :: a
  foo = 2 * a
end function foo

subroutine example(A)
  integer, external :: foo
  integer, intent(out) :: A(:)
  integer :: i

  do i = 1, size(A, 1)
    A(i) = foo(i)
  end do
end subroutine example
