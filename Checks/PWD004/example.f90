! PWD004: Out-of-memory-bounds array access

subroutine example()
  implicit none
  integer :: A(100)
  integer :: i

  do i = 1, size(A, 1)
    A(i + 1) = 1
  end do
end subroutine example
