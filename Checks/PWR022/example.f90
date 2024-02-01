! PWR022: move invariant conditional out of the loop to facilitate vectorization

subroutine example_f(n, A, B, C, D)
  implicit none
  integer i, j, k
  integer, intent(in) :: n
  real :: val
  real, dimension(1:n), intent(in) :: A
  real, dimension(1:n, 1:n), intent(in) :: B
  real, dimension(1:n, 1:n, 1:n), intent(in) :: C
  real, dimension(1:n, 1:n), intent(out) :: D

  do i = 1, n
    do k = 1, n
      do j = 1, n
        if (i .eq. 1) then
          val = B(j, k)
        else
          val = D(j, k)
        end if
        D(j, k) = val + A(i) * C(j, k, i)
      end do
    end do
  end do
end subroutine example_f
