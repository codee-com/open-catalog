! PWR019: Consider interchanging loops to favor vectorization by maximizing
! inner loop's trip count

subroutine example()
  implicit none
  integer :: a(200, 10), i, j

  do i = 1, 200
    do j = 1, 10
      a(i, j) = 0
    end do
  end do
end subroutine example
