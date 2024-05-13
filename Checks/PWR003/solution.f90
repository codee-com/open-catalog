! PWR003: Explicitly declare pure functions

module gravitySolution
  use iso_c_binding, only : c_int
  implicit none
  integer(kind=c_int), parameter :: mercury = 0, venus = 1, earth = 2, &
                                    mars = 3, jupiter = 4, saturn = 5, &
                                    uranus = 6, neptune = 7
contains
  pure function gravityOf(planet)
    use iso_c_binding, only: c_double

    real(kind=c_double) :: gravityOf
    integer(kind=c_int), intent(in) :: planet

    select case (planet) 
      case (mercury)
        gravityOf = 3.7
      case (venus)
        gravityOf = 8.9
      case (earth)
        gravityOf = 9.8
      case (mars)
        gravityOf = 3.7
      case (jupiter)
        gravityOf = 23.1
      case (saturn)
        gravityOf = 9.0
      case (uranus)
        gravityOf = 8.7
      case (neptune)
        gravityOf = 11.0
      case default
        gravityOf = 0.0
    end select
  end function gravityOf
end module gravitySolution

! Computes the weight of each object in a vector
pure subroutine solution_f(n, M, W) bind(c)
  use iso_c_binding, only : c_int, c_double
  use gravitySolution, only : mars, gravityOf

  implicit none
  integer(kind=c_int) :: i
  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(1:n), intent(in) :: M
  real(kind=c_double), dimension(1:n), intent(out) :: W

  do i = 1, n
    W(i) = M(i) * gravityOf(mars)
  end do
end subroutine solution_f
