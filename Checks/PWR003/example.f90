! PWR003: Explicitly declare pure functions

module gravityExample
  use iso_c_binding, only : c_int
  implicit none
  integer(kind=c_int), parameter :: mercury = 0, venus = 1, earth = 2, &
                                    mars = 3, jupiter = 4, saturn = 5, &
                                    uranus = 6, neptune = 7
contains
  function gravityOf(planet)
    use iso_c_binding, only: c_double

    real(kind=c_double) :: gravityOf
    integer(kind=c_int), intent(in) :: planet

    if (planet .eq. mercury) then
      gravityOf = 3.7
    else if (planet .eq. venus) then
      gravityOf = 8.9
    else if (planet .eq. earth) then
      gravityOf = 9.8
    else if (planet .eq. mars) then
      gravityOf = 3.7
    else if (planet .eq. jupiter) then
      gravityOf = 23.1
    else if (planet .eq. saturn) then
      gravityOf = 9.0
    else if (planet .eq. uranus) then
      gravityOf = 8.7
    else if (planet .eq. neptune) then
      gravityOf = 11.0
    else
      gravityOf = 0.0
    endif
  end function gravityOf
end module gravityExample

! Computes the weight of each object in a vector
! NOT-PWR070: Explicit-shape arrays used for C-interoperability
subroutine example_f(n, M, W) bind(c)
  use iso_c_binding, only : c_int, c_double
  use gravityExample, only : mars, gravityOf

  implicit none
  integer(kind=c_int) :: i
  integer(kind=c_int), intent(in), value :: n
  real(kind=c_double), dimension(1:n), intent(in) :: M
  real(kind=c_double), dimension(1:n), intent(out) :: W

  do i = 1, n
    W(i) = M(i) * gravityOf(mars)
  end do
end subroutine example_f
