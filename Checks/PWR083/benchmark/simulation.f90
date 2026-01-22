! PWR083: Match the types of dummy and actual arguments in procedure calls

! NOT-PWR070: Explicit-shape arrays used for C-interoperability
pure subroutine update_simulation_state(numberSteps, n, state)
  use iso_c_binding, only: c_double, c_int
  implicit none

  integer, intent(in) :: numberSteps
  integer(kind=c_int), intent(in) :: n
  real(kind=c_double), dimension(n), intent(inout) :: state

  integer(kind=c_int) :: i

  do i = 1, n
    state(i) = state(i) + numberSteps * 9.81_c_double
  end do
end subroutine update_simulation_state
