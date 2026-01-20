! PWR083: Match dummy and actual argument types in procedure calls

! NOT-PWR068: External procedure used to demonstrate how compilers can't catch
!             the type mismatch
! NOT-PWR070: Explicit-shape arrays used for C-interoperability
subroutine update_simulation_state_by_fixed_amount_improved(n, state) bind(c)
  use iso_c_binding, only: c_double, c_int
  implicit none

  external :: update_simulation_state

  integer(kind=c_int), intent(in) :: n
  real(kind=c_double), dimension(n), intent(inout) :: state

  integer :: numberSteps

  numberSteps = 100
  call update_simulation_state(numberSteps, n, state)
end subroutine update_simulation_state_by_fixed_amount_improved
