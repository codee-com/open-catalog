! PWR083: Match dummy and actual argument types in procedure calls

pure subroutine updateSimulationTime(numberSteps, prevTime, newTime)
  use iso_fortran_env, only: real32
  implicit none

  integer, intent(in) :: numberSteps
  real(kind=real32), intent(in) :: prevTime
  real(kind=real32), intent(out) :: newTime

  ! Each step is 0.1 seconds
  newTime = prevTime + numberSteps * 0.1_real32
end subroutine updateSimulationTime
