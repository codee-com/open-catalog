! PWR088: Add missing arguments to procedure calls

pure subroutine updateSimulationTime(numberSteps, dt, prevTime, newTime)
  use iso_fortran_env, only: real32
  implicit none

  integer, intent(in) :: numberSteps
  real(kind=real32), intent(in) :: dt
  real(kind=real32), intent(in) :: prevTime
  real(kind=real32), intent(out) :: newTime

  newTime = prevTime + numberSteps * dt
end subroutine updateSimulationTime
