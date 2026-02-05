! PWR088: Add missing arguments to procedure calls

! NOT-PWR068: External procedure used to demonstrate how compilers can't catch
!             the argument count mismatch
program correct_call
  use iso_fortran_env, only: real32
  implicit none

  external :: updateSimulationTime
  integer :: numberSteps
  real(kind=real32) :: dt, prevTime, newTime

  numberSteps = 10
  dt = 0.1
  prevTime = 0.0

  call updateSimulationTime(numberSteps, dt, prevTime, newTime)
  print *, "New time = ", newTime
end program correct_call
