! PWR088: Add missing arguments to procedure calls

! NOT-PWR068: External procedure used to demonstrate how compilers can't catch
!             the argument count mismatch
program call_with_missing_arguments
  use iso_fortran_env, only: real32
  implicit none

  external :: updateSimulationTime
  integer :: numberSteps
  real(kind=real32) :: prevTime, newTime

  numberSteps = 10
  prevTime = 0.0

  call updateSimulationTime(numberSteps, prevTime, newTime)
  print *, "New time = ", newTime
end program call_with_missing_arguments
