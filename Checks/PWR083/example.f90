! PWR083: Match the types of dummy and actual arguments in procedure calls

! NOT-PWR068: External procedure used to demonstrate how compilers can't catch
!             the type mismatch
program call_with_type_mismatch
  use iso_fortran_env, only: real32
  implicit none

  external :: updateSimulationTime
  real(kind=real32) :: numberSteps, prevTime, newTime

  numberSteps = 10
  prevTime = 0.0

  call updateSimulationTime(numberSteps, prevTime, newTime)
  print *, "New time = ", newTime
end program call_with_type_mismatch
