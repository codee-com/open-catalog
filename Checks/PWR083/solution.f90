! PWR083: Match dummy and actual argument types in procedure calls

! NOT-PWR068: External procedure used to demonstrate how compilers can't catch
!             the type mismatch
program correct_call
  use iso_fortran_env, only: real32
  implicit none

  external :: updateSimulationTime
  integer :: numberSteps
  real(kind=real32) :: prevTime, newTime

  numberSteps = 10
  prevTime = 0.0

  call updateSimulationTime(numberSteps, prevTime, newTime)
  print *, "New time = ", newTime
end program correct_call
