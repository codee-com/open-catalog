! PWR075: Avoid using GNU Fortran extensions

program solution
  implicit none
  character(len = *), parameter :: file  = "solution.f90"
  logical :: exists

  inquire(file=file, exist=exists)

  if(exists) then
    print *, "I exist"
  end if
end program solution
