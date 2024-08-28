! PWR075: Avoid using GNU Fortran extensions

program example
  implicit none
  character(len = *), parameter :: file  = "example.f90"

  if(access(file, " ") == 0) then
    print *, "I exist"
  end if
end program example
