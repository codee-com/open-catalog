! PWR089: Remove unexpected arguments from procedure calls

subroutine logField(name, step, value)
  use iso_fortran_env, only: real32
  implicit none

  character(*), intent(in) :: name
  integer, intent(in) :: step
  real(kind=real32), intent(in) :: value

  write(*, *) trim(name), " step=", step, " value=", value
end subroutine logField
