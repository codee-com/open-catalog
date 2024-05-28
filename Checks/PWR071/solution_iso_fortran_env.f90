! PWR071: Prefer real(kind=kind_value) for declaring consistent floating types

module my_kinds
  use iso_fortran_env, only: real32, real64
  implicit none
  public

  ! single precision (sp)
  integer, parameter :: sp = real32
  ! double precision (dp)
  integer, parameter :: dp = real64
end module my_kinds

program test_iso_fortran_env
  use my_kinds, only: sp, dp
  implicit none

  real(kind=sp) :: single_precision_variable
  real(kind=dp) :: double_precision_variable

  ! Note the `_sp` and `_dp` suffixes, indicating the precision of the value
  single_precision_variable = 0.123456_sp
  double_precision_variable = 0.123456789012345_dp
  
  print *, 'Single precision variable:', single_precision_variable
  print *, 'Double precision variable:', double_precision_variable
end program test_iso_fortran_env
