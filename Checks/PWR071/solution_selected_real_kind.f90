! PWR071: Prefer real(kind=kind_value) for declaring consistent floating types

module my_kinds
  implicit none
  public

  ! single precision (sp): 6 significant digits, 37 exponent range
  integer, parameter :: sp = selected_real_kind(6, 37)
  ! double precision (dp): 15 significant digits, 307 exponent range
  integer, parameter :: dp = selected_real_kind(15, 307)
end module my_kinds

program test_selected_real_kind
  use my_kinds, only: sp, dp
  implicit none

  real(kind=sp) :: single_precision_variable
  real(kind=dp) :: double_precision_variable

  ! Note the `_sp` and `_dp` suffixes, indicating the precision of the value
  single_precision_variable = 0.123456_sp
  double_precision_variable = 0.123456789012345_dp

  print *, 'Single precision variable:', single_precision_variable
  print *, 'Double precision variable:', double_precision_variable
end program test_selected_real_kind
