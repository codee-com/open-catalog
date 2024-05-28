! PWR071: Prefer real(kind=kind_value) for declaring consistent floating types

program test_discouraged_real_types
  implicit none

  real   :: single_precision_variable
  real*8 :: double_precision_variable
  ! Or `double precision`

  ! Note the `d0` suffix, indicating the precision of the value
  single_precision_variable = 0.123456
  double_precision_variable = 0.123456789012345d0

  print *, 'Single precision variable:', single_precision_variable
  print *, 'Double precision variable:', double_precision_variable
end program test_discouraged_real_types
