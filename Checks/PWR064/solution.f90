! PWR064: Precision loss in floating-point constant

program test_literal_with_suffix
  implicit none
  integer, parameter :: dp = selected_real_kind(15, 307)
  real(kind=dp), parameter :: e = 2.718281828459045_dp
  print *, e
end program test_literal_with_suffix
