! PWR001: Declare global variables as function parameters

module globalsMod
  implicit none
  real :: global_a
end module globalsMod

pure real function example()
  ! NOT-PWR069: The `only` is ommited to trigger PWR001
  use globalsMod
  implicit none
  example = global_a
end function
