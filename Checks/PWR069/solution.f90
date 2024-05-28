! PWR069: Use the keyword only to explicitly state what to import from a module

module Areas
  implicit none
  public :: constant_pi, area_circle, area_rectangle, area_square

  real, parameter :: constant_pi = 3.1415

contains

  real function area_circle(radius)
    real, intent(in) :: radius
    area_circle = constant_pi * radius * radius
  end function area_circle

  real function area_rectangle(length, width)
    real, intent(in) :: length, width
    area_rectangle = length * width
  end function area_rectangle

  real function area_square(side)
    real, intent(in) :: side
    area_square = side * side
  end function area_square
end module Areas

program test_with_only
  use Areas, only : constant_pi, area_circle
  implicit none

  real :: radius = 2.0

  print *, 'Area of circle: ', area_circle(radius)
  print *, 'Value of pi used: ', constant_pi
end program test_with_only
