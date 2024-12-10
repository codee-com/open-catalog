! PWR069: Use the keyword only to explicitly state what to import from a module

module Areas
  use iso_fortran_env, only: real32
  implicit none
  public :: constant_pi, area_circle, area_rectangle, area_square

  real(kind=real32), parameter :: constant_pi = 3.1415

contains

  pure real(kind=real32) function area_circle(radius)
    real(kind=real32), intent(in) :: radius
    area_circle = constant_pi * radius * radius
  end function area_circle

  pure real(kind=real32) function area_rectangle(length, width)
    real(kind=real32), intent(in) :: length, width
    area_rectangle = length * width
  end function area_rectangle

  pure real(kind=real32) function area_square(side)
    real(kind=real32), intent(in) :: side
    area_square = side * side
  end function area_square
end module Areas

program test_with_only
  use Areas, only : constant_pi, area_circle
  use iso_fortran_env, only: real32
  implicit none

  real(kind=real32) :: radius = 2.0

  print *, 'Area of circle: ', area_circle(radius)
  print *, 'Value of pi used: ', constant_pi
end program test_with_only
