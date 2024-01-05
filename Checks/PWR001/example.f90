! PWR001: Declare global variables as function parameters

MODULE globalsMod
  REAL :: global_a
END MODULE globalsMod

REAL FUNCTION example()
  USE globalsMod
  IMPLICIT NONE
  example = global_a
END FUNCTION example
