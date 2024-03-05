! PWR003: Explicitly declare pure functions

module example_module
   implicit none
   integer :: c = 1
contains
   ! Depends on external data (c)
   ! No side effects
   pure function example_pure(a) result(res)
      integer, intent(in) :: a
      integer :: res
      res = a + c
   end function example_pure

   ! Depends on external data (c)
   ! Modifies external data (c)
   function example_impure(a) result(res)
      integer, intent(in) :: a
      integer :: res
      c = c + 1
      res = a + c
   end function example_impure
end module example_module
