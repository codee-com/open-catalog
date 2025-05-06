! PWR075: Avoid using compiler-specific Fortran extensions

program main
  implicit none

  type :: foo_t
    integer :: x
  end type

  type(foo_t) :: foo(2)
  integer :: bar(2)

  data foo%x /1, 0/
  data bar /1, 0/

  print *, foo
  print *, bar
end
