! PWR075: Avoid using compiler-specific Fortran extensions

program main
  implicit none

  type :: foo_t
    integer :: x
  end type

  type(foo_t) :: foo(2)
  integer :: bar(2)

  data foo%x /1/
  data bar /1/

  print *, foo
  print *, bar
end
