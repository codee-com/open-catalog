# PWR069: Use the keyword only to explicitly state what to import from a module

### Issue

Importing modules without specifying elements with the `only` keyword can
obscure their origin, hurting the readability of the code, and even lead to
name conflicts.

### Actions

Enhance code clarity by leveraging the `only` keyword in `use` statements,
clearly specifying which parts of a module are needed.

### Relevance

The default behavior of the `use` statement is to make all public parts of a
module accessible to the program. This makes tracing the origin of each element
more difficult, and can even cause name conflicts, especially in large
codebases. These issues impact both the readability and maintainability of the
code. In contrast, the `only` keyword restricts the visibility to explicitly
mentioned elements, effectively addressing these problems.

### Code examples

First, let's define a module `Areas` that contains logic for calculating the
areas of various shapes:

```f90
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
```

The first version of the program omits the `only` keyword, making all `public`
elements of the module available:

```f90
! example.f90
module Areas
  ...
end module Areas

program test_without_only
  use Areas
  implicit none

  real :: radius = 2.0

  print *, 'Area of circle: ', area_circle(radius)
  print *, 'Value of pi used: ', constant_pi
end program test_without_only
```

Using the `only` keyword is as simple as listing the elements that are actually
needed from the module:

```f90
! solution.f90
module Areas
  ...
end module Areas

program test_with_only
  use Areas, only : constant_pi, area_circle
  implicit none

  real :: radius = 2.0

  print *, 'Area of circle: ', area_circle(radius)
  print *, 'Value of pi used: ', constant_pi
end program test_with_only
```

This approach makes it immediately clear to the reader that `constant_pi` and
`area_circle` are being provided by the `Areas` module.

>**Note:**  
>If you encounter name conflicts between different modules even when using the
>`only` keyword, you can easily rename the conflicting elements to avoid the
>issue:
>
>```f90
>use Mod1, only : element => element_from_Mod1
>use Mod2, only : element => element_from_Mod2
>```

### Related resources

- [PWR069 source code examples](../PWR069/)

### References

- ["Why should I use "use, only" in
Fortran"](https://stackoverflow.com/questions/51686745/why-should-i-use-use-only-in-fortran),
Stack Overflow Community. [last checked May 2024]

- ["USE ONLY
Statement"](https://www4.cs.fau.de/Lehre/SS97/V_PPS/fortran/HTMLNotesnode156.html#UseStatement2),
Adam Marshall, University of Liverpool. [last checked May 2024]

- ["Why I can access private module variable in this program? - Fortran
Discourse"](https://fortran-lang.discourse.group/t/why-i-can-access-private-module-variable-in-this-program/5092),
Fortran Community. [last checked May 2024]

- ["Fortran code
modernization"](https://www.ugent.be/hpc/en/training/2018/modern_fortran_materials/modernfortran2018.pdf),
Reinhold Bader. [last checked May 2024]
