# PWR073: Transform common block into a module for better data encapsulation

### Issue

Common blocks create globally accessible memory, complicating data flow
analysis, and present error-prone characteristics that can lead to
dificult-to-diagnose bugs.

### Actions

To enhance code maintainability and safety, encapsulate data within modules
that offer controlled access interfaces, rather than using common blocks.

### Relevance

The `common block` construct declares a memory area that is accessible
throughout the entire program. Such global accessibility can complicate the
interpretation and maintenance of the program, as modifications in one part of
the code can unexpectedly affect other areas. This is particularly risky when
multiple components of the program depend on this shared data.

Furthermore, each procedure that accesses a common block must re-declare it,
manually ensuring that all variables are defined in the same order. This
requirement introduces significant risks for the programmer due to:

- **Lack of name consistency:** In each re-declaration, different names can be
  assigned to the same memory location, complicating logical reasoning about
  the program.

- **Lack of type safety:** It is also possible to associate different data
  types to the same memory location, potentially leading to memory corruption
  and other subtle errors.

### Code examples

Refactoring common blocks into modules can be complex and impact multiple
source files. Therefore, an incremental approach is recommended:

1. Migrate all variables from the common block to a new module, including any
   `block data` initializations. Replace the common block with the new module
   in all affected locations.

2. Gradually narrow the scope of the module by importing only the necessary
   variables where needed.

3. Consider implementing procedures to manage access to the module's variables,
   thus further enforcing data encapsulation.

Let's start with an old-style program that relies on common blocks:

```fortran
! example.f90
program test_common_block
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32) :: var1
  integer :: var2
  common /my_common/ var1, var2

  var1 = 3.14
  var2 = 20

  call printVar1
  call printVar2

contains

subroutine printVar1
  implicit none
  real(kind=real32) :: var1
  common /my_common/ var1

  print *, "Var1: ", var1
end subroutine printVar1

subroutine printVar2
  implicit none
  integer :: var2
  common /my_common/ var2

  print *, "Var2: ", var2

  ! Did you spot the bug?
  !
  ! In the common block re-definition, `var1` is missing. As a result, `var2`
  ! unintentionally references the memory location of `var1`.
  !
  ! This lack of naming consistency, as well as type safety (note that `var1`
  ! also contains real data), make this error go easily unnoticed, as the code
  ! still compiles.
end subroutine printVar2
end program test_common_block
```

The inherent unsafety of common blocks leads to this unexpected output:

```txt
$ gfortran --version
GNU Fortran (Debian 12.2.0-14) 12.2.0
$ gfortran example.f90
$ ./a.out
Var1:    3.14000010
Var2:   1078523331
```

In contrast, modules prevent such issues by design. Let's refactor the program
to encapsulate the variables inside a module (Step 1). Due to the simplicity of
this example, the `only` keyword (Step 2) is already leveraged:

```fortran
! solution_without_private.f90
module my_module
  use iso_fortran_env, only: real32
  implicit none
  public
  real(kind=real32) :: var1
  integer :: var2
end module my_module

program test_module
  use my_module, only: var1, var2
  implicit none

  var1 = 3.14
  var2 = 20

  call printVar1
  call printVar2

contains

subroutine printVar1
  use my_module, only: var1
  implicit none

  print *, "Var1: ", var1
end subroutine printVar1

subroutine printVar2
  use my_module, only: var2
  implicit none

  print *, "Var2: ", var2
end subroutine printVar2
end program test_module
```

And now the program gives the correct results:

```txt
$ gfortran solution_without_private.f90
$ ./a.out
Var1:    3.14000010
Var2:           20
```

Depending on your preferences, you might also set the module variables as
`private` and create procedures to manage access to the contained data (e.g.,
getters and setters):

```fortran
! solution.f90
module my_module
  use iso_fortran_env, only: real32
  implicit none
  private
  real(kind=real32) :: var1
  integer :: var2
  public :: getVar1, setVar1, getVar2, setVar2

contains

  real(kind=real32) function getVar1()
    getVar1 = var1
  end function getVar1

  subroutine setVar1(value)
    real(kind=real32), intent(in) :: value
    var1 = value
  end subroutine setVar1

  integer function getVar2()
    getVar2 = var2
  end function getVar2

  subroutine setVar2(value)
    integer, intent(in) :: value
    var2 = value
  end subroutine setVar2
end module my_module

program test_module
  use my_module, only: setVar1, setVar2
  implicit none

  call setVar1(3.14)
  call setVar2(20)

  call printVar1
  call printVar2

contains

subroutine printVar1
  use my_module, only: getVar1
  implicit none

  print *, "Var1: ", getVar1()
end subroutine printVar1

subroutine printVar2
  use my_module, only: getVar2
  implicit none

  print *, "Var2: ", getVar2()
end subroutine printVar2
end program test_module
```

### Related resources

- [PWR073 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR073/)

### References

- ["Common blocks - Fortran 77
  Tutorial"](https://web.stanford.edu/class/me200c/tutorial_77/13_common.html),
  Stanford University. [last checked May 2024]

- ["Fortran memory
  management"](https://en.wikibooks.org/wiki/Fortran/memory_management),
  Wikibooks community. [last checked May 2024]

- ["F2018 Interpretation
Document"](https://j3-fortran.org/doc/year/18/18-007r1.pdf), Technical
Committee ISO/IEC JTC 1, Subcommittee SC 22. [last checked May 2024]

- ["Fortran code
modernization"](https://www.ugent.be/hpc/en/training/2018/modern_fortran_materials/modernfortran2018.pdf),
Reinhold Bader. [last checked May 2024]

- ["Making legacy Fortran code type safe through automated program
transformation"](https://link.springer.com/article/10.1007/s11227-021-03839-9),
Wim Vanderbauwhede. [last checked May 2024]
