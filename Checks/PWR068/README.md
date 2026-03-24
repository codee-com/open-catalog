# PWR068: Call procedures through explicit interfaces, preferably as module procedures

### Issue

Calling a procedure without an explicit interface prevents the compiler from
verifying compatibility between the actual arguments at the call site and the
procedure's dummy arguments. As a result, argument mismatches may compile
without warning and later surface as incorrect results and
difficult-to-diagnose runtime bugs.

### Actions

To enhance code safety and reliability, ensure that procedure calls use
explicit interfaces.

For most Fortran code, prefer defining procedures inside modules. Module
procedures automatically provide explicit interfaces to callers through `use`
association mechanisms, making it the safest and most maintainable approach for
most cases.

Other mechanisms can also provide explicit interfaces when module procedures
are not the right fit. For example:

- Internal procedures also provide explicit interfaces automatically through
  host association.
- Explicit `interface` blocks are helpful for C/C++ interoperable procedures,
  dummy procedures, and procedure pointers.

### Relevance

Fortran allows procedure calls even when the caller has no information about
the number, order, or properties of the dummy arguments (`type`, `kind`,
`rank`, attributes, etc.). In such cases, the call uses an _implicit
interface_.

With an implicit interface, the caller simply provides a list of memory
addresses, and the called procedure interprets them according to its dummy
argument declarations. Because the compiler does not know enough about the
procedure at the call site, it isn't able to detect problems such as:

- **Wrong number of arguments:**
  - Missing actual arguments may cause the procedure to access unrelated
    memory, leading to undefined behavior.
  - Excess actual arguments may also lead to undefined behavior; for example,
    by interfering with hidden information passed by the compiler, such as
    descriptors for array arguments.

- **Incompatible argument characteristics:** Passing actual arguments whose
  characteristics are incompatible with those of the dummy arguments. For
  example, passing a `real` where an `integer` is expected, or using a `real32`
  `kind` where `real64` is required.

- **Swapped arguments:** Accidentally changing the order of arguments can also
  introduce incompatibilities even when the number of arguments is correct.

> [!TIP]
> To learn more about these issues and the importance of explicit interfaces,
> see [PWR083](../PWR083/), [PWR088](../PWR088/), and [PWR089](../PWR089/).

In contrast, when a procedure call is made through an explicit interface, the
compiler can verify argument compatibility at compile time, catching any errors
before they reach runtime.

For most Fortran code, the best way to avoid implicit interfaces is to define
procedures inside modules. The interface is automatically derived from the
procedure definition, remaining consistent at all times.

Alternatives for less common scenarios are discussed after the code examples
below.

### Code examples

The following program calculates the factorial of a number. To reflect a common
project layout with multiple source files, the main program and the factorial
procedure are in different files:

```fortran {4,5} showLineNumbers
! example_factorial.f90
pure subroutine factorial(number, result)
  implicit none
  integer, intent(in) :: number
  integer, intent(out) :: result
  integer :: i

  result = 1
  do i = 1, number
    result = result * i
  end do
end subroutine factorial
```

```fortran {5} showLineNumbers
! example.f90
program test_implicit_interface
  use iso_fortran_env, only: real32
  implicit none
  external :: factorial
  real(kind=real32) :: number, result

  number = 5
  call factorial(number, result)
  print *, "Factorial of", number, "is", result
end program test_implicit_interface
```

You may have already noticed that the main program incorrectly assumes that the
`factorial` subroutine uses `real` variables, instead of `integer`. Indeed,
running the program produces an incorrect result:

```txt
$ gfortran --version
GNU Fortran (Debian 12.2.0-14) 12.2.0
$ gfortran example_factorial.f90 example.f90
$ ./a.out
 Factorial of   5.00000000     is   0.00000000
```

The compiler cannot catch this bug during compilation because the called
procedure has an implicit interface: it is an `external` element defined in
another source file.

A simple solution is to encapsulate the procedure within a module. This makes
an explicit interface available to callers through `use` association
mechanisms, allowing the compiler to verify the provided arguments against the
actual dummy arguments.

Moving the `factorial` subroutine to a module is as simple as:

```fortran showLineNumbers
! solution_mod_factorial.f90
module mod_factorial
  implicit none
contains
  pure subroutine factorial(number, result)
    implicit none
    integer, intent(in) :: number
    integer, intent(out) :: result
    integer :: i

    result = 1
    do i = 1, number
      result = result * i
    end do
  end subroutine factorial
end module mod_factorial
```

```fortran showLineNumbers
! solution_with_type_mismatch.f90
program test_explicit_interface
  use iso_fortran_env, only: real32
  use mod_factorial, only: factorial
  implicit none
  real(kind=real32) :: number, result

  number = 5
  call factorial(number, result)
  print *, "Factorial of", number, "is", result
end program test_explicit_interface
```

With the explicit interface, the compiler catches the argument mismatches at
compile-time, avoiding the runtime bug:

```txt
$ gfortran solution_mod_factorial.f90 solution_with_type_mismatch.f90
solution_with_type_mismatch.f90:7:32:

    7 |     call factorial(number, result)
      |                                  1
Error: Type mismatch in argument ‘number’ at (1); passed REAL(4) to INTEGER(4)
solution_with_type_mismatch.f90:7:32:

    7 |     call factorial(number, result)
      |                                  1
Error: Type mismatch in argument ‘result’ at (1); passed REAL(4) to INTEGER(4)
```

Once the appropriate `integer` types are used, the program compiles and
produces the correct result:

```txt
$ gfortran solution_mod_factorial.f90 solution.f90
$ ./a.out
Factorial of           5 is         120
```

The problem is not limited to `external` procedures. Any call made without an
explicit interface carries the same risk. For example, in the following program
there is no `implicit none` statement, so `factorial` is an implicit entity
that also lacks an explicit interface when called:

```fortran {5} showLineNumbers
program test_implicit_interface
  use iso_fortran_env, only: real32
  real(kind=real32) :: number, result

  number = 5
  call factorial(number, result)
  print *, "Factorial of", number, "is", result
end program test_implicit_interface
```

> [!TIP]
> Internal procedures also provide explicit interfaces automatically through
> host association. They are a good choice when a procedure is only needed
> within a single host procedure:
>
> ```fortran showLineNumbers
> subroutine sub()
>   call internal()
> contains
>   subroutine internal()
>     ! statements...
>   end subroutine internal
> end subroutine sub
> ```

> [!WARNING]
> A handwritten `interface` block can provide an explicit interface, but also
> introduces risks. The interface must duplicate the target procedure's
> definition, but the compiler does not verify whether this replica matches the
> actual specification of the procedure or not.
>
> In this example, the interface declared manually for `factorial` is
> incorrect; the dummy arguments are `real` instead of `integer`. The compiler
> will accept the call because it is only checked against the handwritten
> `interface`, producing an incorrect result during execution:
>
> ```fortran {6,7} showLineNumbers
> program test_implicit_interface
>   use iso_fortran_env, only: real32
>   implicit none
> 
>   interface 
>     subroutine factorial(number, result)
>       real(kind=real32), intent(in) :: number
>       real(kind=real32), intent(out) :: result
>     end subroutine factorial
>   end interface
> 
>   real(kind=real32) :: number, result
> 
>   number = 5
>   call factorial(number, result)
>   print *, "Factorial of", number, "is", result
> end program test_implicit_interface
> ```
>
> Generally, manual `interface` blocks should be reserved for cases where
> module procedures aren't applicable, such as calling interoperable C/C++
> procedures, dummy procedures, and procedure pointers.

> [!TIP]
> Many modern Fortran features require explicit interfaces, including
> assumed-shape arrays, optional arguments, procedure arguments, and more.
> Avoiding implicit interfaces is therefore also a prerequisite for writing
> robust modern Fortran.

> [!TIP]
> If modifying legacy code is not feasible, consider creating module procedures
> that wrap the legacy ones. This provides safer interfaces for call sites
> while preserving existing implementations intact.

### Related resources

- [PWR068 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR068/)

### References

- ["Implicit Interfaces"](https://people.cs.vt.edu/~asandu/Courses/MTU/CS2911/fortran_notes/node44.html),
Adrian Sandu. [last checked May 2024]

- ["More on Implicit Interfaces"](https://people.cs.vt.edu/~asandu/Courses/MTU/CS2911/fortran_notes/node181.html),
Adrian Sandu. [last checked May 2024]

- ["Explicit Interfaces"](https://people.cs.vt.edu/~asandu/Courses/MTU/CS2911/fortran_notes/node182.html),
Adrian Sandu. [last checked May 2024]

- ["Doctor Fortran Gets Explicit!"](https://web.archive.org/web/20130803094211/http://software.intel.com/en-us/forums/topic/275071),
Steve Lionel. [last checked May 2024]

- ["Doctor Fortran Gets Explicit - Again!
"](https://web.archive.org/web/20130113070703/http://software.intel.com/en-us/blogs/2012/01/05/doctor-fortran-gets-explicit-again),
Steve Lionel. [last checked May 2024]

- ["Fortran code
modernization"](https://www.ugent.be/hpc/en/training/2018/modern_fortran_materials/modernfortran2018.pdf),
Reinhold Bader. [last checked May 2024]
