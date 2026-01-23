# PWR068: Encapsulate procedures within modules to avoid the risks of calling implicit interfaces

### Issue

Calling a procedure without an explicit interface prevents the compiler from
verifying argument compatibility, increasing the risk of difficult-to-diagnose
runtime bugs.

### Actions

To enhance code safety and reliability, encapsulate procedures within modules
to automatically provide an explicit interface at the point of the call.

### Relevance

Fortran allows procedures to be called without explicit information about the
number of expected arguments, order, or properties such as their type. In such
cases, an _implicit interface_ is used. The caller simply provides a list of
memory addresses, which the called procedure assumes point to variables
matching the dummy arguments. This can easily lead to issues such as:

- **Type mismatches:** Passing variables of one type (e.g., `real`) as another
type (e.g., `integer`) causes errors due to different internal representations.

- **Missing arguments:**
  - **Input arguments:** Omitted arguments are initialized to undefined
    values, resulting in unpredictable behavior.
  - **Output arguments:** Writing omitted arguments results in invalid memory
    accesses, potentially crashing the program.

In contrast, a procedure with an explicit interface informs the compiler about
the expected arguments, allowing it to perform the necessary checks at the
point of the call during compile-time. The preferred approach to ensure a
procedure has an explicit interface is to encapsulate it within a module, as
illustrated below.

### Code examples

The following program calculates the factorial of a number. To simulate a real
project with multiple source files, the main program and the factorial
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

A simple solution is to encapsulate the procedure within a module. This informs
the compiler about the exact location where the called subroutine is defined,
enabling it to verify the provided arguments against the actual dummy
arguments.

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

> [!NOTE]
> The previous example demonstrates how calls to `external` procedures are
> performed through implicit interfaces. The same problem would occur if
> `factorial` were an implicitly declared procedure, as shown in the following
> example:
> 
> ```fortran {5} showLineNumbers
> program test_implicit_interface
>   use iso_fortran_env, only: real32
>   real(kind=real32) :: number, result
> 
>   number = 5
>   call factorial(number, result)
>   print *, "Factorial of", number, "is", result
> end program test_implicit_interface
> ```
>
> Note the absence of `implicit none`, allowing the symbol `factorial` to be
> interpreted as an implicitly declared entity.

> [!WARNING]
> It's possible to manually define explicit interfaces using the `interface`
> construct at the call site. However, this approach introduces risks. The
> procedure's definition must be duplicated, but there's no mechanism to ensure
> this replica matches the actual definition of the original procedure, which
> can easily lead to errors:
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
> In this example, the manually declared interface for `factorial` is
> incorrect; the dummy arguments are declared as `real` instead of `integer`.
> This error won't be caught at compile time, and will still result in an
> unexpected output during execution.

> [!TIP]
> When interoperating between Fortran and C/C++, it's necessary to manually
> define explicit interfaces for the C/C++ procedures to call. Although this is
> not a perfect solution, since there are no guarantees that these interfaces
> will match the actual C/C++ procedures, it's still best to make the
> interfaces as explicit as possible. This includes specifying details such as
> argument intents, to help the Fortran compiler catch early as many issues as
> possible.

> [!TIP]
> If modifying legacy code is not feasible, create a module procedure that wraps
> the legacy procedure as an indirect approach to ensure argument compatibility.

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
