# PWR007: Disable the implicit declaration of variables and procedures

### Issue

Fortran allows implicit typing rules by default, which are error-prone and
reduce the clarity of the code.

### Actions

Add the `implicit none` statement after the `program`, `module`, or procedure
(`function`/`subroutine`) declaration, while also declaring all variables and
procedures explicitly to ensure the code compiles and runs correctly.

### Relevance

Since Fortran 77, variables can be implicitly declared based on their initial
letter:

- Variables starting with I, J, K, L, M, or N are implicitly typed as
  `integer`.
- Variables starting with other letters are implicitly typed as `real`.

This implicit behavior can easily lead to errors and makes the code harder to
understand. For example, a variable intended for floating-point calculations
may be treated as an `integer` because of its name, causing incorrect results.
Another common error during refactoring occurs when most occurrences of a
variable, but not all, are renamed, or one is misspelled. Instead of triggering
compilation errors, new variables are implicitly declared, leading to subtle
bugs that are difficult to identify.

Implicit typing can be disabled by adding the `implicit none` statement to the
affected `program`, `module`, or procedure. This will force the compiler to
alert of any undeclared variables, helping ensure the correct data types are
used.

> [!TIP]
> Starting with Fortran 2018, the extended form `implicit none (type,
> external)` disables both implicit variables and implicit procedures, which
> are called through implicit interfaces. This mitigates the risks associated
> with such procedures, as outlined in [PWR068](/Checks/PWR068/).

### Code example

#### Implicit variables

In the following example, all variables are implicitly typed:

```fortran {3,4} showLineNumbers
! example_variable.f90
program example
  num1 = 7
  num2 = 2.5
  res = num1 / num2
  print *, res
end program example
```

As `num1` and `num2` start with the letter `n`, they are implicitly typed as
`integer`, even though `num2` is intended to hold a floating-point value. As a
result, the division is performed using integer arithmetic, yielding an
incorrect result:

```txt
$ gfortran --version
GNU Fortran (Debian 12.2.0-14) 12.2.0
$ gfortran example_variable.f90 -o example
$ ./example
   3.00000000
```

By adding the `implicit none` statement, the compiler will require explicit
declarations for all variables. This helps ensure that `num2` is assigned the
intended data type:

```fortran {4-6} showLineNumbers
! solution_variable.f90
program solution
  use iso_fortran_env, only: real32
  implicit none
  integer :: num1
  real(kind=real32) :: num2, res

  num1 = 7
  num2 = 2.5
  res = num1 / num2
  print *, res
end program solution
```

The division operation will now use floating-point arithmetic, producing a
correct result:

```txt
$ gfortran solution_variable.f90 -o solution
$ ./solution
   2.79999995
```

> [!WARNING]
> Note that the variable declarations and initializations are placed on
> separate lines to avoid an unintended `save` behavior. While this is not
> problematic in a `program`, it could cause unintended side effects in
> procedures that are called multiple times. Refer to [PWR072](/Checks/PWR072/)
> for more details.

> [!TIP]
> Although not strictly necessary, `num2` is assigned a specific kind following
> the recommendations outlined in [PWR071](/Checks/PWR071/).

#### Implicit procedures

The following example calculates the factorial of a number. Note how, despite
using `implicit none` to disable the implicit declaration of variables, the
main program is still allowed to call the implicit `factorial` procedure:

```fortran showLineNumbers
! example_procedure_factorial.f90
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

```fortran {4,5,8} showLineNumbers
! example_procedure.f90
program example
  use iso_fortran_env, only: real32
  implicit none
  real(kind=real32) :: number, result

  number = 5
  call factorial(number, result)
  print *, "Factorial of", number, "is", result
end program example
```

The main program incorrectly assumes that the `factorial` subroutine uses
`real` variables, instead of `integer` variables. Consequently, running the
program produces an incorrect result:

```txt
$ gfortran --version
GNU Fortran (Debian 12.2.0-14) 12.2.0
$ gfortran example_procedure_factorial.f90 example_procedure.f90 -o example
$ ./example                                      
 Factorial of   5.00000000     is   0.00000000
```

By using the `implicit none (type, external)` statement, we effectively disable
both implicit variables and implicit procedures:

```fortran {4} showLineNumbers
! example_procedure_with_implicit.f90
program example
  use iso_fortran_env, only: real32
  implicit none(type, external)
  real(kind=real32) :: number, result

  number = 5
  call factorial(number, result)
  print *, "Factorial of", number, "is", result
end program example
```

```txt
$ gfortran example_procedure_factorial.f90 example_procedure_with_implicit.f90 -o example
example_procedure_with_implicit.f90:9:32:

    9 |   call factorial(number, result)
      |                                1
Error: Procedure ‘factorial’ called at (1) is not explicitly declared
```

A simple solution is to encapsulate the procedure within a module, thus
providing it with an explicit interface. This allows the compiler to verify the
types of the provided arguments against those of the dummy arguments.

Moving the `factorial` subroutine to a module is as simple as:

```fortran showLineNumbers
! solution_procedure_factorial.f90
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

With the explicit interface, the compiler will refuse to compile the code
unless the proper `integer` data types are used, avoiding the runtime bug:

```fortran {5} showLineNumbers
! solution_procedure.f90
program solution
  use mod_factorial, only: factorial
  implicit none(type, external)
  integer :: number, result

  number = 5
  call factorial(number, result)
  print *, "Factorial of", number, "is", result
end program solution
```

```txt
$ gfortran solution_procedure_factorial.f90 solution_procedure.f90 -o solution
$ ./solution
 Factorial of           5 is         120
```

> [!TIP]
> `implicit none(type, external)` allows simply declaring implicit procedures
> as `external`. However, it's advised to provide an explicit interface at the
> point of call (e.g., through a module), to enable compile-time argument
> checks that prevent bugs like the one shown above. Refer to
> [PWR068](/Checks/PWR068/) for more information.

### Related resources

- [PWR007 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR007/)

### References

- [IMPLICIT - FORTRAN 77 Language Reference, ORACLE](https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vn9v/index.html),
2010 [last checked May 2023]

- [IMPLICIT - Intel® Fortran Compiler Developer Guide and Reference](https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2023-2/overview.html),
April 2022 [last checked July 2023]
