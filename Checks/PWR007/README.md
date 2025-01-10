# PWR007: Disable implicit declaration of variables

### Issue

Fortran allows implicit data typing by default, which is error-prone and
reduces the clarity of the code.

### Actions

Add the `implicit none` statement after the `program`, `module`, or procedure
declaration, while also declaring all variables explicitly to ensure the code
compiles correctly.

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

In the following example, all variables are implicitly typed:

```fortran
! example.f90
program example
  num1 = 7
  num2 = 2.5
  res = num1 / num2
  print *, res
end program example
```

As `num1` and `num2` start with the letter N, they are implicitly typed as
`integer`, even though `num2` is intended to hold a floating-point value. As a
result, the division is performed using integer arithmetic, yielding an
incorrect result:

```txt
$ gfortran --version
GNU Fortran (Debian 12.2.0-14) 12.2.0
$ gfortran example.f90 -o example
$ ./example
   3.00000000
```

By adding the `implicit none` statement, the compiler will require explicit
declarations for all variables. This helps ensure that `num2` is assigned the
intended data type:

```fortran
! solution.f90
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
$ gfortran solution.f90 -o solution
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

### Related resources

- [PWR007 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR007/)

### References

- [IMPLICIT - FORTRAN 77 Language Reference, ORACLE](https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vn9v/index.html),
2010 [last checked May 2023]

- [IMPLICIT - Intel® Fortran Compiler Developer Guide and Reference](https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2023-2/overview.html),
April 2022 [last checked July 2023]
