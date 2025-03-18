# PWR071: Prefer real(kind=kind_value) for declaring consistent floating types

### Issue

Floating-point variables declared with constructs other than `real(kind_value)`
or `real(kind=kind_value)` may not adhere to Fortran standards, and even offer
inconsistent behavior across platforms.

### Actions

To improve code portability and readability, prefer defining floating point
types using `real(kind=kind_value)`, selecting a `kind` value derived from
either `selected_real_kind()` or the `iso_fortran_env` module.

### Relevance

The standard-conforming `real(kind=kind_value)` construct allows developers to
clearly define the desired precision, improving code readability, while also
ensuring consistent behavior of the program across different development
environments.

Therefore, avoid commonly used floating point declarations such as:

- `real`: The precision is unspecified and may vary depending on the compiler
  or compilation flags, such as GNU's `-fdefault-real-8`.

- `double precision`: Just an alias for a high-precision `kind`. It might also
  vary depending on the compiler or compilation flags, such as GNU's
  `-fdefault-real-8`.

- `real*4`, `real*8`, etc.: Common extensions derived from the `character`
  type, specifying the byte size of the variable. They are not recognized by
  the Fortran standard, potentially limiting portability.

### Code example

Consider the following floating-point declarations, which are poor in
portability, flexibility, and explicitness in precision:

```fortran
! example.f90
program test_discouraged_real_types
  implicit none

  real   :: single_precision_variable
  real*8 :: double_precision_variable
  ! Or `double precision`

  ! Note the `_d0` suffix, indicating the precision of the value
  single_precision_variable = 0.123456
  double_precision_variable = 0.123456789012345d0

  print *, 'Single precision variable:', single_precision_variable
  print *, 'Double precision variable:', double_precision_variable
end program test_discouraged_real_types
```

To conveniently control the program's precision and ensure portability, it is
recommended to use a central module where different `kind` values are defined,
either using `selected_real_kind()`, or the `iso_fortran_env` module.

Let's start with an example using `selected_real_kind()`, which allows to
specify a minimum amount of significant digits and exponent range:

```fortran
! solution_selected_real_kind.f90
module my_kinds
  implicit none
  public

  ! single precision (sp): 6 significant digits, 37 exponent range
  integer, parameter :: sp = selected_real_kind(6, 37)
  ! double precision (dp): 15 significant digits, 307 exponent range
  integer, parameter :: dp = selected_real_kind(15, 307)
end module my_kinds

program test_selected_real_kind
  use my_kinds, only: sp, dp
  implicit none

  real(kind=sp) :: single_precision_variable
  real(kind=dp) :: double_precision_variable

  ! Note the `_sp` and `_dp` suffixes, indicating the precision of the value
  single_precision_variable = 0.123456_sp
  double_precision_variable = 0.123456789012345_dp

  print *, 'Single precision variable:', single_precision_variable
  print *, 'Double precision variable:', double_precision_variable
end program test_selected_real_kind
```

Notice how the encapsulation of `sp` and `dp` inside `my_kinds` allows to use a
consistent `kind` value across the codebase. The flexibility of the program is
significantly increased, allowing to rapidly adjust the desired compute
precision. Consider, for instance, that during testing you recognize the need
for higher precision computations due to the accumulation of residual numerical
errors. The central module of `kind`s allows to update the declaration of tens
or hundreds of variables by changing a single line of code.

Another option, depending on your preferences, is using the `iso_fortran_env`
module. It results in a "lower-level" approach that allows to select the byte
size of the variables (from Fortran 2008 onwards):

```fortran
! solution_iso_fortran_env.f90
module my_kinds
  use iso_fortran_env, only: real32, real64
  implicit none
  public

  ! single precision (sp)
  integer, parameter :: sp = real32
  ! double precision (dp)
  integer, parameter :: dp = real64
end module my_kinds

program test_iso_fortran_env
  use my_kinds, only: sp, dp
  implicit none

  real(kind=sp) :: single_precision_variable
  real(kind=dp) :: double_precision_variable

  ! Note the `_sp` and `_dp` suffixes, indicating the precision of the value
  single_precision_variable = 0.123456_sp
  double_precision_variable = 0.123456789012345_dp

  print *, 'Single precision variable:', single_precision_variable
  print *, 'Double precision variable:', double_precision_variable
end program test_iso_fortran_env
```

> [!WARNING]
> Avoid specifying `kind` with hard-coded values. While many compilers interpret
> these as the number of bytes required by the variable, other compilers might
> not, leading to potential bugs. For instance, NAG's compiler uses sequential
> values for `kind` (`kind=1` = 4 bytes, `kind=2` = 8 bytes), unless
> `-kind=byte` is also specified in the compilation flags.

> [!TIP]
> Check out the `kind` values provided by the `iso_c_binding` module to ensure
> proper floating-point interoperability when working with both C and Fortran
> codes.

### Related resources

- [PWR071 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR071/)

### References

- ["Modernizing Old Fortran in Fortran
Wiki"](https://fortranwiki.org/fortran/show/Modernizing+Old+Fortran), Fortran
Community. [last checked May 2024]

- ["Real precision in Fortran
Wiki"](https://fortranwiki.org/fortran/show/Real+precision), Fortran Community.
[last checked May 2024]

- ["Variables -- Fortran Programming
Language"](https://fortran-lang.org/en/learn/quickstart/variables/), Fortran
Community. [last checked May 2024]

- ["Floating Point Numbers -- Fortran Programming
Language"](https://fortran-lang.org/en/learn/best_practices/floating_point/),
Fortran Community. [last checked May 2024]

- ["Best way to declare a double precision in Fortran? - Fortran
Discourse"](https://fortran-lang.discourse.group/t/best-way-to-declare-a-double-precision-in-fortran/69),
Fortran Community. [last checked May 2024]

- ["How useful is select_int_kind and selected_real_kind? - Fortran
Discourse"](https://fortran-lang.discourse.group/t/how-useful-is-selected-int-kind-and-selected-real-kind/6920),
Fortran Community. [last checked May 2024]

- ["It Takes All KINDs - Doctor
Fortran"](https://stevelionel.com/drfortran/2017/03/27/doctor-fortran-in-it-takes-all-kinds/),
Steve Lionel. [last checked May 2024]

- ["Fortran code
modernization"](https://www.ugent.be/hpc/en/training/2018/modern_fortran_materials/modernfortran2018.pdf),
Reinhold Bader. [last checked May 2024]
