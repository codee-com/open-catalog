# PWR064: Precision loss in floating-point constant

### Issue

An unsuffixed real literal has default precision, which may cause precision loss
if the value is actually destined for a wider variable.

### Actions

Suffix the floating-point literal with an appropriately precise `kind` derived
from either `selected_real_kind()` or the `iso_fortran_env` module.

### Relevance

Floating-point literals without a `kind` suffix have default precision, which
may offer inconsistent behavior across compilers or compilation flags. The
default `kind` behavior is the same as for variables, see [PWR071: Prefer
real(kind=kind_value) for declaring consistent floating
types](../PWR071/README.md) for more details.

> [!WARNING]
> Avoid the use of compilation flags such as GNU's `-fdefault-real-8` to
> increase the precision of unsuffixed literals. While many compilers interpret
> these correctly, they are ultimately not part of the Fortran standard, and
> thus code relying on them may silently break on other compilers.

> [!NOTE]
> Unlike their C analogue, the default `kind` of unsuffixed literals is *not*
> that of `double precision`.

### Code example

In the following example, an unsuffixed literal is used to initialize a
`parameter` with an explicitly specified `kind`:

```fortran
! example.f90
program test_literal_without_suffix
  implicit none
  integer, parameter :: dp = selected_real_kind(15, 307)
  real(kind=dp), parameter :: e = 2.718281828459045
  print *, e
end program test_literal_without_suffix
```

In many implementations the default real `kind` is not precise enough, so
rounding errors will be introduced to the literal before it is assigned. To
prevent this, suffix the literal with the same `kind` as the variable:

```fortran
! solution.f90
program test_literal_with_suffix
  implicit none
  integer, parameter :: dp = selected_real_kind(15, 307)
  real(kind=dp), parameter :: e = 2.718281828459045_dp
  print *, e
end program test_literal_with_suffix
```

### Related resources

- [PWR064 examples](https://github.com/codee-com/open-catalog/tree/test_literal_suffix/Checks/PWR064/)

- [PWR071: Prefer real(kind=kind_value) for declaring consistent floating
types](../PWR071/README.md)

### References

- ["It Takes All KINDs - Doctor
Fortran"](https://stevelionel.com/drfortran/2017/03/27/doctor-fortran-in-it-takes-all-kinds/),
Steve Lionel. [last checked Apr 2026]
