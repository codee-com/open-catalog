# PWR031: Replace pow by multiplication, division and/or square root

### Issue

The `pow` function (`**` operator in Fortran) is computationally expensive and
in many cases can be replaced by
[faster mathematical operations](../../Glossary/Strength-reduction.md).

### Actions

Replace `pow` with equivalent calculations using multiplications, divisions
and/or square roots where possible.

### Relevance

The `pow` function is commonly used in scientific computations but is generally
more expensive than alternative methods. When the exponent value is known at
compile time, runtime performance can be significantly improved by substituting
`pow` with a combination of simpler operations like multiplications, divisions,
or square roots.

> [!NOTE]
> Some compilers under some circumstances (e.g. relaxed IEEE 754 semantics) can
> do this optimization automatically. However, doing it manually will guarantee
> best performance across all the compilers.

### Code example

#### C

The following code invokes `pow` to calculate `x` to the power of `1.5`:

```c
#include <math.h>

void example(float *a, float x) {
  for (int i = 0; i < 10; ++i) {
    a[i] = pow(x, 1.5);
  }
}
```

This can also be accomplished by multiplying `x` by its square root, which is
faster:

```c
#include <math.h>

void example(float *a, float x) {
  for (int i = 0; i < 10; ++i) {
    a[i] = x * sqrt(x);
  }
}
```

#### Fortran

The following code uses the `**` operator to compute `x` to the power of `1.5`:

```fortran
subroutine example(a, x)
  use iso_fortran_env, only : real32, int32
  ! dummy args
  real(kind=real32), intent(out) :: a(:)
  real(kind=real32), intent(in) :: x
  ! local vars
  integer(kind=int32) :: i
  !
  do i = 1_int32, 10_int32
    a(i) = x ** 1.5_real32
  end do
end subroutine example
```

This can be optimized by replacing `**` with multiplication and the square root:

```fortran
subroutine example(a, x)
  use iso_fortran_env, only : real32, int32
  ! dummy args
  real(kind=real32), intent(out) :: a(:)
  real(kind=real32), intent(in) :: x
  ! local vars
  integer(kind=int32) :: i
  !
  do i = 1_int32, 10_int32
    a(i) = x * sqrt(x)
  end do
end subroutine example
```

### Related resources

* [PWR031 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR031/)

### References

* [Strength reduction](../../Glossary/Strength-reduction.md)

* [IEEE Arithmetic](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_math.html#:~:text=IEEE%20754%20specifies%20exactly%20the,defined%20by%20the%20IEEE%20standard)

* [Semantics of Floating Point Math in GCC](https://gcc.gnu.org/wiki/FloatingPointMath)
