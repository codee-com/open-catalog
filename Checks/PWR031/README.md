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
// example.c
#include <math.h>
#include <stdio.h>

<<<<<<< HEAD
void example(float *a, float x) {
  for (int i = 0; i < 10; ++i) {
    a[i] = pow(x, 1.5);
  }
=======
__attribute__((const)) double raise_x_to_the_power_of_1_point_5(double x) {
  return pow(x, 1.5);
}

int main() {
  printf("2 raised to the power of 1.5 is: %0.15f\n",
         raise_x_to_the_power_of_1_point_5(2.0));
  return 0;
>>>>>>> bcc5f27 (PWR031: Show transformations' numerical precision)
}
```

This can also be accomplished by multiplying `x` by its square root, which is
faster:

```c
// solution.c
...

__attribute__((const)) double raise_x_to_the_power_of_1_point_5(double x) {
  return x * sqrt(x);
}

...
```

Moreover, we can verify that the computation retains full precision by running
the following commands and comparing the results:

```txt
$ gcc --version
gcc (GCC) 14.2.1 20240910
$ gcc example.c -lm -o example
$ gcc solution.c -lm -o solution
$ ./example
2 raised to the power of 1.5 is: 2.828427124746190
$ ./solution
2 raised to the power of 1.5 is: 2.828427124746190
```

#### Fortran

The following code uses the `**` operator to compute `x` to the power of `1.5`:

```fortran
! example.f90
program main
  use iso_fortran_env, only : real64
  implicit none
  !
  print '(A, F0.15)', '2 raised to the power of 1.5 is: ', &
      raise_x_to_the_power_of_1_point_5(2.0_real64)
contains
  pure function raise_x_to_the_power_of_1_point_5(x)
    implicit none
    ! function return type
    real(kind=real64) :: raise_x_to_the_power_of_1_point_5
    ! dummy args
    real(kind=real64), intent(in) :: x
    !
    raise_x_to_the_power_of_1_point_5 = x ** 1.5_real64
  end function raise_x_to_the_power_of_1_point_5
end program main
```

This can be optimized by replacing `**` with multiplication and the square root:

```fortran
! solution.f90
program main
  ...
contains
  pure function raise_x_to_the_power_of_1_point_5(x)
    ...
    raise_x_to_the_power_of_1_point_5 = x * sqrt(x)
  end function raise_x_to_the_power_of_1_point_5
end program main
```

Moreover, we can verify that the computation retains full precision by running
the following commands and comparing the results:

```txt
$ gfortran --version
GNU Fortran (GCC) 14.2.1 20240910
$ gfortran example.f90 -o example
$ gfortran solution.f90 -o solution
$ ./example
2 raised to the power of 1.5 is: 2.828427124746190
$ ./solution
2 raised to the power of 1.5 is: 2.828427124746190
```

### Related resources

* [PWR031 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR031/)

### References

* [Strength reduction](../../Glossary/Strength-reduction.md)

* [IEEE Arithmetic](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_math.html#:~:text=IEEE%20754%20specifies%20exactly%20the,defined%20by%20the%20IEEE%20standard)

* [Semantics of Floating Point Math in GCC](https://gcc.gnu.org/wiki/FloatingPointMath)
