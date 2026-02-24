# PWR087: Declare array dummy arguments as assumed-shape arrays to favor compiler optimizations

### Issue

Assumed-shape dummy arguments require calls to be made through an explicit
interface so the caller can pass the array's runtime descriptor (describing
bounds, strides, etc.). With this information available, the compiler may
generate more efficient code and might avoid creating expensive temporary
copies when passing non-contiguous actual arguments (e.g., array slices).

### Actions

Transform explicit-shape and assumed-size array dummy arguments to
assumed-shape form to favor compiler optimizations.

### Relevance

Fortran supports various methods for passing allocated arrays to procedures.
Typically, assumed-shape arrays should be preferred over explicit-shape and
assumed-size arrays, as they give the compiler more opportunities to check and
optimize calls:

- **Assumed-shape arrays** are declared with a colon for each dimension; e.g.,
    `real :: arr(:)`, `real :: arr(:, :)`:
  - Procedure calls require an explicit interface because the caller implicitly
    passes the array's runtime descriptor (describing properties such as array
    bounds, strides, etc.).

- **Explicit-shape and assumed-size arrays** require manual specification of
  dimension sizes, often as separate arguments:
  - Explicit-shape arrays specify the size of all dimensions; e.g., `real ::
    arr(i, j)`.
  - Assumed-size arrays leave the size of the last dimension unspecified; e.g.,
    `real :: arr(n, *)`.
  - Procedure calls can be made through implicit interfaces.

Overall, assumed-shape arrays introduce the following benefits:

1. **Mandatory explicit interface:** The compiler has more information about
the called procedure and its arguments (types, intents, ranks, etc.), which can
facilitate compiler optimizations.

2. **Avoidance of hidden array temporaries:** With explicit-shape and
assumed-size dummy arguments, passing a non-contiguous actual argument may
cause the compiler to silently create a contiguous temporary copy, which can be
expensive. Assumed-shape arrays can avoid this for certain scenarios (e.g.,
regularly strided array sections) because the array descriptor describes how to
traverse memory.

> [!TIP]
> Many compilers provide flags to warn about these hidden copies. For
> example, `gfortran` supports `-Warray-temporaries`.

> [!TIP]
> Strided memory access is not always faster than packing data into a
> contiguous temporary; for example, a unit stride access is often more
> favorable for vectorization. Ultimately, the best choice depends on the
> computational kernel and the data. If a kernel benefits from packing, it can
> be forced even for an assumed-shape dummy argument by adding the `contiguous`
> attribute.

#### Summary

In terms of performance optimization:

- **Contiguous actual arguments:** Assumed-shape performance is typically on
  par with explicit-shape and assumed-size arrays.

- **Non-contiguous actual arguments:** Performance depends on whether direct
  strided access or packing is better for the kernel. If packing is better,
  consider using, for example, an assumed-shape array with the `contiguous`
  attribute.

> [!NOTE]
> Assumed-shape arrays significantly improve code quality and
> maintainability, making them worthwhile even if performance gains are not
> obtained. See [PWR070](../PWR070/README.md) for more details.

### Code examples

The following example shows a function that processes an array to clamp its
values to a minimum and maximum value. The initial example uses an
explicit-shape array dummy argument; note how the size `n` of the array is
passed as an additional argument to `clamp_values()`:

```fortran {5,9,10} showLineNumbers
! example.f90
module example
  implicit none
contains
subroutine clamp_values(n, X, min_value, max_value)
  use iso_fortran_env, only: real32

  implicit none
  integer, intent(in) :: n
  real(kind=real32), dimension(n), intent(inout) :: X
  real(kind=real32), intent(in) :: min_value, max_value
  
  integer :: i

  do i = 1, n
    if(X(i) < min_value) then
      X(i) = min_value
    else if(X(i) > max_value) then
      X(i) = max_value
    end if
  end do
end subroutine clamp_values
end module example
```

Making the switch to an assumed-shape array is as simple as dropping the `n`
argument, changing `X` to an assumed-shape dummy argument, and querying its
length using the `size()` intrinsic:

```fortran {5,9,14} showLineNumbers
! solution.f90
module solution
  implicit none
contains
subroutine clamp_values(X, min_value, max_value)
  use iso_fortran_env, only: real32

  implicit none
  real(kind=real32), dimension(:), intent(inout) :: X
  real(kind=real32), intent(in) :: min_value, max_value
  
  integer :: i

  do i = 1, size(X, 1)
    if(X(i) < min_value) then
      X(i) = min_value
    else if(X(i) > max_value) then
      X(i) = max_value
    end if
  end do
end subroutine clamp_values
end module solution
```

> [!TIP]
> Now that `clamp_values()` uses an assumed-shape dummy argument, its
> performance might improve when calling the procedure to operate on
> non-contiguous data.
>
> Check the PWR087 benchmark for a demonstration!

> [!WARNING]
> Beware that any procedures involving assumed-shape array arguments
> must have explicit interfaces at the point of call. If not, the updated code
> won't compile. `clamp_values()` provides an explicit interface to callers due
> to being inside a `module`.
>
> Check the [PWR068 entry](../PWR068/) for more details on implicit and
> explicit interfaces!

### Related resources

- [PWR087
  examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR087/)

### References

- ["Arrays — Fortran Programming
Language"](https://fortran-lang.org/learn/best_practices/arrays/), Fortran
Community. [last checked February 2026]

- ["Passing arrays to subroutines in Fortran: Assumed shape vs explicit
shape"](https://stackoverflow.com/questions/75051887/passing-arrays-to-subroutines-in-fortran-assumed-shape-vs-explicit-shape),
Stack Overflow Community. [last checked February 2026]

- ["Fortran Modernisation
Workshop"](https://blog.rwth-aachen.de/hpc_import_20210107/attachments/39157901/39420371.pdf),
The Numerical Algorithms Group. [last checked February 2026]

- ["-Warray-temporaries"](https://gcc.gnu.org/onlinedocs/gfortran/Error-and-Warning-Options.html#index-Warray-temporaries),
  Free Software Foundation, Inc. [last checked February 2026]

- ["Assumed-shape arrays
  repacking"](https://flang.llvm.org/docs/ArrayRepacking.html), The Flang Team.
  [last checked February 2026]
