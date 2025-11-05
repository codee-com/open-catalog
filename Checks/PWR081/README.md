# PWR081: Uninitialized output arguments can lead to undefined behavior

### Issue

An output argument is not always initialized before the procedure returns to
the caller. Its usage can cause undefined behavior due to its indeterminate
state.

### Actions

To prevent bugs in the code, ensure the problematic output arguments are set in
all possible code paths. In case of conditionality, it may help to add explicit
`else` or `default` branches in control-flow blocks, or even set default
initial values at the start of the procedure.

### Relevance

Some programming languages automatically initialize variables to default
values, but Fortran does not.

More specifically, variables not explicitly initialized contain unpredictable
data, and reading and using them results in undefined behavior, potentially
causing incorrect results, crashes, or other unintended outcomes. Compilers are
not required to warn about these issues and, even if they do, they typically
still allow the code to compile and run.

Some compilers may appear to "help" by zero-initializing variables under
certain conditions (e.g., specific compilation flags). While this can make
technically incorrect code run as originally intended, relying on such
incidental behavior creates a false sense of security and masks underlying
logical errors. However, this apparent safety can vanish under different
compilation settings and can vary between compilers.

Lastly, while some compilers provide options to automatically initialize
certain data types (e.g., `gfortran`'s `-finit-integer=<value>`), these
features reduce portability to other development environments and hide problems
in the code rather than addressing them.

### Code examples

#### Fortran

Consider the following code, which computes statistics from a series of data
points:

```fortran {19,26,32,49,50} showLineNumbers
! example.f90

module StatsMod
  use iso_fortran_env, only: real32
  implicit none
  private

  public :: computeStats, statsType

  type :: statsType
    real(kind=real32) :: mean
    real(kind=real32) :: stddev
  end type statsType

contains

  pure subroutine computeStats(data, result)
    real(kind=real32), intent(in) :: data(:)
    type(statsType), intent(out) :: result

    integer :: i, len

    ! We can't compute stddev from a single data point
    len = size(data)
    if (len <= 1) then
      return
    end if

    ! Also abort if there are invalid data points
    do i = 1, len
      if (data(i) < -900.0) then
        return
      end if
    end do

    result%mean = sum(data) / len
    result%stddev = sqrt(sum((data - result%mean)**2) / (len - 1))
  end subroutine computeStats
end module StatsMod

program main
  use iso_fortran_env, only: real32
  use StatsMod, only: computeStats, statsType
  implicit none

  real(kind=real32), dimension(5) :: sensorData = [1.0, -999.0, 3.0, 5.0, 7.0]
  type(statsType) :: stats

  call computeStats(sensorData, stats)
  print '(A, F4.2, A, F4.2)', "Mean = ", stats%mean, " Stddev = ", stats%stddev
end program main
```

Note how `computeStats()` terminates early if certain requirements aren't met.
As a result, these early exists prevent `result` from being initialized, even
though the caller expects to always obtain a valid result. Since the Fortran
standard does not guarantee any default initialization for variables, the state
of `stats` in this scenario is indeterminate, leading to different outcomes
depending on the compiler and its settings:

- For instance, `gfortran -O2` appears to start the stats at `0`:

```txt
$ gfortran --version
GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
$ gfortran -O2 example.f90 -o example_gfortran
$ ./example_gfortran 
Mean = 0.00 Stddev = 0.00
```

- However, with `flang -O2`, the stats seem to contain arbitrary data:

```txt
$ flang-new --version
Ubuntu flang-new version 18.1.3 (1ubuntu1)
$ flang-new -O2 example.f90 -o example_flang
$ ./example_flang 
Mean = **** Stddev = ****
```

It is important to note that each compiler is not necessarily choosing specific
initial values for the stats. Instead, an undefined behavior means the outcome
of the code is always unpredictable, and could even lead to crashes; in fact,
these compilers could also behave differently under other compilation settings.
Ultimately, code with undefined behavior should never be relied upon.

To prevent these problems, we must always initialize output arguments in all
possible code paths to comply with the expectations of the caller. This
principle applies to all variable types, including elemental types like
`integer`, derived types, and arrays.

There are multiple ways to solve the bug. For example, we can simply initialize
`result` with default values at the start of `computeStats()`, ensuring it is
always defined for the caller, regardless of whether the rest of the procedure
can execute or not:

```fortran
! solution.f90

type(statsType), intent(out) :: result

! <...>

result%mean = 0.0
result%stddev = 0.0
```

### Related resources

- [PWR081
  examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR081/)

### References

- ["Fortran 2023 Interpretation
Document"](https://j3-fortran.org/doc/year/24/24-007.pdf), Technical Committee
ISO/IEC JTC1/SC22/WG5. [last checked November 2025]

- ["Undefined Variables - Fortran
Discourse"](https://fortran-lang.discourse.group/t/undefined-variables/3708),
Fortran Community. [last checked November 2025]

- ["Code Gen Options (The GNU Fortran
Compiler)"](https://gcc.gnu.org/onlinedocs/gfortran/Code-Gen-Options.html),
  Free Software Foundation, Inc. [last checked November 2025]
