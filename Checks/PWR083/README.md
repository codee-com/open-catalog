# PWR083: Match dummy and actual argument types in procedure calls

### Issue

Calling a procedure with actual arguments that are incompatible with the
characteristics of the procedure's dummy arguments (e.g., different `type`,
`kind`, `rank`, etc.) can lead to compilation failures or, if not caught,
undefined behavior at runtime.

### Actions

Ensure the arguments supplied at the call site match the characteristics of the
procedure's dummy arguments. Depending on the situation, this may involve
actions such as:

- Changing the `type` and/or `kind` of the actual argument declarations.
- Applying explicit type conversions (e.g., `int(...)`, `real(...)`).
- Providing alternative procedure implementations for different argument types.

Also verify that arguments are passed in the intended order; a type mismatch
can be caused by accidentally swapping argument positions.

### Relevance

When calling a Fortran procedure, actual and dummy arguments are expected to
have compatible characteristics (`type`, `kind`, `rank`, etc.). A common
mistake is to inadvertently mix numeric types; for example:

- Passing a `real` where an `integer` is expected.
- Using the wrong `kind` (e.g., `real32` instead of `real64`).

When an explicit procedure interface is available at the call site, compilers
can typically catch and report type mismatch issues during compilation.

However, Fortran also allows calling procedures without information about the
expected arguments; in such cases, an _implicit interface_ is used. The caller
effectively passes a list of memory addresses, which the called procedure
interprets according to its dummy argument declarations. If the actual
arguments do not match the dummy arguments, the result is undefined behavior
and may manifest as incorrect results, "random" behavior, or even crashes.

> [!TIP]
> To enhance code safety and reliability, ensure procedures provide
> explicit interfaces to their callers. Check the [PWR068 entry](../PWR068/)
> for more details!

### Code examples

The following example advances a simulation time using a step count
`numberSteps`. The procedure expects an `integer` step count, but the caller
accidentally provides a `real` value:

```fortran {6} showLineNumbers
! simulation.f90
pure subroutine updateSimulationTime(numberSteps, prevTime, newTime)
  use iso_fortran_env, only: real32
  implicit none

  integer, intent(in) :: numberSteps
  real(kind=real32), intent(in) :: prevTime
  real(kind=real32), intent(out) :: newTime

  ! Each step is 0.1 seconds
  newTime = prevTime + numberSteps * 0.1_real32
end subroutine updateSimulationTime
```

```fortran {6,7,12} showLineNumbers
! example.f90
program call_with_type_mismatch
  use iso_fortran_env, only: real32
  implicit none

  external :: updateSimulationTime
  real(kind=real32) :: numberSteps, prevTime, newTime

  numberSteps = 10
  prevTime = 0.0

  call updateSimulationTime(numberSteps, prevTime, newTime)
  print *, "New time = ", newTime
end program call_with_type_mismatch
```

Because `updateSimulationTime()` is an `external` procedure defined in another
source file, the call uses an implicit interface. Compilers will typically
allow this program to compile despite the type mismatch, producing an incorrect
result at runtime:

```txt
$ gfortran --version
GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
$ gfortran simulation.f90 example.f90
$ ./a.out
 New time =    109261624.
```

A simple solution is to declare `numberSteps` using the type expected by the
procedure:

```fortran {7} showLineNumbers
! solution.f90
program correct_call
  use iso_fortran_env, only: real32
  implicit none

  external :: updateSimulationTime
  integer :: numberSteps
  real(kind=real32) :: prevTime, newTime

  numberSteps = 10
  prevTime = 0.0

  call updateSimulationTime(numberSteps, prevTime, newTime)
  print *, "New time = ", newTime
end program correct_call
```

Now, the program will produce the correct result:

```txt
$ gfortran simulation.f90 solution.f90
$ ./a.out
 New time =    1.00000000
```

Ultimately, it is recommended to encapsulate all procedures within modules so
that callers have explicit interfaces. This enables the compiler to verify the
provided arguments against the actual dummy arguments, preventing
difficult-to-diagnose runtime bugs.

> [!TIP]
> Check the [PWR068 entry](../PWR068/) for more details on implicit and
> explicit interfaces!

### Related resources

- [PWR083
  examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR083/)

### References

- ["Implicit
Interfaces"](https://people.cs.vt.edu/~asandu/Courses/MTU/CS2911/fortran_notes/node44.html),
Adrian Sandu. [last checked January 2026]

- ["More on Implicit
Interfaces"](https://people.cs.vt.edu/~asandu/Courses/MTU/CS2911/fortran_notes/node181.html),
Adrian Sandu. [last checked January 2026]

- ["Explicit
Interfaces"](https://people.cs.vt.edu/~asandu/Courses/MTU/CS2911/fortran_notes/node182.html),
Adrian Sandu. [last checked January 2026]

- ["Doctor Fortran Gets
Explicit!"](https://web.archive.org/web/20130803094211/http://software.intel.com/en-us/forums/topic/275071),
Steve Lionel. [last checked January 2026]

- ["Doctor Fortran Gets Explicit - Again!
"](https://web.archive.org/web/20130113070703/http://software.intel.com/en-us/blogs/2012/01/05/doctor-fortran-gets-explicit-again),
Steve Lionel. [last checked January 2026]
