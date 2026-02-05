# PWR088: Add missing arguments to procedure calls

### Issue

Calling a procedure while omitting one or more required arguments (i.e., dummy
arguments that are not `optional`) can lead to compilation failures or, if not
caught, undefined behavior at runtime.

### Actions

Ensure all arguments required by the procedure (i.e., not marked `optional`)
are supplied at the call site. If a call with fewer arguments is actually
needed, adapt the procedure accordingly; for example, by converting arguments
to `optional` and handling the missing scenarios in the implementation.

### Relevance

When calling a Fortran procedure, the caller is expected to supply all
mandatory arguments. If an explicit procedure interface is available at the
call site, compilers can typically detect and report a mismatch in the number
of supplied arguments during compilation.

However, Fortran also allows calling procedures without information about the
expected arguments; in such cases, an _implicit interface_ is used. The caller
effectively passes a list of memory addresses, which the called procedure
interprets according to its dummy argument declarations. If the actual
arguments are fewer than the dummy arguments, the result is undefined behavior
and may manifest as incorrect results, "random" behavior, or even crashes.

> [!TIP]
> To enhance code safety and reliability, ensure procedures provide
> explicit interfaces to their callers. Check the [PWR068 entry](../PWR068/)
> for more details!

### Code examples

The following example advances a simulation time by `numberSteps * dt`. The
procedure expects four arguments, but the caller accidentally passes only
three:

```fortran {7,11} showLineNumbers
! simulation.f90
pure subroutine updateSimulationTime(numberSteps, dt, prevTime, newTime)
  use iso_fortran_env, only: real32
  implicit none

  integer, intent(in) :: numberSteps
  real(kind=real32), intent(in) :: dt
  real(kind=real32), intent(in) :: prevTime
  real(kind=real32), intent(out) :: newTime

  newTime = prevTime + numberSteps * dt
end subroutine updateSimulationTime
```

```fortran {6,13} showLineNumbers
! example.f90
program call_with_missing_arguments
  use iso_fortran_env, only: real32
  implicit none

  external :: updateSimulationTime
  integer :: numberSteps
  real(kind=real32) :: prevTime, newTime

  numberSteps = 10
  prevTime = 0.0

  call updateSimulationTime(numberSteps, prevTime, newTime)
  print *, "New time = ", newTime
end program call_with_missing_arguments
```

Because `updateSimulationTime()` is an `external` procedure, the call uses an
implicit interface. Compilers will typically allow this program to compile
despite the argument count mismatch, producing an incorrect result at runtime:

```txt
$ gfortran --version
GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
$ gfortran simulation.f90 example.f90
$ ./a.out
 New time =   -2.23126931E+24
```

A simple solution is to pass the missing `dt` in the procedure call:

```fortran {8,11,14} showLineNumbers
! solution.f90
program correct_call
  use iso_fortran_env, only: real32
  implicit none

  external :: updateSimulationTime
  integer :: numberSteps
  real(kind=real32) :: dt, prevTime, newTime

  numberSteps = 10
  dt = 0.1
  prevTime = 0.0

  call updateSimulationTime(numberSteps, dt, prevTime, newTime)
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

- [PWR088
  examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR088/)

### References

- ["Implicit
Interfaces"](https://people.cs.vt.edu/~asandu/Courses/MTU/CS2911/fortran_notes/node44.html),
Adrian Sandu. [last checked February 2026]

- ["More on Implicit
Interfaces"](https://people.cs.vt.edu/~asandu/Courses/MTU/CS2911/fortran_notes/node181.html),
Adrian Sandu. [last checked February 2026]

- ["Explicit
Interfaces"](https://people.cs.vt.edu/~asandu/Courses/MTU/CS2911/fortran_notes/node182.html),
Adrian Sandu. [last checked February 2026]

- ["Doctor Fortran Gets
Explicit!"](https://web.archive.org/web/20130803094211/http://software.intel.com/en-us/forums/topic/275071),
Steve Lionel. [last checked February 2026]

- ["Doctor Fortran Gets Explicit - Again!
"](https://web.archive.org/web/20130113070703/http://software.intel.com/en-us/blogs/2012/01/05/doctor-fortran-gets-explicit-again),
Steve Lionel. [last checked February 2026]
