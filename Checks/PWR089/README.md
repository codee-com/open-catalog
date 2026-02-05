# PWR089: Remove unexpected arguments from procedure calls

### Issue

Calling a procedure while supplying excess arguments (i.e., more actual
arguments than the procedure has dummy arguments) can lead to compilation
failures or, if not caught, undefined behavior at runtime.

### Actions

Ensure the number of actual arguments supplied at the call site matches the
number of dummy arguments in the procedure. If additional information is
needed, adapt the procedure accordingly; for example, by adding new dummy
arguments and handling them in the implementation.

### Relevance

When calling a Fortran procedure, the caller is expected to match the
procedure's dummy argument list. If an explicit procedure interface is
available at the call site, compilers can typically detect and report a
mismatch in the number of supplied arguments during compilation.

However, Fortran also allows calling procedures without information about the
expected arguments; in such cases, an _implicit interface_ is used. The caller
effectively passes a list of memory addresses, which the called procedure
interprets according to its dummy argument declarations. If the actual
arguments exceed the dummy arguments, the program is exposed to undefined
behavior.

A common misconception is that excess arguments are simply ignored. This may
appear to work for some procedures, but it is not guaranteed. More
specifically, some Fortran features can cause the compiler to supply hidden
values to the called procedure; for example, character lengths, or array
descriptors. If excess arguments are provided, those hidden values can be
misinterpreted, resulting in incorrect results, "random" behavior, or even
crashes.

> [!TIP]
> To enhance code safety and reliability, ensure procedures provide
> explicit interfaces to their callers. Check the [PWR068 entry](../PWR068/)
> for more details!

### Code examples

The following example logs diagnostics for a simulation data field. The called
procedure expects three arguments, but the caller accidentally passes an
additional verbosity argument:

```fortran showLineNumbers
! io.f90
subroutine logField(name, step, value)
  use iso_fortran_env, only: real32
  implicit none

  character(*), intent(in) :: name
  integer, intent(in) :: step
  real(kind=real32), intent(in) :: value

  write(*, *) trim(name), " step=", step, " value=", value
end subroutine logField
```

```fortran {6,10,17} showLineNumbers
! example.f90
program call_with_excess_arguments
  use iso_fortran_env, only: real32
  implicit none

  external :: logField
  character(len=20) :: name
  integer :: step
  real(kind=real32) :: val
  logical :: verbose

  name = "field"
  step = 10
  val = 3.14
  verbose = .true.

  call logField(name, step, val, verbose)
end program call_with_excess_arguments
```

Because `logField()` is an `external` procedure, the call uses an implicit
interface. Compilers will typically allow this program to compile despite the
argument count mismatch, which can corrupt the call frame if hidden information
is passed for the `character` argument. In fact, a crash is triggered at
runtime:

```txt
$ gfortran --version
GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
$ gfortran io.f90 example.f90
$ ./a.out

Program received signal SIGSEGV: Segmentation fault - invalid memory reference.

Backtrace for this error:
#0  0x76abe9223e59 in ???
#1  0x76abe9222e75 in ???
#2  0x76abe8e4532f in ???
        at ./signal/../sysdeps/unix/sysv/linux/x86_64/libc_sigaction.c:0
#3  0x76abe94d20f9 in ???
#4  0x76abe94d2194 in ???
#5  0x63e416093238 in ???
#6  0x63e416093359 in ???
#7  0x63e416093395 in ???
#8  0x76abe8e2a1c9 in __libc_start_call_main
        at ../sysdeps/nptl/libc_start_call_main.h:58
#9  0x76abe8e2a28a in __libc_start_main_impl
        at ../csu/libc-start.c:360
#10  0x63e4160930f4 in ???
#11  0xffffffffffffffff in ???
```

A simple solution is to remove the excess `verbose` argument from the procedure
call:

```fortran {15} showLineNumbers
! solution.f90
program correct_call
  use iso_fortran_env, only: real32
  implicit none

  external :: logField
  character(len=20) :: name
  integer :: step
  real(kind=real32) :: val

  name = "field"
  step = 10
  val = 3.14

  call logField(name, step, val)
end program correct_call
```

Now, the program will behave correctly:

```txt
$ gfortran io.f90 solution.f90
$ ./a.out
 field step=          10  value=   3.14000010 
```

Ultimately, it is recommended to encapsulate all procedures within modules so
that callers have explicit interfaces. This enables the compiler to verify the
provided arguments against the actual dummy arguments, preventing
difficult-to-diagnose runtime bugs.

> [!TIP]
> Check the [PWR068 entry](../PWR068/) for more details on implicit and
> explicit interfaces!

### Related resources

- [PWR089
  examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR089/)

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
