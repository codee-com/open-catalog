
import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

# PWR075: Avoid using compiler-specific Fortran extensions

### Issue

Using compiler-specific extensions, such as those provided by GNU's `gfortran`
or Intel's `ifort` and `ifx`, compromises the portability and consistency of
code across different development environments.

### Actions

Refactor the code to replace existing compiler-specific extensions with
standard-compliant Fortran code. The specific steps for refactoring will depend
on the particular features being replaced.

### Relevance

Fortran compilers often extend their support beyond the Fortran standard by
introducing additional language features. While these extensions can provide
enhanced functionality and convenience for programmers, they also present
significant drawbacks.

Even though some extensions may be supported by multiple compilers, there will
inevitably be environments where the code is not compilable, complicating
future maintenance. Additionally, even when multiple vendors support the same
extension, its behavior can vary, as these features are not regulated by the
Fortran standard. Ultimately, Fortran code that depends on compiler-specific
extensions cannot be guaranteed to be portable and consistent across different
development environments.

This issue is particularly problematic when inheriting legacy code. If the code
relies on compiler-specific extensions from a now unsupported or inaccessible
compiler version, developers are forced to port the code to new environments,
further complicating the already challenging task of maintaining legacy code.

Some compiler-specific extensions include:

- GNU Fortran: shadowing host entities in internal procedures, forward
referencing symbols in the specification part, or using `REAL` expressions in
array subscripts.

- Intel Fortran: calling functions as if they were subroutines, allowing
`DATA` statements that do not fully initialize arrays or derived types, or
accessing arrays beyond their declared bounds without triggering any semantic
errors.

For more information on which extensions are implemented by different compilers,
see the **References** section below for links to their official documentation.

### Code examples

#### GNU Fortran extension

Consider the following code, which checks if the file itself exists:

```fortran
! example-gnu.f90
program example
  implicit none
  character(len = *), parameter :: file  = "example-gnu.f90"

  if(access(file, " ") == 0) then
    print *, "I exist"
  end if
end program example
```

This code uses
[`access()`](https://gcc.gnu.org/onlinedocs/gfortran/ACCESS.html), a GNU
Fortran extension that compiles successfully with `gfortran`:

```txt
$ gfortran --version
GNU Fortran (Debian 14.2.0-3) 14.2.0
$ gfortran example-gnu.f90 -o example-gnu
$ ./example-gnu
 I exist
```

However, the same code fails to compile with LLVM's `flang` compiler:

```txt
$ flang-new-18 --version
Debian flang-new version 18.1.8 (9)
$ flang-new-18 example-gnu.f90 -o example-gnu
error: Semantic errors in example-gnu.f90
./example.f90:7:6: error: No explicit type declared for 'access'
    if(access(file, " ") == 0) then
       ^^^^^^
```

To resolve this issue, the programmer needs to provide a replacement for the
functionality of the GNU extension. Fortunately, the Fortran standard provides
the built-in
[`inquire`](https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2024-2/inquire.html#GUID-D0115A20-D0BD-4B0F-92A5-F6CB6D2E985C):

```fortran
! solution-gnu.f90
program solution
  implicit none
  character(len = *), parameter :: file  = "solution-gnu.f90"
  logical :: exists

  inquire(file=file, exist=exists)

  if(exists) then
    print *, "I exist"
  end if
end program solution
```

This revised code compiles and runs successfully with `flang`:

```txt
$ flang-new-18 solution-gnu.f90 -o solution-gnu
$ ./solution-gnu
 I exist
```

#### Intel Fortran extension

Consider the following code:

```fortran
! example-intel.f90
program main
  implicit none

  type :: foo_t
    integer :: x
  end type

  type(foo_t) :: foo(2)
  integer :: bar(2)

  data foo%x /1/
  data bar /1/

  print *, foo
  print *, bar
end
```

This code relies on an Intel Fortran extension that allows `DATA` statements to
partially initialize arrays or derived types. In this case, only the first
element of `foo%x` and `bar` is initialized explicitly, while the remaining
elements are implicitly zero-initialized.

When compiled with Intel's `ifx` compiler, the program produces:

```txt
$ ifx --version
ifx (IFX) 2024.0.0 20231017
Copyright (C) 1985-2023 Intel Corporation. All rights reserved.
$ ifx example-intel.f90 -o example-intel
$ ./example-intel
           1           0
           1           0
```

By contrast, compilers that follow the Fortran standard strictly reject the
code:

```txt
$ flang-new-18 --version
Debian flang-new version 18.1.8 (9)
$ flang-new-18 example-intel.f90 -o example-intel
error: Semantic errors in example-intel.f90
example-intel.f90:13:3: error: DATA statement set has no value for 'foo(2_8)%x'
    data foo%x /1/
    ^^^^^^^^^^^^^^
example-intel.f90:14:3: error: DATA statement set has no value for 'bar(2_8)'
    data bar /1/
    ^^^^^^^^^^^^
```

To make the code portable and accepted by all standard-compliant Fortran
compilers, each element of the arrays and derived types must be explicitly
initialized. The corrected version of the program is:

```fortran
! solution-intel.f90
program main
  implicit none

  type :: foo_t
    integer :: x
  end type

  type(foo_t) :: foo(2)
  integer :: bar(2)

  data foo%x /1, 0/
  data bar /1, 0/

  print *, foo
  print *, bar
end
```

Now, the `DATA` statements provide values for all elements of `foo%x` and `bar`,
eliminating any reliance on implicit zero initialization by a particular
compiler:

```txt
$ flang-new-18 solution-intel.f90 -o solution-intel
$ ./solution-intel
 1 0
 1 0
```

### Alternatives to compiler-specific extensions

The following table lists GNU intrinsic extensions and their corresponding Fortran
Standard equivalents. It also includes a subset of GNU extensions. This table is 
expected to grow as more GNU extensions, as well from other compilers, are 
included in this PWR075 documentation.

<Tabs>

<TabItem value="GNU Intrinsics Extensions">

| GNU Intrinsic Extension                                                                                                                                                       | Fortran Standard                                                                                                                   |
|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------|
| Functions for program termination and error reporting: `ABORT`, `EXIT`,`PERROR`                                                                                               | Use the generic control termination statements: `STOP` or `ERROR STOP`                                                             |
| Functions to query file status and access information: `ACCESS`, `FSTAT`, `LSTAT`, `STAT`                                                                                     | Use the generic I/O statement `INQUIRE` to check file properties                                                                   |
| Function to obtain a program call stack for debugging: `BACKTRACE`                                                                                                            | Standard Fortran doesn't have an intrinsic function to generate a backtrace                                                        |
| Non-standard Bessel functions: `BESJ0`, `DBESJ0`, `BESJ1`, `DBESJ1`, `BESY0`, `DBESY0`, `BESY1`, `DBESY1`, `BESJN`, `DBESJN`, `BESYN`, `DBESYN`                               | Use the generic intrinsic procedures: `BESSEL_J0`, `BESSEL_J1`, `BESSEL_Y0`, `BESSEL_Y1`, `BESSEL_JN(N, X)` or `BESSEL_JN(N1, N2, X)`, `BESSEL_YN (N, X)` or `BESSEL_YN (N1, N2, X)`                                                                                                                                   |
| Functions to compute the remainder for different integer precisions: `BMOD`, `IMOD`, `JMOD`, `KMOD`                                                                           | Use the generic intrinsic to compute the remainder of division: `MOD(A, P)`                                                        |
| Bitwise operations: `BNOT`, `INOT`, `JNOT`, `KNOT`, `BIAND`, `IIAND`, `JIAND`, `KIAND`,  `BIEOR`, `IIEOR`, `JIEOR`. `KIEOR`, `BIOR`, `IIOR`, `JIOR`, `KIOR`, `BSHFT`, `IISHFT`, `JISHFT`, `KISHFT`, `BSHFTC`, `IISHFTC`, `JISHFTC`, `KISHFTC`, `BBTEST`, `BITEST`, `BJTEST`, `BKTEST`, `BBCLR`, `IIBCLR`, `JIBCLEAR`, `KIBCLR`, `BBSET`, `IIBSET`, `JIBSET`, `KIBSET`, `BBITS`, `IIBITS`, `JIBITS`, `KIBITS`, `BMVBITS`, `IMVBITS`, `JMVBITS`, `KMVBITS`                                              | Use the standard generic intrinsic procedures: `NOT`, `IAND`, `IEOR`, `IOR`, `ISHFT`, `ISHFTC`, `BTEST`, `IBCLR`, `IBSET`, `IBITS` and `MVBITS`|
| Functions for system operations and environment control: `CHDIR`,`FNUM`, `ISATTY`, `SLEEP`, `CHMOD`, `ALARM`, `HOSTNM`, `KILL`, `LINK`, `SYMLNK`, `SIGNAL`, `TTYNAM`, `UMASK` | This is usually managed with interoperability of C functions                                                                       |
| Intrinsic function to manage complex numbers: `COMPLEX`                                                                                                                       | Use the standard function to construct complex numbers: `CMPLX(X [, KIND])` or `CMPLX(X [, Y, KIND])`                              |
| Non-standard trigonometric functions: `COTAN`, `DCOTAN`, `COTAND`, `DCOTAND`, `CCOTAN`, `ZCOTAN`                                                                              | Use the standard to compute the equivalents with variants of: `1.0 / TAN(X)`                                                       |
| Non-standard trigonometric functions in degrees: `DACOSD`, `DASIND`, `DATAND`, `DCOSD`, `DSIND`, `DTAND`, `DATAN2D`                                                           | Standard Fortran 2023 introduced generic trigonometric functions that accept angles in degrees: `ACOSD`, `ASIND`, `ATAND`, `ATAN2D`, `COSD`, `SIND`, `TAND`|
| Non-standard double precision hyperbolic trigonometric functions: `DACOSH`, `DASINH`, `DATANH`                                                                                | Use the generic intrinsic procedures: `ACOSH`, `ASINH`, `ATANH`                                                                    |
| Mathematical function to compute the Gamma function for double precision arguments: `DGAMMA`                                                                                  | Use the generic `GAMMA` that also accepts double precision arguments                                                               |
| Mathematical function for double precision complementary error function: `DERFC`                                                                                              | Use the generic intrinsic function for the complementary error function: `ERFC`                                                    |
| Functions for processor time measurements: `DTIME`,  `SECOND`                                                                                                                 | Use the generic intrinsic subroutine `CPU_TIME(TIME)`                                                                              |
| Functions to retrieve date and time information: `FDATE`, `IDATE`, `ITIME`, `CTIME`, `LTIME`, `GMTIME`                                                                        | Use the generic intrinsic subroutine `DATE_AND_TIME([DATE, TIME, ZONE, VALUES])`                                                   |
| Functions for low-level file input: `FGET`, `FGETC`                                                                                                                           | Use `READ` or C interoperability                                                                                                   |
| Functions to indicate integers of different precisions: `FLOATI`, `FLOATJ`, `FLOATK`                                                                                          | Use the generic `REAL(A)` function or `DBLE(A)` function if double precision is required.                                          |
| Function to flush buffered output to a file or unit: `FLUSH`                                                                                                                  | Use the standard I/O declaration `FLUSH`, not the intrinsic procedure (e.g. `FLUSH file-unit-number` or `FLUSH (flush-spec-list)`) |
| Functions for low-level file output: `FPUT`, `FPUTC`                                                                                                                          | Use `WRITE` or C interoperability                                                                                                  |   
| Function to release an allocated or opened Fortran unit: `FREE`                                                                                                               | Use the generic statement to release allocated memory for allocatable arrays or pointers: `DEALLOCATE`                             |
| Function for low-level file positioning: `FSEEK`                                                                                                                              | Use the generic `REWIND`, `BACKSPACE` or `POS=` specifier for file positioning                                                     |
| Function to obtain the current file position: `FTELL`                                                                                                                         | Use `POS=` specifier in the `INQUIRE` statement                                                                                    |
| Function to report the last runtime I/O or system error: `GERROR`                                                                                                             | Use the generic specifier for retrieving error messages from I/O operations: `IOMSG=/`                                             |
| Functions to access command-line arguments: `GETARG`, `IARGC`                                                                                                                 | Use the generic intrinsic procedures `GET_COMMAND_ARGUMENT` and `COMMAND_ARGUMENT_COUNT`                                           |
| Functions to access environment variables and logical I/O: `GETENV`, `GETLOG`                                                                                                 | Use the generic intrinsic subroutine `GET_ENVIRONMENT_VARIABLE`                                                                    |
| Function to retrieve the system error number: `IERRNO`                                                                                                                        | Use the generic specifier for error and status handling in I/O statements: `IOSTAT=/`                                              |
| Function to extract the imaginary part of a complex number: `IMAGPART`                                                                                                        | Use the generic intrinsic function `AIMAG(Z)` that returns the imaginary part of a complex number                                  |
| Types to convert values to integers of different precisions: `INT2`, `INT8`                                                                                                   | Use the generic intrinsic function `INT(A, KIND)` along with standard kind type parameters (e.g. `C_INT16_T` or `C_INT64_T`)       |
| Mathematical functions to compute the natural logarithm of the Gamma function: `LGAMMA`, `ALGAMA`, `DLGAMA`                                                                   | Use the generic intrinsic function `LOG_GAMMA`                                                                                     |
| Function to find the last non-blanck character in a string: `LNBLNK`                                                                                                          | Use the generic intrinsic function `LEN_TRIM(STRING [, KIND])`                                                                     |
| Functions for generating random numbers: `RAND`, `RAN`,`IRAND`, `SRAND`                                                                                                       | Use the generic intrinsic to generate pseudorandom numbers: `RANDOM_NUMBER`                                                        |
| Function to extract the real part of a complex number: `REALPART`                                                                                                             | Use the generic intrinsic function `REAL(A [, KIND])` or `DBLE(A)` if double precision is required to obtain the real part         |
| Function to execute a system command from Fortran: `SYSTEM`                                                                                                                   | Use the generic intrinsic subroutine `EXECUTE_COMMAND_LINE`                                                                        |
| Functions for time measurement and system time: `TIME`, `TIME8`, `MCLOCK`, `MCLOCK8`, `SECNDS`                                                                                | Use the generic intrinsic subroutine `SYSTEM_CLOCK([COUNT, COUNT_RATE, COUNT_MAX])` that checks the system clock                   |
| Function to delete a file from the filesystem: `UNLINK`                                                                                                                       | Use `CLOSE` with `STATUS='DELETE'` or C interoperability                                                                           |
| Functions for operations on complex numbers: `ZCOS`, `ZEXP`, `ZLOG`, `ZSIN`, `ZSQRT`                                                                                          | Use the generic intrinsic procedures as they accept arguments of complex type, including double complex type: `COS(X)`, `EXP(X)`, `LOG(X)`, `SIN(X)`, `SQRT(X)`|
 
</TabItem>

<TabItem value="GNU Extensions">

| GNU Extension                                                                         | Fortran Standard                                                             |
|---------------------------------------------------------------------------------------|------------------------------------------------------------------------------|
| Real Array Indices: This allows the use of real type expressions for array indexing   | Fortran Standard requires that array subscripts are integer type expressions | 

</TabItem>

</Tabs>

### Related resources

- [PWR075 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR075/)

### References

- ["Extensions (The GNU Fortran
Compiler)"](https://gcc.gnu.org/onlinedocs/gfortran/Extensions.html), Free
  Software Foundation, Inc. [last checked May 2025]

- ["Additional Language
Features"](https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2025-1/additional-language-features.html),
Intel Corporation. [last checked May 2025]

- ["HPE Cray Fortran Language
Extensions"](https://support.hpe.com/hpesc/public/docDisplay?docId=dp00004438en_us&docLocale=en_US),
Hewlett Packard Enterprise Development LP. [last checked May 2025]

- ["Fortran 2023 Interpretation Document"](https://j3-fortran.org/doc/year/24/24-007.pdf), 
ISO/IEC JTC 1 [WD 1539-1, 18th December 2023]
