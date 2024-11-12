# PWR075: Avoid using GNU Fortran extensions

### Issue

Using compiler-specific extensions, such as those provided by GNU's `gfortran`,
compromises the portability and consistency of code across different
development environments.

### Actions

Refactor the code to replace existing GNU Fortran extensions with
standard-compliant Fortran code. The specific steps for refactoring will depend
on the particular features being replaced.

### Relevance

Fortran compilers, including GNU, often extend their support beyond the Fortran
standard by introducing additional language features. While these extensions
can provide enhanced functionality and convenience for programmers, they also
present significant drawbacks.

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

For more information on which extensions are supported by GNU's `gfortran`,
refer to the following resources:

- [Extensions implemented in GNU
  Fortran](https://gcc.gnu.org/onlinedocs/gfortran/Extensions-implemented-in-GNU-Fortran.html).
- [Extensions not implemented in GNU
  Fortran](https://gcc.gnu.org/onlinedocs/gfortran/Extensions-not-implemented-in-GNU-Fortran.html).

### Code examples

Consider the following code, which checks if the file itself exists:

```f90
! example.f90
program example
  implicit none
  character(len = *), parameter :: file  = "example.f90"

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
$ gfortran example.f90 -o example
$ ./example
 I exist
```

However, the same code fails to compile with LLVM's `flang` compiler:

```txt
$ flang-new-18 --version
Debian flang-new version 18.1.8 (9)
$ flang-new-18 example.f90 -o example
error: Semantic errors in example.f90
./example.f90:7:6: error: No explicit type declared for 'access'
    if(access(file, " ") == 0) then
       ^^^^^^
```

To resolve this issue, the programmer needs to provide a replacement for the
functionality of the GNU extension. Fortunately, the Fortran standard provides
the built-in
[`inquire`](https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2024-2/inquire.html#GUID-D0115A20-D0BD-4B0F-92A5-F6CB6D2E985C):

```f90
! solution.f90
program solution
  implicit none
  character(len = *), parameter :: file  = "solution.f90"
  logical :: exists

  inquire(file=file, exist=exists)

  if(exists) then
    print *, "I exist"
  end if
end program solution
```

This revised code compiles and runs successfully with `flang`:

```txt
$ flang-new-18 solution.f90 -o solution
$ ./solution
 I exist
```

### Related resources

- [PWR075 examples](../PWR075/)

### References

- ["Extensions (The GNU Fortran
Compiler)"](https://gcc.gnu.org/onlinedocs/gfortran/Extensions.html), Free
  Software Foundation, Inc. [last checked August 2024]

- ["Additional Language
Features"](https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2024-2/additional-language-features.html#dropdown-1-0-3-18),
Intel Corporation. [last checked August 2024]

- ["HPE Cray Fortran Language
Extensions"](https://support.hpe.com/hpesc/public/docDisplay?docId=dp00004438en_us&docLocale=en_US),
Hewlett Packard Enterprise Development LP. [last checked August 2024]
