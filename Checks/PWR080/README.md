# PWR080: Conditionally initialized variables can lead to undefined behavior

### Issue

A variable is initialized only under certain conditions. If there are control
flow paths that read the variable when uninitialized, this can lead to
undefined behavior due to its indeterminate value.

### Actions

To prevent bugs in the code, ensure the problematic variable is initialized in
all possible code paths. It may help to add explicit `else` or `default`
branches in control-flow blocks, or even set a default initial value
inmediately after declaring the variable.

### Relevance

Some programming languages automatically initialize variables to default
values, but Fortran, C, and C++ often do not. Consequently, variables left
uninitialized contain unpredictable data until they are explicitly set by the
programmer:

- **Fortran:** Reading any variable that has not been explicitly initialized
  results in undefined behavior.

- **C:** Automatic (i.e., declared within functions) variables are not
  initialized by default. However, `static`, `thread_local`, and file-scope
  variables are zero-initialized (e.g., scalar values are set to `0`).

- **C++:** Similar to C, with additional rules for default initialization of
  class-like objects and array elements in certain contexts.

Since uninitialized variables may contain any arbitrary values, reading and
using them can lead to undefined behavior, potentially causing incorrect
results, crashes, or other unintended outcomes. Compilers are not required to
warn about these issues and, even if they do, they typically still allow the
code to compile and run.

Some compilers may appear to "help" by zero-initializing variables under
certain conditions (e.g., specific compilation flags). While this can make
technically incorrect code run as originally intended, relying on such
incidental behavior creates a false sense of security and masks underlying
logical errors. Ultimately, this can vanish under different compilation
settings and can vary between compilers.

Lastly, while some compilers provide options to automatically initialize
certain data types (e.g., `gfortran`'s `-finit-integer=<value>` or `gcc`'s
`-ftrivial-auto-var-init=<value>`), these features reduce portability to other
development environments and hide problems in the code rather than addressing
them.

### Code examples

#### C

Consider the following code, which sums the elements of an array after applying
the specified transformation:

```c {8,24} showLineNumbers
// example.c
#include <stdio.h>
#include <string.h>

double transform_and_sum(const double *array, size_t size, const char *option) {
  double sum = 0.0;

  double factor;
  if (strcmp(option, "half") == 0) {
    factor = 0.5;
  } else if (strcmp(option, "double") == 0) {
    factor = 2.0;
  }

  for (size_t i = 0; i < size; ++i) {
    sum += array[i] * factor;
  }

  return sum;
}

int main() {
  double array[] = {0.25, 0.25, 0.25, 0.25};
  printf("Sum is: %f\n", transform_and_sum(array, 4, "unknownOption"));

  return 0;
}
```

Note how `factor`, an automatic variable, is only explicitly initialized when
the received `option` is known. Since the C standard does not guarantee any
specific initial value, the state of `factor` is indeterminate in the previous
scenario (using an `unknownOption`), leading to different outcomes depending on
the compiler and its settings:

- For instance, `gcc -O2` behaves as if the `if` branch was taken:

```txt
$ gcc --version
gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
$ gcc -O2 example.c -o example_gcc
$ ./example_gcc 
Sum is: 0.500000
```

- However, `clang -O2` behaves as if the `else if` branch was taken:

```txt
$ clang --version
Ubuntu clang version 18.1.3 (1ubuntu1)
$ clang -O2 example.c -o example_clang
$ ./example_clang 
Sum is: 2.000000
```

It is important to note that the compiler is not "randomly choosing" a branch
to execute. Instead, an undefined behavior means the outcome of the code is
unpredictable, and could even lead to completely incorrect results or crashes.
In other words, code with undefined behavior should never be relied upon.

To prevent these problems, we must always initialize variables before using
them. This principle applies to all variable types, including other elemental
types like `int`, `struct` elements, and pointers.

There are multiple ways to solve the bug. For example, we can simply initialize
`factor` with a default value after its declaration, ensuring it is always
defined regardless of the received `option`:

```c
// solution.c
// Identity transformation by default
double factor = 1.0;
```

#### Fortran

Consider the following code, which sums the elements of an array after applying
the specified transformation:

```fortran {9,20} showLineNumbers
! example.f90
program main
  use iso_fortran_env, only: real32
  implicit none

  real(kind=real32) :: array(4)
  array = [0.25, 0.25, 0.25, 0.25]

  print *, "Sum is:", transform_and_sum(array, "unknownOption")

contains

  real(kind=real32) function transform_and_sum(array, option)
    implicit none

    real(kind=real32), intent(in) :: array(:)
    character(len=*), intent(in) :: option

    real(kind=real32) :: sum
    real(kind=real32) :: factor
    integer :: i

    sum = 0.0

    if (option == "half") then
      factor = 0.5
    else if (option == "double") then
      factor = 2.0
    end if

    do i = 1, size(array, 1)
      sum = sum + array(i) * factor
    end do

    transform_and_sum = sum
  end function transform_and_sum

end program main
```

Note how `factor` is only explicitly initialized when the received `option` is
known. Since the Fortran standard does not guarantee any specific initial
value, the state of `factor` is indeterminate in the previous scenario (using
an `unknownOption`), leading to different outcomes depending on the compiler
and its settings:

- For instance, `gfortran -O2` behaves as if the `if` branch was taken:

```txt
$ gfortran --version
GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
$ gfortran -O2 example.f90 -o example_gfortran
$ ./example_gfortran 
 Sum is:   0.500000
```

- However, `flang -O2` behaves as if the `else if` branch was taken:

```txt
$ flang-new --version
Ubuntu flang-new version 18.1.3 (1ubuntu1)
$ flang-new -O2 example.f90 -o example_flang
$ ./example_flang 
 Sum is: 2.
```

It is important to note that the compiler is not "randomly choosing" a branch
to execute. Instead, an undefined behavior means the outcome of the code is
unpredictable, and could even lead to completely incorrect results or crashes.
In other words, code with undefined behavior should never be relied upon.

To prevent these problems, we must always initialize variables before using
them. This principle applies to all variable types, including other elemental
types like `integer`, derived types, and arrays.

There are multiple ways to solve the bug. For example, we can simply initialize
`factor` with a default value after its declaration, ensuring it is always
defined regardless of the received `option`:

```fortran
! solution.f90
real(kind=real32) :: factor

! Identity transformation by default
factor = 1.0
```

> [!TIP]
> Refer to [PWR072](/Checks/PWR072/) for more information on how to safely
> initialize variables after their declaration.

### Related resources

- [PWR080
  examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR080/)

### References

- ["Fortran 2023 Interpretation
Document"](https://j3-fortran.org/doc/year/24/24-007.pdf), Technical Committee
ISO/IEC JTC1/SC22/WG5. [last checked October 2025]

- ["Undefined Variables - Fortran
Discourse"](https://fortran-lang.discourse.group/t/undefined-variables/3708),
Fortran Community. [last checked October 2025]

- ["C -
Initialization"](https://en.cppreference.com/w/c/language/initialization),
cppreference.com. [last checked October 2025]

- ["What happens to a declared, uninitialized variable in C? Does it have a
value?"](https://stackoverflow.com/questions/1597405/what-happens-to-a-declared-uninitialized-variable-in-c-does-it-have-a-value),
Stack Overflow Community. [last checked October 2025]

- ["C++ -
Default-initialization"](https://en.cppreference.com/w/cpp/language/default_initialization),
cppreference.com. [last checked October 2025]

- ["Code Gen Options (The GNU Fortran
Compiler)"](https://gcc.gnu.org/onlinedocs/gfortran/Code-Gen-Options.html),
  Free Software Foundation, Inc. [last checked October 2025]

- ["Optimize Options (Using the GNU Compiler
  Collection)"](https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html#index-ftrivial-auto-var-init),
  Free Software Foundation, Inc. [last checked October 2025]
