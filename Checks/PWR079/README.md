# PWR079: Avoid undefined behavior due to uninitialized variables

### Issue

Accessing an uninitialized variable can lead to undefined behavior due to its
indeterminate value.

### Actions

Always initialize variables before using them, helping ensure deterministic
behavior and prevent bugs in the code.

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

Consider the following code, which aims to sum the elements of an array:

```c {5} showLineNumbers
// example_array.c
#include <stdio.h>

__attribute__((pure)) double sum_array(double *array, size_t size) {
  double sum;

  for (size_t i = 0; i < size; ++i) {
    sum += array[i];
  }

  return sum;
}

int main() {
  double array[] = {0.24, 0.33, 0.17, 0.89, 0.05};
  printf("Sum is: %f\n", sum_array(array, 5));

  return 0;
}
```

Note how `sum`, an automatic variable, is never explicitly initialized.
Although it might seem like it "should" logically start at `0`, the C standard
does not guarantee this. Thus, the initial value of `sum` is indeterminate,
leading to different outcomes depending on the compiler and its settings:

- For instance, `gcc -O2` appears to start `sum` at `0`, allowing the program
  to work as intended:

```txt
$ gcc --version
gcc (Debian 14.2.0-8) 14.2.0
$ gcc -O2 example_array.c -o example_array_gcc
$ ./example_array_gcc 
Sum is: 1.680000
```

- However, with `clang -O2`, `sum` appears to contain arbitrary data, leading
  to incorrect results:

```txt
$ clang --version
Debian clang version 19.1.5 (1)
$ clang -O2 example_array.c -o example_array_clang
$ ./example_array_clang 
Sum is: nan
```

The solution is straightforward, always initialize variables before using them:

```c
double sum = 0.0;
```

This principle applies to all variable types, including other elemental types
like `int`, `struct` elements, and pointers.

Pointers are particularly important in C, as they are commonly used to
represent n-dimensional arrays. However, uninitialized pointers can easily lead
to invalid memory accesses and program crashes. Let's consider another example
code with pointers, where a computational function ensures the received
pointers are valid before accessing their contents:

```c {14} showLineNumbers
// example_matrix.c
#include <stdio.h>

void perform_computation(double *matrix_a, double *matrix_b) {
  if (matrix_a == NULL || matrix_b == NULL) {
    printf("A matrix is NULL; skipping computation\n");
    return;
  }

  printf("Performing computation...\n");
}

int main() {
  double *matrix_a, *matrix_b;
  perform_computation(matrix_a, matrix_b);

  return 0;
}
```

Note how `matrix_a` and `matrix_b` are never initialized. Although this example
may seem trivial, similar scenarios can occur in large, complex codebases where
pointers traverse multiple functions and conditional logic, making such issues
hard to diagnose and correct.

Since the contents of the pointers are indeterminate, we can obtain different
results depending on the compiler and settings:

- `gcc -O2` appears to set the pointers to `NULL`, preventing the computation:

```txt
$ gcc -O2 example_matrix.c -o example_matrix_gcc
$ ./example_matrix_gcc 
A matrix is NULL; skipping computation
```

- However, with `clang -O2`, the pointers seem to hold arbitrary values,
  allowing the computation to proceed and likely crash due to invalid memory
  accesses later on:

```txt
$ clang -O2 example_matrix.c -o example_matrix_clang
$ ./example_matrix_clang 
Performing computation...
```

To help prevent these types of issues, it's a good practice to initialize
pointers to `NULL` by default if they aren't assigned at their declaration:

```c
double *matrix_a = NULL, *matrix_b = NULL;
```

#### Fortran

Consider the following code, which aims to sum the elements of an array:

```fortran {16} showLineNumbers
! example_array.f90
program main
  use iso_fortran_env, only: real32
  implicit none

  real(kind=real32) :: array(5)
  array = [0.24, 0.33, 0.17, 0.89, 0.05]

  print *, "Sum is:", sum_array(array)

contains

  pure real(kind=real32) function sum_array(array)
    implicit none
    real(kind=real32), intent(in) :: array(:)
    real(kind=real32) :: sum
    integer :: i

    do i = 1, size(array, 1)
      sum = sum + array(i)
    end do

    sum_array = sum
  end function sum_array

end program main
```

Note how `sum` is never explicitly initialized. Although it might seem like it
"should" logically start at `0`, the Fortran standard does not guarantee this.
Thus, the initial value of `sum` is indeterminate, leading to different
outcomes depending on the compiler and its settings:

- For instance, `gfortran -O2` appears to start `sum` at `0`, allowing the
  program to work as intended:

```txt
$ gfortran --version
GNU Fortran (Debian 14.2.0-8) 14.2.0
$ gfortran -O2 example_array.f90 -o example_array_gfortran
$ ./example_array_gfortran 
 Sum is:   1.67999995
```

- However, with `flang -O2`, `sum` appears to contain arbitrary data, leading
  to incorrect results:

```txt
$ flang-new --version
Debian flang-new version 19.1.5 (1)
$ flang-new -O2 example_array.f90 -o example_array_flang
$ ./example_array_flang 
 Sum is: NaN
```

The solution is straightforward, always initialize variables before using them:

```fortran
real(kind=real32) :: sum

sum = 0
```

This principle applies to all variable types, including other elemental types
like `integer`, derived types, and arrays.

Arrays are a critical part of simulation codes. For managing dynamic,
n-dimensional arrays in Fortran, both `pointer` and `allocatable` variables are
available. The latter, introduced in Fortran 2003, are generally safer and more
robust. Unlike `pointer`, variables with the `allocatable` attribute
automatically free their memory and are always set by default to the
`unallocated` state.

For example, the following code technically leads to undefined behavior because
a `pointer` is used without explicit initialization:

```fortran
program main
  implicit none
  integer, pointer :: array(:)

  if (.not. associated(array)) then
    print *, "Undefined behavior"
  end if
end program main
```

In contrast, an `allocatable` array can always be safely checked using the
`allocated` function, even when not explicitly initialized:

```fortran
program main
  implicit none
  integer, allocatable :: array(:)

  if (.not. allocated(array)) then
    print *, "Defined behavior"
  end if
end program main
```

While these examples may seem trivial, these types of issues can arise in
large, complex codebases where variables traverse multiple procedures and are
subject to intricate conditional logic.

If, for any reason, you still need to use `pointer` variables, it's a good
practice to nullify them as early as possible for additional safety. For module
variables and derived type initializations, you can nullify right at the point
of declaration:

```fortran
type :: t
  integer, pointer :: array(:) => null()
end type
```

But within procedures, it's best to declare the pointer variable first and then
nullify it. This avoids inadvertently introducing an implicit `save` behavior:

```fortran
integer, pointer :: array(:)

nullify(array)
```

> [!TIP]
> Refer to [PWR072](/Checks/PWR072/) for more information on the dangers of the
> implicit `save` behavior.

### Related resources

- [PWR079
  examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR079/)

### References

- ["Fortran 2023 Interpretation
Document"](https://j3-fortran.org/doc/year/24/24-007.pdf), Technical Committee
ISO/IEC JTC1/SC22/WG5. [last checked December 2024]

- ["Undefined Variables - Fortran
Discourse"](https://fortran-lang.discourse.group/t/undefined-variables/3708/12),
Fortran Community. [last checked December 2024]

- ["C -
Initialization"](https://en.cppreference.com/w/c/language/initialization),
cppreference.com. [last checked December 2024]

- ["What happens to a declared, uninitialized variable in C? Does it have a
value?"](https://stackoverflow.com/questions/1597405/what-happens-to-a-declared-uninitialized-variable-in-c-does-it-have-a-value),
Stack Overflow Community. [last checked December 2024]

- ["C++ -
Default-initialization"](https://en.cppreference.com/w/cpp/language/default_initialization),
cppreference.com. [last checked December 2024]

- ["Code Gen Options (The GNU Fortran
Compiler)"](https://gcc.gnu.org/onlinedocs/gfortran/Code-Gen-Options.html),
  Free Software Foundation, Inc. [last checked December 2024]

- ["Optimize Options (Using the GNU Compiler
  Collection)"](https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html#index-ftrivial-auto-var-init),
  Free Software Foundation, Inc. [last checked December 2024]

- ["Allocatable Arrays -- Fortran Programming
Language"](https://fortran-lang.org/en/learn/best_practices/allocatable_arrays/),
Fortran Community. [last checked December 2024]

- ["Understanding Fortran
pointers"](https://stackoverflow.com/questions/28929053/understanding-fortran-pointers),
Stack Overflow Community. [last checked December 2024]

- ["Difference between nullify(pointer) and pointer =>
null()"](https://stackoverflow.com/questions/26675185/difference-between-nullifypointer-and-pointer-null),
Stack Overflow Community. [last checked December 2024]
