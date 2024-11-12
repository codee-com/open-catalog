# PWR012: Pass only required fields from derived data types as parameters

### Issue

Pass only used fields from derived data types as parameters to promote data
hiding.

### Actions

Pass the used fields as separate parameters instead of the whole derived type.

### Relevance

Derived data types (such as structs in C) are convenient constructs to group and
move around related variables. While in many cases this is an effective method
to organize data, the compilers can have a hard time optimizing this code
because increased visibility of data also renders optimizations more complex.

Functions having derived data types used as parameters should make use of most
if not all its fields. Ensuring that all fields from derived types passed as
function parameters are used in the function body has several benefits: promotes
data hiding, makes inputs and outputs more explicit, helps to prevent unintended
variable modifications, and also contributes to improve compiler and static
analyzer code coverage.

In parallel programming, derived data types are often discouraged when
offloading to the GPU  because they may inhibit compiler analyses and
optimizations due to [pointer aliasing](../../Glossary/Pointer-aliasing.md). Also, it
can cause unnecessary data movements impacting performance or incorrect data
movements impacting correctness and even crashes impacting code quality.

### Code example

#### C

In the following example, a struct containing two arrays is passed to the `foo`
function, which only uses one of the arrays:

```c
#include <stdlib.h>

typedef struct {
  int A[1000];
  int B[1000];
} data;

int foo(data *d) {
  int result = 0;
  for (int i = 0; i < 1000; i++) {
    result += d->A[i];
  }
  return result;
}

void example() {
  data *d = (data *) malloc(sizeof(data));
  for (int i = 0; i < 1000; i++) {
    d->A[i] = d->B[i] = 1;
  }
  int result = foo(d);
  free(d);
}
```

This can be easily addressed by only passing the required array and rewriting
the function body accordingly:

```c
#include <stdlib.h>

typedef struct {
  int A[1000];
  int B[1000];
} data;

int foo(int *A) {
  int result = 0;
  for (int i = 0; i < 1000; i++) {
    result += A[i];
  }
  return result;
}

void example() {
  data *d = (data *) malloc(sizeof(data));
  for (int i = 0; i < 1000; i++) {
    d->A[i] = d->B[i] = 1;
  }
  int result = foo(d->A);
  free(d);
}
```

#### Fortran

In the following example, a derived type containing two arrays is passed to the
`foo` function, which only uses one of the arrays:

```f90
program example

  implicit none

  type data
    integer :: a(10)
    integer :: b(10)
  end type data

contains

  subroutine foo(d)
    implicit none
    type(data), intent(in) :: d
    integer :: i, sum

    do i = 1, 10
      sum = sum + d%a(i)
    end do
  end subroutine foo

  subroutine bar()
    implicit none
    type(data) :: d
    integer :: i

    do i = 1, 10
      d%a(i) = 1
      d%b(i) = 1
    end do

    call foo(d)
  end subroutine bar

end program example
```

This can be easily addressed by only passing the required array and rewriting
the procedure body accordingly:

```f90
program example

  implicit none

  type data
    integer :: a(10)
    integer :: b(10)
  end type data

contains

  subroutine foo(a)
    implicit none
    integer, intent(in) :: a(:)
    integer :: i, sum

    do i = 1, size(a, 1)
      sum = sum + a(i)
    end do
  end subroutine foo

  subroutine bar()
    implicit none
    type(data) :: d
    integer :: i

    do i = 1, 10
      d%a(i) = 1
      d%b(i) = 1
    end do

    call foo(d%a)
  end subroutine bar

end program example
```

### Related resources

* [PWR012 examples](../PWR012/)

* [PWR001: Declare global variables as function parameters](../PWR001/README.md)

* [PWR002: Declare scalar variables in the smallest possible scope](../PWR002/README.md)

* [PWR008: Declare the intent for each procedure parameter](../PWR008/README.md)

* [PWD006: Missing deep copy of non-contiguous data to the GPU](../PWD006/README.md)

### References

* [Data Hiding in C - Stephen Friederichs](https://www.embeddedrelated.com/showarticle/166.php)
[last checked October 2020]

* [Information hiding](https://en.wikipedia.org/wiki/Information_hiding)
[last checked October 2020]
