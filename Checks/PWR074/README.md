# PWR074: Pass only required fields from derived type as arguments to increase code clarity

### Issue

Pass only used fields from derived data types as arguments to increase code
clarity.

### Actions

Pass the used fields as separate arguments instead of the whole derived type.

### Relevance

Derived data types are convenient constructs to group and move around related
variables. While in many cases this is an effective method to organize data, it
can also obscure a function's purpose and introduce unneeded dependencies.

This is specifically the case for _Plain Old Data_ types, such as C `struct`s
or Fortran derived types that do not use the Object-Oriented features available
since Fortran 2003. In C++ or Fortran code with an Object-Oriented design,
a better alternative is to use encapsulation to avoid depending on the
implementation of the type.

Functions having these _Plain Old Data_ types used as arguments should make
use of most if not all its fields. This promotes data hiding, makes inputs and
outputs more explicit and helps to prevent unintended variable modifications.

> [!NOTE]
> This issue can also impact optimization. See check
> [PWR012](../PWR012/README.md) for more details.

### Code example

#### C

In the following example, a struct containing two arrays is passed to the `foo`
function, which only uses one of the arrays:

```c
// example.c
#include <stdlib.h>

typedef struct {
  int A[1000];
  int B[1000];
} data;

__attribute__((pure)) int foo(const data *d) {
  int result = 0;
  for (int i = 0; i < 1000; i++) {
    result += d->A[i];
  }
  return result;
}

void example() {
  data *d = (data *)malloc(sizeof(data));
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
// solution.c
#include <stdlib.h>

typedef struct {
  int A[1000];
  int B[1000];
} data;

__attribute__((pure)) int foo(const int *A) {
  int result = 0;
  for (int i = 0; i < 1000; i++) {
    result += A[i];
  }
  return result;
}

void solution() {
  data *d = (data *)malloc(sizeof(data));
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

```fortran
! example.f90
program example

  implicit none

  type data
    integer :: a(10)
    integer :: b(10)
  end type data

contains

  pure subroutine foo(d)
    implicit none
    type(data), intent(in) :: d
    integer :: i, sum

    sum = 0
    do i = 1, 10
      sum = sum + d%a(i)
    end do
  end subroutine foo

  pure subroutine bar()
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

```fortran
! solution.f90
program solution

  implicit none

  type data
    integer :: a(10)
    integer :: b(10)
  end type data

contains

  pure subroutine foo(a)
    implicit none
    integer, intent(in) :: a(:)
    integer :: i, sum

    sum = 0
    do i = 1, size(a, 1)
      sum = sum + a(i)
    end do
  end subroutine foo

  pure subroutine bar()
    implicit none
    type(data) :: d
    integer :: i

    do i = 1, 10
      d%a(i) = 1
      d%b(i) = 1
    end do

    call foo(d%a)
  end subroutine bar

end program solution
```

### Related resources

* [PWR074 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR074/)

* [PWR001: Declare global variables as function arguments](../PWR001/README.md)

* [PWR002: Declare scalar variables in the smallest possible scope](../PWR002/README.md)

* [PWR008: Declare the intent for each procedure argument](../PWR008/README.md)

### References

* [Plain old data structures](https://en.wikipedia.org/wiki/Passive_data_structure)
[last checked February 2026]

* [Encapsulation](https://en.wikipedia.org/wiki/Encapsulation_(computer_programming))
[last checked February 2026]

* [Data Hiding in C - Stephen Friederichs](https://www.embeddedrelated.com/showarticle/166.php)
[last checked October 2020]

* [Information hiding](https://en.wikipedia.org/wiki/Information_hiding)
[last checked October 2020]
