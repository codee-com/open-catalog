# PWR016: Use separate arrays instead of an Array-of-Structs

### Issue

Using separate arrays instead of an Array-of-Structs is recommended to maximize
[locality of reference](../../Glossary/Locality-of-reference.md).

### Actions

Convert the Array-of-Structs (AoS) into separate plain arrays.

### Relevance

Using an Array-of-Structs (AoS) leads to inefficient use of the memory subsystem
unless data from all of its fields is used. One example case where using an AoS
is justified would be iterating over an array of points, each point being a
struct containing the coordinates that are consumed on each iteration. However,
most structs contain fields that will not be accessed every time: data locality
can be enhanced by breaking the struct and creating an array for each individual
field.

### Code example

#### C

The following example shows a loop processing the `x` and `y` coordinates for an
array of points:

```c
typedef struct {
  int x;
  int y;
  int z;
} point;

void example() {
  point points[1000];
  for (int i = 0; i < 1000; i++) {
    points[i].x = 1;
    points[i].y = 1;
  }
}
```

This could seem like an example where using an Array-of-Structs is justified.
However, since the `z` coordinate is never accessed, the memory subsystem is
not used optimally. This could be avoided by creating one array for each
coordinate:

```c
void example() {
  int points_x[1000];
  int points_y[1000];
  int points_z[1000];
  for (int i = 0; i < 1000; i++) {
    points_x[i] = 1;
    points_y[i] = 1;
  }
}
```

#### Fortran

The following example shows a loop processing the `x` and `y` coordinates for
an array of points:

```fortran
program main
  implicit none

  type point
    integer :: x
    integer :: y
    integer :: z
  end type point

contains

  subroutine foo()
    implicit none
    type(point) :: points(1000)
    integer :: i

    do i = 1, 1000
      points(i)%x = 1
      points(i)%y = 1
    end do
  end subroutine foo

end program main
```

This could seem like an example where using an Array-of-Structs is justified.
However, since the `z` coordinate is never accessed, the memory subsystem is
not used optimally. This could be avoided by creating one array for each
coordinate:

```fortran
subroutine foo()
  implicit none
  integer :: points_x(1000), points_y(1000), points_z(1000)
  integer :: i

  do i = 1, 1000
    points_x(i) = 1
    points_y(i) = 1
  end do
end subroutine foo
```

### Related resources

* [PWR016 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR016/)

### References

* [AoS and SoA](https://en.wikipedia.org/wiki/AoS_and_SoA)
