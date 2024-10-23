# PWD004: Out-of-memory-bounds array access

### Issue

A position outside the bounds of the array memory is being accessed which
results in undefined behavior most likely causing invalid memory accesses and
crashes.

### Actions

Fix the array access so that only positions within the array memory bounds are
accessed.

### Relevance

An array is essentially a collection of items that can be randomly accessed
through an integer index. Obviously, only a subset of the possible integer
values will correspond to array element positions; accessing an array using
index values outside that subset will access a memory position not associated
with any array element. This is called an out-of-memory-bounds access and has
undefined behavior in C/C++, most likely causing invalid memory accesses and
crashes.

### Code example

#### C

The following code uses an integer index ranging from `1` to `100` to access
the array `A`:

```c
void foo() {
  int A[100];
  for (int i = 0; i < 100; i++) {
    A[i + 1] = 1;
  }
}
```

This is incorrect since the array positions in C range from `0` to `99`. Thus,
the array accesses must be fixed to prevent invalid memory accesses during
runtime. For instance, we can modify the array reference from `A[i + 1]` to
`A[i]`:

```c
void foo() {
  int A[100];
  for (int i = 0; i < 100; i++) {
    A[i] = 1;
  }
}
```

#### Fortran

The following code uses an integer index ranging from `2` to `101` to access
the array `A`:

```f90
subroutine example()
  integer :: A(100)
  integer :: i

  do i = 1, size(A, 1)
    A(i + 1) = 1
  end do
end subroutine example
```

This is incorrect since the array positions in Fortran range from `1` to `100`.
Thus, the array accesses must be fixed to prevent invalid memory accesses
during runtime. For instance, we can modify the array reference from `A(i + 1)`
to `A(i)`:

```f90
subroutine example()
  integer :: A(100)
  integer :: i

  do i = 1, size(A, 1)
    A(i) = 1
  end do
end subroutine example
```

### Related resources

* [PWD004 examples](../PWD004)

### References

* [CWE - CWE-125: Out-of-bounds Read (4.2)](https://cwe.mitre.org/data/definitions/125.html)
[last checked October 2020]

* [Index checking - Bounds checking](https://en.wikipedia.org/wiki/Bounds_checking#Index_checking)
[last checked October 2020]
