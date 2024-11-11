# PWR029: Remove integer increment preventing performance optimization

### Issue

A loop contains an integer increment statement enabling access to the elements
of an array, but the integer increment introduces dependencies between the
iterations of the loop that defeat performance optimization.

### Actions

Remove the integer increment statement from the loop body, updating the array
references in the loop body and recalculating the index expressions
correspondingly.

### Relevance

Performance optimization best practices give recommendations regarding the
efficient [memory access pattern](../../Glossary/Memory-access-pattern.md), but do
not give recommendations regarding a coding style that favors performance
optimizations for the compiler.

Programmers may write programs where the array references are driven by the loop
index variable or by another integer variable incremented in the loop body. From
a memory access pattern perspective, both ways of coding lead to accessing the
same data in the same order at run-time. However, there is a fundamental
difference from the point of view of the instructions executed by the processor:
using another integer variable incremented in the loop body introduces
dependencies between the iterations of the loop.

Programmers may write different programs, and tools are expected to detect these
challenging situations, help programmers to understand the implications from the
performance viewpoint, and suggest how to overcome these issues through
alternative ways of coding that are more hardware-friendly.

### Code example

#### C

In this example, the access to array `a` using the variable `k` can be
challenging to optimize for some compilers:

```c
void example(float *a, float *b, unsigned size) {
  unsigned k = 0;
  for (unsigned i = 0; i < size; i++) {
    b[i] = a[k] + 1;
    k = k + 1;
  }
}
```

Since `k == i` in this context, we can fix the issue by removing the variable
`k` altogether and the corresponding increment statement:

```c
for (unsigned i = 0; i < size; i++) {
  b[i] = a[i] + 1;
}
```

#### Fortran

In this example, the access to array `a` using the variable `k` can be
challenging to optimize for some compilers:

```fortran
subroutine example(a, b)
  real, intent(in) :: a
  real, intent(out) :: b
  integer :: i, k

  k = 1
  do i = 1, size(b, 1)
    b(i) = a(k) + 1
    k = k + 1
  end do
end subroutine example
```

Since `k == i` in this context, we can fix the issue by removing the variable
`k` altogether and the corresponding increment statement:

```fortran
subroutine example(a, b)
  real, intent(in) :: a
  real, intent(out) :: b
  integer :: i

  do i = 1, size(b, 1)
    b(i) = a(i) + 1
  end do
end subroutine example
```

### Related resources

* [PWR029 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR029/)
