# PWD005: Array range copied to or from the GPU does not cover the used range

### Issue

Only a part of an array was transferred to the GPU and it does not cover the
used range.

### Actions

Update the copied array range to match the actual array usage in the code.

### Relevance

Minimising data transfers is one of the main optimization points when offloading
computations to the GPU. An opportunity for such optimization occurs whenever
only part of an array is required in a computation. In such cases, only a part
of the array may be transferred to or from the GPU. However, the developer must
ensure that the copied array range includes those array positions accessed in
the code. Otherwise, the result is undefined behavior, most likely causing
invalid memory accesses and crashes.

### Code example

#### C

The following code performs the sum of two arrays:

```c
void foo() {
  int A[100], B[100], sum[100];
  #pragma omp target map(to: A[0:50], B[0:50]) map(from: sum[0:50])
  #pragma omp parallel for
  for (int i = 0; i < 100; i++) {
    sum[i] = A[i] + B[i];
  }
}
```

However, only half of the array elements have been copied to the GPU. This must
be fixed by matching the transferred range with array range actually used in the
loop:

```c
void foo() {
  int A[100], B[100], sum[100];
  #pragma omp target map(to: A[0:100], B[0:100]) map(from: sum[0:100])
  #pragma omp parallel for
  for (int i = 0; i < 100; i++) {
    sum[i] = A[i] + B[i];
  }
}
```

#### Fortran

The following code performs the sum of two arrays:

```f90
subroutine example()
  integer, dimension(100) :: A, B, sum
  integer :: i

  !$omp target map(to: A(1:50), B(1:50)) map(from: sum(1:50))
  !$omp parallel do
  do i = 1, size(sum, 1)
    sum(i) = A(i) + B(i)
  end do
  !$omp end target
end subroutine example
```

However, only half of the array elements have been copied to the GPU. This must
be fixed by matching the transferred range with array range actually used in
the loop:

```f90
subroutine example()
  integer, dimension(100) :: A, B, sum
  integer :: i

  !$omp target map(to: A, B) map(from: sum)
  !$omp parallel do
  do i = 1, size(sum, 1)
    sum(i) = A(i) + B(i)
  end do
  !$omp end target
end subroutine example
```

When explicit ranges are not provided, the compiler automatically determines
the full size of the arrays based on their declarations.


### Related resources

* [PWD005 examples](../PWD005)

* [PWR015: Avoid copying unnecessary array elements to the GPU](../PWR015/README.md)

* [OpenMP 4.5 Complete Specifications](https://www.openmp.org/wp-content/uploads/openmp-4.5.pdf),
November 2015 [last checked May 2019]

### References

* [Race condition](https://en.wikipedia.org/wiki/Race_condition)
