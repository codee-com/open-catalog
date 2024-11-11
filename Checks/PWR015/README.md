# PWR015: Avoid copying unnecessary array elements to or from the GPU

### Issue

Unused data should never be copied to or from the GPU to prevent unnecessary
[data movements](../../Glossary/Offloading.md) between the CPU and the GPU, which
impacts performance.

### Actions

Restrict the array range to be copied to the GPU to the range strictly required.

### Relevance

One of the key challenges when offloading work to the GPU is minimizing the data
transfers between CPU memory and GPU memory. These transfers can greatly affect
performance and should be performed only when needed. Thus, only the strictly
required data should be copied to or from the GPU memory.

### Code example

#### C

The following code performs the sum of two arrays:

```c
void example() {
  int A[100], B[100], sum[100];
  #pragma omp target map(to: A[0:100], B[0:100]) map(from: sum[0:100])
  #pragma omp parallel for private(i)
  for (int i = 0; i < 50; i++) {
    sum[i] = A[i] + B[i];
  }
}
```

However, only half of the total array elements are actually being used. Thus,
there is no need to transfer the entire arrays:

```c
void example() {
  int A[100], B[100], sum[100];
  #pragma omp target map(to: A[0:50], B[0:50]) map(from: sum[0:50])
  #pragma omp parallel for private(i)
  for (int i = 0; i < 50; i++) {
    sum[i] = A[i] + B[i];
  }
}
```

#### Fortran

The following code performs the sum of two arrays:

```fortran
subroutine example(A, B, sum)
  implicit none
  integer, intent(in) :: A(:), B(:)
  integer, intent(out) :: sum(:)
  integer :: i

  !$omp target parallel do default(none) shared(A, B, sum) private(i) &
  !$omp& map(to: a, b) map(from: sum)
  do i = 1, size(sum, 1) / 2
    sum(i) = A(i) + B(i)
  end do
  !$omp end target parallel do
end subroutine example
```

However, only half of the total array elements are actually being used. Thus,
there is no need to transfer the entire arrays:

```fortran
subroutine example(A, B, sum)
  implicit none
  integer, intent(in) :: A(:), B(:)
  integer, intent(out) :: sum(:)
  integer :: i, half_size

  half_size = size(sum, 1) / 2

  !$omp target parallel do default(none) shared(A, B, sum) private(i) &
  !$omp& map(to: a(1:half_size), b(1:half_size)) map(from: sum(1:half_size))
  do i = 1, half_size
    sum(i) = A(i) + B(i)
  end do
  !$omp end target parallel do
end subroutine example
```

### Related resources

* [PWR015 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWR015/)

* [PWD005: Array range copied to the GPU does not cover the used range](../PWD005/README.md)
