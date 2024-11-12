# PWD003: Missing array range in data copy to the GPU

### Issue

Copying data to the GPU from an array whose size is not known to the compiler
requires specifying information about the desired array data range to be copied.

### Actions

Specify the array range to be copied to device memory.

### Relevance

When offloading to the GPU, copying array data from the host memory to the GPU
memory requires information about the data range of the array that must be
copied. If the compiler knows the array size (e.g., for C static arrays or
Fortran assumed shape arrays) and the whole array must be copied, specifying the
data range is optional for both OpenMP and OpenACC standards. However, in the
case of arrays whose size is not known to the compiler, specifying the array
range is compulsory. Some compilers do not enforce this, which leads to
undefined behavior. For instance, for C dynamic arrays the pointer scalar value
might be copied instead of any pointed-to data; for Fortran assumed size arrays,
an invalid memory access might occur or erroneous memory (i.e., from wrong
memory locations) might be copied.

### Code example

#### C

In the following OpenMP code, a pointer is being copied to the offloading target
device instead of the dynamic array data pointed by it.

```c
void foo(int *a, int *b, int *sum, int size) {
  #pragma omp target map(to: a, b) map(from: sum)
  #pragma omp parallel for
  for (int i = 0; i < size; i++) {
    sum[i] = a[i] + b[i];
  }
}
```

In this case, it suffices to specify the array bounds in the OpenMP map clauses:

```c
void foo(int *a, int *b, int *sum, int size) {
  #pragma omp target map(to: a[0:size], b[0:size]) map(from: sum[0:size])
  #pragma omp parallel for
  for (int i = 0; i < size; i++) {
    sum[i] = a[i] + b[i];
  }
}
```

The same applies to the analogous OpenACC example.

```c
void foo(int *a, int *b, int *sum, int size) {
  #pragma acc data copyin(a, b) copyout(sum)
  #pragma acc parallel loop
  for (int i = 0; i < size; i++) {
    sum[i] = a[i] + b[i];
  }
}
```

And again, specifying the array bounds fixes the problem:

```c
void foo(int *a, int *b, int *sum, int size) {
  #pragma acc data copyin(a[0:size], b[0:size]) copyout(sum[0:size])
  #pragma acc parallel loop
  for (int i = 0; i < size; i++) {
    sum[i] = a[i] + b[i];
  }
}
```

#### Fortran

The following OpenMP code copies multiple assumed-size arrays to the offloading
target:

```f90
subroutine foo(a, b, sum, size)
  implicit none
  integer, dimension(*), intent(in) :: a, b
  integer, dimension(*), intent(out) :: sum
  integer, intent(in) :: size
  integer :: i

  !$omp target map(to: a, b) map(from: sum)
  !$omp parallel do default(none) shared(a, b, sum)
  do i = 1, size
    sum(i) = a(i) + b(i)
  end do
  !$omp end parallel do
  !$omp end target
end subroutine foo
```

Since the array bounds are not known by the compiler, the code might not work
as expected (e.g., copying only the array descriptors instead of the actual
data), or even raise an error during compilation:

```txt
$ gfortran --version
GNU Fortran (Debian 12.2.0-14) 12.2.0
$ gfortran foo.f90 -fopenmp
foo.f90:8:23:

    8 |   !$omp target map(to: a, b) map(from: sum)
      |                       1
Error: Assumed size array ‘a’ in MAP clause at (1)
foo.f90:8:25:

    8 |   !$omp target map(to: a, b) map(from: sum)
      |                         1
Error: Assumed size array ‘b’ in MAP clause at (1)
foo.f90:8:39:

    8 |   !$omp target map(to: a, b) map(from: sum)
      |                                       1
Error: Assumed size array ‘sum’ in MAP clause at (1)
```

Specifying the array bounds is as simple as updating the OpenMP map clauses as
follows:

```f90
subroutine foo(a, b, sum, size)
  implicit none
  integer, dimension(*), intent(in) :: a, b
  integer, dimension(*), intent(out) :: sum
  integer, intent(in) :: size
  integer :: i

  !$omp target map(to: a(1:size), b(1:size)) map(from: sum(1:size))
  !$omp parallel do default(none) shared(a, b, sum)
  do i = 1, size
    sum(i) = a(i) + b(i)
  end do
  !$omp end parallel do
  !$omp end target
end subroutine foo
```

>**Note:**  
>Another option would be to use assumed-shape arrays instead, which
>automatically provide the compiler with all the necessary information.
>
>Check the [PWR070 entry](../PWR070/) for more details on them!

### Related resources

* [PWD003 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWD003/)

* [OpenMP 4.5 Complete Specifications](https://www.openmp.org/wp-content/uploads/openmp-4.5.pdf),
November 2015 [last checked July 2019]

* [The OpenACC Application Programming Interface, Version 2.6](https://www.openacc.org/sites/default/files/inline-files/OpenACC.2.6.final.pdf),
November 2017 [last checked July 2019]

### References

* [OpenMP 4.5 Complete Specifications](https://www.openmp.org/wp-content/uploads/openmp-4.5.pdf)
(see page 44, Section 2.4 Array Sections) [last checked July 2019]

* [The OpenACC Application Programming Interface, Version 2.6](https://www.openacc.org/sites/default/files/inline-files/OpenACC.2.6.final.pdf)
(see page 33, Section 2.7.1. Data Specification in Data Clauses) [last checked
July 2019]

* [Race condition](https://en.wikipedia.org/wiki/Race_condition)
