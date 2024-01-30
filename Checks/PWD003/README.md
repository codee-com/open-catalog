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

You can find this and more examples [at GitHub](/Checks/PWD003).

### Related resources

* [PWD003 examples at GitHub](/Checks/PWD003)

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
