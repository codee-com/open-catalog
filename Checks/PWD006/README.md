# PWD006: Missing deep copy of non-contiguous data to the GPU

### Issue

The copy of a non-scalar variable to an accelerator device has been requested
but none or  only a part of its data will be transferred because it is laid out
non-contiguously in memory.

### Actions

Use OpenMP 4.5 *enter/exit data* execution statements to ensure that all the
memory segments are copied to the memory of the accelerator device.

### Relevance

The data of non-scalar variables might be spread across memory, laid out in
non-contiguous regions. One classical example is a dynamically-allocated
two-dimensional array in C/C++, which consists of a contiguous array of pointers
pointing to separate contiguous arrays that contain the actual data. Note that
the elements of each individual array are contiguous in memory but the different
arrays are scattered in the memory. This also holds for dynamically-allocated
multi-dimensional arrays as well as for structs containing pointers in C/C++.

In order to offload such non-scalar variables to an accelerator device using
OpenMP or OpenACC, it is not enough to add it to a data movement clause. This is
known as deep copy and currently is not automatically supported by either OpenMP
or OpenACC. To overcome this limitation, all the non-contiguous memory segments
must be explicitly transferred by the programmer.  In OpenMP 4.5, this can be
achieved through the *enter/exit data* execution statements. Alternatively, the
code could be refactored so that it uses variables with contiguous data layouts
(eg. flatten an array of arrays).

### Code example

The following OpenMP code declares that the bi-dimensional array `A` should be
copied to the accelerator device (see the clause `map(tofrom: A)` and the data
type `int **` of the array `A`). However, this is incorrect and will not copy
the data to be accessed in the loop body because OpenMP treats the pointer `A`
as a zero-length array. Thus, the actual data of the variable will not be
copied. As a result, dereferencing `A` in the GPU will cause invalid memory
accesses, since its data has not been copied.

```c
void foo(int **A) {
  #pragma omp target teams distribute parallel for map(tofrom:A)
  for (size_t i = 0; i < 10; i++) {
    A[i][i] += i;
  }
}
```

Adding the array ranges (see `map(tofrom: A[0:10][0:10])`) could be seen as a
solution:

```c
void foo(int **A) {
  #pragma omp target teams distribute parallel for map(tofrom:A[0:10][0:10])
  for (size_t i = 0; i < 10; i++) {
    A[i][i] += i;
  }
}
```

However, this OpenMP code does not handle non-contiguous memory properly because
deep copy is not automatically supported. Therefore, each contiguous memory
segment must be individually mapped to the accelerator device. This can be done
through OpenMP 4.5 *enter/exit data* execution statements as follows:

```c
void foo(int **A) {
  #pragma omp target enter data map(to: A[0:10])
  for (size_t i = 0; i < 10; i++) {
    #pragma omp target enter data map(to: A[i][0:10])
  }

  #pragma omp target teams distribute parallel for
  for (int i = 0; i < 10; i++) {
    A[i][i] += i;
  }

  for (size_t i = 0; i < 10; i++) {
    #pragma omp target exit data map(from: A[i][0:10])
  }
  #pragma omp target exit data map(from: A[0:10])
}
```

The *enter/exit data* statements resemble how the dynamic bi-dimensional memory
is allocated in the CPU. An array of pointers is allocated first, followed by
the allocation of all the separate arrays that contain the actual data. Each
allocation constitutes a contiguous memory segment and must be transferred
individually using *enter data*. The deallocation takes place in the inverted
order and the same happens with the *exit *data statements.

### Related resources

* [PWD006 examples](https://github.com/codee-com/open-catalog/tree/main/Checks/PWD006/)

* [PWD003: Missing array range in data copy to accelerator device](../PWD003/README.md)

### References

* [OpenMP 4.5 Complete Specifications](https://www.openmp.org/wp-content/uploads/openmp-4.5.pdf)
(see page 44, Section 2.4 Array Sections) [last checked September 2020]

* [The OpenACC Application Programming Interface, Version 2.6](https://www.openacc.org/sites/default/files/inline-files/OpenACC.2.6.final.pdf)
(see page 33, Section 2.7.1. Data Specification in Data Clauses)
[last checked September 2020]

* [Deep copy - Object copying](https://en.wikipedia.org/wiki/Object_copying#Deep_copy)
[last checked September 2020]

* [Deep Copy Attach and Detach - OpenACC Technical Report TR-16-1](https://www.openacc.org/sites/default/files/inline-files/TR-16-1.pdf)
[last checked September 2020]
