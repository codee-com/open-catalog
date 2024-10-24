# PWD010: Incorrect sharing in OpenMP parallel region

### Issue

A variable is being incorrectly shared in the OpenMP datascoping clauses.

### Actions

Change the data scope of the variable from shared to private.

### Relevance

Specifying an invalid scope for a variable may introduce race conditions and
produce incorrect results. For instance, when a variable is written from
parallel threads and the specified scoping is shared instead of private.

### Code example

#### C

In the following code, no variable is privatized. According to OpenMP's default
behavior, the iterator variable `i` of the outermost loop (where the OpenMP
directive is positioned) is automatically privatized for each thread, but the
variable `j` of the innermost loop is incorrectly left as shared among threads:

```c
void example(int **result, unsigned rows, unsigned cols) {
  int i, j;

  // j is implicitly shared and it should be private!
  #pragma omp parallel for shared(result)
  for (i = 0; i < rows; i++) {
    for (j = 0; j < cols; j++) {
      result[i][j] = 0;
    }
  }
}
```

This introduces a data race among threads on the variable `j`. It should be
explicitly privatized with a `private` clause:

```c
void example(int **result, unsigned rows, unsigned cols) {
  int i, j;

  #pragma omp parallel for default(none) shared(result) private(i, j)
  for (i = 0; i < rows; i++) {
    for (j = 0; j < cols; j++) {
      result[i][j] = 0;
    }
  }
}
```

#### Fortran

In the following code, no variable is privatized. According to OpenMP's default
behavior, the iterator variable `j` of the outermost loop (where the OpenMP
directive is positioned) is automatically privatized for each thread, but the
variable `i` of the innermost loop is incorrectly left as shared among threads:

```f90
subroutine example(result)
  integer, intent(out) :: result(:, :)
  integer :: i, j

  !$omp parallel do shared(result)
  do j = 1, size(result, 2)
    do i = 1, size(result, 1)
      result(i, j) = 0
    end do
  end do
end subroutine example
```

This introduces a data race among threads on the variable `i`. It should be
explicitly privatized with a `private` clause:

```f90
subroutine example(result)
  integer, intent(out) :: result(:, :)
  integer :: i, j

  !$omp parallel do default(none) shared(result) private(i, j)
  do j = 1, size(result, 2)
    do i = 1, size(result, 1)
      result(i, j) = 0
    end do
  end do
end subroutine example
```

### Related resources

* [PWD010 examples](../PWD010)

### References

* [Data-Sharing Attribute Clauses - OPENMP API Specification: Version 5.0 November 2018](https://www.openmp.org/spec-html/5.0/openmpsu106.html)
[last checked August 2021]
