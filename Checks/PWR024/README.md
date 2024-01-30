# PWR024: Loop can be rewritten in OpenMP canonical form

### Issue

The loop is currently not in
[OpenMP canonical](/Glossary/OpenMP-canonical-form.md) form but it can be made
OpenMP compliant through refactoring.

### Actions

Rewrite the loop to make it an OpenMP canonical form using single statements
involving only the iterator variable in the loop header.

### Relevance

OpenMP requires a loop to fulfill some requirements: it must be a `for` loop,
have single statements in its loop headers, use only one iterator variable which
must be an integer and use only integer increments. Failing to fulfill any of
these requirements inhibits the loop parallelization with OpenMP. In many cases,
a simple rewrite can fix this.

### Code example

The following loop is not in OpenMP canonical form because the iterator is
initialized outside the loop:

```c
void example(int **A, int n, int m) {
  int i = 0;
  for (; i < n; i++) {
    A[i] = m;
  }
}
```

This can be easily fixed by removing the `j` increment and calculating it as a
function of variable `i`.

```c
void example(int **A, int n, int m) {
  for (int i = 0; i < n; i++) {
    A[i] = m;
  }
}
```

### References

* [Canonical Loop Form - OPENMP API Specification: Version 5.0 November 2018](https://www.openmp.org/spec-html/5.0/openmpsu40.html)
