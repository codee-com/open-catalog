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

The following code uses an integer index ranging from 1 to 100 to access the
array `A`:

```c
void foo() {
  int A[100];
  for (int i = 0; i < 100; i++) {
    A[i + 1] = 1;
  }
}
```

This is incorrect since the array positions range from 0 to 99. Thus, the array
access must be fixed, for instance by changing the array reference from
`A[i + 1]` to `A[i]`:

```c
void foo() {
  int A[100];
  for (int i = 0; i < 100; i++) {
    A[i] = 1;
  }
}
```

### Related resources

* [PWD004 examples](../PWD004)

### References

* [CWE - CWE-125: Out-of-bounds Read (4.2)](https://cwe.mitre.org/data/definitions/125.html)
[last checked October 2020]

* [Index checking - Bounds checking](https://en.wikipedia.org/wiki/Bounds_checking#Index_checking)
[last checked October 2020]
