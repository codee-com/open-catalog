# Forall pattern

A forall is a loop that updates the elements of an array, each iteration
updating a different element of the array. 

```c
for (int i = 0; i < N; ++i) {
  A[i] = ...
}
```

`A` is the array variable and `i` is the loop index variable, which can be
determined at compile-time. 

### Code examples

#### C

```c
for (int i = 0; i < n; ++i) {
  D[i] = a * X[i] + Y[i];
} 
```

#### Fortran

```f90
do i = 1, n
  D(i) = a * X(i) + Y(i)
end do
```

### Parallelizing forall loops with OpenMP and OpenACC

OpenMP and OpenACC provide built-in support for the computation of a forall
pattern in parallel. The programmer needs to identify the forall pattern and
provide the relevant directives to the compiler (the parallel loop directive in
both OpenMP and OpenACC).
