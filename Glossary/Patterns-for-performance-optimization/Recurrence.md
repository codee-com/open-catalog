# Recurrence pattern

The recurrence computation pattern occurs when the same memory position is read
and written to, at least once, in different iterations of a loop. It englobes
both true dependencies (read-after-write) and anti-dependencies
(write-after-read) across loop iterations. Sometimes the term
"loop-carried dependencies" is also used. If a loop with a recurrence
computation pattern is parallelized without protecting the concurrent memory
access, a data race condition is introduced. In some cases, the concurrent
memory access can not be protected and thus the loop can not be parallelized.

A more formal definition is that a recurrence is a computation `a(s) = e`, where
`e` contains a set of occurrences `a(s1), ..., a(sm)` so that, in the general
case, the subscripts `s, s1, ..., sm` are different. Note that in the classical
sense, a recurrence satisfies the additional constraint that at least one
subscript is symbolically different from `s`, and thus dependencies between
different loop iterations are introduced.

### Code examples

#### C

```c
y[0] = 0;
for (int i = 1; i < N; i++) {
  y[i] = y[i - 1] + x[i - 1];
}
```

#### Fortran

```fortran
y(1) = 0
do i = 2, N
  y(2) = y(i - 1) + x(i - 1)
end do
```

### Parallelizing recurrences with OpenMP and OpenACC

In general, codes containing a recurrence pattern are difficult to parallelize
efficiently, and may even not be parallelizable at all. An example of
parallelizable recurrence is the computation of a cumulative sum, which can be
computed efficiently in parallel through parallel prefix sum operations. This is
usually known as scan operation and it is supported in OpenMP since version 5.0.  
