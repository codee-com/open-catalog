# OpenMP canonical form

OpenMP pragmas can be applied only to loops in OpenMP canonical form. For a loop
to be in a canonical form, it must have the following properties:

* It must be a for loop.

* The iterator variable is of int, pointer, or iterator type.

* The iterator variable remains unmodified throughout the body of the iteration.

* The iterator variable changes only within the for clause and the change is
iteration invariant.

* The limit on the iterator variable is an iteration invariant.

* The body of the iteration does not transfer control outside itself.

Example of loops not in canonical form:

```c
// Not a for loop
while(*p != 0) {
  p++;
}

// Modifies the iterator variable in the loop body
for (int i = 0; i < n; i++) {
  if (b[i] == 0) {
    i++;
  } else {
    // ...
  }
}

// Iterator variable changes unpredictably
for (int i = 0; i < n; i+= inc) {
  // ...
  if (b[i] == 0) {
    inc = 1;
  } else {
    inc = 2;
  }
}

// The limit on the iterator variable changes
for (int i = 0; i < n; i++) {
  // ...
  if (b[i] == 0) {
    n++;
  }
}

// Transfer of control outside of the loop
for (int i = 0; i < n; i++) {
  if (b[i] ==0) break;
  // ...
}
```
