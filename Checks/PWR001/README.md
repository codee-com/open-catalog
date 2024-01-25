# PWR001: Declare global variables as function parameters

### Issue

Best programming practices recommend writing self-contained functions, meaning
that all the global variables used in the function must be declared locally
either as parameters or as local variables in the function body.

### Actions

Explicitly declare all the global variables read or written in the function
either as parameters or as local variables in the function body, including the
output variables.

### Relevance

Using global variables makes it hard to reason about where and how those
variables are used. By including all data inputted and outputted by a function
in its signature, the interactions with external data can be determined by
inspecting the function call sites. As a result it is easier to identify the
function's side effects. A function modifying a global variable can easily go
unnoticed since the function body must be inspected to detect it. This
facilitates reasoning about the flow of data in and out of the function, which
helps from the point of view of maintainability, testing and compiler
optimization.

>**Note**  
>Enforcing this recommendation in large projects can be difficult and
>time-consuming because a large number of code changes can be needed, even
>leading to deep code refactorizations. In some situations, an alternative
>approach would be to outline the critical loop into a separate function. In
>case there is a single output variable, the function return value may be used
>instead of a parameter.

>**Note**  
>Enforcing this recommendation may be critical in the case the functions to be
>executed in parallel, since hidden race conditions may be difficult to detect,
>making the result of the code unpredictable.

### Code examples

#### C

In the following example, the flow of the data across function calls is not
explicit. The global variables `A` and `B` are read or written in functions
`init` and `add`. As the functions have no parameters, it is necessary to keep
track of the read and write operations to `A` and `B` across multiple functions
(and in real codes even across multiple files).

```c
#define N 100

int A[N];
int B[N];

void init() {
  for (int i = 0; i < N; i++) {
    A[i] = i;
  }
}

void add() {
  for (int i = 0; i < N; i++) {
    B[i] = A[i] + 1;
  }
}

void example() {
  init();
  add();
}
```

When the arrays are passed explicitly as function parameters, with a simple
glance at the code you notice which are the arrays involved during the execution
of function `example`. In the refactored code the inputs and outputs of the
function calls `init(A)` and `add(A, B)` are explicit, so it is easier to
determine the flow of data at runtime, and thus to determine the function's side
effects.

```c
#define N 100

int A[N];
int B[N];

void init(int *A) {
  for (int i = 0; i < N; i++) {
    A[i] = i;
  }
}

void add(int *A, int *B) {
  for (int i = 0; i < N; i++) {
    B[i] = A[i] + 1;
  }
}

void example() {
  init(A);
  add(A, B);
}
```

#### Fortran

In the following example, the flow of the data across function calls is not
explicit. The function declares the use of a module, but with no specification
of the module variables actually used in the function body. Thus, it is
necessary to keep track of the read and write operations to the variables
declared in the module (and in real codes even across multiple files).

```f90
module globalsMod
  implicit none
  real :: global_a
end module globalsMod

real function example()
  use globalsMod
  implicit none
  example = global_a
end function
```

When the use of module variables is declared explicitly, with a simple glance at
the code you notice which are the variables actually used in the function body.
Thus, it is easier to determine the flow of data at runtime, and thus to
determine the function's side effects.

```f90
module globalsMod
  implicit none
  real :: global_a
end module globalsMod

real function example()
  use globalsMod, only : global_a
  implicit none
  example = global_a
end function example
```

### Related resources

* [PWR001 examples at GitHub](/Checks/PWR001)

### References

* [Scope of variables](https://users.cs.cf.ac.uk/Dave.Marshall/PERL/node52.html)
