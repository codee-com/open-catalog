// PWR003: Explicitly declare pure functions

// Depends only on its arguments
// No side effects
__attribute__((const)) int example_const(int a, int b) {
  return a + b;
}

int c = 1;

// Depends on external data (c)
// No side effects
__attribute__((pure)) int example_pure(int a) {
  return a + c;
}

// Depends on external data (c)
// Modifies external data (c)
int example_impure(int a) {
  c += 1;
  return a + c;
}
