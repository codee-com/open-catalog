// PWR003: Explicitly declare pure functions

#ifdef __GNUC__
  #define PURE __attribute__((const))
#else
  #define PURE
#endif

/* PURE */ int example_pure(int a, int b) {
  return a + b;
}

int example_impure(int a, int *b) {
  *b = a + 1;
  return a + *b;
}

void example() {
  int result[10];
  int b = 1;

  for (int i = 0; i < 10; i++) {
    result[i] = example_pure(i, b); // No side effects
  }

  for (int i = 0; i < 10; i++) {
    result[i] = example_impure(i, &b); // Side effects on variable t
  }
}
