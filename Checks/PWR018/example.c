// PWR018: Call to recursive function within a loop may inhibit vectorization

double fib(unsigned n) {
  if (n == 0) {
    return 0.0;
  }
  if (n == 1) {
    return 1.0;
  }
  return fib(n - 1) + fib(n - 2);
}

double example(unsigned times) {
  double sum = 0.;
  for (unsigned i = 0; i < times; i++) {
    sum += fib(i);
  }
  return sum;
}
