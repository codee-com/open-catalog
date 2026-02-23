// PWR018: Call to recursive function within a loop may inhibit vectorization

__attribute__((pure)) double solution(unsigned times) {
  double sum = 0.0;
  double fib_0 = 0.0;
  double fib_1 = 1.0;
  for (unsigned i = 2; i < times; i++) {
    double fib = fib_0 + fib_1;
    sum += fib;
    fib_0 = fib_1;
    fib_1 = fib;
  }
  return sum;
}
