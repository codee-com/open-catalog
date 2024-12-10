// PWR048: Replace multiplication/addition combo with an explicit call to fused
// multiply-add

__attribute__((const)) double example(double a, double b, double c) {
  return a + b * c;
}
