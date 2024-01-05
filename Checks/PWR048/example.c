// PWR048: Replace multiplication/addition combo with an explicit call to fused
// multiply-add

double example(double a, double b, double c) {
  return a + b * c;
}
