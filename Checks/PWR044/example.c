// PWR044: Avoid unnecessary floating-point data conversions involving constants

__attribute__((const)) float example(float a) {
  return a * 2.2;
}
