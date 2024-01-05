// PWR030: Remove pointer assignment preventing performance optimization for
// perfectly nested loops

void example(float *a, float *b, unsigned size) {
  for (unsigned i = 0; i < size; i++) {
    b = a + i * size;
    for (unsigned j = 0; j < size; j++) {
      b[j * size] = 0;
    }
  }
}
