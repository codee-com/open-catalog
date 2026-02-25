// PWR086: Prefer array-based notation over pointer-based notation for
// readability

void pointer_increment_improved(float *a, float *b, float *c, unsigned size,
                                unsigned inc) {
  for (unsigned i = 0; i < size; i++) {
    c[0] += (a[i] * b[-(int)(i * inc)]);
  }
}
