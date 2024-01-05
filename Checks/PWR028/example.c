// PWR028: Remove pointer increment preventing performance optimization

void example(float *a, float *b, float *c, unsigned size, unsigned inc) {
  float *bTemp1 = b;
  for (unsigned i = 0; i < size; i++) {
    c[0] += (a[i] * bTemp1[0]);
    bTemp1 -= inc;
  }
}
