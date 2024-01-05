// PWR045: Replace division with a multiplication with a reciprocal

void example(float *out, float *in, int n, float b) {
  for (int i = 0; i < n; ++i) {
    out[i] = in[i] / b;
  }
}
