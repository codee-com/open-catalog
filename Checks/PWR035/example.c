// PWR035: Avoid non-consecutive array access to improve performance

void reverseArray(float *array, unsigned size) {
  for (unsigned i = 0; i < size / 2; ++i) {
    float temp = array[i];
    array[i] = array[size - i - 1];
    array[size - i - 1] = temp;
  }
}
