// PWR046: Replace two divisions with a division and a multiplication

__attribute__((const)) float example(float a, float b, float c) {
  return a / b / c;
}
