// PWR074: Pass only required fields from derived type as arguments to increase code clarity

#include <stdlib.h>

typedef struct {
  int A[1000];
  int B[1000];
} data;

__attribute__((pure)) int foo(const data *d) {
  int result = 0;
  for (int i = 0; i < 1000; i++) {
    result += d->A[i];
  }
  return result;
}

void example() {
  data *d = (data *)malloc(sizeof(data));
  for (int i = 0; i < 1000; i++) {
    d->A[i] = d->B[i] = 1;
  }
  int result = foo(d);
  free(d);
}
