// PWR012: Pass only required fields from derived type as arguments

#include <stdlib.h>

typedef struct {
  int A[1000];
  int B[1000];
} data;

__attribute__((pure)) int foo(const int *A) {
  int result = 0;
  for (int i = 0; i < 1000; i++) {
    result += A[i];
  }
  return result;
}

void solution() {
  data *d = (data *)malloc(sizeof(data));
  for (int i = 0; i < 1000; i++) {
    d->A[i] = d->B[i] = 1;
  }
  int result = foo(d->A);
  free(d);
}
