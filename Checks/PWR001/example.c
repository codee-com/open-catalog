// PWR001: Pass global variables as function arguments

#define N 100

int A[N];
int B[N];

void init() {
  for (int i = 0; i < N; i++) {
    A[i] = i;
  }
}

void add() {
  for (int i = 0; i < N; i++) {
    B[i] = A[i] + 1;
  }
}

void example() {
  init();
  add();
}
