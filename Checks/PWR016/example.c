// PWR016: Use separate arrays instead of an Array-of-Structs

typedef struct {
  int x;
  int y;
  int z;
} point;

void example() {
  point points[1000];
  for (int i = 0; i < 1000; i++) {
    points[i].x = 1;
    points[i].y = 1;
  }
}
