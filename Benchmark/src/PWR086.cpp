#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
void pointer_increment_example(float *a, float *b, float *c, unsigned size,
                               unsigned inc);
void pointer_increment_improved(float *a, float *b, float *c, unsigned size,
                                unsigned inc);
}

// Size adjusted to fit execution on micro-seconds
constexpr unsigned N_VEC = 1024 * 1024;
constexpr unsigned INC = 1;

#if OCB_ENABLE_C

static void CPointerIncrementExampleBench(benchmark::State &state) {
  auto a = OpenCatalog::CreateRandomVector<float>(N_VEC);
  auto b = OpenCatalog::CreateRandomVector<float>(N_VEC);
  auto c = OpenCatalog::CreateZeroVector<float>(1);
  // Point to the last element so that b[-i*inc] stays in bounds
  float *bEnd = b.data() + N_VEC - 1;
  for (auto _ : state) {
    pointer_increment_example(a.data(), bEnd, c.data(), N_VEC, INC);
    benchmark::DoNotOptimize(c);
  }
}

static void CPointerIncrementImprovedBench(benchmark::State &state) {
  auto a = OpenCatalog::CreateRandomVector<float>(N_VEC);
  auto b = OpenCatalog::CreateRandomVector<float>(N_VEC);
  auto c = OpenCatalog::CreateZeroVector<float>(1);
  // Point to the last element so that b[-i*inc] stays in bounds
  float *bEnd = b.data() + N_VEC - 1;
  for (auto _ : state) {
    pointer_increment_improved(a.data(), bEnd, c.data(), N_VEC, INC);
    benchmark::DoNotOptimize(c);
  }
}

OC_BENCHMARK("PWR086 C Pointer Increment Example",
             CPointerIncrementExampleBench);
OC_BENCHMARK("PWR086 C Pointer Increment Improved",
             CPointerIncrementImprovedBench);

#endif
