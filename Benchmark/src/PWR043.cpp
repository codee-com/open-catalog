#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
void matmul(int n, const double *A, const double *B, double *C);
void matmul_improved(int n, const double *A, const double *B, double *C);
}

// Size adjusted to fit execution on micro-seconds
constexpr int N = 128;

#if OCB_ENABLE_C

static void CExampleBench(benchmark::State &state) {
  auto A = OpenCatalog::CreateRandomVector<double>(N * N);
  auto B = OpenCatalog::CreateRandomVector<double>(N * N);
  auto C = OpenCatalog::CreateUninitializedVector<double>(N * N);
  for (auto _ : state) {
    matmul(N, A.data(), B.data(), C.data());
    benchmark::DoNotOptimize(C);
  }
}

static void CImprovedBench(benchmark::State &state) {
  auto A = OpenCatalog::CreateRandomVector<double>(N * N);
  auto B = OpenCatalog::CreateRandomVector<double>(N * N);
  auto C = OpenCatalog::CreateUninitializedVector<double>(N * N);
  for (auto _ : state) {
    matmul_improved(N, A.data(), B.data(), C.data());
    benchmark::DoNotOptimize(C);
  }
}

OC_BENCHMARK("PWR043 C Example", CExampleBench);
OC_BENCHMARK("PWR043 C Improved", CImprovedBench);

#endif
