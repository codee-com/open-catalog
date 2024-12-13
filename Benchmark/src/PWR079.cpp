#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
double sum_array(const int n, const double *array);
double sum_array_improved(const int n, const double *array);
double sum_array_f(const int n, const double *array);
double sum_array_improved_f(const int n, const double *array);
}

// Size adjusted to fit execution on micro-seconds
constexpr int N = 1024 * 1024;

#if OCB_ENABLE_C

static void CExampleBench(benchmark::State &state) {
  auto array = OpenCatalog::CreateRandomVector<double>(N);

  for (auto _ : state) {
    double result = sum_array(N, array.data());
    benchmark::DoNotOptimize(result);
  }
}

static void CImprovedBench(benchmark::State &state) {
  auto array = OpenCatalog::CreateRandomVector<double>(N);

  for (auto _ : state) {
    double result = sum_array_improved(N, array.data());
    benchmark::DoNotOptimize(result);
  }
}

// The goal of these benchmarks is to demonstrate that the suggested check does
// not incur any performance penalty
OC_BENCHMARK("PWR079 C Example", CExampleBench);
OC_BENCHMARK("PWR079 C Improved", CImprovedBench);

#endif

#if OCB_ENABLE_Fortran

static void FortranExampleBench(benchmark::State &state) {
  auto array = OpenCatalog::CreateRandomVector<double>(N);

  for (auto _ : state) {
    double result = sum_array_f(N, array.data());
    benchmark::DoNotOptimize(result);
  }
}

static void FortranImprovedBench(benchmark::State &state) {
  auto array = OpenCatalog::CreateRandomVector<double>(N);

  for (auto _ : state) {
    double result = sum_array_improved_f(N, array.data());
    benchmark::DoNotOptimize(result);
  }
}

// The goal of these benchmarks is to demonstrate that the suggested check does
// not incur any performance penalty
OC_BENCHMARK("PWR079 Fortran Example", FortranExampleBench);
OC_BENCHMARK("PWR079 Fortran Improved", FortranImprovedBench);

#endif
