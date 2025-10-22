#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
double transform_and_sum(const int n, const double *array, const int option);
double transform_and_sum_improved(const int n, const double *array,
                                  const int option);
double transform_and_sum_f(const int n, const double *array, const int option);
double transform_and_sum_improved_f(const int n, const double *array,
                                    const int option);
}

// Size adjusted to fit execution on micro-seconds
constexpr int N = 1024 * 1024;

#if OCB_ENABLE_C

static void CExampleBench(benchmark::State &state) {
  auto array = OpenCatalog::CreateRandomVector<double>(N);

  for (auto _ : state) {
    double result = transform_and_sum(N, array.data(), /*option=*/-1);
    benchmark::DoNotOptimize(result);
  }
}

static void CImprovedBench(benchmark::State &state) {
  auto array = OpenCatalog::CreateRandomVector<double>(N);

  for (auto _ : state) {
    double result = transform_and_sum_improved(N, array.data(), /*option=*/-1);
    benchmark::DoNotOptimize(result);
  }
}

// The goal of these benchmarks is to demonstrate that the suggested check does
// not incur any performance penalty
OC_BENCHMARK("PWR080 C Example", CExampleBench);
OC_BENCHMARK("PWR080 C Improved", CImprovedBench);

#endif

#if OCB_ENABLE_Fortran

static void FortranExampleBench(benchmark::State &state) {
  auto array = OpenCatalog::CreateRandomVector<double>(N);

  for (auto _ : state) {
    double result = transform_and_sum_f(N, array.data(), /*option=*/-1);
    benchmark::DoNotOptimize(result);
  }
}

static void FortranImprovedBench(benchmark::State &state) {
  auto array = OpenCatalog::CreateRandomVector<double>(N);

  for (auto _ : state) {
    double result =
        transform_and_sum_improved_f(N, array.data(), /*option=*/-1);
    benchmark::DoNotOptimize(result);
  }
}

// The goal of these benchmarks is to demonstrate that the suggested check does
// not incur any performance penalty
OC_BENCHMARK("PWR080 Fortran Example", FortranExampleBench);
OC_BENCHMARK("PWR080 Fortran Improved", FortranImprovedBench);

#endif
