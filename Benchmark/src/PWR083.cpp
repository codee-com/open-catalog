#include "Benchmark.h"

// Forward-declare the functions to benchmark
extern "C" {
void update_simulation_state_by_fixed_amount(const int *n, double *state);
void update_simulation_state_by_fixed_amount_improved(const int *n,
                                                      double *state);
}

constexpr int N = 1024 * 1024;

#if OCB_ENABLE_Fortran

static void FortranExampleBench(benchmark::State &state) {
  auto array = OpenCatalog::CreateRandomVector<double>(N);

  for (auto _ : state) {
    update_simulation_state_by_fixed_amount(&N, array.data());
    benchmark::DoNotOptimize(array);
  }
}

static void FortranImprovedBench(benchmark::State &state) {
  auto array = OpenCatalog::CreateRandomVector<double>(N);

  for (auto _ : state) {
    update_simulation_state_by_fixed_amount_improved(&N, array.data());
    benchmark::DoNotOptimize(array);
  }
}

// The improved benchmark prevents the runtime bug, while not incurring in any
// performance penalty
OC_BENCHMARK("PWR083 Fortran Example", FortranExampleBench);
OC_BENCHMARK("PWR083 Fortran Improved", FortranImprovedBench);

#endif
