#ifndef OPEN_CATALOG_BENCHMARK_H
#define OPEN_CATALOG_BENCHMARK_H

#include <algorithm>
#include <random>
#include <vector>

#include <benchmark/benchmark.h>

// Custom benchmark macro to fine tune the benchmark result
#define OC_BENCHMARK(NAME, ...)                                                \
  BENCHMARK(__VA_ARGS__)->Name(NAME)->Unit(benchmark::kMicrosecond)

namespace OpenCatalog {

/// Returns a vector of `n` zeros
template <typename Ty> std::vector<Ty> CreateZeroVector(const std::size_t n) {
  return std::vector<Ty>(n, static_cast<Ty>(0));
}

/// Returns a vector of `n` random floats uniformly distributed between
/// `lowerBound` and `upperBound` (default to small number to avoid overflow and
/// underflow)
template <typename Ty>
std::vector<Ty> CreateRandomVector(const std::size_t n, const Ty lowerBound = 1,
                                   const Ty upperBound = 128) {
  std::vector<Ty> result(n);
  // Use the same seed for all randomized vectors to be more deterministic
  std::mt19937 generator{0};
  std::uniform_real_distribution<Ty> distribution(lowerBound, upperBound);
  std::generate(result.begin(), result.end(),
                [&]() { return distribution(generator); });
  return result;
}

} // namespace OpenCatalog

#endif // OPEN_CATALOG_BENCHMARK_H
