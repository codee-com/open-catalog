---
sidebar_position: 1
sidebar_label: Index
---

# Open Catalog of Code Guidelines for Correctness, Modernization, and Optimization

## About

This Open Catalog is a collaborative effort to consolidate expert knowledge on
code guidelines for the correctness, modernization, and optimization of code
written in C, C++, and Fortran programming languages. The Catalog consists of a
comprehensive set of checks (rules) that describe specific issues in the source
code and provide guidance on corrective actions, along with extensive
documentation, example codes and references to additional reading resources.

## Benchmarks

The Open Catalog includes
[a suite of microbenchmarks](https://github.com/codee-com/open-catalog/tree/main/Benchmark/)
designed to demonstrate:

- No performance degradation when implementing the correctness and
  modernization recommendations.
- Potential performance enhancements achievable through the optimization
  recommendations.

## Checks

| ID                       | Title                                                                                                                                                              |    Category   | C | Fortran | C++ | AutoFix |
|:------------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------ |:-------------:|:-:|:-------:|:---:|:-------:|
| [PWR001](Checks/PWR001/) | Declare global variables as function parameters                                                                                                                    | correctness   | ✓ | ✓       | ✓   |         |
| [PWR002](Checks/PWR002/) | Declare scalar variables in the smallest possible scope                                                                                                            | correctness   | ✓ |         | ✓   |         |
| [PWR003](Checks/PWR003/) | Explicitly declare pure functions                                                                                                                                  | modernization | ✓ | ✓       | ✓   |         |
| [PWR004](Checks/PWR004/) | Declare OpenMP scoping for all variables                                                                                                                           | correctness   | ✓ | ✓       | ✓   |         |
| [PWR005](Checks/PWR005/) | Disable default OpenMP scoping                                                                                                                                     | correctness   | ✓ | ✓       | ✓   |         |
| [PWR006](Checks/PWR006/) | Avoid privatization of read-only variables                                                                                                                         | optimization  | ✓ | ✓       | ✓   |         |
| [PWR007](Checks/PWR007/) | Disable implicit declaration of variables                                                                                                                          | correctness   |   | ✓       |     | ✓[^1]   |
| [PWR008](Checks/PWR008/) | Declare the intent for each procedure parameter                                                                                                                    | correctness   |   | ✓       |     | ✓[^1]   |
| [PWR009](Checks/PWR009/) | Use OpenMP teams to offload work to GPU                                                                                                                            | optimization  | ✓ | ✓       | ✓   |         |
| [PWR010](Checks/PWR010/) | Avoid column-major array access in C/C++                                                                                                                           | optimization  | ✓ |         | ✓   |         |
| [PWR012](Checks/PWR012/) | Pass only required fields from derived type as parameters                                                                                                          | optimization  | ✓ | ✓       | ✓   |         |
| [PWR013](Checks/PWR013/) | Avoid copying unused variables to or from the GPU                                                                                                                  | optimization  | ✓ | ✓       | ✓   |         |
| [PWR014](Checks/PWR014/) | Out-of-dimension-bounds matrix access                                                                                                                              | correctness   | ✓ |         | ✓   |         |
| [PWR015](Checks/PWR015/) | Avoid copying unnecessary array elements to or from the GPU                                                                                                        | optimization  | ✓ | ✓       | ✓   |         |
| [PWR016](Checks/PWR016/) | Use separate arrays instead of an Array-of-Structs                                                                                                                 | optimization  | ✓ | ✓       | ✓   |         |
| [PWR017](Checks/PWR017/) | Using countable while loops instead of for loops may inhibit vectorization                                                                                         | optimization  | ✓ |         | ✓   |         |
| [PWR018](Checks/PWR018/) | Call to recursive function within a loop inhibits vectorization                                                                                                    | optimization  | ✓ | ✓       | ✓   |         |
| [PWR019](Checks/PWR019/) | Consider interchanging loops to favor vectorization by maximizing inner loop's trip count                                                                          | optimization  | ✓ | ✓       | ✓   |         |
| [PWR020](Checks/PWR020/) | Consider loop fission to enable vectorization                                                                                                                      | optimization  | ✓ | ✓       | ✓   |         |
| [PWR021](Checks/PWR021/) | Consider loop fission with scalar to vector promotion to enable vectorization                                                                                      | optimization  | ✓ | ✓       | ✓   |         |
| [PWR022](Checks/PWR022/) | Move invariant conditional out of the loop to facilitate vectorization                                                                                             | optimization  | ✓ | ✓       | ✓   |         |
| [PWR023](Checks/PWR023/) | Add 'restrict' for pointer function parameters to hint the compiler that vectorization is safe                                                                     | optimization  | ✓ |         | ✓   |         |
| [PWR024](Checks/PWR024/) | Loop can be rewritten in OpenMP canonical form                                                                                                                     | optimization  | ✓ |         | ✓   |         |
| [PWR025](Checks/PWR025/) | Consider annotating pure function with OpenMP 'declare simd'                                                                                                       | optimization  | ✓ | ✓       | ✓   |         |
| [PWR026](Checks/PWR026/) | Annotate function for OpenMP Offload                                                                                                                               | optimization  | ✓ | ✓       | ✓   |         |
| [PWR027](Checks/PWR027/) | Annotate function for OpenACC Offload                                                                                                                              | optimization  | ✓ | ✓       | ✓   |         |
| [PWR028](Checks/PWR028/) | Remove pointer increment preventing performance optimization                                                                                                       | optimization  | ✓ |         | ✓   |         |
| [PWR029](Checks/PWR029/) | Remove integer increment preventing performance optimization                                                                                                       | optimization  | ✓ | ✓       | ✓   |         |
| [PWR030](Checks/PWR030/) | Remove pointer assignment preventing performance optimization for perfectly nested loops                                                                           | optimization  | ✓ | ✓       | ✓   |         |
| [PWR031](Checks/PWR031/) | Replace pow by multiplication, division and/or square root                                                                                                         | optimization  | ✓ | ✓       | ✓   |         |
| [PWR032](Checks/PWR032/) | Avoid calls to mathematical functions with higher precision than required                                                                                          | optimization  | ✓ |         | ✓   |         |
| [PWR033](Checks/PWR033/) | Move invariant conditional out of the loop to avoid redundant computations                                                                                         | optimization  | ✓ | ✓       | ✓   |         |
| [PWR034](Checks/PWR034/) | Avoid strided array access to improve performance                                                                                                                  | optimization  | ✓ | ✓       | ✓   |         |
| [PWR035](Checks/PWR035/) | Avoid non-consecutive array access to improve performance                                                                                                          | optimization  | ✓ | ✓       | ✓   |         |
| [PWR036](Checks/PWR036/) | Avoid indirect array access to improve performance                                                                                                                 | optimization  | ✓ | ✓       | ✓   |         |
| [PWR037](Checks/PWR037/) | Potential precision loss in call to mathematical function                                                                                                          | correctness   | ✓ |         | ✓   |         |
| [PWR039](Checks/PWR039/) | Consider loop interchange to improve the locality of reference and enable vectorization                                                                            | optimization  | ✓ | ✓       | ✓   | ✓[^1]   |
| [PWR040](Checks/PWR040/) | Consider loop tiling to improve the locality of reference                                                                                                          | optimization  | ✓ | ✓       | ✓   |         |
| [PWR042](Checks/PWR042/) | Consider loop interchange by promoting the scalar reduction variable to an array                                                                                   | optimization  | ✓ | ✓       | ✓   |         |
| [PWR043](Checks/PWR043/) | Consider loop interchange by replacing the scalar reduction value                                                                                                  | optimization  | ✓ | ✓       | ✓   |         |
| [PWR044](Checks/PWR044/) | Avoid unnecessary floating-point data conversions involving constants                                                                                              | optimization  | ✓ |         | ✓   |         |
| [PWR045](Checks/PWR045/) | Replace division with a multiplication with a reciprocal                                                                                                           | optimization  | ✓ |         | ✓   |         |
| [PWR046](Checks/PWR046/) | Replace two divisions with a division and a multiplication                                                                                                         | optimization  | ✓ | ✓       | ✓   |         |
| [PWR048](Checks/PWR048/) | Replace multiplication/addition combo with an explicit call to fused multiply-add                                                                                  | optimization  | ✓ |         | ✓   |         |
| [PWR049](Checks/PWR049/) | Move iterator-dependent condition outside of the loop                                                                                                              | optimization  | ✓ | ✓       | ✓   |         |
| [PWR050](Checks/PWR050/) | Consider applying multithreading parallelism to forall loop                                                                                                        | optimization  | ✓ | ✓       | ✓   | ✓[^1]   |
| [PWR051](Checks/PWR051/) | Consider applying multithreading parallelism to scalar reduction loop                                                                                              | optimization  | ✓ | ✓       | ✓   | ✓[^1]   |
| [PWR052](Checks/PWR052/) | Consider applying multithreading parallelism to sparse reduction loop                                                                                              | optimization  | ✓ | ✓       | ✓   | ✓[^1]   |
| [PWR053](Checks/PWR053/) | Consider applying vectorization to forall loop                                                                                                                     | optimization  | ✓ | ✓       | ✓   | ✓[^1]   |
| [PWR054](Checks/PWR054/) | Consider applying vectorization to scalar reduction loop                                                                                                           | optimization  | ✓ | ✓       | ✓   | ✓[^1]   |
| [PWR055](Checks/PWR055/) | Consider applying offloading parallelism to forall loop                                                                                                            | optimization  | ✓ | ✓       | ✓   | ✓[^1]   |
| [PWR056](Checks/PWR056/) | Consider applying offloading parallelism to scalar reduction loop                                                                                                  | optimization  | ✓ | ✓       | ✓   | ✓[^1]   |
| [PWR057](Checks/PWR057/) | Consider applying offloading parallelism to sparse reduction loop                                                                                                  | optimization  | ✓ | ✓       | ✓   | ✓[^1]   |
| [PWR060](Checks/PWR060/) | Consider loop fission to separate gather memory access pattern                                                                                                     | optimization  | ✓ | ✓       | ✓   |         |
| [PWR062](Checks/PWR062/) | Consider loop interchange by removing accumulation on array value                                                                                                  | optimization  | ✓ | ✓       | ✓   |         |
| [PWR063](Checks/PWR063/) | Avoid using legacy Fortran constructs                                                                                                                              | modernization |   | ✓       |     |         |
| [PWR068](Checks/PWR068/) | Encapsulate procedures within modules to avoid the risks of calling implicit interfaces                                                                            | correctness   |   | ✓       |     |         |
| [PWR069](Checks/PWR069/) | Use the keyword only to explicitly state what to import from a module                                                                                              | correctness   |   | ✓       |     | ✓[^1]   |
| [PWR070](Checks/PWR070/) | Declare array dummy arguments as assumed-shape arrays                                                                                                              | correctness   |   | ✓       |     |         |
| [PWR071](Checks/PWR071/) | Prefer real(kind=kind_value) for declaring consistent floating types                                                                                               | modernization |   | ✓       |     |         |
| [PWR072](Checks/PWR072/) | Split the variable initialization from the declaration to prevent the implicit 'save' behavior                                                                     | correctness   |   | ✓       |     | ✓[^1]   |
| [PWR073](Checks/PWR073/) | Transform common block into a module for better data encapsulation                                                                                                 | modernization |   | ✓       |     |         |
| [PWR075](Checks/PWR075/) | Avoid using GNU Fortran extensions                                                                                                                                 | modernization |   | ✓       |     |         |
| [PWR079](Checks/PWR079/) | Avoid undefined behavior due to uninitialized variables                                                                                                            | correctness   | ✓ | ✓       | ✓   |         |
| [PWD002](Checks/PWD002/) | Unprotected multithreading reduction operation                                                                                                                     | correctness   | ✓ | ✓       | ✓   |         |
| [PWD003](Checks/PWD003/) | Missing array range in data copy to the GPU                                                                                                                        | correctness   | ✓ | ✓       | ✓   |         |
| [PWD004](Checks/PWD004/) | Out-of-memory-bounds array access                                                                                                                                  | correctness   | ✓ | ✓       | ✓   |         |
| [PWD005](Checks/PWD005/) | Array range copied to or from the GPU does not cover the used range                                                                                                | correctness   | ✓ | ✓       | ✓   |         |
| [PWD006](Checks/PWD006/) | Missing deep copy of non-contiguous data to the GPU                                                                                                                | correctness   | ✓ | ✓       | ✓   |         |
| [PWD007](Checks/PWD007/) | Unprotected multithreading recurrence                                                                                                                              | correctness   | ✓ | ✓       | ✓   |         |
| [PWD008](Checks/PWD008/) | Unprotected multithreading recurrence due to out-of-dimension-bounds array access                                                                                  | correctness   | ✓ | ✓       | ✓   |         |
| [PWD009](Checks/PWD009/) | Incorrect privatization in parallel region                                                                                                                         | correctness   | ✓ | ✓       | ✓   |         |
| [PWD010](Checks/PWD010/) | Incorrect sharing in parallel region                                                                                                                               | correctness   | ✓ | ✓       | ✓   |         |
| [PWD011](Checks/PWD011/) | Missing OpenMP lastprivate clause                                                                                                                                  | correctness   | ✓ | ✓       | ✓   |         |
| [RMK001](Checks/RMK001/) | Loop nesting that might benefit from hybrid parallelization using multithreading and SIMD                                                                          | optimization  | ✓ | ✓       | ✓   |         |
| [RMK002](Checks/RMK002/) | Loop nesting that might benefit from hybrid parallelization using offloading and SIMD                                                                              | optimization  | ✓ | ✓       | ✓   |         |
| [RMK003](Checks/RMK003/) | Potentially privatizable temporary variable                                                                                                                        | optimization  | ✓ |         | ✓   |         |
| [RMK007](Checks/RMK007/) | SIMD opportunity within a multithreaded region                                                                                                                     | optimization  | ✓ | ✓       | ✓   |         |
| [RMK008](Checks/RMK008/) | SIMD opportunity within an offloaded region                                                                                                                        | optimization  | ✓ | ✓       | ✓   |         |
| [RMK009](Checks/RMK009/) | Outline loop to increase compiler and tooling code coverage                                                                                                        | optimization  | ✓ |         | ✓   |         |
| [RMK010](Checks/RMK010/) | The vectorization cost model states the loop is not a SIMD opportunity due to strided memory accesses in the loop body                                             | optimization  | ✓ | ✓       | ✓   |         |
| [RMK012](Checks/RMK012/) | The vectorization cost model states the loop is not a SIMD opportunity because conditional execution renders vectorization inefficient                             | optimization  | ✓ | ✓       | ✓   |         |
| [RMK013](Checks/RMK013/) | The vectorization cost model states the loop is not a SIMD opportunity because loops with low trip count unknown at compile time do not benefit from vectorization | optimization  | ✓ | ✓       | ✓   |         |
| [RMK014](Checks/RMK014/) | The vectorization cost model states the loop is not a SIMD opportunity due to unpredictable memory accesses in the loop body                                       | optimization  | ✓ | ✓       | ✓   |         |
| [RMK015](Checks/RMK015/) | Tune compiler optimization flags to increase the speed of the code                                                                                                 | optimization  | ✓ | ✓       | ✓   |         |
| [RMK016](Checks/RMK016/) | Tune compiler optimization flags to avoid potential changes in floating point precision                                                                            | correctness   | ✓ | ✓       | ✓   |         |

**AutoFix**: Denotes tools that support automatic correction of the
corresponding check. Readers are encouraged to report additional tools with
autofix capabilities for these checks. The tools are tagged in the table as
follows:

## Contributing

We welcome and encourage contributions to the Open Catalog! Here's how you can
get involved:

1. **Join the discussion:**

    Got ideas, questions, or suggestions? Head over to our [GitHub
    Discussions](https://github.com/codee-com/open-catalog/discussions). It's
    the perfect place for open-ended conversations and brainstorming!

2. **Report issues:**

    Found inaccuracies, unclear explanations, or other problems? Please open an
    [Issue](https://github.com/codee-com/open-catalog/issues). Detailed reports
    help us quickly improve the quality of the project!

3. **Submit pull requests:**

    Interested in solving any issues? Feel free to fork the repository, make
    your changes, and submit a [Pull
    Request](https://github.com/codee-com/open-catalog/pulls). We'd love to see
    your contributions!


[^1]: [Codee](https://www.codee.com)
