# Benchmarks for the Open Catalog on best practices for performance

## Requirements

- CMake version 3.10 or higher
- A C++ compiler with C++11 support
- A Fortran 90 compiler (optional if you want to run the Fortran benchmarks)
- Google benchmark library version 1.5.3 or above (optional)
- Python 3 (optional if you want to use the benchmark script)

## Instructions to run the benchmarks

Use CMake to configure and build as a normal project. Execute the `run` target
to run all the available benchmarks, or run-<CHECK ID> to run a specific one.

Or simply run the following command to do it automatically:
```
./run-benchmarks.py
```

### Advanced execution
Execute with `--help` to see all the features of the script!

```
./run-benchmarks.py --help
```

For instance, you can export the results of the benchmarks as a CSV file:

```
./run-benchmarks.py --out-csv results.csv
```

There's also a convenience script `multiple-compilers.sh` to compare multiple
compilers for the same language. You need to use a text editor and update the
following lines to choose which compilers to use:

```
# Just modify the compilers that you want to benchmark here. It will be passed
# to the CMAKE_<LANG>_COMPILER variables, so use the search rules that work there
ccompilers=("gcc" "clang")
fcompilers=("gfortran")
```

Now, you can run the script and it will output a table with the aggregated results of the benchmarks:

```
./multiple-compilers.sh
<...>
+ tee result.csv
check,architecture,processor,cpu_scaling,language,compiler,compiler_flags,runtime_original,runtime_optimized,speedup
PWR039,x86_64,13th Gen Intel(R) Core(TM) i7-13700H,True,C,Clang 14.0.6,-O3 -DNDEBUG,1056.04 us,356.37 us,2.96
PWR039,x86_64,13th Gen Intel(R) Core(TM) i7-13700H,True,C,GNU 12.2.0,-O3 -DNDEBUG,1103.94 us,385.79 us,2.86
PWR039,x86_64,13th Gen Intel(R) Core(TM) i7-13700H,True,Fortran,GNU 12.2.0,-O3 -DNDEBUG -O3,934.73 us,259.75 us,3.60
<...>
```

## Benchmarking tips

In order to produce noiseless and more consistent results, you can take
different actions that vary depending on your OS. For Linux, we recommend the
[LLVM benchmarking tips](https://llvm.org/docs/Benchmarking.html) and the
[Google Benchmark tips](https://github.com/google/benchmark/blob/main/docs/reducing_variance.md).

## CMake options

- **`OCB_ENABLE_C`**:BOOL

  Enable the available benchmarks for the C family language (defaults to ON).

- **`OCB_ENABLE_Fortran`**:BOOL

  Enable the available benchmarks for the Fortran language (default to ON if
  there is a recognized Fortran compiler in the system).
