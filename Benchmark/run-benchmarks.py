#!/usr/bin/env python3

import argparse
import csv
import json
import os
import platform
import re
import subprocess
import sys

from contextlib import nullcontext
from pathlib import Path

SCRIPT_DIR = Path(os.path.dirname(os.path.realpath(__file__)))


def echorun(command, *args, **kwargs):
    print(f"* Running command '{' '.join(command)}'")
    return subprocess.run(command, *args, **kwargs)


def get_processor_name():
    if platform.system() == "Windows":
        return platform.processor()

    if platform.system() == "Darwin":
        modified_env = os.environ.copy()
        modified_env["PATH"] += os.pathsep + "/usr/sbin"
        command = "sysctl -n machdep.cpu.brand_string"
        return subprocess.check_output(command, env=modified_env).decode().strip()

    if platform.system() == "Linux":
        command = "cat /proc/cpuinfo"
        all_info = subprocess.check_output(command, shell=True).decode().strip()
        for line in all_info.split("\n"):
            if "model name" in line:
                return re.sub(".*model name.*:", "", line, 1).strip()
    return ""


def get_language_from_benchmark(benchmark_name):
    if " C " in benchmark_name:
        return "C"
    if " Fortran " in benchmark_name:
        return "Fortran"
    sys.exit("Unexpected benchmark language")


def run_output_csv(csvfile, benchmarks, omit_header, compiler_info):
    csvwriter = csv.DictWriter(
        csvfile,
        [
            "check",
            "architecture",
            "processor",
            "cpu_scaling",
            "language",
            "compiler",
            "compiler_flags",
            "runtime_original",
            "runtime_optimized",
            "speedup",
        ],
        quoting=csv.QUOTE_MINIMAL,
    )
    if not omit_header:
        csvwriter.writeheader()
    context_info = None
    for benchmark in benchmarks:
        result = json.loads(
            subprocess.run(
                [benchmark, "--benchmark_format=json"],
                check=True,
                capture_output=True,
                encoding="UTF-8",
            ).stdout
        )
        context_info = result["context"]
        benchmark_results = result["benchmarks"]
        # All of our benchmarks should be in pairs of "original -> improved"
        sequential_pairs = [
            (benchmark_results[i], benchmark_results[i + 1])
            for i in range(0, len(benchmark_results) - 1, 2)
        ]
        for original, improved in sequential_pairs:
            language = get_language_from_benchmark(original["name"])
            csvwriter.writerow(
                {
                    "check": benchmark.name,
                    "architecture": platform.machine(),
                    "processor": get_processor_name(),
                    "cpu_scaling": context_info["cpu_scaling_enabled"],
                    "language": language,
                    "compiler": f"{compiler_info[language]['id']} {compiler_info[language]['version']}",
                    "compiler_flags": compiler_info[language]["flags"],
                    "runtime_original": f"{original['cpu_time']:.2f} {original['time_unit']}",
                    "runtime_optimized": f"{improved['cpu_time']:.2f} {improved['time_unit']}",
                    "speedup": f"{original['cpu_time'] / improved['cpu_time']:.2f}",
                }
            )


def join(first, second):
    if first and second:
        return first + " " + second
    return first + second


def get_compiler_info(build_dir):
    cached_variables = subprocess.run(
        ["cmake", "-LA", "-N", f"{SCRIPT_DIR}"],
        cwd=build_dir,
        check=True,
        capture_output=True,
        encoding="UTF-8",
    ).stdout
    # Set the flags per compiler. We're not setting target-specific flags ATM
    empty_compiler_object = {"id": "", "version": "", "flags": ""}
    compiler_info = {
        "C": empty_compiler_object.copy(),
        "Fortran": empty_compiler_object.copy(),
    }
    for assignment in cached_variables.splitlines():
        (variable_and_type, _, value) = assignment.partition("=")
        variable = variable_and_type.rsplit(":")[0]
        if variable == "OCB_C_COMPILER_ID":
            compiler_info["C"]["id"] = value
        elif variable == "OCB_C_COMPILER_VERSION":
            compiler_info["C"]["version"] = value
        elif variable == "OCB_Fortran_COMPILER_ID":
            compiler_info["Fortran"]["id"] = value
        elif variable == "OCB_Fortran_COMPILER_VERSION":
            compiler_info["Fortran"]["version"] = value
        elif variable == "CMAKE_C_FLAGS":
            compiler_info["C"]["flags"] = join(
                compiler_info["C"].get("flags", ""), value
            )
        elif variable == "CMAKE_Fortran_FLAGS":
            compiler_info["Fortran"]["flags"] = join(
                compiler_info["Fortran"].get("flags", ""), value
            )
        elif variable == "CMAKE_C_FLAGS_RELEASE":
            compiler_info["C"]["flags"] = join(
                value, compiler_info["C"].get("flags", "")
            )
        elif variable == "CMAKE_Fortran_FLAGS_RELEASE":
            compiler_info["Fortran"]["flags"] = join(
                value, compiler_info["Fortran"].get("flags", "")
            )

    return compiler_info


def parse_args():
    parser = argparse.ArgumentParser(description="run the benchmark suite")
    parser.add_argument(
        "--build",
        default=Path("build/"),
        type=Path,
        help="the build directory for the benchmark artifacts",
    )
    parser.add_argument("--check", help="execute only the given check's benchmark")
    parser.add_argument("--out-csv", type=Path, help="output the results to a CSV")
    parser.add_argument(
        "--omit-csv-header",
        action="store_true",
        help="when enabled, omits the CSV column headers",
    )
    parser.add_argument(
        "cmake_args", nargs="*", help="Use '--' to separate the cmake arguments"
    )
    return parser.parse_args()


def main():
    args = parse_args()
    if not args.build.exists():
        args.build.mkdir()
    # CMake 3.10 doesn't support the -B flag
    echorun(["cmake", f"{SCRIPT_DIR}", *args.cmake_args], cwd=args.build, check=True)
    echorun(["cmake", "--build", f"{args.build}", "--", "all"], check=True)

    benchmarks_to_run = list((args.build / Path("bin")).iterdir())
    if args.check:
        benchmarks_to_run = [
            benchmark for benchmark in benchmarks_to_run if benchmark.name == args.check
        ]
        if not benchmarks_to_run:
            print(f"error: no benchmarks matching {args.check}", file=sys.stderr)
            exit(1)

    if not args.out_csv:
        for benchmark in benchmarks_to_run:
            subprocess.run(
                [benchmark],
                check=True,
                encoding="UTF-8",
            )
    else:
        with (
            nullcontext(sys.stdout)
            if str(args.out_csv) == "-"
            else open(args.out_csv, "w", newline="")
        ) as csvfile:
            run_output_csv(
                csvfile,
                benchmarks_to_run,
                args.omit_csv_header,
                get_compiler_info(args.build),
            )


if __name__ == "__main__":
    main()
