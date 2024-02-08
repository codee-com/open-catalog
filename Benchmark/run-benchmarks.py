#!/usr/bin/env python3

import argparse
import os
import subprocess
from pathlib import Path

SCRIPT_DIR = Path(os.path.dirname(os.path.realpath(__file__)))


def echorun(command, *args, **kwargs):
    print(f"* Running command '{' '.join(command)}'")
    return subprocess.run(command, *args, **kwargs)

def parse_args():
    parser = argparse.ArgumentParser(description="run the benchmark suite")
    parser.add_argument(
        "--build",
        default=Path("build/"),
        type=Path,
        help="the build directory for the benchmark artifacts",
    )
    return parser.parse_args()


def main():
    args = parse_args()
    if not args.build.exists():
        args.build.mkdir()
        # CMake 3.10 doesn't support the -B flag
        echorun(["cmake", f"{SCRIPT_DIR}"], cwd=args.build)

    echorun(["cmake", "--build", f"{args.build}", "--", "run"], check=True)

if __name__ == "__main__":
    main()
