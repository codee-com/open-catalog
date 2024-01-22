#!/usr/bin/env bash

set -xe

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
BUILD_DIR="build/"

if [ ! -e "${BUILD_DIR}" ]; then
  # CMake 3.10 doesn't support the -B flag
  mkdir "${BUILD_DIR}"
  ( cd "${BUILD_DIR}" && cmake "${SCRIPT_DIR}")
fi

cmake --build "${BUILD_DIR}" -- run
