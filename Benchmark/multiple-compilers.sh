#!/usr/bin/env bash

# This script helps you run the benchmarks for a bunch of compilers, and store
# the results in a single CSV table

set -e

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)

# Just modify the compilers that you want to benchmark here. It will be passed
# to the CMAKE_<LANG>_COMPILER variables, so use the search rules that work there
ccompilers=("gcc" "clang")
fcompilers=("gfortran")

numccompilers=${#ccompilers[@]}
numfcompilers=${#fcompilers[@]}
if [ $numccompilers -gt $numfcompilers ]; then
	maxindices=("${!ccompilers[@]}")
else
	maxindices=("${!fcompilers[@]}")
fi

set -x

tmpresult=$(mktemp)
for i in ${!maxindices[@]}; do
	cmake_args=" -- "
	if [ $i -ge $numfcompilers ]; then
		cmake_args="${cmake_args} -DOCB_ENABLE_Fortran=OFF"
	else
		cmake_args="${cmake_args} -DCMAKE_Fortran_COMPILER=${fcompilers[i]}"
	fi

	if [ $i -ge $numccompilers ]; then
		cmake_args="${cmake_args} -DOCB_ENABLE_C=OFF"
	else
		cmake_args="${cmake_args} -DCMAKE_C_COMPILER=${ccompilers[i]}"
	fi

	looptmpdir=$(mktemp -d)
	$SCRIPT_DIR/run-benchmarks.py --out-csv $looptmpdir/tmp.csv --build $looptmpdir $cmake_args
	cat $looptmpdir/tmp.csv >>$tmpresult
done

cat $tmpresult | sort | uniq | tee result.csv
rm $tmpresult
