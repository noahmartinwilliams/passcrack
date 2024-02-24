#! /bin/bash

set -e
stack build -j$(nproc)

STR="hello"

echo "without parallelism:"
time ./.stack-work/dist/x86_64-linux-tinfo6/ghc-*/build/passcrack-exe/passcrack-exe +RTS -N1 -RTS $(echo -n "$STR" | sha256sum - | sed 's/^\(.*\)[[:space:]]*.*$/\1/g')

echo "with parallelism: "
time ./.stack-work/dist/x86_64-linux-tinfo6/ghc-*/build/passcrack-exe/passcrack-exe +RTS -N$(nproc) -RTS $(echo -n "$STR" | sha256sum - | sed 's/^\(.*\)[[:space:]]*.*$/\1/g')
