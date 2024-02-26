#! /bin/bash

set -e
stack clean
stack build -j$(nproc)

STR="hello"
CKSUM=$(echo -n "$STR" | sha256sum - | sed 's/^\(.*\)[[:space:]]*-$/\1/g' )

echo "with parallelism: "
time stack exec passcrack-exe +RTS -N$(nproc) -RTS $CKSUM

echo "without parallelism:"
time stack exec passcrack-exe +RTS -N1 -RTS $CKSUM

