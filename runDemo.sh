#! /bin/bash

set -e
stack build -j$(nproc)

STR="hello"
CKSUM=$(echo -n "$STR" | sha256sum - | sed 's/^\(.*\)[[:space:]]*-$/\1/g' )

echo "with parallelism: "
time stack exec --rts-options -N$(nproc) passcrack-exe $CKSUM

echo "without parallelism:"
time stack exec --rts-options -N1 passcrack-exe $CKSUM

