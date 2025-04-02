#!/usr/bin/env bash

set -eu

## FIXME: Actually depend on fchk properly.
for file in test/*.c; do
   echo "Running tests in file $file..."
   ../Source/fchk/out/fchk --prefix // "$file" -D cc=./simplecc
done
