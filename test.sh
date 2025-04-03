#!/usr/bin/env bash

set -u

## FIXME: Actually depend on fchk properly.
for file in test/*.c; do
   echo "Running tests in file $file..."
   ./simplecc "$file" > /tmp/a.c
   /bin/clang "$file" -E -P > /tmp/b.c
   diff /tmp/a.c /tmp/b.c -w
done
