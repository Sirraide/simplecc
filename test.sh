#!/usr/bin/env bash

set -u

## FIXME: Actually depend on fchk properly.
for file in test/pp/*.c; do
   echo "Running tests in file $file..."
   ./simplecc "$file" > /tmp/a.c
   /bin/clang "$file" -E -P -Wno-comment > /tmp/b.c
   diff /tmp/a.c /tmp/b.c -w
done
