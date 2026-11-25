#!/bin/sh
# Build and run the C kernel unit tests.  Set SAN=1 for ASan+UBSan.
set -e
cd "$(dirname "$0")/../.."
mkdir -p build
SRCS=$(ls src/*.c | grep -v 'src/main.c')
if [ "${SAN:-0}" = "1" ]; then
  FLAGS="-O1 -g -fsanitize=address,undefined -fno-omit-frame-pointer"
else
  FLAGS="-O2 -g"
fi
${CC:-cc} $FLAGS -std=gnu11 -fno-strict-aliasing -Wall -Wno-unused-function \
  -o build/kernel_tests tests/c/kernel_tests.c $SRCS -lm -lpthread
ASAN_OPTIONS=detect_leaks=0 ./build/kernel_tests
