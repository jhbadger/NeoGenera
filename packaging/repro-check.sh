#!/bin/sh
# Build the system twice in two different directories and require byte-identical results.
set -e
SRC="$(cd "$(dirname "$0")/.." && pwd)"
A="$(mktemp -d)"; B="$(mktemp -d)"
trap 'rm -rf "$A" "$B"' EXIT
for d in "$A" "$B"; do
  mkdir -p "$d/neogenera"
  (cd "$SRC" && cp -R lisp src tests docs examples packaging bin Makefile "$d/neogenera/" 2>/dev/null || cp -R lisp src tests docs bin Makefile "$d/neogenera/")
  (cd "$d/neogenera" && make world >/dev/null 2>&1)
done
if cmp -s "$A/neogenera/build/neogenera.img" "$B/neogenera/build/neogenera.img"; then
  echo "repro-check: images identical ($(wc -c < "$A/neogenera/build/neogenera.img") bytes)"
else
  echo "repro-check: IMAGES DIFFER" >&2; exit 1
fi
if cmp -s "$A/neogenera/build/neogenera" "$B/neogenera/build/neogenera"; then
  echo "repro-check: binaries identical"
else
  echo "repro-check: BINARIES DIFFER" >&2; exit 1
fi
