#!/bin/sh
# Assemble a static website in build/site from the exported documentation, the gallery and the project files.
set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUT="$ROOT/build/site"
rm -rf "$OUT" && mkdir -p "$OUT/manual" "$OUT/examples"
cp -R "$ROOT/build/docs/." "$OUT/manual/"
cp "$ROOT/examples/"*.lisp "$OUT/examples/" 2>/dev/null || true
cp "$ROOT/examples/gallery.html" "$OUT/examples/index.html" 2>/dev/null || true
cp "$ROOT/LICENSE" "$OUT/"
cp "$ROOT/docs/tour.cast" "$OUT/" 2>/dev/null || true
cp "$ROOT/packaging/site/index.html" "$OUT/index.html"
echo "wrote build/site/index.html"
