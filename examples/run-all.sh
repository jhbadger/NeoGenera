#!/bin/sh
# Run every example; each must exit 0 and print the text on its ";;; expect:" line.  Writes build/examples/ and
# examples/gallery.html (sources and outputs) for the website.
cd "$(dirname "$0")/.." || exit 1
BIN=build/neogenera; IMG=build/neogenera.img
OUT=build/examples; mkdir -p "$OUT"
fail=0
html() { sed -e 's/&/\&amp;/g' -e 's/</\&lt;/g' -e 's/>/\&gt;/g'; }
{
  echo '<!DOCTYPE html><html lang="en"><head><meta charset="utf-8"><title>NeoGenera examples</title><style>body{font-family:sans-serif;max-width:60em;margin:2em auto;padding:0 1em}pre{background:#f4f4f4;padding:.7em;overflow:auto}h2{margin-top:2em}</style></head><body><h1>NeoGenera example gallery</h1>'
} > examples/gallery.html
for f in examples/[0-9]*.lisp; do
  name=$(basename "$f" .lisp)
  want=$(sed -n 's/^;;; expect: *//p' "$f" | head -1)
  NG_SOURCE_ROOT=. "$BIN" -i "$IMG" --no-toplevel --load "$f" > "$OUT/$name.out" 2>&1
  rc=$?
  if [ $rc -ne 0 ]; then echo "FAIL $name (exit $rc)"; tail -5 "$OUT/$name.out"; fail=1
  elif [ -n "$want" ] && ! grep -qF -- "$want" "$OUT/$name.out"; then echo "FAIL $name (output lacks: $want)"; fail=1
  else echo "ok   $name"; fi
  { echo "<h2>$name</h2><pre>"; html < "$f"; echo "</pre><h3>Output</h3><pre>"; html < "$OUT/$name.out"; echo "</pre>"; } >> examples/gallery.html
done
echo '</body></html>' >> examples/gallery.html
exit $fail
