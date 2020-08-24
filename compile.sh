#!/bin/sh

set -e

ELM="src/Main.elm"
JAVASCRIPT="elm.js"
MINIFIED="elm.min.js"

elm make --optimize --output="$JAVASCRIPT" "$ELM"

uglifyjs "$JAVASCRIPT" --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle > "$MINIFIED"

echo "Compiled size:$(wc -c < "$JAVASCRIPT") bytes  ($JAVASCRIPT)"
echo "Minified size:$(wc -c < "$MINIFIED") bytes  ($MINIFIED)"
echo "Gzipped size: $(gzip --stdout "$MINIFIED" | wc -c) bytes"