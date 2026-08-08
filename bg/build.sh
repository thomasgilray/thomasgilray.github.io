#!/usr/bin/env bash
# Build the background renderer to ../js/. Requires:
#   rustup target add wasm32-unknown-unknown
#   cargo install wasm-bindgen-cli   (version must match the wasm-bindgen crate)
# wasm-opt is optional; if `npx wasm-opt` resolves it will be used to shrink output.
set -euo pipefail
cd "$(dirname "$0")"

OUT=../js

build() {
  local name=$1; shift
  # --lib only: the preview binary is native-only (pollster, png).
  cargo build --release --lib --target wasm32-unknown-unknown "$@"
  wasm-bindgen --target web --no-typescript \
    --out-dir "$OUT" --out-name "$name" \
    target/wasm32-unknown-unknown/release/conwaybg.wasm
  if command -v wasm-opt >/dev/null 2>&1; then
    wasm-opt -Oz --enable-bulk-memory --enable-nontrapping-float-to-int \
      -o "$OUT/${name}_bg.wasm" "$OUT/${name}_bg.wasm"
  fi
}

# Small module for browsers with WebGPU; larger one carrying the WebGL2 fallback.
build conwaybg-webgpu --no-default-features
build conwaybg

# Stamp a content hash of the freshly built wasm onto every page that loads the
# renderer. The filenames are stable, so without this a returning visitor can be
# served a cached module against fresh markup — and background.js in particular
# cannot bust itself, since a stale copy just keeps asking for its own stale version.
VER=$(cat "$OUT"/conwaybg-webgpu_bg.wasm "$OUT"/conwaybg_bg.wasm | sha1sum | cut -c1-10)
# The notes generator (../notes/build.py) embeds the same URL, so it is stamped
# too, along with any pages it has already emitted; both may not exist yet.
for page in ../index.html ../news/*.html ../notes/*/*.html ../notes/build.py; do
  [ -f "$page" ] || continue
  sed -i -E "s#(/js/background\.js)(\?v=[A-Za-z0-9]+)?#\1?v=$VER#" "$page"
done
echo "stamped /js/background.js?v=$VER onto the pages"

ls -la "$OUT"
