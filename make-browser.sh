#!/bin/bash
WASM32GHC=${WASM32GHC:-wasm32-wasi-ghc-9.14}

mkdir -p pages
${WASM32GHC} browser.hs -o pages/minihm.wasm -no-hs-main -optl-mexec-model=reactor -optl-Wl,--export=minihm,--export=hs_init && \
$(${WASM32GHC} --print-libdir)/post-link.mjs -i pages/minihm.wasm -o pages/ghc_wasm_jsffi.js
