#!/bin/bash

cd src

# Resolve absolute paths
BJVM2_TS_ABS=$(realpath ../../bjvm2.ts)
BJVM_MAIN_JS_ABS=$(realpath ../../../build/bjvm_main.js)
BJVM_MAIN_D_TS_ABS=$(realpath ../../../build/bjvm_main.d.ts)
BJVM_MAIN_WASM=$(realpath ../../../build/bjvm_main.wasm)

# Create symbolic links (force overwrite if they exist)
ln -sf "$BJVM2_TS_ABS" lib/bjvm_links/bjvm_interface/bjvm2.ts
ln -sf "$BJVM_MAIN_JS_ABS" lib/bjvm_links/build/bjvm_main.js
ln -sf "$BJVM_MAIN_D_TS_ABS" lib/bjvm_links/build/bjvm_main.d.ts
ln -sf "$BJVM_MAIN_WASM" ../static/bjvm_links/bjvm_main.wasm

echo "Setup completed successfully."
