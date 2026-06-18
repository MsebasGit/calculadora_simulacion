#!/usr/bin/env bash
set -euo pipefail

# 1. Load the GHC WASM environment
if [ -f "$HOME/.ghc-wasm/env" ]; then
    source "$HOME/.ghc-wasm/env"
else
    echo "Error: No se encontró el entorno de ghc-wasm en $HOME/.ghc-wasm/env"
    exit 1
fi

echo "=== 1. Compilando aplicación Haskell a WebAssembly ==="
wasm32-wasi-cabal build

# Find the compiled .wasm file automatically
WASM_PATH=$(find dist-newstyle/ -name "calculadora-simulacion.wasm" -print -quit)

if [ -z "$WASM_PATH" ]; then
    echo "Error: No se encontró calculadora-simulacion.wasm en dist-newstyle/"
    exit 1
fi

echo "WASM encontrado en: $WASM_PATH"

# 2. Extract JS FFI bindings
echo "=== 2. Extrayendo bindings de JavaScript FFI ==="
mkdir -p public
node "$HOME/.ghc-wasm/wasm32-wasi-ghc/lib/post-link.mjs" -i "$WASM_PATH" -o public/calculadora-simulacion.js

# 3. Copy WASM binary to public/
echo "=== 3. Copiando binario WASM a la carpeta public/ ==="
cp "$WASM_PATH" public/calculadora-simulacion.wasm

echo "=== ¡Listo! Aplicación compilada con éxito en la carpeta public/ ==="
