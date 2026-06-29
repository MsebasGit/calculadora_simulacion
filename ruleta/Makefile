.PHONY: all clean update build serve

SHELL := /usr/bin/env bash

all: build

update:
	source ~/.ghc-wasm/env && wasm32-wasi-cabal update

build:
	# Build project
	source ~/.ghc-wasm/env && wasm32-wasi-cabal build app
	# Prepare public folder
	rm -rf public
	cp -r static public
	# Locate compiled wasm binary and perform post-linking
	source ~/.ghc-wasm/env && \
	my_wasm=$$(wasm32-wasi-cabal list-bin app | tail -n 1) && \
	$$(wasm32-wasi-ghc --print-libdir)/post-link.mjs --input "$$my_wasm" --output public/ghc_wasm_jsffi.js && \
	cp -v "$$my_wasm" public/app.wasm

serve:
	@echo "Servidor de desarrollo iniciado en http://localhost:8080"
	python3 -m http.server --directory public 8080

clean:
	rm -rf dist-newstyle public
