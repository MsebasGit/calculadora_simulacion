// public/lanzador.js
import { WASI, Fd } from 'https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/+esm';
import ffiBuilder from './calculadora-simulacion.js';

globalThis.miso = {};

class ConsoleStdout extends Fd {
    constructor() { super(); this.decoder = new TextDecoder("utf-8"); }
    write(data) { console.log(this.decoder.decode(data).trim()); return data.length; }
}

class ConsoleStderr extends Fd {
    constructor() { super(); this.decoder = new TextDecoder("utf-8"); }
    write(data) { console.error(this.decoder.decode(data).trim()); return data.length; }
}

const args = [];
const env = [];
const fds = [ new Fd(), new ConsoleStdout(), new ConsoleStderr() ];

async function run() {
    try {
        const wasi = new WASI(args, env, fds);
        const ffi_exports = {};
        const ffi_imports = ffiBuilder(ffi_exports);

        const response = await fetch('./calculadora-simulacion.wasm');
        const wasmBytes = await response.arrayBuffer();

        const { instance } = await WebAssembly.instantiate(wasmBytes, {
            wasi_snapshot_preview1: wasi.wasiImport,
            ghc_wasm_jsffi: ffi_imports,
        });

        Object.assign(ffi_exports, instance.exports);
        wasi.initialize(instance);

        const loadingUI = document.getElementById("loading-ui");
        if (loadingUI) loadingUI.remove();

        instance.exports.hs_start();
    } catch (err) {
        console.error("Error al cargar WASM:", err);
    }
}

run();
