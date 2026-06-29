import { WASI, OpenFile, File, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/index.js";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

const args = [];
const env = ["GHCRTS=-H64m"];
const fds = [
  new OpenFile(new File([])), // stdin
  ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)),
];
const options = { debug: false };
const wasi = new WASI(args, env, fds, options);

const instance_exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("app.wasm"), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);
await instance.exports.hs_start();

// Decoupled HTML5 Drag and Drop handlers for chips placement
document.addEventListener("dragstart", (e) => {
  if (e.target.classList.contains("chip")) {
    const val = e.target.getAttribute("data-chip-value");
    e.dataTransfer.setData("text/plain", val);
    e.target.classList.add("dragging");
  }
});

document.addEventListener("dragend", (e) => {
  if (e.target.classList.contains("chip")) {
    e.target.classList.remove("dragging");
  }
});

document.addEventListener("dragover", (e) => {
  const cell = e.target.closest(".bet-cell");
  if (cell) {
    e.preventDefault(); // Required to allow drop!
    cell.classList.add("drag-hover");
  }
});

document.addEventListener("dragleave", (e) => {
  const cell = e.target.closest(".bet-cell");
  if (cell) {
    cell.classList.remove("drag-hover");
  }
});

document.addEventListener("drop", (e) => {
  const cell = e.target.closest(".bet-cell");
  if (cell) {
    e.preventDefault();
    cell.classList.remove("drag-hover");
    const val = e.dataTransfer.getData("text/plain");
    if (val) {
      // 1. Select the chip of value 'val' by clicking its button
      const chipBtn = document.querySelector(`.chip-${val}`);
      if (chipBtn) {
        chipBtn.click();
      }
      // 2. Click the cell itself to place the bet
      cell.click();
    }
  }
});
