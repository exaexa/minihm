
import { WASI } from "https://cdn.jsdelivr.net/npm/@runno/wasi@0.7.0/dist/wasi.js";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

const wasi = new WASI({
    stdout: (out) => console.log("[wasm stdout]", out),
});

const jsffiExports = {};
const wasm = await WebAssembly.instantiateStreaming(
  fetch("./minihm.wasm"),
  Object.assign(
    { ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports) },
    wasi.getImportObject()
  )
);
Object.assign(jsffiExports, wasm.instance.exports);

wasi.initialize(wasm, {
    ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports)
});

export const minihm = wasi.instance.exports.minihm;
