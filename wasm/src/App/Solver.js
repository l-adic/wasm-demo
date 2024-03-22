import { WASI } from "@bjorn3/browser_wasi_shim";


export async function _initWASM () {
    const wasi = new WASI([], [], []);
    const wasiImportObj = { wasi_snapshot_preview1: wasi.wasiImport };
    const wasm = await WebAssembly.instantiateStreaming(fetch(new URL('./ormolu.wasm', import.meta.url)), wasiImportObj);
    wasi.inst = wasm.instance;
    return wasm
}

export function _solve(wasm, input) {
    const exports = wasm.instance.exports;
    const memory = exports.memory;
    const encoder = new TextEncoder();
    const decoder = new TextDecoder();
    const outputPtrPtr = exports.mallocPtr();
    console.log("Initialized WASI reactor.");
    console.log("Received message in WASI reactor.");
    const inputLen = Buffer.byteLength(input);
    const inputPtr = exports.malloc(inputLen);
    const inputArr = new Uint8Array(memory.buffer, inputPtr, inputLen);
    encoder.encodeInto(input, inputArr);
    const outputLen = exports.calculateWitnessRaw(inputPtr, inputLen, outputPtrPtr);
    const outputPtrArr = new Uint32Array(memory.buffer, outputPtrPtr, 1);
    const outputPtr = outputPtrArr[0];
    const outputArr = new Uint8Array(memory.buffer, outputPtr, outputLen);
    const output = decoder.decode(outputArr);
    exports.free(outputPtr);
    console.log("Output: ", output);
    return output
}