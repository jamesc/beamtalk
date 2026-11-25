// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// The service worker's message handler (BT-5194): it answers only this
// extension's own tokenize requests, and checks a request's shape and size
// before tokenizing anything. `chrome` and `fetch` are stubbed.

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { test } from "node:test";
import { LIMITS, MESSAGE_TYPE } from "../src/protocol.js";

const require = createRequire(import.meta.url);
const assets = {
  "onig.wasm": () => readFileSync(require.resolve("vscode-oniguruma/release/onig.wasm")),
  "beamtalk.tmLanguage.json": () =>
    readFileSync(new URL("../../vscode/syntaxes/beamtalk.tmLanguage.json", import.meta.url)),
};

let listener;
let fetches = 0;
globalThis.chrome = {
  runtime: {
    id: "this-extension",
    getURL: (name) => name,
    onMessage: { addListener: (fn) => (listener = fn) },
  },
};
globalThis.fetch = async (name) => {
  fetches += 1;
  const bytes = assets[name]();
  return {
    arrayBuffer: async () => bytes.buffer.slice(bytes.byteOffset, bytes.byteOffset + bytes.length),
    json: async () => JSON.parse(bytes.toString("utf8")),
  };
};
await import("../src/background.js");

const self = { id: "this-extension" };

/** Sends `message` from `sender`; resolves to [handled, response]. */
function send(message, sender = self) {
  return new Promise((resolve) => {
    let handled;
    const timer = setTimeout(() => resolve([handled, undefined]), 2000);
    handled = listener(message, sender, (response) => {
      clearTimeout(timer);
      resolve([handled, response]);
    });
    if (handled === false) {
      clearTimeout(timer);
      // A synchronous `sendResponse` has already resolved; otherwise none comes.
      setImmediate(() => resolve([handled, undefined]));
    }
  });
}

test("malformed or over-limit requests are refused before any tokenizing", async () => {
  for (const blocks of [
    undefined,
    "lines",
    [{ lines: ["x"] }],
    Array.from({ length: LIMITS.maxBlocks + 1 }, () => ({ lines: [], continuous: true })),
    [{ lines: ["x".repeat(LIMITS.maxLineLength + 1)], continuous: true }],
  ]) {
    const [, response] = await send({ type: MESSAGE_TYPE, blocks });
    assert.match(response?.error ?? "", /rejected tokenize request/, JSON.stringify(blocks)?.slice(0, 80));
  }
  // Runs first in this file: nothing has loaded the tokenizer yet.
  assert.equal(fetches, 0, "a refused request must not load or run the tokenizer");
});

test("a well-formed request is tokenized", async () => {
  const [handled, response] = await send({
    type: MESSAGE_TYPE,
    blocks: [{ lines: ['"hi"'], continuous: true }],
  });
  assert.equal(handled, true);
  assert.deepEqual(response, { blocks: [[[[0, 4, "string"]]]], incomplete: [] });
});

test("other messages and other senders are ignored", async () => {
  assert.deepEqual(await send({ type: "something-else" }), [false, undefined]);
  const ok = { type: MESSAGE_TYPE, blocks: [{ lines: ["x"], continuous: true }] };
  assert.deepEqual(await send(ok, { id: "another-extension" }), [false, undefined]);
});
