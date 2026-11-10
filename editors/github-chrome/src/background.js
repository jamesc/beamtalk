// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// Service worker: owns the Oniguruma WASM engine and the grammar, and answers
// tokenize requests from the content script. WASM runs here rather than in
// the content script so it is governed by the extension's own CSP.

import { createTokenizer } from "./tokenize.js";

/** @type {Promise<Awaited<ReturnType<typeof createTokenizer>>> | null} */
let tokenizerPromise = null;

function getTokenizer() {
  if (!tokenizerPromise) {
    tokenizerPromise = (async () => {
      const [wasm, grammar] = await Promise.all([
        fetch(chrome.runtime.getURL("onig.wasm")).then((r) => r.arrayBuffer()),
        fetch(chrome.runtime.getURL("beamtalk.tmLanguage.json")).then((r) => r.json()),
      ]);
      return createTokenizer({ wasm, grammar });
    })();
    tokenizerPromise.catch(() => {
      tokenizerPromise = null;
    });
  }
  return tokenizerPromise;
}

chrome.runtime.onMessage.addListener((message, _sender, sendResponse) => {
  if (message?.type !== "beamtalk-tokenize") return false;
  getTokenizer()
    .then((tokenizer) =>
      sendResponse({
        blocks: message.blocks.map((b) => tokenizer.tokenizeLines(b.lines, b.continuous)),
      })
    )
    .catch((err) => sendResponse({ error: String(err) }));
  return true;
});
