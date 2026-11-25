// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// Service worker: owns the Oniguruma WASM engine and the grammar, and answers
// tokenize requests from the content script. WASM runs here rather than in
// the content script so it is governed by the extension's own CSP.

import { MESSAGE_TYPE, requestError } from "./protocol.js";
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

chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
  if (message?.type !== MESSAGE_TYPE) return false;
  // Only this extension's own content scripts; the shape and size are checked
  // before any tokenizing (BT-5194).
  if (sender?.id !== chrome.runtime.id) return false;
  const error = requestError(message);
  if (error) {
    sendResponse({ error: `rejected tokenize request: ${error}` });
    return false;
  }
  getTokenizer()
    .then((tokenizer) => sendResponse(tokenizer.tokenizeBlocks(message.blocks)))
    .catch((err) => sendResponse({ error: String(err) }));
  return true;
});
