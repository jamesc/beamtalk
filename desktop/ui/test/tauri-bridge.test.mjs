// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// `tauri-bridge.js` (BT-5194): the picker's `invoke`/`listen` over
// `__TAURI_INTERNALS__`, which replaced the `withGlobalTauri` API bundle.
// Run with `node --test desktop/ui/test/*.test.mjs` (`just test-desktop`).

import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { runInNewContext } from "node:vm";

const uiDir = new URL("../", import.meta.url);
const bridgeSource = readFileSync(new URL("tauri-bridge.js", uiDir), "utf8");

// Load the bridge into a fresh global with a recording `__TAURI_INTERNALS__`.
function loadBridge() {
  const calls = [];
  const unregistered = [];
  const callbacks = [];
  const sandbox = {
    __TAURI_INTERNALS__: {
      invoke(cmd, args) {
        // JSON round-trip: the bridge builds `args` in another vm realm, whose
        // objects `deepStrictEqual` would not match against this realm's.
        calls.push(JSON.parse(JSON.stringify({ cmd, args })));
        return Promise.resolve(cmd === "plugin:event|listen" ? 7 : "ok");
      },
      transformCallback(callback) {
        callbacks.push(callback);
        return callbacks.length;
      },
    },
    __TAURI_EVENT_PLUGIN_INTERNALS__: {
      unregisterListener(event, eventId) {
        unregistered.push({ event, eventId });
      },
    },
  };
  sandbox.globalThis = sandbox;
  runInNewContext(bridgeSource, sandbox);
  return { ipc: sandbox.pickerIpc, calls, unregistered, callbacks };
}

test("invoke passes the command and its arguments straight through", async () => {
  const { ipc, calls } = loadBridge();
  assert.equal(await ipc.invoke("attach", { workspaceId: "abc" }), "ok");
  assert.equal(await ipc.invoke("quit"), "ok");
  assert.deepEqual(calls, [
    { cmd: "attach", args: { workspaceId: "abc" } },
    { cmd: "quit", args: {} },
  ]);
});

test("listen registers a callback for any target and returns an unlisten", async () => {
  const { ipc, calls, unregistered, callbacks } = loadBridge();
  const seen = [];
  const unlisten = await ipc.listen("attach-progress", (e) => seen.push(e));
  assert.deepEqual(calls, [
    {
      cmd: "plugin:event|listen",
      args: { event: "attach-progress", target: { kind: "Any" }, handler: 1 },
    },
  ]);
  callbacks[0]({ payload: 1 });
  assert.deepEqual(seen, [{ payload: 1 }]);

  await unlisten();
  assert.deepEqual(unregistered, [{ event: "attach-progress", eventId: 7 }]);
  assert.deepEqual(calls[1], {
    cmd: "plugin:event|unlisten",
    args: { event: "attach-progress", eventId: 7 },
  });
});

test("the picker loads the bridge before main.js and never uses window.__TAURI__", () => {
  const html = readFileSync(new URL("index.html", uiDir), "utf8");
  const bridgeAt = html.indexOf('<script src="tauri-bridge.js"></script>');
  const mainAt = html.indexOf('<script src="main.js"></script>');
  assert.ok(bridgeAt >= 0 && mainAt > bridgeAt);
  const main = readFileSync(new URL("main.js", uiDir), "utf8");
  assert.doesNotMatch(main, /__TAURI__\./);
});
