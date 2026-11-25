// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// The content script (BT-5194): finding Beamtalk code in github.com's markup,
// mapping tokens to DOM ranges, and bounding what it sends the worker. Runs
// against jsdom; `chrome`, `CSS.highlights` and `Highlight` are stubbed.

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";
import { JSDOM } from "jsdom";
import {
  blobBlocks,
  blockRanges,
  diffBlocks,
  fenceBlocks,
  locate,
  scan,
  textIndex,
  tokenize,
} from "../src/content.js";
import { LIMITS, MESSAGE_TYPE } from "../src/protocol.js";

/** Installs a jsdom page as the globals the content script reads. */
function page(html, url = "https://github.com/owner/repo") {
  const { window } = new JSDOM(`<!doctype html><body>${html}</body>`, { url });
  Object.assign(globalThis, {
    document: window.document,
    location: window.location,
    NodeFilter: window.NodeFilter,
    Range: window.Range,
  });
  return window.document;
}

/** Records every message sent to the worker and answers with one token per line. */
function stubWorker() {
  const sent = [];
  globalThis.chrome = {
    runtime: {
      async sendMessage(message) {
        sent.push(message);
        return { blocks: message.blocks.map((b) => b.lines.map(() => [[0, 1, "keyword"]])) };
      },
    },
  };
  return sent;
}

test("fenced ```beamtalk blocks are found with each line's offset", () => {
  page('<pre lang="beamtalk">x := 1.\ny := 22</pre><pre lang="python">no</pre>');
  const [block, ...rest] = fenceBlocks();
  assert.equal(rest.length, 0);
  assert.equal(block.continuous, true);
  assert.deepEqual(block.lines, ["x := 1.", "y := 22"]);
  assert.deepEqual(block.places.map((p) => p.base), [0, 8]);
});

test("a .bt file view is found; other files are not", () => {
  const html = '<div class="react-file-line">a</div><div class="react-file-line">b\n</div>';
  page(html, "https://github.com/o/r/blob/main/src/Counter.bt");
  assert.deepEqual(blobBlocks().map((b) => b.lines), [["a", "b"]]);
  page(html, "https://github.com/o/r/blob/main/README.md");
  assert.deepEqual(blobBlocks(), []);
});

test("diff regions are found only for Beamtalk paths, one fresh state per line", () => {
  const region = (path) =>
    `<div role="region"><h3><code>‎${path}‏</code></h3>` +
    '<code class="diff-text"><span class="diff-text-inner">+ x := 1</span></code></div>';
  page(region("src/A.bt") + region("src/B.rs"));
  const blocks = diffBlocks();
  assert.equal(blocks.length, 1);
  assert.equal(blocks[0].continuous, false);
  assert.deepEqual(blocks[0].lines, ["+ x := 1"]);
});

test("token offsets map to ranges across text-node boundaries", () => {
  const doc = page('<pre lang="beamtalk"><span>foo</span>bar<span>baz</span></pre>');
  const root = doc.querySelector("pre");
  const index = textIndex(root);
  assert.equal(index.nodes.length, 3);
  // preferNext picks the node a token starts in, not the one it ends.
  assert.equal(locate(index, 3, true)[0].data, "bar");
  assert.equal(locate(index, 3, false)[0].data, "foo");

  const block = { continuous: true, lines: ["foobarbaz"], places: [{ root, base: 0 }] };
  const ranges = blockRanges(block, [[[2, 7, "string"]]]);
  assert.deepEqual(
    ranges.map(([range, category]) => [range.toString(), category]),
    [["obarb", "string"]]
  );
});

test("tokenize sends one typed request and caches the answer", async () => {
  page("");
  const sent = stubWorker();
  const block = { continuous: true, lines: ["cache me"], places: [] };
  const first = await tokenize([block]);
  const second = await tokenize([{ ...block }]);
  assert.equal(sent.length, 1);
  assert.equal(sent[0].type, MESSAGE_TYPE);
  assert.deepEqual(sent[0].blocks, [{ lines: ["cache me"], continuous: true }]);
  assert.deepEqual(second, first);
});

test("a scan never sends the worker more than the request limits", async () => {
  // More fences than `maxBlocks`, one with an over-long line.
  const fences = Array.from(
    { length: LIMITS.maxBlocks + 25 },
    (_, i) => `<pre lang="beamtalk">block ${i}</pre>`
  ).join("");
  page(`<pre lang="beamtalk">${"x".repeat(LIMITS.maxLineLength + 1)}</pre>${fences}`);
  const sent = stubWorker();
  globalThis.CSS = { highlights: new Map() };
  globalThis.Highlight = class extends Set {};

  await scan();

  assert.equal(sent.length, 1);
  const { blocks } = sent[0];
  assert.equal(blocks.length, LIMITS.maxBlocks);
  assert.deepEqual(blocks[0].lines, [""], "an over-long line is sent empty");
  assert.ok(CSS.highlights.get("beamtalk-keyword").size > 0);
});

test("the manifest injects only on github.com, whose markup the selectors follow", () => {
  const manifest = JSON.parse(readFileSync(new URL("../manifest.json", import.meta.url)));
  assert.deepEqual(
    manifest.content_scripts.map((c) => c.matches),
    [["https://github.com/*"]]
  );
});
