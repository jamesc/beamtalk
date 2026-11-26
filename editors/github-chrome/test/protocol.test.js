// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// The tokenize request's shape and size checks (BT-5194).

import assert from "node:assert/strict";
import { test } from "node:test";
import { boundBlocks, LIMITS, requestError } from "../src/protocol.js";

const ok = { lines: ["x := 1"], continuous: true };

test("a well-formed request within the limits is accepted", () => {
  assert.equal(requestError({ blocks: [ok, { lines: [], continuous: false }] }), null);
});

test("malformed requests are rejected", () => {
  for (const message of [
    undefined,
    {},
    { blocks: "x" },
    { blocks: [null] },
    { blocks: [{ lines: ["x"] }] },
    { blocks: [{ lines: "x", continuous: true }] },
    { blocks: [{ lines: [1], continuous: true }] },
  ]) {
    assert.notEqual(requestError(message), null, JSON.stringify(message));
  }
});

test("requests over each limit are rejected", () => {
  const blocks = (n, block = ok) => ({ blocks: Array.from({ length: n }, () => block) });
  assert.match(requestError(blocks(LIMITS.maxBlocks + 1)), /blocks/);
  const longBlock = { lines: Array(LIMITS.maxLinesPerBlock + 1).fill(""), continuous: true };
  assert.match(requestError(blocks(1, longBlock)), /lines/);
  const longLine = { lines: ["x".repeat(LIMITS.maxLineLength + 1)], continuous: true };
  assert.match(requestError(blocks(1, longLine)), /longer/);
  const wide = { lines: Array(LIMITS.maxLinesPerBlock).fill("x".repeat(LIMITS.maxLineLength)), continuous: true };
  assert.match(requestError(blocks(LIMITS.maxBlocks, wide)), /characters/);
});

test("boundBlocks clips any page's blocks to an accepted request", () => {
  const huge = Array.from({ length: LIMITS.maxBlocks + 5 }, () => ({
    lines: Array(LIMITS.maxLinesPerBlock + 5).fill("x".repeat(LIMITS.maxLineLength)),
    places: Array(LIMITS.maxLinesPerBlock + 5).fill(0),
    continuous: true,
  }));
  huge[0].lines[0] = "y".repeat(LIMITS.maxLineLength + 1);
  const bounded = boundBlocks(huge);
  assert.equal(requestError({ blocks: bounded }), null);
  assert.ok(bounded.length > 0);
  assert.equal(bounded[0].lines[0], "");
  assert.equal(bounded[0].places.length, bounded[0].lines.length);
});
