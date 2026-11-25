// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// The tokenize request the content script sends the service worker, and the
// bounds on it (BT-5194). The text comes from whatever page is open (any
// README, issue or pull request), and one service worker tokenizes for every
// tab, so a request is bounded before it is sent and checked again when it
// arrives.

export const MESSAGE_TYPE = "beamtalk-tokenize";

export const LIMITS = Object.freeze({
  /** Code blocks in one request. */
  maxBlocks: 200,
  /** Lines in one block. */
  maxLinesPerBlock: 5000,
  /** A longer line is sent as "" and left uncoloured. */
  maxLineLength: 2000,
  /** Characters across a whole request. */
  maxTotalChars: 1_000_000,
  /** Per-line `tokenizeLine` time limit; the rest of that line is left uncoloured. */
  lineTimeLimitMs: 50,
  /** Time for a whole request; lines after it are left uncoloured. */
  requestBudgetMs: 2000,
});

/**
 * Clip blocks (each `{ lines, continuous, ... }`) to {@link LIMITS}: at most
 * `maxBlocks` blocks of at most `maxLinesPerBlock` lines, over-long lines
 * replaced by "", and nothing after the line that would pass
 * `maxTotalChars`. Other block properties are kept, and `places` (the content
 * script's per-line DOM positions) is clipped with `lines`.
 * @template {{ lines: string[], continuous: boolean, places?: unknown[] }} B
 * @param {B[]} blocks
 * @returns {B[]}
 */
export function boundBlocks(blocks) {
  const out = [];
  let total = 0;
  for (const block of blocks.slice(0, LIMITS.maxBlocks)) {
    const lines = [];
    for (const line of block.lines.slice(0, LIMITS.maxLinesPerBlock)) {
      const kept = line.length > LIMITS.maxLineLength ? "" : line;
      if (total + kept.length > LIMITS.maxTotalChars) break;
      total += kept.length;
      lines.push(kept);
    }
    if (lines.length === 0 && block.lines.length > 0) break;
    const bounded = { ...block, lines };
    if (block.places) bounded.places = block.places.slice(0, lines.length);
    out.push(bounded);
    if (lines.length < Math.min(block.lines.length, LIMITS.maxLinesPerBlock)) break;
  }
  return out;
}

/**
 * The error for a tokenize request that is malformed or over {@link LIMITS},
 * or null for one the worker may tokenize.
 * @param {unknown} message
 * @returns {string | null}
 */
export function requestError(message) {
  const blocks = /** @type {{ blocks?: unknown }} */ (message)?.blocks;
  if (!Array.isArray(blocks)) return "blocks must be an array";
  if (blocks.length > LIMITS.maxBlocks) return `more than ${LIMITS.maxBlocks} blocks`;
  let total = 0;
  for (const block of blocks) {
    if (typeof block !== "object" || block === null) return "a block must be an object";
    if (typeof block.continuous !== "boolean") return "block.continuous must be a boolean";
    if (!Array.isArray(block.lines)) return "block.lines must be an array";
    if (block.lines.length > LIMITS.maxLinesPerBlock) {
      return `a block has more than ${LIMITS.maxLinesPerBlock} lines`;
    }
    for (const line of block.lines) {
      if (typeof line !== "string") return "every line must be a string";
      if (line.length > LIMITS.maxLineLength) {
        return `a line is longer than ${LIMITS.maxLineLength} characters`;
      }
      total += line.length;
    }
  }
  if (total > LIMITS.maxTotalChars) return `more than ${LIMITS.maxTotalChars} characters`;
  return null;
}
