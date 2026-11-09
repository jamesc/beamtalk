// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// Finds Beamtalk code on GitHub pages and colours it via the CSS Custom
// Highlight API. Highlights are painted from Range objects, so GitHub's
// React-managed DOM is never modified (mutating it breaks React's
// reconciliation). Tokenizing happens in the service worker.

const BEAMTALK_PATH = /\.(bt|btscript)$/;
const CATEGORIES = ["comment", "string", "constant", "keyword", "type", "entity", "heading"];
const FENCE_SELECTOR = [
  'pre[lang="beamtalk" i]',
  'pre[lang="btscript" i]',
  ".highlight-source-beamtalk > pre",
].join(",");

/**
 * A run of source lines tokenized together. `places[i]` says where line i
 * lives in the DOM: its root element and the offset of the line within the
 * root's text.
 * @typedef {{ continuous: boolean, lines: string[], places: Array<{ root: Element, base: number }> }} Block
 */

/** @param {Element} el */
function lineText(el) {
  return (el.textContent ?? "").replace(/\n$/, "");
}

/** @param {Element[]} els */
function lineElementsBlock(els, continuous) {
  return {
    continuous,
    lines: els.map(lineText),
    places: els.map((root) => ({ root, base: 0 })),
  };
}

/** File view: `/<owner>/<repo>/blob/<ref>/<path>.bt`. */
function blobBlocks() {
  if (!/\/blob\//.test(location.pathname) || !BEAMTALK_PATH.test(location.pathname)) return [];
  const lines = [...document.querySelectorAll(".react-file-line")];
  return lines.length ? [lineElementsBlock(lines, true)] : [];
}

/** Markdown fences: ```beamtalk renders as <pre lang="beamtalk">. */
function fenceBlocks() {
  return [...document.querySelectorAll(FENCE_SELECTOR)].map((pre) => {
    const lines = (pre.textContent ?? "").split("\n");
    let base = 0;
    const places = lines.map((line) => {
      const place = { root: pre, base };
      base += line.length + 1;
      return place;
    });
    return { continuous: true, lines, places };
  });
}

/** Commit and pull-request diffs; each line is tokenized on its own. */
function diffBlocks() {
  const blocks = [];
  for (const region of document.querySelectorAll('[role="region"]')) {
    const path = region.querySelector("h3 code")?.textContent?.replace(/[‎‏]/g, "").trim();
    if (!path || !BEAMTALK_PATH.test(path)) continue;
    const lines = [...region.querySelectorAll("code.diff-text .diff-text-inner")];
    if (lines.length) blocks.push(lineElementsBlock(lines, false));
  }
  return blocks;
}

// --- tokenizing -------------------------------------------------------------

/** @type {Map<string, Array<Array<[number, number, string]>>>} */
const tokenCache = new Map();
const TOKEN_CACHE_LIMIT = 1000;

/** @param {Block} block */
function cacheKey(block) {
  return `${block.continuous ? 1 : 0}\u0000${block.lines.join("\n")}`;
}

/** @param {Block[]} blocks */
async function tokenize(blocks) {
  const missing = blocks.filter((b) => !tokenCache.has(cacheKey(b)));
  if (missing.length) {
    const response = await chrome.runtime.sendMessage({
      type: "beamtalk-tokenize",
      blocks: missing.map((b) => ({ lines: b.lines, continuous: b.continuous })),
    });
    if (!response || response.error) {
      throw new Error(`beamtalk tokenizer: ${response?.error ?? "no response"}`);
    }
    missing.forEach((b, i) => tokenCache.set(cacheKey(b), response.blocks[i]));
    while (tokenCache.size > TOKEN_CACHE_LIMIT) {
      tokenCache.delete(tokenCache.keys().next().value);
    }
  }
  return blocks.map((b) => tokenCache.get(cacheKey(b)));
}

// --- ranges -----------------------------------------------------------------

/** Text nodes of a root with their starting offsets in the root's text. */
function textIndex(root) {
  const nodes = [];
  const starts = [];
  let offset = 0;
  const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
  for (let node = walker.nextNode(); node; node = walker.nextNode()) {
    nodes.push(node);
    starts.push(offset);
    offset += node.data.length;
  }
  return { nodes, starts };
}

/** Maps a text offset to a (node, offset) boundary point. */
function locate(index, offset, preferNext) {
  const { nodes, starts } = index;
  let lo = 0;
  let hi = nodes.length - 1;
  while (lo < hi) {
    const mid = (lo + hi + 1) >> 1;
    if (starts[mid] < offset || (starts[mid] === offset && preferNext)) lo = mid;
    else hi = mid - 1;
  }
  const node = nodes[lo];
  return node ? [node, Math.min(offset - starts[lo], node.data.length)] : null;
}

/**
 * Per-root ranges, reused across scans while the root's text nodes are
 * untouched. @type {WeakMap<Element, { nodes: Text[], text: string, ranges: Array<[Range, string]> }>}
 */
const rangeCache = new WeakMap();

function cachedRanges(root) {
  const cached = rangeCache.get(root);
  if (!cached || cached.text !== root.textContent) return null;
  return cached.nodes.every((n) => n.parentNode && root.contains(n)) ? cached.ranges : null;
}

/** @param {Block} block @param {Array<Array<[number, number, string]>>} tokens */
function blockRanges(block, tokens) {
  /** @type {Map<Element, Array<[Range, string]>>} */
  const byRoot = new Map();
  const indexes = new Map();
  block.places.forEach(({ root, base }, i) => {
    if (!byRoot.has(root)) {
      const reused = cachedRanges(root);
      byRoot.set(root, reused ?? []);
      if (!reused) indexes.set(root, textIndex(root));
    }
    const index = indexes.get(root);
    if (!index) return;
    for (const [start, end, category] of tokens[i] ?? []) {
      const from = locate(index, base + start, true);
      const to = locate(index, base + end, false);
      if (!from || !to) continue;
      const range = new Range();
      range.setStart(from[0], from[1]);
      range.setEnd(to[0], to[1]);
      byRoot.get(root).push([range, category]);
    }
  });
  for (const [root, index] of indexes) {
    rangeCache.set(root, { nodes: index.nodes, text: root.textContent, ranges: byRoot.get(root) });
  }
  return [...byRoot.values()].flat();
}

// --- scanning ---------------------------------------------------------------

let scanning = false;
let rescan = false;

async function scan() {
  if (scanning) {
    rescan = true;
    return;
  }
  scanning = true;
  try {
    const blocks = [...blobBlocks(), ...fenceBlocks(), ...diffBlocks()];
    const tokens = blocks.length ? await tokenize(blocks) : [];
    const highlights = Object.fromEntries(CATEGORIES.map((c) => [c, new Highlight()]));
    blocks.forEach((block, i) => {
      for (const [range, category] of blockRanges(block, tokens[i])) {
        highlights[category]?.add(range);
      }
    });
    for (const category of CATEGORIES) {
      CSS.highlights.set(`beamtalk-${category}`, highlights[category]);
    }
  } catch (err) {
    console.warn(err);
  } finally {
    scanning = false;
    if (rescan) {
      rescan = false;
      schedule();
    }
  }
}

let timer = 0;
function schedule() {
  clearTimeout(timer);
  timer = setTimeout(scan, 100);
}

if (typeof CSS !== "undefined" && "highlights" in CSS) {
  new MutationObserver(schedule).observe(document.documentElement, {
    childList: true,
    subtree: true,
    characterData: true,
  });
  schedule();
}
