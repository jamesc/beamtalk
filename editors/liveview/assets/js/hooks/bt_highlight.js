// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// Beamtalk syntax highlighting for CodeMirror 6 (BT-2538). A CodeMirror
// `ViewPlugin` that tokenizes the document with the SAME regex grammar as the
// legacy <pre> overlay (`highlight.js`) — we import its `RULES`/`RESERVED`, so
// there is one regex definition feeding both editors — and paints the tokens as
// decorations using the existing `.tok-*` classes (assets/css/app.css). Those
// classes resolve the per-theme `--t-*` variables, so the TweaksPanel keeps
// working unchanged.
//
// This deliberately replaces an earlier TextMate-grammar-via-WASM approach: for
// the small snippets these editors hold, running oniguruma in WASM (~250 KB
// gzipped, more than CodeMirror itself, async first paint) was disproportionate.
// The regex tokenizer is synchronous, adds ~nothing to the bundle, and reuses
// rules we already maintain. If the editor ever needs real structural features
// (folding, smart indent, structural autocomplete), the CodeMirror-native answer
// is a Lezer grammar, not TextMate.
//
// The whole document is tokenized on each change (these are small buffers), so
// multi-line tokens (block comments, strings) match correctly in one pass.

import { RangeSetBuilder } from "@codemirror/state"
import { Decoration, ViewPlugin } from "@codemirror/view"
import { RULES, RESERVED } from "./highlight"

const CLASSES = [
  "comment",
  "string",
  "interp",
  "symbol",
  "number",
  "keyword",
  "reserved",
  "global",
  "var",
  "field",
  "binary",
  "assign",
  "return",
  "arrow",
  "match",
  "send",
  "cascade",
  "punct",
]

const MARKS = {}
for (const cls of CLASSES) MARKS[cls] = Decoration.mark({ class: "tok-" + cls })

// Split a "double-quoted string" token into `.tok-string` runs with `.tok-interp`
// spans lifted out for `{interpolation}` — mirrors highlight.js's `hlString`,
// but as non-overlapping ranges instead of nested HTML.
function* stringRanges(start, text) {
  let i = 0
  let segStart = 0
  while (i < text.length) {
    if (text[i] === "{") {
      const end = text.indexOf("}", i)
      if (end > i) {
        if (i > segStart) yield { from: start + segStart, to: start + i, cls: "string" }
        yield { from: start + i, to: start + end + 1, cls: "interp" }
        i = end + 1
        segStart = i
        continue
      }
    }
    i += 1
  }
  if (segStart < text.length) {
    yield { from: start + segStart, to: start + text.length, cls: "string" }
  }
}

// Walk the source with the shared RULES, yielding {from, to, cls} ranges. Mirrors
// the classification in highlight.js's `highlightBeamtalk` (identifiers resolve
// to reserved/global/var; `.field` splits into a punct dot + field name).
function* tokenize(src) {
  let i = 0
  const n = src.length
  while (i < n) {
    const rest = src.slice(i)
    let matched = false
    for (const rule of RULES) {
      const m = rest.match(rule.re)
      if (!m) continue
      const text = m[0]
      const start = i
      i += text.length
      matched = true
      if (rule.cls === "ws" || rule.cls === "other") {
        // no decoration for whitespace / unclassified single chars
      } else if (rule.cls === "_string") {
        yield* stringRanges(start, text)
      } else if (rule.cls === "field") {
        yield { from: start, to: start + 1, cls: "punct" }
        yield { from: start + 1, to: i, cls: "field" }
      } else if (rule.cls === "ident") {
        const cls = RESERVED.has(text) ? "reserved" : /^[A-Z]/.test(text) ? "global" : "var"
        yield { from: start, to: i, cls }
      } else {
        yield { from: start, to: i, cls: rule.cls }
      }
      break
    }
    if (!matched) i += 1
  }
}

function computeDecorations(view) {
  const builder = new RangeSetBuilder()
  for (const t of tokenize(view.state.doc.toString())) {
    if (t.to > t.from && MARKS[t.cls]) builder.add(t.from, t.to, MARKS[t.cls])
  }
  return builder.finish()
}

// CodeMirror extension: Beamtalk highlighting via the shared regex grammar.
export function beamtalkHighlighting() {
  return ViewPlugin.fromClass(
    class {
      constructor(view) {
        this.decorations = computeDecorations(view)
      }

      update(update) {
        if (update.docChanged) this.decorations = computeDecorations(update.view)
      }
    },
    { decorations: (plugin) => plugin.decorations },
  )
}
