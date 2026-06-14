// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// Beamtalk syntax highlighting for CodeMirror 6 (BT-2538). A CodeMirror
// `ViewPlugin` that tokenizes the document with a regex grammar and paints the
// tokens as decorations using the `.tok-*` classes (assets/css/app.css). Those
// classes resolve the per-theme `--t-*` variables, so the TweaksPanel keeps
// working unchanged.
//
// The grammar (`RULES`/`RESERVED`) used to live in `highlight.js` alongside the
// legacy <pre>-overlay highlighter; that overlay (CodeEditor) was retired in
// BT-2539 when the method-editor tabs moved to CodeMirror, so the rules now live
// here — their only remaining consumer.
//
// Beamtalk specifics vs classic Smalltalk:
//   - comments are // line and /* block */ (NOT double-quoted)
//   - strings are "double quoted" with {interpolation}
//   - single quotes are reserved for #'quoted symbols'
//   - method bodies use  selector => body
//   - field access via self.field   - assignment :=   - early return ^
//   - async send suffix !   - symbols #name   - maps and lists
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

// Reserved (pseudo-variable) identifiers — coloured `.tok-reserved`.
const RESERVED = new Set(["self", "super", "nil", "true", "false", "thisContext"])

// The ordered token grammar. Each rule's `^`-anchored regex is matched against
// the remaining source; the first hit wins. `_string`/`field`/`ident` get
// special post-classification in `tokenize` below.
const RULES = [
  { cls: "comment", re: /^\/\/[^\n]*/ }, // // line comment
  { cls: "comment", re: /^\/\*[\s\S]*?\*\// }, // /* block comment */
  { cls: "_string", re: /^"(?:\\.|[^"\\])*"/ }, // "double quoted string"
  {
    cls: "symbol",
    re: /^#(?:'(?:''|[^'])*'|\{|\(|[A-Za-z_][\w]*[:]?|[-+*/~<>=&|@%,?!]+)/,
  }, // #sym #{ #(
  { cls: "field", re: /^\.[A-Za-z_]\w*/ }, // .field (after self/obj)
  { cls: "number", re: /^\d+r[0-9A-Za-z]+/ }, // radix
  { cls: "number", re: /^\d+(?:\.\d+)?(?:e-?\d+)?/ }, // int / float
  { cls: "arrow", re: /^=>/ }, // method body arrow
  { cls: "match", re: /^->/ }, // pattern / association arrow
  { cls: "assign", re: /^:=/ }, // assignment
  { cls: "return", re: /^\^/ }, // early return
  { cls: "keyword", re: /^[A-Za-z_]\w*:/ }, // keyword message part  foo:
  { cls: "ident", re: /^[A-Za-z_]\w*/ }, // identifier (resolved below)
  { cls: "send", re: /^!/ }, // async send suffix
  { cls: "cascade", re: /^;/ }, // cascade
  { cls: "binary", re: /^(?:=:=|=\/=|\/=|==|\*\*|\+\+|<=|>=|[-+*/<>=~&|@%?])+/ },
  { cls: "punct", re: /^[\[\]{}().|]/ },
  { cls: "ws", re: /^\s+/ },
  { cls: "other", re: /^./ },
]

// Sticky (`y`) variants of the rules so the tokenizer scans in true O(n):
// matching at an explicit `lastIndex` avoids the per-character `src.slice(i)`
// (O(n²)) a `^`-anchored `.match()` would force here. The leading `^` anchor is
// dropped — `y` already pins the match to `lastIndex`.
//
// These compiled regexes live at module scope and `tokenize` mutates their
// `lastIndex`, so a single `tokenize` call must run to completion before another
// starts. JS's single-threaded execution guarantees that (no two generators
// interleave mid-call), so this is safe — but it's why the regexes aren't shared
// across concurrent tokenizations.
const STICKY_RULES = RULES.map((rule) => ({
  cls: rule.cls,
  re: new RegExp(rule.re.source.replace(/^\^/, ""), "y"),
}))

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
// spans lifted out for `{interpolation}`, as non-overlapping ranges.
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

// Walk the source with the RULES, yielding {from, to, cls} ranges (identifiers
// resolve to reserved/global/var; `.field` splits into a punct dot + field name).
function* tokenize(src) {
  let i = 0
  const n = src.length
  while (i < n) {
    let matched = false
    for (const rule of STICKY_RULES) {
      rule.re.lastIndex = i
      const m = rule.re.exec(src)
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
    // Defensive guard against an infinite loop: advance past a char no rule
    // claimed. With the current rules this never actually fires — `ws` (`\s+`,
    // and `\s` includes the line terminators `.` skips in non-dotall mode) plus
    // `other` (`/^./`) together match every code unit — but it keeps the scan
    // safe if the rule set ever stops being total.
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
