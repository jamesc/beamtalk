// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// Token + editor-context helpers shared by the cockpit's hover (cm_hover.js) and
// go-to-definition (cm_goto.js) extensions (BT-4455). Both identify the token
// under the pointer the same way and send the backend the same editor context
// (`code` / `before` / `suffix`), which the backend's single keyword-send
// recognizer (`beamtalk_repl_ops_dev:keyword_send_at/3`) turns into a full
// keyword selector. Keeping the token rule here means the two cannot disagree on
// what `fetch:` is.

// Identifier characters of a navigable token — class names and unary selectors
// (matches cm_autocomplete's TOKEN). A keyword part additionally takes its
// trailing `:` (see `wordAt`), so `fetch:` is hovered as a keyword (BT-4343).
export const TOKEN = /[A-Za-z0-9_]/

// Lines of context sent either side of the hovered line, so a keyword send split
// across lines (`Fetcher fetch: u` / `  options: o`) resolves as one send. The
// backend bounds the total it scans, so this only needs to cover a typical send.
export const CONTEXT_LINES = 10

// The identifier token at `pos`, as {from, to}. Grows left then right over
// identifier characters, so a pointer resting just after a token (e.g. on the
// space following it) still resolves that token — benign, and matches the
// completion plugin's `matchBefore` behaviour. A keyword part keeps its
// trailing `:` (`fetch:`), including when the pointer rests on that colon; a
// `:` starting `:=` or `::` is assignment/annotation, not a keyword, and is left
// out. Returns an empty range (from === to) only when there is no identifier on
// either side of `pos`.
export function wordAt(state, pos) {
  const line = state.doc.lineAt(pos)
  const s = line.text
  const rel = pos - line.from
  let start = rel
  // Pointer just past a keyword's colon: step back onto the keyword.
  if (isKeywordColon(s, rel - 1) && !TOKEN.test(s[rel] ?? "")) start = rel - 1
  let end = start
  while (start > 0 && TOKEN.test(s[start - 1])) start--
  while (end < s.length && TOKEN.test(s[end])) end++
  if (end > start && isKeywordColon(s, end)) end++
  return { from: line.from + start, to: line.from + end }
}

// True when `s[i]` is a keyword part's `:` — directly after an identifier char
// and not the start of `:=` / `::`.
function isKeywordColon(s, i) {
  return (
    i > 0 &&
    s[i] === ":" &&
    TOKEN.test(s[i - 1]) &&
    s[i + 1] !== "=" &&
    s[i + 1] !== ":"
  )
}

// The editor context the backend's keyword-send recognizer consumes for a token
// ending at `to` on `pos`'s line: `code` (the line from its start through the
// token), `before` (up to CONTEXT_LINES lines above) and `suffix` (the text after
// the token, up to CONTEXT_LINES lines below).
export function editorContext(state, pos, to) {
  const doc = state.doc
  const line = doc.lineAt(pos)
  const firstLine = doc.line(Math.max(1, line.number - CONTEXT_LINES))
  const lastLine = doc.line(Math.min(doc.lines, line.number + CONTEXT_LINES))
  return {
    code: state.sliceDoc(line.from, to),
    before: state.sliceDoc(firstLine.from, line.from),
    suffix: state.sliceDoc(to, lastLine.to),
  }
}
