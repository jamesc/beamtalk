// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// Backend-driven live diagnostics for the cockpit's CodeMirror editors
// (BT-2556). Squiggles come from the LIVE COMPILER, not a client-side guess: a
// `@codemirror/lint` source round-trips the full editor buffer to the LiveView
// (`diagnostics` event), which dispatches the term-returning `diagnostics` op on
// the attached workspace. The op runs the compiler's SIDE-EFFECT-FREE
// `diagnostics` path (parse + semantic check only — no codegen, no install, no
// eval, no ChangeLog), so it is safe to fire as the buffer changes and is a
// `:read` op (the Observer sees diagnostics too).
//
// Shared by `cm_editor.js` (Workspace + method editors) and `repl_input.js`
// (REPL composer), mirroring the cm_autocomplete.js / cm_hover.js structure:
// each passes a `requestDiagnostics(code) => Promise<Diagnostic[]>` callback
// resolving to backend diagnostics (`{from, to, severity, message}` with `from`/
// `to` as BYTE offsets into the buffer — the backend speaks UTF-8 byte spans).
//
// Debounce is built in: `linter(…, { delay })` only invokes the source after the
// editor has been idle for `delay` ms, so rapid keystrokes never flood the
// workspace (the same idea as the LSP debouncing `didChange`). No per-keystroke
// round-trip happens.

import { linter } from "@codemirror/lint"

// How long the editor must be idle before diagnostics are re-fetched. ~150ms
// mirrors the LSP's didChange debounce: long enough to coalesce a fast typist's
// keystrokes, short enough that the squiggle feels live.
const LINT_DELAY_MS = 150

// Backend severity → CodeMirror `Diagnostic` severity. CodeMirror knows
// "error" | "warning" | "info" | "hint"; the backend emits
// "error" | "warning" | "lint" | "hint". `lint` findings (effect-free statement
// hints, BT-979) surface as "info"; anything unrecognised falls back to "error"
// so a new backend severity is never silently dropped.
const SEVERITY = { error: "error", warning: "warning", lint: "info", hint: "hint" }

// Build the lint extension. `requestDiagnostics(code)` must return a
// Promise<Array<{from, to, severity, message}>> — the backend diagnostics for
// the whole buffer, with `from`/`to` as UTF-8 BYTE offsets.
export function backendLint(requestDiagnostics) {
  return linter(
    async (view) => {
      const doc = view.state.doc.toString()
      const diagnostics = await requestDiagnostics(doc)
      if (!diagnostics || diagnostics.length === 0) return []

      const len = doc.length
      return diagnostics.map((d) => {
        // Map UTF-8 byte offsets to CodeMirror positions (UTF-16 code units),
        // then clamp into the current doc so a stale reply (the buffer shrank
        // while the request was in flight) can never throw a range error.
        let from = clamp(byteToPos(doc, d.from), 0, len)
        let to = clamp(byteToPos(doc, d.to), 0, len)
        if (to < from) [from, to] = [to, from]
        // A zero-width range renders nothing; widen by one whole code point
        // where the doc allows so the marker is always visible. Stepping a full
        // code point (not one UTF-16 code unit) keeps the range off the middle
        // of a surrogate pair when the boundary lands on a supplementary-plane
        // character (emoji, etc.).
        if (from === to) {
          if (to < len) {
            to += doc.codePointAt(to) > 0xffff ? 2 : 1
          } else if (from > 0) {
            // Widen backwards: a low surrogate just before `from` means the
            // preceding character is an astral pair, so step back two units.
            const lo = doc.charCodeAt(from - 1)
            from -= lo >= 0xdc00 && lo <= 0xdfff ? 2 : 1
          }
        }
        return {
          from,
          to,
          severity: SEVERITY[d.severity] || "error",
          message: d.message || "",
        }
      })
    },
    { delay: LINT_DELAY_MS },
  )
}

// Convert a UTF-8 byte offset into a CodeMirror position (a UTF-16 code-unit
// index into `doc`). Walks code points from the start, accumulating each one's
// UTF-8 byte length until the byte that *contains* `byteOffset` is reached, then
// returns that code point's start. ASCII (the common case) advances one byte /
// one code unit per step, so this degrades to the identity mapping; multibyte
// identifiers and string literals stay aligned. The `next > byteOffset` test
// returns the *containing* code point for both boundary offsets (the Rust
// `diagnostics` path always emits these) and the degenerate case of an offset
// that lands mid–code-point, rather than overshooting to the next character.
function byteToPos(doc, byteOffset) {
  if (byteOffset <= 0) return 0
  let bytes = 0
  let i = 0
  while (i < doc.length) {
    const cp = doc.codePointAt(i)
    const next = bytes + utf8Len(cp)
    if (next > byteOffset) return i
    bytes = next
    i += cp > 0xffff ? 2 : 1
  }
  return doc.length
}

// UTF-8 byte length of a single Unicode code point.
function utf8Len(cp) {
  if (cp < 0x80) return 1
  if (cp < 0x800) return 2
  if (cp < 0x10000) return 3
  return 4
}

function clamp(n, lo, hi) {
  return n < lo ? lo : n > hi ? hi : n
}

// Wrap a hook's `pushEvent` into the `(code) => Promise<Diagnostic[]>` contract
// the source expects. The LiveView `diagnostics` handler replies
// `{diagnostics: [...]}` on the same event (a `{:reply, …}` from
// `handle_event`), resolving the callback. A reply that carries no session / an
// unreachable workspace / a denied op all resolve to `[]` (the handler still
// replies `[]`), so the editor simply shows no squiggles.
//
// If the LiveView socket drops mid-flight the callback never fires and this
// Promise stays pending — benign (no diagnostics ever appear; CM drops the
// pending lint when the editor is destroyed) and the same accepted gap as
// `completionQuery` / `hoverQuery`.
export function lintQuery(pushEvent) {
  return (code) =>
    new Promise((resolve) => {
      pushEvent("diagnostics", { code }, (reply) => {
        resolve((reply && reply.diagnostics) || [])
      })
    })
}
