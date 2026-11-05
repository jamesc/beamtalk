// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// Backend-driven hover for the cockpit's CodeMirror editors (BT-2555). Hover
// docs come from the LIVE IMAGE, not a static parse: a CodeMirror `hoverTooltip`
// source round-trips the editor line up to the hovered token to the LiveView
// (`hover` event), which dispatches the term-returning `hover` op on the
// attached workspace. The op resolves the token against the live class registry
// (a class name → its docs; a `Receiver selector` pair → that method's docs)
// via `beamtalk_repl_docs` — the same live doc engine the CLI REPL `:help`
// uses — so hover covers REPL-defined and live-patched classes the static LSP
// hover (`hover_provider.rs`, on-disk ASTs) cannot see. The op runs no user
// code (pure reflection), so it is safe for the Observer role (`:read`).
//
// Shared by `cm_editor.js` (Workspace + method editors) and `repl_input.js`
// (REPL composer), mirroring the cm_autocomplete.js structure: each passes a
// `requestHover(linePrefix, lineSuffix) => Promise<string>` callback resolving
// to the docs markdown (an empty string means "nothing to show", so no tooltip
// opens).

import { hoverTooltip } from "@codemirror/view"

// Identifier characters of a hoverable token — class names and unary selectors
// (matches cm_autocomplete's TOKEN). A keyword part additionally takes its
// trailing `:` (see `wordAt`), so `fetch:` is hovered as a keyword (BT-4343).
const TOKEN = /[A-Za-z0-9_]/

// Build the hover extension. `requestHover(linePrefix, lineSuffix)` must return
// a Promise<string> — the hover markdown for the line up to the hovered token
// (an empty string when there is nothing to show). `lineSuffix` is the rest of
// the line after the token: the backend needs it to recover a keyword send's
// FULL selector (`fetch:` in `X fetch: a options: b` → `fetch:options:`).
export function backendHover(requestHover) {
  // CM's source signature is (view, pos, side); we ignore `side` because
  // `wordAt` expands symmetrically from `pos` and so already covers both edges
  // of the token regardless of which side the pointer favours.
  return hoverTooltip(async (view, pos) => {
    // Grow left/right from the pointer position over identifier characters to
    // find the hovered token's range. Not over an identifier ⇒ no hover.
    const { from, to } = wordAt(view.state, pos)
    if (from === to) return null

    // The backend infers receiver + token from the line up to the token's end,
    // reusing the completion parser (`Counter` → class docs; `42 factorial` →
    // method docs). Send the line from its start through the hovered token's
    // end, plus the rest of the line so a hovered keyword part resolves every
    // keyword of its send (`Fetcher fetch: u options: o` → `fetch:options:`).
    const line = view.state.doc.lineAt(pos)
    const linePrefix = view.state.sliceDoc(line.from, to)
    const lineSuffix = view.state.sliceDoc(to, line.to)

    const docs = await requestHover(linePrefix, lineSuffix)
    if (!docs) return null

    return {
      pos: from,
      end: to,
      above: true,
      create() {
        const dom = document.createElement("div")
        dom.className = "cm-hover-doc"
        // The docs are the same plain text `:help` shows in the REPL, so render
        // verbatim in a <pre> via textContent — no markdown/HTML interpretation,
        // which also keeps live doc text from injecting markup into the page.
        const pre = document.createElement("pre")
        pre.textContent = docs
        dom.appendChild(pre)
        return { dom }
      },
    }
  })
}

// The identifier token at `pos`, as {from, to}. Grows left then right over
// identifier characters, so a pointer resting just after a token (e.g. on the
// space following it) still resolves that token — benign, and matches the
// completion plugin's `matchBefore` behaviour. A keyword part keeps its
// trailing `:` (`fetch:`), including when the pointer rests on that colon; a
// `:` starting `:=` or `::` is assignment/annotation, not a keyword, and is left
// out. Returns an empty range (from === to) only when there is no identifier on
// either side of `pos`.
function wordAt(state, pos) {
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

// Wrap a hook's `pushEvent` into the `(linePrefix, lineSuffix) => Promise<string>` contract
// the source expects. The LiveView `hover` handler replies `{hover: "…"}` on the
// same event (a `{:reply, …}` from `handle_event`), resolving the callback. A
// reply that carries no session / an unreachable workspace / nothing to show
// resolves to "" (the handler still replies ""), so no tooltip opens.
//
// If the LiveView socket drops mid-flight the callback never fires and this
// Promise stays pending — benign (no tooltip ever appears, CM drops it when the
// editor is destroyed) and the same accepted gap as `completionQuery`. The
// correct fix when it matters is a shared per-hook cancel signal, not a local
// try/catch — left uncancelled deliberately.
export function hoverQuery(pushEvent) {
  return (linePrefix, lineSuffix = "") =>
    new Promise((resolve) => {
      pushEvent("hover", { code: linePrefix, suffix: lineSuffix }, (reply) => {
        resolve((reply && reply.hover) || "")
      })
    })
}
