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
// `requestHover(linePrefix) => Promise<string>` callback resolving to the docs
// markdown (an empty string means "nothing to show", so no tooltip opens).

import { hoverTooltip } from "@codemirror/view"

// Identifier characters of a hoverable token — class names and unary selectors
// (matches cm_autocomplete's TOKEN). Keyword-selector hover is future work.
const TOKEN = /[A-Za-z0-9_]/

// Build the hover extension. `requestHover(linePrefix)` must return a
// Promise<string> — the hover markdown for the line up to the hovered token
// (an empty string when there is nothing to show).
export function backendHover(requestHover) {
  return hoverTooltip(async (view, pos) => {
    // Grow left/right from the pointer position over identifier characters to
    // find the hovered token's range. Not over an identifier ⇒ no hover.
    const { from, to } = wordAt(view.state, pos)
    if (from === to) return null

    // The backend infers receiver + token from the line up to the token's end,
    // reusing the completion parser (`Counter` → class docs; `42 factorial` →
    // method docs). Send the line from its start through the hovered token's end.
    const line = view.state.doc.lineAt(pos)
    const linePrefix = view.state.sliceDoc(line.from, to)

    const docs = await requestHover(linePrefix)
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

// The identifier token covering `pos`, as {from, to}. Returns an empty range
// (from === to) when `pos` is not inside an identifier.
function wordAt(state, pos) {
  const line = state.doc.lineAt(pos)
  const s = line.text
  const rel = pos - line.from
  let start = rel
  let end = rel
  while (start > 0 && TOKEN.test(s[start - 1])) start--
  while (end < s.length && TOKEN.test(s[end])) end++
  return { from: line.from + start, to: line.from + end }
}

// Wrap a hook's `pushEvent` into the `(linePrefix) => Promise<string>` contract
// the source expects. The LiveView `hover` handler replies `{hover: "…"}` on the
// same event (a `{:reply, …}` from `handle_event`), resolving the callback. A
// reply that carries no session / an unreachable workspace / nothing to show
// resolves to "" (the handler still replies ""), so no tooltip opens.
export function hoverQuery(pushEvent) {
  return (linePrefix) =>
    new Promise((resolve) => {
      pushEvent("hover", { code: linePrefix }, (reply) => {
        resolve((reply && reply.hover) || "")
      })
    })
}
