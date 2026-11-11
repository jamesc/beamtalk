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
// `requestHover({code, before, suffix}) => Promise<string>` callback resolving
// to the rendered docs HTML (an empty string means "nothing to show", so no
// tooltip opens).

import { hoverTooltip } from "@codemirror/view"
import { wordAt, editorContext } from "./cm_token.js"

// Build the hover extension. `requestHover({code, before, suffix})` must return
// a Promise<string> — the hover docs HTML (an empty string when there is nothing
// to show). `code` is the line up to and including the hovered token; `before`
// (the lines above) and `suffix` (the text after the token) let the backend
// recover a keyword send's FULL selector (`fetch:` in `X fetch: a options: b`
// → `fetch:options:`).
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
    // end, plus a few lines of context either side so a hovered keyword part
    // resolves every keyword of its send (`fetch:options:`), even across lines.
    const docs = await requestHover(editorContext(view.state, pos, to))
    if (!docs) return null

    return {
      pos: from,
      end: to,
      above: true,
      create() {
        const dom = document.createElement("div")
        dom.className = "cm-hover-doc"
        // The reply is HTML rendered server-side by `BtAttach.DocFormat`
        // (BT-4344): the `:help` layout preformatted, a method's doc-comment
        // as formatted docs (`## Options` → heading + bullets). DocFormat
        // HTML-escapes every piece of author text and emits only its own tags,
        // so live doc text cannot inject markup — the same contract the System
        // Browser doc block relies on when it renders this HTML.
        dom.innerHTML = docs
        return { dom }
      },
    }
  })
}

// Wrap a hook's `pushEvent` into the `({code, before, suffix}) => Promise<string>` contract
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
  return (payload) =>
    new Promise((resolve) => {
      pushEvent("hover", payload, (reply) => {
        resolve((reply && reply.hover) || "")
      })
    })
}
