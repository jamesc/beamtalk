// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// Backend-driven autocomplete for the cockpit's CodeMirror editors (BT-2544).
// Completion candidates come from the LIVE IMAGE, not a client-side guess: a
// CodeMirror `CompletionSource` round-trips the current line up to the caret to
// the LiveView (`complete` event), which dispatches the term-returning `complete`
// op on the attached workspace (receiver-aware, binding-aware — the same engine
// the CLI REPL uses, BT-783). The op runs no user code (pure reflection/index),
// so it is safe for the Observer role (`:read` capability).
//
// Shared by `cm_editor.js` (Workspace editor; the method editor can opt in too)
// and `repl_input.js` (REPL composer): each passes a `requestCompletions`
// callback that resolves to a `string[]` of server-ranked candidates. We keep
// the server's order via a descending `boost` so CodeMirror does not re-sort
// alphabetically and lose the ranking.

import { autocompletion, completionKeymap } from "@codemirror/autocomplete"
import { keymap } from "@codemirror/view"
import { Prec } from "@codemirror/state"

// Identifier / selector characters of the token being completed. Beamtalk
// selectors are alnum + underscore (keyword `:` parts complete one part at a
// time, and the trailing `:` is part of the candidate the backend returns).
const TOKEN = /[A-Za-z0-9_]*/

// Build the autocompletion extension(s). `requestCompletions(linePrefix)` must
// return a Promise<string[]> — the candidates for the line up to the caret.
//
// Returns an ARRAY: the completion config plus the completion keymap at HIGH
// precedence, so when the popup is open Enter/Tab accept the selection and ↑/↓
// move it — winning over the host editor's own Enter binding (a newline in the
// Workspace, a submit in the REPL). When no completion is active those bindings
// return false and fall through to the host keymap, so normal editing is
// unaffected. `override` REPLACES CodeMirror's default sources: the backend is
// the only source of truth, so there is no client-side word list to drift from
// the live image.
export function backendCompletion(requestCompletions) {
  const config = autocompletion({
    override: [
      async (context) => {
        // The backend infers receiver + prefix from the line up to the caret
        // (BT-783): `s` → bare prefix; `Integer ` → all of Integer's selectors;
        // `Integer fa` → selectors of Integer matching `fa`.
        const line = context.state.doc.lineAt(context.pos)
        const linePrefix = context.state.sliceDoc(line.from, context.pos)

        // The word under the caret tells CodeMirror which range to replace.
        const word = context.matchBefore(TOKEN)
        const atTokenStart = word && word.from === word.to

        // Implicit (typing) activation: only fire when there is a token being
        // typed, or right after a receiver-trailing space (the "all selectors of
        // the receiver" case). An explicit invoke (Ctrl-Space) always fires.
        if (!context.explicit && atTokenStart && !/\s$/.test(linePrefix)) {
          return null
        }

        const candidates = await requestCompletions(linePrefix)
        if (!candidates || candidates.length === 0) return null

        // Empty prefix (receiver-trailing space) inserts at the caret; otherwise
        // replace the token under the caret with the chosen candidate.
        const from = word ? word.from : context.pos

        return {
          from,
          options: candidates.map((label, i) => ({
            label,
            // Preserve the server's ranking: first candidate gets the highest
            // boost so CodeMirror keeps the backend order.
            boost: candidates.length - i,
          })),
        }
      },
    ],
  })

  return [config, Prec.high(keymap.of(completionKeymap))]
}

// Wrap a hook's `pushEvent` into the Promise<string[]> contract the source
// expects. The LiveView `complete` handler replies `{completions: [...]}` on the
// same event (a `{:reply, …}` from `handle_event`), which resolves the callback.
// A dropped reply (workspace unreachable, no session) yields `[]`, so the popup
// simply shows nothing rather than hanging.
export function completionQuery(pushEvent) {
  return (linePrefix) =>
    new Promise((resolve) => {
      pushEvent("complete", { code: linePrefix }, (reply) => {
        resolve((reply && reply.completions) || [])
      })
    })
}
