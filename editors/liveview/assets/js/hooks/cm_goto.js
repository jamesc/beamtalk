// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// Ctrl/Cmd-click "go to definition" for the cockpit's CodeMirror editors
// (BT-2666). Holding the platform modifier (Cmd on macOS, Ctrl elsewhere) and
// clicking a symbol jumps to its definition — an LSP-style affordance reusing
// the live nav machinery rather than a static parse:
//
//   * a class name  → opens that class's definition tab
//   * a selector send → opens the implementor(s) (the BT-2495 popover when many)
//
// Resolution happens on the SERVER (the `goto_definition` LiveView event): the
// extension only detects the modifier-click, identifies the clicked token, and
// round-trips the same `code` line-prefix the hover op consumes (`Receiver
// selector`) plus the bare `token`, so the server can decide class vs selector
// against the LIVE image (matching `definition_provider.rs`'s class-or-selector
// resolution, but live rather than on-disk ASTs). Locals/params are left to a
// future client-side resolve — the core ask is class + selector navigation.
//
// While the modifier is held, hovering a token underlines it (the familiar
// "this is a link" affordance); the plain click is unchanged. Mirrors the
// cm_hover.js / cm_autocomplete.js shape: a `requestGoto(payload)` callback the
// hook wires to its `pushEvent`.

import { EditorView, Decoration } from "@codemirror/view"
import { StateField, StateEffect } from "@codemirror/state"

// Identifier characters of a navigable token — class names and unary selectors
// (matches cm_hover.js's TOKEN). Keyword-selector navigation rides the same
// line-prefix the server parses, so the bare token need only be the unary part.
const TOKEN = /[A-Za-z0-9_]/

// Effect + field that hold the currently underlined token range (or null). A
// StateField keeps the decoration in the editor state so it survives doc/view
// updates until the modifier is released or the pointer leaves the token.
const setGotoLink = StateEffect.define()

const gotoLinkMark = Decoration.mark({ class: "cm-goto-link" })

const gotoLinkField = StateField.define({
  create() {
    return Decoration.none
  },
  update(deco, tr) {
    deco = deco.map(tr.changes)
    for (const effect of tr.effects) {
      if (effect.is(setGotoLink)) {
        deco =
          effect.value == null
            ? Decoration.none
            : Decoration.set([gotoLinkMark.range(effect.value.from, effect.value.to)])
      }
    }
    return deco
  },
  provide: (field) => EditorView.decorations.from(field),
})

// True when the platform's go-to-definition modifier is held: Cmd on macOS,
// Ctrl elsewhere — the same split CodeMirror and most editors use for mod-click.
function isModifier(event) {
  const mac = typeof navigator !== "undefined" && /Mac|iP(hone|ad|od)/.test(navigator.platform)
  return mac ? event.metaKey : event.ctrlKey
}

// The identifier token at `pos`, as {from, to} — grows left then right over
// identifier chars (cm_hover.js's wordAt). Empty range (from === to) ⇒ no token.
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

// Build the go-to-definition extension. `requestGoto({code, token})` fires the
// LiveView `goto_definition` event with the clicked token's line-prefix (through
// the token's end — the receiver+selector context the server parses) and the
// bare token text. The server resolves + opens; nothing comes back to the editor.
export function gotoDefinition(requestGoto) {
  return [
    gotoLinkField,
    EditorView.domEventHandlers({
      // Modifier-click resolves the clicked token. We act on the modifier only —
      // a plain click falls through to CodeMirror's normal cursor placement, so
      // editing is unaffected.
      mousedown(event, view) {
        if (!isModifier(event)) return false
        const pos = view.posAtCoords({ x: event.clientX, y: event.clientY })
        if (pos == null) return false
        const { from, to } = wordAt(view.state, pos)
        if (from === to) return false

        // Send the line from its start through the token's end (the hover op's
        // contract) so the server sees `Receiver selector`, plus the bare token
        // so it can test it as a class name. Prevent the default so the
        // modifier-click does not also drag-select or move the caret.
        event.preventDefault()
        const line = view.state.doc.lineAt(pos)
        const code = view.state.sliceDoc(line.from, to)
        const token = view.state.sliceDoc(from, to)
        requestGoto({ code, token })
        return true
      },
      // While the modifier is held, underline the token under the pointer so the
      // affordance reads as a link. Cleared when the modifier is released
      // (keyup) or the pointer leaves a token / the editor.
      mousemove(event, view) {
        if (!isModifier(event)) {
          clearLink(view)
          return false
        }
        const pos = view.posAtCoords({ x: event.clientX, y: event.clientY })
        if (pos == null) {
          clearLink(view)
          return false
        }
        const range = wordAt(view.state, pos)
        if (range.from === range.to) {
          clearLink(view)
          return false
        }
        setLink(view, range)
        return false
      },
      mouseleave(_event, view) {
        clearLink(view)
        return false
      },
      keyup(_event, view) {
        // Releasing the modifier drops the link affordance immediately rather
        // than waiting for the next pointer move.
        clearLink(view)
        return false
      },
    }),
  ]
}

function setLink(view, range) {
  const current = view.state.field(gotoLinkField, false)
  // Skip the dispatch when the same range is already marked — avoids a
  // decoration churn (and re-render) on every pixel of pointer movement.
  if (current && current.size) {
    let same = false
    current.between(range.from, range.to, (f, t) => {
      if (f === range.from && t === range.to) same = true
    })
    if (same) return
  }
  view.dispatch({ effects: setGotoLink.of(range) })
}

function clearLink(view) {
  const current = view.state.field(gotoLinkField, false)
  if (current && current.size) view.dispatch({ effects: setGotoLink.of(null) })
}

// Wrap a hook's `pushEvent` into the `({code, token}) => void` contract the
// extension expects. Fire-and-forget: the server resolves the symbol and opens
// the target (or flashes "no definition found") via its own re-render, so unlike
// hover/complete there is no reply to await.
export function gotoQuery(pushEvent) {
  return ({ code, token }) => pushEvent("goto_definition", { code, token })
}
