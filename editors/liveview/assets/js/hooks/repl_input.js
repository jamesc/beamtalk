// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// ReplInput — the REPL tab's bottom-pinned composer (BT-2543). A thin CodeMirror
// 6 input, a sibling of the Workspace `CmEditor` (cm_editor.js) but tuned to the
// classic terminal/chat idiom rather than the editor-primary Workspace:
//
//   * Enter SUBMITS the expression (terminal convention) — confirmed divergence
//     from the Workspace, where Enter is a newline. Shift-Enter and ⌘/Ctrl-Enter
//     insert a newline, so the composer stays multi-line capable.
//   * ↑ / ↓ recall prior expressions, but ONLY at the composer's edges: ↑ on the
//     first line and ↓ on the last line ask the server (`repl_history_prev` /
//     `repl_history_next`) for the recalled text; anywhere else they move the
//     cursor between lines as usual. The server owns the history ring and pushes
//     the recalled text back via `repl_set_input` (the input is hook-owned /
//     phx-update="ignore", so the server can't set it through morphdom).
//   * Submitting clears the input — the server pushes an empty `repl_set_input`.
//
// LiveView form integration mirrors CmEditor: a hidden <textarea name="expr">
// stays the posted field (kept current by mirroring the doc on every edit) and a
// `.cm-host` div CodeMirror mounts into. Expected DOM (rendered by the LiveView):
//
//   <form id="repl-form" phx-submit="repl_eval">
//     <div id="repl-input" phx-hook="ReplInput" data-placeholder="…">
//       <textarea name="expr" phx-update="ignore" hidden></textarea>
//       <div class="cm-host" id="repl-input-cm" phx-update="ignore"></div>
//     </div>
//   </form>

import { EditorState } from "@codemirror/state"
import {
  EditorView,
  keymap,
  drawSelection,
  placeholder as placeholderExt,
} from "@codemirror/view"
import {
  defaultKeymap,
  history,
  historyKeymap,
  insertNewlineAndIndent,
} from "@codemirror/commands"
import { indentUnit } from "@codemirror/language"
import { beamtalkHighlighting } from "./bt_highlight"

export const ReplInput = {
  mounted() {
    this.field = this.el.querySelector("textarea")
    this.host = this.el.querySelector(".cm-host")
    if (!this.field || !this.host) {
      console.error("ReplInput: expected a <textarea> and a .cm-host inside", this.el)
      return
    }

    const placeholderText = this.el.dataset.placeholder || ""

    // REPL keymap takes precedence over the defaults: Enter submits; Shift/⌘-Enter
    // keep the newline; ↑/↓ recall history at the edges (else fall through to
    // cursor movement). Bound BEFORE defaultKeymap so our Enter wins over its
    // insertNewlineAndIndent.
    const replKeymap = keymap.of([
      { key: "Enter", run: () => this.submit() },
      { key: "Shift-Enter", run: insertNewlineAndIndent },
      { key: "Mod-Enter", run: insertNewlineAndIndent },
      { key: "ArrowUp", run: (view) => this.recallPrev(view) },
      { key: "ArrowDown", run: (view) => this.recallNext(view) },
    ])

    const extensions = [
      history(),
      drawSelection(),
      indentUnit.of("  "),
      EditorState.tabSize.of(2),
      replKeymap,
      keymap.of([...defaultKeymap, ...historyKeymap]),
      beamtalkHighlighting(),
      EditorView.lineWrapping,
      EditorView.updateListener.of((u) => this.onUpdate(u)),
    ]
    if (placeholderText) extensions.push(placeholderExt(placeholderText))

    this.view = new EditorView({
      state: EditorState.create({ doc: this.field.value, extensions }),
      parent: this.host,
    })
    // Expose the view for the Playwright e2e suite (which dispatches a doc-replace
    // transaction to set the input deterministically — there is no <textarea>
    // value to fill any more), matching CmEditor's `el.cmView` convention.
    this.el.cmView = this.view

    // Server→input pushes: history recall and the post-submit clear both arrive as
    // `repl_set_input`, replacing the doc with the server-supplied text.
    this.handleEvent("repl_set_input", (payload) => this.setInput(payload))
    // After each appended entry the server asks the scrollback to scroll to the
    // newest result (classic terminal behaviour), so a submission's result is
    // revealed even if the user had scrolled up to read older output. Scope the
    // lookup to this hook's own pane (not a global id) so the relationship stays
    // explicit and rename-safe.
    this.handleEvent("repl_scroll_bottom", () => {
      const sb = this.el.closest(".repl-pane")?.querySelector(".repl-scrollback")
      if (sb) sb.scrollTop = sb.scrollHeight
    })
  },

  destroyed() {
    if (this.view) this.view.destroy()
    if (this.el) delete this.el.cmView
  },

  onUpdate(update) {
    if (update.docChanged) this.syncField()
  },

  // Mirror the editor doc into the form field so the `repl_eval` submit (and the
  // LiveView tests' render_submit(%{expr: …})) read the current text.
  syncField() {
    this.field.value = this.view.state.doc.toString()
    this.field.dispatchEvent(new Event("input", { bubbles: true }))
  },

  // Enter: submit the surrounding form via LiveView. requestSubmit triggers
  // phx-submit (unlike .submit(), which bypasses it). The field is already in
  // sync from the last docChange. Returns true to consume the key (no newline).
  submit() {
    this.syncField()
    const form = this.el.closest("form")
    if (form) form.requestSubmit()
    return true
  },

  // ↑: recall an older expression, but only when the cursor sits on the first
  // line with no selection — otherwise let the arrow move the cursor up a line.
  recallPrev(view) {
    const { state } = view
    const range = state.selection.main
    if (!range.empty) return false
    if (state.doc.lineAt(range.head).number !== 1) return false
    this.pushEvent("repl_history_prev", {})
    return true
  },

  // ↓: recall a newer expression (or restore the live input past the newest),
  // but only on the last line with no selection.
  recallNext(view) {
    const { state } = view
    const range = state.selection.main
    if (!range.empty) return false
    if (state.doc.lineAt(range.head).number !== state.doc.lines) return false
    this.pushEvent("repl_history_next", {})
    return true
  },

  // Replace the whole doc with server-supplied text (history recall / clear) and
  // drop the cursor at the end, then mirror into the field.
  setInput(payload) {
    if (!this.view || !payload || typeof payload.text !== "string") return
    const text = payload.text
    this.view.dispatch({
      changes: { from: 0, to: this.view.state.doc.length, insert: text },
      selection: { anchor: text.length },
    })
    this.syncField()
    this.view.focus()
  },
}
