// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// CmEditor — a CodeMirror 6 code editor LiveView hook (BT-2538). Replaces the
// transparent-textarea-over-<pre> overlay (the old CodeEditor + SelectionTracker
// hooks): CodeMirror owns the text, selection, history, scrolling and syntax
// highlighting in one real editor, which fixes the caret/highlight drift of the
// overlay. Highlighting comes from the shared regex highlighter (bt_highlight.js).
//
// LiveView form integration — CodeMirror is NOT a form control, so the posted
// value must still come from a real field. Expected DOM (rendered by the
// LiveView): a hidden <textarea> that stays the form field, plus an ignored host
// div that CodeMirror mounts into:
//
//   <div id="…" phx-hook="CmEditor"
//        data-select-event="select_workspace"   (optional: report selection)
//        data-placeholder="…">                  (optional)
//     <textarea name="expr" hidden>…server value…</textarea>
//     <div id="…-cm" phx-update="ignore"></div>
//   </div>
//
// The hidden <textarea> keeps its `name` (expr / source) and `phx-debounce`, so
// the existing `eval` / `save_method` / `edit_source` handlers and the LiveView
// tests that drive `render_submit(%{expr: …})` keep working unchanged. On every
// edit we mirror CodeMirror's doc into that textarea and dispatch `input`, which
// drives phx-change. `phx-update="ignore"` keeps morphdom from clobbering the
// CodeMirror DOM, while the textarea (a direct child, not ignored) is still
// server-patched — `updated()` syncs a server-pushed value back into the editor.
//
// Keyboard chords (⌘D/⌘P/⌘I/⌘S) are intentionally NOT bound here: they live on
// the surrounding <form>'s KeyboardShortcuts hook and a keydown inside the editor
// bubbles up to it, so the same chords work whether or not focus is in the editor.

import { EditorState } from "@codemirror/state"
import {
  EditorView,
  keymap,
  drawSelection,
  highlightActiveLine,
  placeholder as placeholderExt,
} from "@codemirror/view"
import { defaultKeymap, history, historyKeymap, indentWithTab } from "@codemirror/commands"
import { indentUnit } from "@codemirror/language"
import { beamtalkHighlighting } from "./bt_highlight"

export const CmEditor = {
  mounted() {
    this.field = this.el.querySelector("textarea")
    this.host = this.el.querySelector("[phx-update='ignore']") || this.el
    if (!this.field) return

    this.selectEvent = this.el.dataset.selectEvent || null
    this.lastSelection = null
    const readOnly = this.field.readOnly || this.el.dataset.readonly === "true"
    const placeholderText = this.el.dataset.placeholder || ""

    const extensions = [
      history(),
      drawSelection(),
      highlightActiveLine(),
      indentUnit.of("  "),
      EditorState.tabSize.of(2),
      keymap.of([...defaultKeymap, ...historyKeymap, indentWithTab]),
      beamtalkHighlighting(),
      EditorView.editable.of(!readOnly),
      EditorState.readOnly.of(readOnly),
      EditorView.updateListener.of((u) => this.onUpdate(u)),
    ]
    if (placeholderText) extensions.push(placeholderExt(placeholderText))

    this.view = new EditorView({
      state: EditorState.create({ doc: this.field.value, extensions }),
      parent: this.host,
    })
    // Expose the view on the wrapper for debugging and the Playwright e2e suite
    // (which dispatches a doc-replace transaction to set editor contents
    // deterministically — there is no <textarea> value to fill any more).
    this.el.cmView = this.view
  },

  // The LiveView may patch the hidden textarea (eval clears it, a server-pushed
  // method loads into it). Push that value into CodeMirror when it actually
  // differs, so we don't fight our own edits (which produce equal values).
  updated() {
    if (!this.view || !this.field) return
    const incoming = this.field.value
    if (incoming === this.view.state.doc.toString()) return
    this.view.dispatch({
      changes: { from: 0, to: this.view.state.doc.length, insert: incoming },
    })
  },

  destroyed() {
    if (this.view) this.view.destroy()
    if (this.el) delete this.el.cmView
  },

  onUpdate(update) {
    if (update.docChanged) this.syncField(update)
    if (this.selectEvent && (update.selectionSet || update.docChanged)) {
      this.reportSelection(update)
    }
  },

  // Mirror the editor doc into the form field and fire `input` so LiveView's
  // change tracking (phx-change / phx-debounce) and form submit see it.
  syncField(update) {
    this.field.value = update.state.doc.toString()
    this.field.dispatchEvent(new Event("input", { bubbles: true }))
  },

  reportSelection(update) {
    const range = update.state.selection.main
    const text = update.state.sliceDoc(range.from, range.to)
    const key = range.from + ":" + range.to + ":" + text
    if (key === this.lastSelection) return
    this.lastSelection = key
    this.pushEvent(this.selectEvent, { text, start: range.from, end: range.to })
  },
}
