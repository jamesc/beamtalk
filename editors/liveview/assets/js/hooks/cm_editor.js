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
//     <textarea name="expr" phx-update="ignore" hidden>…server value…</textarea>
//     <div class="cm-host" id="…-cm" phx-update="ignore"></div>
//   </div>
//
// The hidden <textarea> keeps its `name` (expr / source) and is marked
// `phx-update="ignore"`, so it is the hook's to own: CodeMirror is the source of
// truth, and on every edit we mirror the doc into that textarea and dispatch
// `input` (driving phx-change where the form wants it). Marking it ignore is
// essential — otherwise an unrelated server re-render makes morphdom reset the
// textarea to the last server value, silently reverting the editor and sending a
// stale value on submit. The existing `eval` / `save_method` / `edit_source`
// handlers and the LiveView tests driving `render_submit(%{expr: …})` are
// unaffected (they read the form param / DOM value the hook keeps current).
// `phx-update="ignore"` on the CodeMirror host likewise keeps morphdom off its
// DOM. Server→editor pushes aren't needed: the server only ever echoes the
// submitted value (which the editor already holds), and surfaces that load
// distinct content (the method-editor tabs) by re-keying the element to remount.
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
    // Mount into the dedicated host. NOT `[phx-update=ignore]` — the textarea
    // also carries that attribute (so morphdom can't revert it), and it comes
    // first in the DOM, so the attribute selector would mount CodeMirror into the
    // textarea. Fail loud rather than silently mounting into `this.el`: the
    // wrapper is NOT phx-update=ignore, so every server patch would overwrite the
    // editor — an intermittent corruption far harder to diagnose than this error.
    this.host = this.el.querySelector(".cm-host")
    if (!this.field || !this.host) {
      console.error("CmEditor: expected a <textarea> and a .cm-host inside", this.el)
      return
    }

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
      // ⌘D/⌘P/⌘I are NOT bound here: they're the Workspace's Do-it/Print-it/
      // Inspect-it chords, handled by the form's KeyboardShortcuts hook when the
      // keydown bubbles out of the editor. defaultKeymap has no Mod-d/p/i binding
      // (selectNextOccurrence lives in @codemirror/search, which we don't bundle),
      // so these chords pass straight through. Tab indents; Enter stays a newline.
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
