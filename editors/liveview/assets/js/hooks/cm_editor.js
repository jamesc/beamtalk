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
import { inlineResultsField, addInlineResult } from "./inline_results"
import { backendCompletion, completionQuery } from "./cm_autocomplete"

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
    // Tab-id stamp (BT-2549): the method editor re-keys this element per active
    // tab, so `data-tab-id` captures the tab THIS editor instance edits. We send
    // it with every selection push; the server ignores any push whose stamp ≠ the
    // active tab, so a `select_source` the departing editor dispatched just before
    // `destroyed()` can't re-populate `:edit_selection` with stale coordinates
    // after a tab switch. Absent (Workspace editor) → null, which that handler
    // doesn't read.
    this.tabId = this.el.dataset.tabId || null
    this.lastSelection = null
    this.hadSelection = false
    // Opt-in to inline eval results (BT-2542). Only the Workspace editor sets
    // data-inline-results; the REPL input (BT-2543) shows results in its own
    // scrollback, and it diverges further (Enter submits, ↑/↓ recall history), so
    // it ships as a sibling hook (repl_input.js) rather than a flag here.
    this.inlineResults = this.el.dataset.inlineResults === "true"
    // Opt-in to backend-driven autocomplete (BT-2544). The Workspace editor sets
    // data-autocomplete; the method-editor tabs can adopt it by adding the same
    // attribute (the completion source is editor-agnostic — it round-trips the
    // current line to the live image). Never enabled for a read-only editor.
    const autocomplete = this.el.dataset.autocomplete === "true"
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
    if (this.inlineResults) extensions.push(inlineResultsField)
    // Backend-driven autocomplete (BT-2544): candidates come from the live image
    // via the `complete` LiveView event. Read-only editors never complete.
    if (autocomplete && !readOnly) {
      extensions.push(backendCompletion(completionQuery(this.pushEvent.bind(this))))
    }

    this.view = new EditorView({
      state: EditorState.create({ doc: this.field.value, extensions }),
      parent: this.host,
    })
    // Expose the view on the wrapper for debugging and the Playwright e2e suite
    // (which dispatches a doc-replace transaction to set editor contents
    // deterministically — there is no <textarea> value to fill any more).
    this.el.cmView = this.view

    // Print it inserts the result inline (BT-2542): the server pushes the
    // rendered result here after eval; we drop it as a collapsible block widget
    // after the evaluated region. Only registered for the Workspace editor.
    if (this.inlineResults) {
      this.handleEvent("ws_insert_result", (payload) => this.insertInlineResult(payload))
    }
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

  // Anchor a `→ result` block widget after the evaluated region: the end of the
  // selection if one is active (we evaluated the selection), else the end of the
  // buffer (we evaluated the whole buffer). Anchoring at the END of that line's
  // doc position lets the block widget render on the line(s) below it. No doc
  // edit happens, so the eval path (which reads the mirrored textarea text) never
  // sees the result and "evaluate buffer" stays pure code.
  insertInlineResult(payload) {
    if (!this.view || !payload || typeof payload.text !== "string") return
    // push_event is page-wide; honour the server's element-id target so only the
    // intended editor inserts even if a second editor also registered the handler.
    if (payload.target && payload.target !== this.el.id) return
    const state = this.view.state
    const main = state.selection.main
    // Prefer the server-echoed anchor (the evaluated selection's end at submit
    // time) so a cursor move during the eval round-trip can't misplace the
    // result; fall back to the live selection/buffer end. Clamp in case the doc
    // shrank while waiting.
    const fallback = main.empty ? state.doc.length : main.to
    const raw = payload.anchor != null ? payload.anchor : fallback
    const anchor = Math.min(Math.max(raw, 0), state.doc.length)
    const pos = state.doc.lineAt(anchor).to
    // Scroll to the widget position (not the cursor): this is a pure-effect
    // transaction with no selection move, so `scrollIntoView: true` would target
    // the unchanged cursor and leave a result below the fold unrevealed.
    this.view.dispatch({
      effects: [addInlineResult.of({ pos, text: payload.text }), EditorView.scrollIntoView(pos)],
    })
  },

  reportSelection(update) {
    const range = update.state.selection.main
    // Skip cursor-only moves (range.empty): they'd push text:"" to the server on
    // every keystroke — a no-op re-render (only the "evaluates buffer/selection"
    // label). Still send ONE empty report when collapsing a prior non-empty
    // selection, so `ws_selection` is cleared and eval falls back to the buffer.
    if (range.empty && !this.hadSelection) return
    this.hadSelection = !range.empty
    const text = update.state.sliceDoc(range.from, range.to)
    const key = range.from + ":" + range.to + ":" + text
    if (key === this.lastSelection) return
    this.lastSelection = key
    this.pushEvent(this.selectEvent, {
      text,
      start: range.from,
      end: range.to,
      tab_id: this.tabId,
    })
  },
}
