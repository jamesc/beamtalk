// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// CodeEditor — a syntax-highlighting editor overlay LiveView hook.
//
// Ported from the spike's `CodeEditor` React component
// (`spikes/cockpit-ux-spike/components.jsx`). The technique: a transparent
// `<textarea>` sits exactly on top of a `<pre><code>` highlight layer. The user
// types into the (caret-only) textarea; on every input we re-run the Beamtalk
// highlighter over its value and paint the result into the `<pre>`. The two
// layers share identical font/padding/line-height (see `.bt-editor-*` in
// `app.css`) so the highlighted glyphs line up under the (transparent) typed
// glyphs. Scroll is kept in sync so the highlight tracks the caret.
//
// Expected DOM (rendered by the LiveView):
//
//   <div class="bt-editor-wrap" phx-hook="CodeEditor" id="...">
//     <pre class="bt-editor-pre" aria-hidden="true"><code></code></pre>
//     <textarea class="bt-editor-ta" ...></textarea>
//   </div>
//
// Data attributes on the wrapper:
//   data-tab-size      spaces inserted on Tab (default "2")
//   (the textarea's own `readonly` attribute disables Tab handling + edits)
//
// This hook owns ONLY the highlight overlay + Tab-indent behaviour. Keyboard
// shortcuts (Cmd/Ctrl+S/D/P/I) and selection reporting live in the sibling
// `KeyboardShortcuts` / `SelectionTracker` hooks so each concern stays small and
// independently attachable.

import { highlightBeamtalk } from "./highlight"

export const CodeEditor = {
  mounted() {
    this.ta = this.el.querySelector("textarea")
    this.pre = this.el.querySelector("pre")
    this.code = this.pre && this.pre.querySelector("code")
    if (!this.ta || !this.code) return

    this.tabSize = parseInt(this.el.dataset.tabSize || "2", 10)
    if (!Number.isFinite(this.tabSize) || this.tabSize < 1) this.tabSize = 2

    this.onInput = () => this.repaint()
    this.onScroll = () => this.syncScroll()
    this.onKeyDown = (ev) => this.handleKeyDown(ev)

    this.ta.addEventListener("input", this.onInput)
    this.ta.addEventListener("scroll", this.onScroll)
    this.ta.addEventListener("keydown", this.onKeyDown)

    this.repaint()
  },

  // The LiveView may patch the textarea's value (e.g. opening a different
  // method into the same editor element). Repaint so the highlight follows the
  // server-pushed content, not just local typing.
  updated() {
    this.repaint()
  },

  destroyed() {
    if (!this.ta) return
    this.ta.removeEventListener("input", this.onInput)
    this.ta.removeEventListener("scroll", this.onScroll)
    this.ta.removeEventListener("keydown", this.onKeyDown)
  },

  // Re-highlight the current textarea value into the <pre>. The trailing-space
  // fixup mirrors the spike: a value ending in "\n" would otherwise drop its
  // final (empty) line in the <pre>, so the highlight layer would be one line
  // short of the textarea and the two would desync on scroll.
  repaint() {
    const value = this.ta.value
    const padded = value.endsWith("\n") ? value + " " : value
    this.code.innerHTML = highlightBeamtalk(padded)
    this.syncScroll()
  },

  syncScroll() {
    this.pre.scrollTop = this.ta.scrollTop
    this.pre.scrollLeft = this.ta.scrollLeft
  },

  handleKeyDown(ev) {
    if (this.ta.readOnly) return
    if (ev.key !== "Tab" || ev.shiftKey || ev.metaKey || ev.ctrlKey) return

    // Insert spaces rather than letting Tab move focus out of the editor.
    ev.preventDefault()
    const indent = " ".repeat(this.tabSize)
    const { selectionStart: s, selectionEnd: e, value } = this.ta
    this.ta.value = value.slice(0, s) + indent + value.slice(e)
    this.ta.selectionStart = this.ta.selectionEnd = s + indent.length
    // Notify LiveView's input tracking + repaint the highlight.
    this.ta.dispatchEvent(new Event("input", { bubbles: true }))
  },
}
