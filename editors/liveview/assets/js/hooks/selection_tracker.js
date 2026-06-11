// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// SelectionTracker — a LiveView hook that reports a `<textarea>`'s current
// selection to the server, so the Workspace can show "evaluates selection" vs
// "evaluates buffer" and a later Do-it/Print-it can target the selected
// expression (the spike tracked this in a React ref — see the `onSelect`
// plumbing in `spikes/cockpit-ux-spike/app.jsx`).
//
// Attach to the textarea (or a wrapper that contains one):
//
//   <textarea phx-hook="SelectionTracker" id="..." data-select-event="select">
//
// On every selection change it pushes `data-select-event` (default "select")
// with `{text, start, end}`. Pushes are coalesced on an animation frame and
// suppressed when the selection is unchanged, so dragging a selection doesn't
// flood the socket. An empty selection (caret only) still reports `text: ""`,
// which the server reads as "no selection → evaluate the whole buffer".

export const SelectionTracker = {
  mounted() {
    this.ta = this.el.tagName === "TEXTAREA" ? this.el : this.el.querySelector("textarea")
    if (!this.ta) return

    this.event = this.el.dataset.selectEvent || "select"
    this.last = null
    this.frame = null

    this.onSelect = () => this.schedule()
    // `select` fires on mouse/keyboard selection; `keyup`/`click` catch caret
    // moves that collapse a selection (which `select` does not always emit).
    this.ta.addEventListener("select", this.onSelect)
    this.ta.addEventListener("keyup", this.onSelect)
    this.ta.addEventListener("click", this.onSelect)
  },

  destroyed() {
    if (this.frame) cancelAnimationFrame(this.frame)
    if (!this.ta) return
    this.ta.removeEventListener("select", this.onSelect)
    this.ta.removeEventListener("keyup", this.onSelect)
    this.ta.removeEventListener("click", this.onSelect)
  },

  schedule() {
    if (this.frame) return
    this.frame = requestAnimationFrame(() => {
      this.frame = null
      this.report()
    })
  },

  report() {
    const start = this.ta.selectionStart
    const end = this.ta.selectionEnd
    const text = this.ta.value.slice(start, end)
    const key = start + ":" + end + ":" + text
    if (key === this.last) return
    this.last = key
    this.pushEvent(this.event, { text, start, end })
  },
}
