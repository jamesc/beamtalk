// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// WindowDrag — the floating inspector window's drag + focus hook (BT-2493, epic
// BT-2482 Phase 3). Attached to each `.insp-window`, it makes the window
// draggable by its title bar and brings it to the front on a click, mirroring the
// spike's window management (spikes/cockpit-ux-spike/app.jsx).
//
// Architecture (per the issue): position (x, y) and z-order are CLIENT-SIDE — the
// drag moves the element directly via inline `left`/`top` for a smooth, no-latency
// drag, and the final position is reported to the server ONCE on drop
// (`pushEvent("window_moved", {id, x, y})`), never per-mousemove. The server holds
// the authoritative x/y/z in LiveView state, so a window's position survives an
// unrelated re-render. A mousedown anywhere in the window pushes
// `window_focus` so z-order follows focus (the server bumps its z above the rest).
//
//   <section class="insp-window" phx-hook="WindowDrag" data-window-id="win-3"
//            style="left:120px;top:96px;z-index:11;">
//     <header data-window-drag-handle> … title bar … </header>
//     …
//   </section>
//
// Only a drag that STARTS on the `[data-window-drag-handle]` title bar moves the
// window — a mousedown on a button / table inside the body brings the window to
// front (focus) but does not drag, so the close button, breadcrumb and ivar rows
// stay clickable.

// Clamp `n` into the inclusive `[lo, hi]` box.
const clamp = (n, lo, hi) => Math.min(hi, Math.max(lo, n))

export const WindowDrag = {
  mounted() {
    this.id = this.el.dataset.windowId
    this.handle = this.el.querySelector("[data-window-drag-handle]") || this.el

    // Drag state — null when not dragging.
    this.drag = null

    this.onDown = (e) => this.start(e)
    this.onMove = (e) => this.move(e)
    this.onUp = (e) => this.end(e)

    // A press anywhere in the window raises it to the front (focus → z-order).
    // We listen on the whole element (capture) so a click on the body raises it
    // too, but only a press that lands on the title bar begins a drag.
    this.onFocusDown = () => this.raise()

    this.el.addEventListener("mousedown", this.onFocusDown)
    this.handle.addEventListener("mousedown", this.onDown)
  },

  updated() {
    // A server re-render re-emits the window's `left`/`top` from the server's
    // authoritative x/y. The window_focus z-bump fired at drag-start (and any
    // sibling open/close, coalesced field refresh, or poke result) lands here
    // mid-gesture, carrying the PRE-drag position — without re-asserting, that
    // patch snaps the window back to where the drag began until the next
    // mousemove (a visible flicker, BT-2527 #1/#2). While a drag is in flight,
    // re-assert the live drag position so the window stays under the cursor; the
    // authoritative x/y is still reported once on drop.
    if (this.drag) {
      this.el.style.left = `${this.drag.x}px`
      this.el.style.top = `${this.drag.y}px`
    }
  },

  destroyed() {
    // Belt-and-braces: a window can be closed mid-drag (the close button lives on
    // the title bar), so always tear the document-level listeners down.
    this.el.removeEventListener("mousedown", this.onFocusDown)
    this.handle.removeEventListener("mousedown", this.onDown)
    document.removeEventListener("mousemove", this.onMove)
    document.removeEventListener("mouseup", this.onUp)
  },

  // Bring this window to the front. Cheap to fire on every press — the server
  // bumps the z only if needed and the re-render just updates one z-index.
  raise() {
    this.pushEvent("window_focus", { id: this.id })
  },

  start(e) {
    // Don't begin a drag from an interactive control on the title bar (the freeze
    // toggle, the close button): let those receive their click.
    if (e.target.closest("button")) return
    // Left button only.
    if (e.button !== 0) return

    e.preventDefault()

    const rect = this.el.getBoundingClientRect()
    const parent = this.el.offsetParent || document.body
    const parentRect = parent.getBoundingClientRect()

    this.drag = {
      // Pointer offset within the window, so the grab point stays under the cursor.
      offsetX: e.clientX - rect.left,
      offsetY: e.clientY - rect.top,
      parentLeft: parentRect.left,
      parentTop: parentRect.top,
      // Upper drag bounds: keep the window fully within its offset parent (the
      // viewport-filling overlay) so it can't be stranded off-screen and become
      // unreachable (BT-2527 #4). Captured at drag-start — the window's size is
      // stable for the duration of a drag.
      maxX: Math.max(0, parentRect.width - rect.width),
      maxY: Math.max(0, parentRect.height - rect.height),
      x: this.el.offsetLeft,
      y: this.el.offsetTop,
    }

    this.el.classList.add("dragging")
    document.addEventListener("mousemove", this.onMove)
    document.addEventListener("mouseup", this.onUp)
  },

  move(e) {
    if (!this.drag) return
    e.preventDefault()

    // New top-left, relative to the offset parent, clamped to the viewport box
    // [0, parent − window] so a window can't be dragged off any edge into
    // oblivion (lower bound keeps the top/left on-screen, upper bound the
    // bottom/right — BT-2527 #4).
    const x = clamp(e.clientX - this.drag.parentLeft - this.drag.offsetX, 0, this.drag.maxX)
    const y = clamp(e.clientY - this.drag.parentTop - this.drag.offsetY, 0, this.drag.maxY)

    this.drag.x = Math.round(x)
    this.drag.y = Math.round(y)

    // Move the element directly for a smooth drag — no server round-trip per move.
    this.el.style.left = `${this.drag.x}px`
    this.el.style.top = `${this.drag.y}px`
  },

  end() {
    if (!this.drag) return

    this.el.classList.remove("dragging")
    document.removeEventListener("mousemove", this.onMove)
    document.removeEventListener("mouseup", this.onUp)

    // Report the FINAL position once, so the server persists it in LV state and
    // the window keeps its place across re-renders.
    this.pushEvent("window_moved", { id: this.id, x: this.drag.x, y: this.drag.y })
    this.drag = null
  },
}
