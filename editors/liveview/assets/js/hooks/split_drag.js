// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// SplitDrag — the draggable divider (gutter) hook for the cockpit's resizeable
// panels (BT-2576). One axis-aware hook drives every splitter:
//
//   * VERTICAL gutters (`data-axis="y"`) sit between two stacked panels and
//     rebalance their heights — the System Browser's class tree vs. method list
//     (`.browser-split`) and the Bindings vs. Inspector stack (`.right-split`).
//   * HORIZONTAL gutters (`data-axis="x"`) sit on a cockpit column seam and
//     resize the side column's width — the `--browser-w` / `--inspector-w` grid
//     tracks.
//
// Like the floating-window TweaksPanel/WindowDrag pattern, the size is PURE
// PRESENTATION: it lives in a CSS custom property on a target element, is moved
// client-side for a smooth, no-latency drag, and is persisted to `localStorage`
// (NOT the server) so it survives a reload or LiveView reconnect with no server
// round-trip — exactly how TweaksPanel persists theme/density.
//
//   <div class="split-gutter split-gutter-y" phx-hook="SplitDrag"
//        data-split="browser" data-axis="y" data-edge="start"
//        data-var="--browser-split" data-min="80" data-min-other="120"></div>
//
// The controlled size is reported by `data-edge`:
//   * "start" — size grows as the pointer moves away from the container's
//     start edge (top/left): size = pointer − containerStart. Used by the
//     vertical splits (top pane) and the left column (browser width).
//   * "end"   — size grows as the pointer moves toward the start edge:
//     size = containerEnd − pointer. Used by the right column (inspector width),
//     whose seam is on its LEFT but which grows to the RIGHT.
//
// The CSS var is set on `this.el.parentElement` (the `.browser-split` /
// `.right-split` / `.cockpit` container the gutter lives in). The server never
// renders that var, so a LiveView re-render can't clobber the inline value.

// Clamp `n` into the inclusive `[lo, hi]` box. When the available range is
// degenerate (lo > hi, e.g. a tiny viewport) the lower bound wins.
const clamp = (n, lo, hi) => Math.max(lo, Math.min(hi, n))

const STORE_PREFIX = "bt.split."

export const SplitDrag = {
  mounted() {
    this.key = this.el.dataset.split
    this.axis = this.el.dataset.axis === "x" ? "x" : "y"
    this.edge = this.el.dataset.edge === "end" ? "end" : "start"
    this.varName = this.el.dataset.var
    this.min = parseInt(this.el.dataset.min || "80", 10)
    this.minOther = parseInt(this.el.dataset.minOther || "80", 10)
    this.target = this.el.parentElement

    // Drag state — null when not dragging.
    this.drag = null

    this.onDown = (e) => this.start(e)
    this.onMove = (e) => this.move(e)
    this.onUp = () => this.end()

    this.el.addEventListener("mousedown", this.onDown)

    // Re-apply any size the user saved in a previous session. The CSS default
    // (50% / 286px / 348px) renders first; this overrides it once on mount.
    this.restore()
  },

  // Re-apply the size after a LiveView re-render (BT-2638). The CSS var lives as
  // an inline `style` on the target (`this.el.parentElement`). When LiveView
  // patches that target — most visibly the center `.col` that holds the editor
  // and the workspace dock, re-rendered on every "open diff" / "new method tab"
  // — morphdom reconciles the element's attributes against the server template,
  // which never renders the var, and strips the JS-set inline value, snapping
  // the split back to its CSS default. `updated()` fires AFTER that patch, so
  // re-applying here puts the persisted size back. Skipped while a drag is in
  // flight so it never fights the live pointer value. `restore()` reads this
  // instance's own `data-split` key, so it is idempotent and never crosses over
  // to another splitter — safe for every shared instance.
  //
  // NOTE: this only runs for gutters WITHOUT `phx-update="ignore"`; LiveView
  // skips ignored elements on patch, so the dock gutter drops that attribute
  // (its div is empty — nothing to preserve) to let this callback fire.
  updated() {
    if (!this.drag) this.restore()
  },

  destroyed() {
    // A reconnect (network blip / deploy) can destroy the hook mid-drag. Run
    // end() first so it tears the drag down — removes the document listeners and
    // the body `*-resizing` classes (whose `transition: none` would otherwise
    // permanently suppress the BT-2559 collapse animation until a full reload).
    // It is a guarded no-op when no drag is in flight.
    this.end()
    this.el.removeEventListener("mousedown", this.onDown)
  },

  // Apply the persisted size (a CSS length string, e.g. "240px") to the target's
  // custom property. Ignored if nothing was saved or storage is unavailable.
  restore() {
    const saved = this.read()
    if (saved) this.target.style.setProperty(this.varName, saved)
  },

  start(e) {
    if (e.button !== 0) return
    e.preventDefault()

    const rect = this.target.getBoundingClientRect()
    // The container's extent along the drag axis, and the bounds within which the
    // controlled size may move — `min` for this pane, `minOther` (plus the gutter)
    // reserved for the rest.
    const span = this.axis === "x" ? rect.width : rect.height
    this.drag = {
      lo: this.min,
      hi: Math.max(this.min, span - this.minOther),
      start: this.axis === "x" ? rect.left : rect.top,
      end: this.axis === "x" ? rect.right : rect.bottom,
      size: null,
    }

    document.body.classList.add(this.axis === "x" ? "col-resizing" : "row-resizing")
    document.addEventListener("mousemove", this.onMove)
    document.addEventListener("mouseup", this.onUp)
  },

  move(e) {
    if (!this.drag) return
    e.preventDefault()

    const pointer = this.axis === "x" ? e.clientX : e.clientY
    const raw = this.edge === "end" ? this.drag.end - pointer : pointer - this.drag.start
    const size = Math.round(clamp(raw, this.drag.lo, this.drag.hi))

    this.drag.size = size
    // Move the divider directly for a smooth drag — no server round-trip.
    this.target.style.setProperty(this.varName, `${size}px`)
  },

  end() {
    if (!this.drag) return

    document.body.classList.remove("col-resizing", "row-resizing")
    document.removeEventListener("mousemove", this.onMove)
    document.removeEventListener("mouseup", this.onUp)

    // Persist the final size once, so the split keeps its place across reloads
    // and reconnects (pure-presentation preference, like the Tweaks panel).
    if (this.drag.size != null) this.write(`${this.drag.size}px`)
    this.drag = null
  },

  read() {
    try {
      return window.localStorage.getItem(STORE_PREFIX + this.key)
    } catch (_e) {
      return null
    }
  },

  write(value) {
    try {
      window.localStorage.setItem(STORE_PREFIX + this.key, value)
    } catch (_e) {
      // Storage unavailable (private mode / quota) — the drag still worked for
      // this session; only persistence is lost.
    }
  },
}
