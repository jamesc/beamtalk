// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// FieldFlash — the live-Inspector field-flash hook (BT-2492, epic BT-2482
// Phase 3). When the inspected actor commits a state write, the workspace pushes
// `{:object_changed, …}` to the LiveView (backend BT-2489), which re-reads the
// object's fields and bumps `data-flash-gen` on the ivar table. This hook
// watches that table: on each `updated()` it compares every value cell's current
// value (`data-flash-val`, keyed by `data-flash-key`) against the value it last
// saw, and briefly adds `.vflash` to the cells that *changed* — the "subscribed,
// tracking live" tell from the spike Inspector (`inspector.jsx`, the `flashed[k]`
// → `vflash` class).
//
// Why a JS hook (and not server-rendered): the flash is a transient visual pulse
// driven by a CSS animation, applied only to the cells whose value differs from
// the previous render. `Phoenix.LiveViewTest` never loads `app.js`, so this is a
// connected-render behavior covered by the Playwright suite, not LiveViewTest.
//
// No flash storm: the server already coalesces a burst of change pushes into a
// single re-read (one `data-flash-gen` bump per burst, see `WorkspaceLive`), and
// this hook only flashes on an *actual value change* and auto-clears the class
// after the animation window — so a hot actor pulses once per coalesced refresh,
// never a strobing flicker. A debounce timer collapses multiple `updated()`
// callbacks landing in the same frame.
//
// Expected DOM (rendered by `WorkspaceLive.render/1`):
//
//   <table id="inspector-fields" phx-hook="FieldFlash" data-flash-gen="N">
//     …<td class="v …" data-flash-key="count" data-flash-val="7">7</td>…
//   </table>
//
// The first paint establishes the baseline (no flash on initial inspect); only
// subsequent value changes flash.

// How long the `.vflash` class stays on a changed cell (ms). Matches the CSS
// pulse window in `assets/css/app.css` (`@keyframes vflash`).
const FLASH_MS = 700

export const FieldFlash = {
  mounted() {
    // Baseline snapshot: record current values so the *initial* inspect does not
    // flash — only changes after this point pulse.
    this.prev = this.snapshot()
    this.gen = this.el.dataset.flashGen
    this.timers = new Map()
  },

  updated() {
    // Only react when the server signalled a fresh live refresh (the flash-gen
    // bumped). A re-render for an unrelated reason (e.g. a sibling assign) does
    // not re-flash, even if LiveView patched the table DOM.
    const gen = this.el.dataset.flashGen
    if (gen === this.gen) {
      // No live refresh — just re-baseline in case the rows changed shape (a
      // drill/crumb swap re-keys the cells), so a later change diffs correctly.
      this.prev = this.snapshot()
      return
    }
    this.gen = gen

    const next = this.snapshot()
    next.forEach((val, key) => {
      const had = this.prev.has(key)
      if (had && this.prev.get(key) !== val) this.flash(key)
    })
    this.prev = next
  },

  destroyed() {
    this.timers.forEach((t) => clearTimeout(t))
    this.timers.clear()
  },

  // A map of data-flash-key → data-flash-val across the value cells, the snapshot
  // we diff between refreshes.
  snapshot() {
    const map = new Map()
    this.cells().forEach((cell) => {
      const key = cell.dataset.flashKey
      if (key != null) map.set(key, cell.dataset.flashVal)
    })
    return map
  },

  cells() {
    return this.el.querySelectorAll("td[data-flash-key]")
  },

  // Pulse the value cell for `key`: add `.vflash`, then remove it after the
  // animation window. Re-flashing a cell already flashing restarts its timer (the
  // class is re-applied), so rapid changes keep pulsing without stacking timers.
  flash(key) {
    const cell = this.el.querySelector(
      `td[data-flash-key="${cssEscape(key)}"]`,
    )
    if (!cell) return

    cell.classList.remove("vflash")
    // Force a reflow so re-adding the class restarts the CSS animation even when
    // the class was still present from a previous, very recent flash.
    void cell.offsetWidth
    cell.classList.add("vflash")

    const existing = this.timers.get(key)
    if (existing) clearTimeout(existing)
    this.timers.set(
      key,
      setTimeout(() => {
        cell.classList.remove("vflash")
        this.timers.delete(key)
      }, FLASH_MS),
    )
  },
}

// Minimal CSS.escape fallback for attribute-selector safety (field names are
// Beamtalk identifiers, but stay defensive against a non-identifier key).
function cssEscape(s) {
  if (window.CSS && typeof window.CSS.escape === "function") {
    return window.CSS.escape(s)
  }
  return String(s).replace(/["\\\]]/g, "\\$&")
}
