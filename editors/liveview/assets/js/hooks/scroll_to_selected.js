// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// ScrollToSelected — keep the highlighted class row visible in the System
// Browser tree (BT-2495). When a method is opened from *outside* the tree — a
// Senders/Implementors jump to another class (`navigate_browser/3`) — the server
// moves the `.row.sel` highlight to the target class, but that row can sit below
// the fold in a long hierarchy. This hook scrolls it into view so the navigation
// is visible, mirroring how clicking a class already keeps it on screen.
//
// Why a JS hook: scrolling is a viewport concern the server can't express, and
// `Phoenix.LiveViewTest` never loads `app.js`, so this rides the Playwright
// suite, not LiveViewTest.
//
// Scoped to the class-tree scroll container (`this.el` = the tree's panel-body),
// so `.row.sel` only ever matches a class row — never the protocol/method panel,
// which lives in a separate panel and reuses the same `.sel` class.
//
// Only scrolls when the selection actually *changes* (tracked by the selected
// class name), and uses `block: "nearest"` so an already-visible row is left
// untouched — no jump on unrelated tree re-renders (view/side toggles, badge
// refreshes).

export const ScrollToSelected = {
  mounted() {
    this.sync()
  },

  updated() {
    this.sync()
  },

  sync() {
    const sel = this.el.querySelector(".row.sel")
    if (!sel) {
      this.prevKey = null
      return
    }

    // `title` carries the class name (see `class_rows/1`); fall back to text.
    const key = sel.getAttribute("title") || sel.textContent
    if (key === this.prevKey) return

    this.prevKey = key
    sel.scrollIntoView({ block: "nearest" })
  },
}
