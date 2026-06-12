// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// OmniSearch — the top-bar symbol search keyboard nav (BT-2495, epic BT-2482
// Phase 3). Filtering + ranking is server-side (the `nav-symbols` index, fetched
// + filtered in `WorkspaceLive.run_omni_search/2`); this hook owns only the
// CLIENT-side concerns a server render cannot do:
//
//   * highlight movement — ArrowDown/ArrowUp move the `.active` row WITHOUT a
//     server round-trip per keypress, and `preventDefault` so the arrows don't
//     move the text caret instead;
//   * open — Enter (or a click) reads the active row's `data-*` identity and
//     pushes `omni_open` with {kind, class, side, selector}, so the server opens
//     the class in the System Browser / the selector in an editable method tab;
//   * dismiss — Escape pushes `omni_close` and `preventDefault`s so it doesn't
//     bubble to a browser default.
//
// `Phoenix.LiveViewTest` never loads `app.js`, so none of this runs there — the
// arrow-key + enter flow is covered by the `:playwright` real-browser suite.
//
// The hook is bound on the search INPUT (`phx-hook="OmniSearch"`); the results
// live in a sibling `.omni-results` container the server re-renders on each
// keystroke. After a server patch (`updated/0`) the active index is re-clamped
// to the new result count and re-applied, since the row set changed under us.

export const OmniSearch = {
  mounted() {
    this.active = 0
    this.lastQuery = this.el.value
    this.onKeyDown = (ev) => this.handleKeyDown(ev)
    this.el.addEventListener("keydown", this.onKeyDown)
    // Clicking a result row opens it; delegate from the stable `.omni` wrapper
    // (`parentElement`) so the listener survives the server re-rendering the row
    // set (the `.omni-results` popover is created/destroyed as results come and
    // go). `mousedown`, not `click`, so the row opens BEFORE the input's blur
    // fires `phx-click-away="omni_close"` (which would tear the popover down out
    // from under a `click` and swallow it).
    this.onMouseDown = (ev) => this.handleClick(ev)
    this.el.parentElement.addEventListener("mousedown", this.onMouseDown)
  },

  // The server re-renders the result rows on each keystroke. CRUCIAL: a plain
  // arrow-key navigation also fires `keyup` → `phx-keyup="omni_search"` with the
  // SAME query, so the server re-renders the IDENTICAL list and this `updated/0`
  // runs. We must NOT snap the highlight back to the top in that case, or every
  // arrow press would be undone. So reset to row 0 ONLY when the query text
  // actually changed (a real new search); otherwise keep the user's highlight,
  // merely clamping it to the (possibly shorter) fresh row count.
  updated() {
    const rows = this.rows()
    if (this.el.value !== this.lastQuery) {
      this.lastQuery = this.el.value
      this.active = 0
    } else {
      this.active = Math.min(this.active, Math.max(rows.length - 1, 0))
    }
    this.applyActive()
  },

  destroyed() {
    this.el.removeEventListener("keydown", this.onKeyDown)
    this.el.parentElement.removeEventListener("mousedown", this.onMouseDown)
  },

  // The `.omni-results` container is the input's next-sibling popover. It may be
  // absent (popover closed); callers guard for that.
  container() {
    return this.el.parentElement.querySelector(".omni-results")
  },

  rows() {
    const c = this.container()
    return c ? Array.from(c.querySelectorAll(".omni-row")) : []
  },

  handleKeyDown(ev) {
    const rows = this.rows()

    switch (ev.key) {
      case "ArrowDown":
        if (rows.length === 0) return
        ev.preventDefault()
        this.active = Math.min(this.active + 1, rows.length - 1)
        this.applyActive()
        return
      case "ArrowUp":
        if (rows.length === 0) return
        ev.preventDefault()
        this.active = Math.max(this.active - 1, 0)
        this.applyActive()
        return
      case "Enter":
        if (rows.length === 0) return
        ev.preventDefault()
        this.openRow(rows[this.active])
        return
      case "Escape":
        ev.preventDefault()
        this.pushEvent("omni_close", {})
        return
      default:
        return
    }
  },

  handleClick(ev) {
    const row = ev.target.closest(".omni-row")
    if (row) {
      ev.preventDefault()
      this.openRow(row)
    }
  },

  // Move the `.active` class to `this.active` and scroll it into view.
  applyActive() {
    const rows = this.rows()
    rows.forEach((row, i) => {
      const on = i === this.active
      row.classList.toggle("active", on)
      row.setAttribute("aria-selected", on ? "true" : "false")
      if (on) row.scrollIntoView({ block: "nearest" })
    })
  },

  // Read a result row's identity from its data attributes and push it to the
  // server. A class row carries only kind+class; a selector row carries the
  // class/side/selector the editor tab needs.
  openRow(row) {
    if (!row) return
    this.pushEvent("omni_open", {
      kind: row.dataset.kind,
      class: row.dataset.class,
      side: row.dataset.side,
      selector: row.dataset.selector,
    })
  },
}
