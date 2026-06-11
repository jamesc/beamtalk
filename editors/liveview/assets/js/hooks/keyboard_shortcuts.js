// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// KeyboardShortcuts — a LiveView hook that turns Cmd/Ctrl chords into actions,
// so the cockpit's editor and Workspace expose the same shortcuts the spike's
// React app bound by hand (⌘S compile, ⌘D do-it, ⌘P print-it, ⌘I inspect-it —
// see `spikes/cockpit-ux-spike/app.jsx`).
//
// The bindings are declared on the element as JSON, keyed by the lowercased key
// with a `mod+` prefix meaning "Cmd on macOS / Ctrl elsewhere":
//
//   <form phx-hook="KeyboardShortcuts" id="..."
//         data-shortcuts='{"mod+s":"submit","mod+d":"do_it"}'>
//
// Each value is the action fired when the chord matches; the chord's default
// browser action (e.g. the Save dialog for ⌘S) is prevented either way:
//
//   "submit" — request-submit the hook element's form (or its closest form).
//              This carries the form's field values through the normal
//              phx-submit, which a bare pushEvent could not. ⌘S = "Save Method"
//              uses this so the class/selector/source ride along.
//   "submit:<action>" — set the form's hidden `action` input to <action>, then
//              request-submit. This is how the Workspace dock's ⌘D/⌘P/⌘I
//              (do_it / print_it / inspect_it) ride the SAME eval form submit as
//              the buttons, carrying the entered code (or selection) along — a
//              bare pushEvent could not. The form must contain
//              `<input type="hidden" name="action">` for the value to ride.
//   any other string — pushed as a `phx-` server event name with an empty
//              payload (for parameterless actions a later pane wires up).
//
// Listening is scoped to the element by default (so panes can bind overlapping
// chords without colliding); add `data-scope="window"` to listen globally for
// an editor-wide chord like ⌘S.

function isMod(ev) {
  // Treat both Cmd (mac) and Ctrl (everywhere else) as the modifier. We don't
  // require *exactly* one so a user with Ctrl held on macOS still triggers it,
  // but we DO reject Alt/Shift so plain editing chords don't misfire.
  return (ev.metaKey || ev.ctrlKey) && !ev.altKey && !ev.shiftKey
}

export const KeyboardShortcuts = {
  mounted() {
    this.bindings = this.parseBindings()
    this.target = this.el.dataset.scope === "window" ? window : this.el
    this.onKeyDown = (ev) => this.handleKeyDown(ev)
    this.target.addEventListener("keydown", this.onKeyDown)
  },

  // Re-read bindings on patch so the server can re-key shortcuts live.
  updated() {
    this.bindings = this.parseBindings()
  },

  destroyed() {
    if (this.target && this.onKeyDown) {
      this.target.removeEventListener("keydown", this.onKeyDown)
    }
  },

  parseBindings() {
    const raw = this.el.dataset.shortcuts
    if (!raw) return {}
    try {
      const parsed = JSON.parse(raw)
      return parsed && typeof parsed === "object" ? parsed : {}
    } catch (_err) {
      // A malformed data-shortcuts must not break key handling for the page.
      return {}
    }
  },

  handleKeyDown(ev) {
    if (!isMod(ev) || !ev.key) return
    const chord = "mod+" + ev.key.toLowerCase()
    const action = this.bindings[chord]
    if (!action) return
    ev.preventDefault()

    if (typeof action !== "string") return
    if (action === "submit" || action.startsWith("submit:")) {
      const form =
        this.el.tagName === "FORM" ? this.el : this.el.closest("form")
      if (!form) return
      // "submit:<action>" sets a hidden `action` input so the chord rides the
      // same form submit as the matching button (the Workspace dock's
      // ⌘D/⌘P/⌘I → do_it/print_it/inspect_it). Plain "submit" leaves it alone.
      const colon = action.indexOf(":")
      if (colon !== -1) {
        const value = action.slice(colon + 1)
        const hidden = form.querySelector('input[name="action"]')
        if (hidden) hidden.value = value
      }
      form.requestSubmit()
      return
    }

    // Otherwise push the mapped server event. `this.el` is the hook element,
    // which the server uses as the default event target.
    this.pushEvent(action, {})
  },
}
