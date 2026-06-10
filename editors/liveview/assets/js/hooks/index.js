// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// The cockpit's LiveView JS hooks (BT-2485, epic BT-2482 Phase 1). Registered
// on the `LiveSocket` in `app.js` under their PascalCase names, referenced from
// the server markup via `phx-hook="<Name>"`:
//
//   CodeEditor        — syntax-highlighting editor overlay (highlight + Tab)
//   KeyboardShortcuts — Cmd/Ctrl chords → LiveView events (⌘S/⌘D/⌘P/⌘I)
//   SelectionTracker  — reports a textarea's selection (selection vs buffer)
//
// The Workspace dock and method editor (later Phase 1 issues) build on these.

import { CodeEditor } from "./code_editor"
import { KeyboardShortcuts } from "./keyboard_shortcuts"
import { SelectionTracker } from "./selection_tracker"

export const Hooks = {
  CodeEditor,
  KeyboardShortcuts,
  SelectionTracker,
}
