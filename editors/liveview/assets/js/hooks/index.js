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
//   TweaksPanel       — appearance panel (theme/accent/syntax/density/fonts);
//                       flips :root CSS vars + persists to localStorage (BT-2487)
//   FieldFlash        — pulses the Inspector's changed ivar cells on a live
//                       per-object change refresh (BT-2492, backend BT-2489)
//   OmniSearch        — top-bar symbol search keyboard nav (arrow/enter/escape);
//                       filtering is server-side, the hook drives the highlight
//                       + open (BT-2495, epic BT-2482 Phase 3)
//
// The Workspace dock and method editor (later Phase 1 issues) build on these.

import { CodeEditor } from "./code_editor"
import { KeyboardShortcuts } from "./keyboard_shortcuts"
import { SelectionTracker } from "./selection_tracker"
import { TweaksPanel } from "./tweaks_panel"
import { FieldFlash } from "./field_flash"
import { OmniSearch } from "./omni_search"

export const Hooks = {
  CodeEditor,
  KeyboardShortcuts,
  SelectionTracker,
  TweaksPanel,
  FieldFlash,
  OmniSearch,
}
