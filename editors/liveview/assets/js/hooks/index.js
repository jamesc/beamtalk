// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
//
// The cockpit's LiveView JS hooks (BT-2485, epic BT-2482 Phase 1). Registered
// on the `LiveSocket` in `app.js` under their PascalCase names, referenced from
// the server markup via `phx-hook="<Name>"`:
//
//   CmEditor          — CodeMirror 6 code editor (BT-2538, BT-2539); owns the
//                       text, selection, history and highlighting for both the
//                       workspace eval input and the method-editor tabs. Replaced
//                       the CodeEditor overlay + SelectionTracker, now retired.
//   KeyboardShortcuts — Cmd/Ctrl chords → LiveView events (⌘S/⌘D/⌘P/⌘I)
//   TweaksPanel       — appearance panel (theme/accent/syntax/density/fonts);
//                       flips :root CSS vars + persists to localStorage (BT-2487)
//   FieldFlash        — pulses the Inspector's changed ivar cells on a live
//                       per-object change refresh (BT-2492, backend BT-2489)
//   OmniSearch        — top-bar symbol search keyboard nav (arrow/enter/escape);
//                       filtering is server-side, the hook drives the highlight
//                       + open (BT-2495, epic BT-2482 Phase 3)
//   WindowDrag        — drags a floating inspector window by its title bar and
//                       raises it to the front on click; reports the final
//                       position on drop (BT-2493, epic BT-2482 Phase 3)
//
// The Workspace dock and method editor (later Phase 1 issues) build on these.

import { CmEditor } from "./cm_editor"
import { KeyboardShortcuts } from "./keyboard_shortcuts"
import { TweaksPanel } from "./tweaks_panel"
import { FieldFlash } from "./field_flash"
import { OmniSearch } from "./omni_search"
import { WindowDrag } from "./window_drag"
import { ScrollToSelected } from "./scroll_to_selected"

export const Hooks = {
  CmEditor,
  KeyboardShortcuts,
  TweaksPanel,
  FieldFlash,
  OmniSearch,
  WindowDrag,
  ScrollToSelected,
}
