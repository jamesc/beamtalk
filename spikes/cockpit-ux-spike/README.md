# cockpit-ux-spike — LiveView IDE UX spike (throwaway)

A LiveView-IDE design spike for Beamtalk: an interactive, high-fidelity prototype of
two switchable perspectives over one live image. This is a **front-end UX exploration**
for the real IDE in [`editors/liveview`](../../editors/liveview) — not wired into it.

- **Cockpit** (`Beamtalk Cockpit.html`) — System Browser (class hierarchy → protocol →
  methods), a method write-surface with Compile + ChangeLog/flush, a Workspace with
  doIt/printIt/inspectIt, live session Bindings, and a reference-following Inspector
  that can be **docked** or opened as **floating, stackable windows**. Floating
  inspectors subscribe to `changed:` and tick live, with a per-window freeze toggle.
- **Morphic** (`Beamtalk Morphic.html`) — a direct-manipulation world where running
  objects are draggable morphs with Squeak-style halos; poke them in place while they run.

Honors the repo's design decisions: real Beamtalk syntax (`=>` bodies, `self.field`,
`:=`, `//` comments, `!` async sends, `"{interpolation}"`), the Attach topology
(ADR 0017), static-first/live-augmented tooling (ADR 0024), the method write-surface +
ChangeLog (ADR 0082), and owner/observer roles.

## Status
Design spike / prototype. Pure front-end: static HTML + React (via CDN Babel) with a
faked-but-believable in-memory image (`image.js`) and a tiny evaluator (`evaluator.js`).
No build step — open either HTML file in a browser. This is for UX exploration, not a
wiring into the actual LiveView/Phoenix app.

## Files
| File | Role |
|---|---|
| `Beamtalk Cockpit.html` | Cockpit entry point |
| `Beamtalk Morphic.html` | Morphic world entry point |
| `styles.css` | Shared theme (Paper/Squeak/Dusk), layout, syntax palette |
| `morphic.css` | Morphic world + halos |
| `image.js` | Faked live image: classes, methods, running objects |
| `highlight.js` | Beamtalk syntax highlighter |
| `evaluator.js` | Small believable Workspace evaluator |
| `tweaks-panel.jsx` | In-prototype tweak controls (theme/accent/fonts/role/etc.) |
| `components.jsx` | System Browser + method list + code editor |
| `inspector.jsx` | Bindings list + docked/floating Inspector |
| `app.jsx` | Cockpit app shell + wiring |
| `morphic.jsx` | Morphic world app |

## Run
Open `Beamtalk Cockpit.html` (or `Beamtalk Morphic.html`) directly in a browser, or
serve the folder: `python3 -m http.server` then visit the file. Needs network access
for the React/Babel/font CDNs.
