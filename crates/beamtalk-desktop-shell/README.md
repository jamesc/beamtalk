# beamtalk-desktop-shell

Desktop picker shell-agnostic logic (ADR 0097, BT-2986): attach-twice /
focus-existing decisions, window-per-workspace label bookkeeping, and
first-run empty-state classification.

This is a plain Rust library — no GUI dependency, same posture as
[`beamtalk-desktop-broker`](../beamtalk-desktop-broker). It sits between the
broker (process supervision: discovery, spawn, readiness, monitoring,
reaping) and the actual GUI shell (`desktop/` at the repo root — a Tauri
application, **not** a Cargo workspace member; see that directory's README
for why and what that means for verification).

The split exists so the decision logic that most needs to be *correct* —
"does clicking Attach twice spawn a second front or focus the existing
window", "is zero-workspaces a real empty state or a bug" — is unit-tested by
`just test`/`just ci` without requiring a Tauri toolchain, `webkit2gtk`
headers, or a display server, none of which are guaranteed to be available in
every environment this crate is built in.

## Modules

| Module | Responsibility |
|---|---|
| `attach` | `AttachManager`: attach-twice / focus-existing decisions, window-per-workspace label bookkeeping |
| `empty_state` | First-run empty-state classification (workspace list vs "create a workspace" vs setup instructions) |

## What this crate does not do

- Open windows, register commands, or touch a webview — that is `desktop/`'s
  job.
- Spawn processes or poll HTTP — that is `beamtalk-desktop-broker`'s job;
  this crate only decides what a GUI shell should do next, given
  broker-shaped inputs.
