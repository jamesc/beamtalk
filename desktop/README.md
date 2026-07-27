# Beamtalk desktop picker

A Tauri (Rust) shell over `beamtalk-desktop-broker` (ADR 0097, BT-2985) and
`beamtalk-desktop-shell` (BT-2986): a native picker window listing live
workspaces, attach/detach, a first-run empty state, and window-per-workspace.

See `docs/ADR/0097-desktop-attach-client-node-per-workspace.md` and
`docs/research/desktop-shell-spike.md` for the design and the spike that
settled (with caveats — see "Shell decision status" below) the window model
and single-instance policy this app implements.

## Layout

```
desktop/
  src-tauri/          Rust backend (NOT a Cargo workspace member — see below)
    src/
      main.rs          App setup: single-instance plugin, orphan-sweep, state
      commands.rs       Tauri commands: list_workspaces, attach, detach,
                         create_workspace, quit
      state.rs          AppState: AttachManager + spawned Child handles
      launcher.rs        Resolves the bundled bt_attach `bin/server` path
      dto.rs             JSON view models for the frontend
    tauri.conf.json
    capabilities/default.json
  ui/                  Frontend: plain HTML/CSS/JS, no build step
```

## Why this crate is excluded from the root Cargo workspace

`desktop/src-tauri`'s `Cargo.toml` has its own `[workspace]` table and is
listed in the root `Cargo.toml`'s `exclude` — the same treatment `fuzz/` gets,
for the same kind of reason: it needs a toolchain the rest of the repo
doesn't. Concretely, `tauri` needs `webkit2gtk-4.1` + `glib`/`gtk3` dev
packages on Linux (WebView2 on Windows, WKWebView on macOS — no extra
packages needed there) and the `cargo-tauri` CLI. Requiring those for
`just build`/`just ci`'s `cargo build --workspace` would break the build for
everyone, not just this crate. Wiring an actual CI build lane that installs
the Linux/macOS toolchain is BT-2987's job (packaging); this issue (BT-2986)
only writes the picker's source.

## Shell decision status (read before extending this app)

The BT-2984 spike (`docs/research/desktop-shell-spike.md`) leans Tauri but
says plainly it did **not** hands-on validate criterion (e) — webview
keybinding/rendering parity — because its sandbox had no display server, no
`cargo-tauri`, and no target OS. **This crate's development sandbox had the
identical constraint** (see "What was and wasn't verified" below), so nothing
in this implementation newly confirms (e) either. Treat the Tauri choice as
still "leaning," not "closed," until someone runs `cargo tauri dev` on a real
desktop and exercises `Cmd/Ctrl-W` interception and WebKitGTK-on-Linux
stability under real LiveView traffic.

## What was and wasn't verified

This app was written in a sandbox with **no display server, no `cargo-tauri`
CLI, and no `webkit2gtk-4.1`/`glib`/`gtk3` development packages** (and no
root/sudo access to install them). What that means concretely:

- **Verified**: `cargo check` in `desktop/src-tauri` successfully resolved
  the full dependency graph against real published crates (`tauri = "2"`,
  `tauri-plugin-single-instance = "2"`, `tauri-build = "2"`, plus every
  transitive dependency) and started compiling — it got as far as building
  several dozen crates before failing at `glib-sys`'s build script, which
  needs `pkg-config` to find `glib-2.0` (not installed here). The resulting
  `Cargo.lock` is committed, so the dependency graph itself is pinned and
  known-good.
- **Not verified**: this crate's own Rust source (`main.rs`, `commands.rs`,
  `state.rs`, `launcher.rs`, `dto.rs`) has **not** been type-checked by
  `rustc` — the build never got far enough to reach this crate's own
  compilation unit. The Tauri API usage (window building, event emission,
  the single-instance plugin's callback signature, command registration) is
  written from documented Tauri v2 APIs and cross-checked against
  `beamtalk-desktop-broker`'s actual public signatures (read directly from
  its source, not guessed), but has not compiled.
- **Not verified at all**: anything requiring a webview or window manager —
  the frontend (`ui/`) rendering correctly, IPC payload shapes matching
  between `dto.rs`'s `serde` output and `main.js`'s field access, the
  single-instance plugin's runtime focus behavior, `WebviewWindow::set_title`
  actually updating a real title bar, or criterion (e) (keybinding/rendering
  parity) at all.
- **Fully verified**: the shell-agnostic decision logic
  (`beamtalk-desktop-shell`'s `attach`/`empty_state` modules) — that crate has
  no GUI dependency, is a normal workspace member, and its full test suite
  passes under `just test`.

**Before this ships**, someone with a real Linux/macOS/Windows desktop needs
to: install the Tauri prerequisites (`https://v2.tauri.app/start/prerequisites/`),
run `cargo tauri dev` from this directory, fix whatever `rustc` errors surface
(expect some — nontrivial Tauri API surface written without compiler
feedback almost always needs at least minor fixes), and manually exercise the
picker end-to-end against a real `beamtalk workspace create --background
--persistent` workspace.

## Local development (once you have the Tauri toolchain)

```bash
# Point the broker at a from-source dist-liveview build instead of a
# packaged bundle (BT-2987 hasn't wired bundling yet):
just dist-liveview   # from editors/liveview/, produces bin/server
export BEAMTALK_ATTACH_LAUNCHER=/path/to/dist-liveview/bin/server

cd desktop
cargo tauri dev
```

## Security note on capabilities

`capabilities/default.json` scopes command access to the `picker` window
only — workspace windows (label `ws-<id>`) load the attached front's own
`http://127.0.0.1:<port>/` LiveView page and have **no** capability grant, so
that (arbitrary, workspace-authored) content cannot invoke any of this app's
Tauri commands. Detach/quit/window-title updates for those windows are all
driven from the Rust side, never from JS running inside them.
