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
      main.rs          App setup: single-instance plugin, orphan-sweep, state,
                         OS-level-quit (Cmd-Q/SIGTERM) detach-all cleanup, and
                         (BT-3252) the picker window's own `CloseRequested`
                         handler — see "Closing the picker" below
      commands.rs       Tauri commands: list_workspaces, attach, detach,
                         create_workspace, quit (a thin wrapper around
                         `quit_app`, the detach-all-then-exit path BT-3252's
                         picker close handler shares with it)
      menu.rs            App-wide menu (BT-3244): swaps the native ⌘/Ctrl+W
                           "Close Window" item for a plain ⇧⌘W one, freeing
                           ⌘/Ctrl+W so the LiveView cockpit's own `mod+w`
                           binding can close the focused editor tab instead
      state.rs          AppState: AttachManager + spawned Child handles
      launcher.rs        Resolves the bundled bt_attach launcher path
                           (`bin/server` on Unix, `bin\bt_attach.bat` on
                           Windows — BT-2988)
      dto.rs             JSON view models for the frontend
    entitlements.plist  Hardened Runtime entitlements for the bundled BEAM
                         release (BT-2987 — see the packaging section below)
    icons/               Placeholder icon set (resized from
                         editors/vscode/images/icon.png — the existing
                         Beamtalk brand asset) so the bundler has something
                         to embed; swap for real desktop-app art before a
                         real release. icon.ico (BT-2988) is generated from
                         the same source (128x128@2x.png) — Tauri's Windows
                         MSI/NSIS bundlers need it; the placeholder-icon
                         provenance/swap note above applies equally to it
    tauri.conf.json
    capabilities/default.json
    Cargo.lock            Committed (unlike the main workspace's) — this is a
                         shipped binary application, not a library; see the
                         root .gitignore's Cargo.lock exception and its
                         comment
    tests/
      menu_main_thread.rs `harness = false` integration test (BT-3244):
                           `#[path]`-includes `src/menu.rs` into a plain
                           `fn main` so it runs on the real OS main thread,
                           the one thing `src/menu.rs`'s own `#[cfg(test)]`
                           unit tests can't do — see its doc comment
  ui/                  Frontend: plain HTML/CSS/JS, no build step
  e2e/                 BT-2989 E2E validation scripts (attach-cycle.sh +
                         eval-roundtrip.mjs) — see e2e/README.md for what
                         they cover and why, absent a working Tauri
                         toolchain in any sandbox to date, they validate the
                         broker's real spawn/readiness/detach contract and a
                         real browser eval round-trip rather than the
                         compiled picker binary itself
```

## Packaging (BT-2987 Linux/macOS, BT-2988 Windows)

`.github/workflows/desktop-release.yml` builds this app for Linux x86_64,
macOS arm64 + x86_64, and Windows x86_64, bundling a freshly-built
`dist-liveview` release (`just dist-liveview`) as a Tauri resource —
`tauri.conf.json`'s `bundle.resources` maps repo-root `dist-liveview/` to
`dist-liveview/` inside the app's resource dir, exactly where `launcher.rs`'s
`BUNDLED_LAUNCHER_RELATIVE_PATH` expects it once no
`BEAMTALK_ATTACH_LAUNCHER` override is set. `bin/server`/`bin\bt_attach.bat`
are never modified — the workflow runs the same `just dist-liveview` recipe a
from-source dev uses (via a `[windows]`-tagged PowerShell variant of that
Justfile recipe on Windows, since `bin/server`'s bash-script role there has no
POSIX-shell counterpart to build).

On macOS, every nested Mach-O binary in that bundled release (`beam.smp`,
`epmd`, any NIF `.so`/`.dylib`) is signed by
`scripts/ci/sign-macos-nested-binaries.sh` *before* `cargo tauri build` copies
them in, because Tauri's own macOS bundler only reliably signs the top-level
`.app` — arbitrary resource binaries are a known gap
(tauri-apps/tauri#11992) and Apple's notary service rejects a bundle
containing any unsigned Mach-O. `entitlements.plist` grants the Hardened
Runtime allowances BEAM needs (`allow-jit` +
`allow-unsigned-executable-memory` for BeamAsm's JIT, `disable-library-
validation` for independently-signed NIFs) to both that pre-pass and the
top-level app (`bundle.macOS.entitlements`). See the workflow file's header
comment for the full signing/notarization flow and the required secrets.
Windows has no equivalent code-signing pass in this lane — the `.msi`/`.exe`
Tauri produces are unsigned; that's a real gap (unsigned installers trigger
SmartScreen warnings), tracked as follow-up rather than silently worked
around here.

On Linux, `bundle.linux.deb.depends` in `tauri.conf.json` declares
`libssl3t64` — Tauri's generated `.deb` metadata only covers the
webkit2gtk/gtk3 stack it needs directly, and has no idea the bundled
`dist-liveview` release also ships a full BEAM release whose `:crypto`
NIF (`crypto.so`) dynamically links `libcrypto.so.3`. That package name
and the rest of this list (BT-3017) came from running `ldd` against a real
`just dist-liveview` build's `erts-*/bin/beam.smp`, `erts-*/bin/epmd`, and
every NIF `.so` under `lib/*/priv/lib/` on Ubuntu 24.04 (`noble`) — the OS
`desktop-release.yml`'s `ubuntu-latest` runner used at the time. Every
other shared-library dependency those binaries have (`libc.so.6`,
`libm.so.6`, `libgcc_s.so.1`, `libstdc++.so.6`, `libtinfo.so.6`,
`libz.so.1`) is already pulled in transitively by the webkit2gtk/gtk3/
appindicator packages Tauri's bundler declares on its own, so they're
deliberately left off this list rather than declared redundantly. **This
list needs re-deriving (same `ldd` procedure) if the CI runner's Ubuntu
version ever changes** — package names and transitive-dependency
guarantees are specific to `noble`'s package set and aren't guaranteed to
hold on a future Ubuntu release.

**This packaging lane is unverified for the same reason the app itself is**
(see below) — no macOS/Linux/Windows Tauri toolchain has been available to
actually run `cargo tauri build` end-to-end here, so it is wired as
`workflow_dispatch`/`workflow_call` only, not yet called from `release.yml`'s
`on: release` path. Flip that once a real run confirms the build (and,
separately, the signing/notarization steps against real Apple secrets) works
on every platform.

### Windows-specific gaps (BT-2988, documented rather than silently worked around)

- **Broker spawn path is unverified against a real Windows `mix release`
  boot.** `beamtalk-desktop-broker`'s Windows `build_launch_command`
  (`crates/beamtalk-desktop-broker/src/spawn.rs`) resolves
  `BT_WORKSPACE_NODE`/`BT_WORKSPACE_COOKIE` from disk, generates an ephemeral
  `SECRET_KEY_BASE`, and invokes `bin\bt_attach.bat start` directly — the
  logic cross-checks against `bin/server`'s actual Unix behavior and
  `config/runtime.exs`'s env-var contract, and has unit test coverage, but
  has not been exercised against a real built `dist-liveview` release on
  Windows.
- **Orphan/process-tree cleanup is closed by construction, not just hoped
  for.** `bin\bt_attach.bat` can only run via `cmd.exe` (Windows cannot
  `CreateProcessW` a `.bat` directly), so `std::process::Child::kill` alone
  would always terminate `cmd.exe` and orphan `erl.exe` underneath it — not
  an unverified maybe, a certainty given how Windows executes batch files.
  `beamtalk_desktop_broker::winjob::JobHandle` (`JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE`)
  closes that: the spawned process tree is assigned to a job object that
  kills every process in it — including everything spawned after
  assignment — the moment the job handle closes, whether that's an explicit
  detach or this broker process itself dying uncleanly. What remains
  unverified is only the *mechanism's* real-world behavior (a small
  assign-after-spawn race — see `spawn.rs`'s and `winjob.rs`'s module doc
  comments) and whether `AssignProcessToJobObject`/`SetInformationJobObject`
  behave as documented against a real `mix release` boot; `reap`'s PID-file
  sweep remains the fallback net for whatever this mechanism still misses.
- **No code signing.** See the Packaging section above — the `.msi`/NSIS
  `.exe` this lane produces are unsigned.
- **CI build lane is unverified.** `desktop-release.yml`'s Windows leg (WiX/
  NSIS self-download, WebView2 Runtime presence, `package-desktop-release.sh`'s
  `windows-x86_64` case) has not been exercised against a real
  `windows-2022` run — see that workflow's header comment for the specific
  list of unverified assumptions.
- **Console-window suppression and the CI build-lane's `--config` quoting
  were both real bugs, now fixed rather than open gaps.** Without
  `windows_subsystem = "windows"` (`desktop/src-tauri/src/main.rs`) a
  release build links console-subsystem, popping a visible window behind
  the picker's GUI on every launch; `crate::spawn::detach` additionally
  sets `CREATE_NO_WINDOW` so the console-subsystem `cmd.exe` wrapper (see
  the process-tree point above) doesn't pop one *per front* either. The
  `Justfile`'s `dist-desktop-platform` Windows recipe now writes its
  `--config` JSON to a temp file instead of inlining it, because Windows
  PowerShell 5.1 does not re-escape embedded quotes when building a native
  command's argv — the inline form silently reached `cargo-tauri.exe` as
  invalid JSON. None of this has been exercised on a real Windows build
  either (same caveat as everything else here), but these are code fixes,
  not open questions.
- **epmd/dist behavior differences were not independently investigated**
  beyond the process-tree point above — Windows epmd is the same `erl`-
  distributed epmd binary as Unix (no protocol difference expected), but this
  was not confirmed against a live Windows BEAM boot in this session.

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

### Unit test CI coverage (BT-3061)

Being excluded from the root workspace does NOT mean this crate's own
`#[cfg(test)]` unit tests go untested in CI — `.github/workflows/ci.yml`'s
`test-desktop` job installs the same Linux Tauri toolchain (Linux only;
Windows/macOS runners already ship what's needed) and runs `just
test-desktop` on every push/PR, across Linux, macOS, and Windows, same as
every other test job in that workflow. That's a separate concern from the
packaging lane above: `test-desktop` only compiles and runs this crate's own
unit tests (fast, no `dist-liveview` release build required — an empty
placeholder directory satisfies `tauri.conf.json`'s `bundle.resources`
existence check that this crate's `build.rs` enforces via `tauri_build::
build()`), where `desktop-release.yml` builds and bundles the full shippable
app. Run it locally with `just test-desktop` once you have the same
prerequisites `cargo tauri dev` needs (below).

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
  needs `pkg-config` to find `glib-2.0` (not installed here, and this
  environment has no root/sudo to add it either — re-confirmed identically
  in the BT-2987 packaging session, same missing-`glib-2.0` failure).
  `Cargo.lock` is now actually committed (BT-2987 — the BT-2986 session that
  first wrote this paragraph generated one locally but it was silently
  caught by the root `.gitignore`'s blanket `Cargo.lock` rule and never
  made it into git; see that file's `desktop/src-tauri/Cargo.lock`
  exception), so the dependency graph is genuinely pinned now, not just
  described as such.
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
- **Specific risk flagged by adversarial review, worth checking first**:
  `commands.rs`'s post-attach monitor (`spawn_monitor`) is a raw
  `std::thread::spawn` loop that calls `WebviewWindow::set_title` (via
  `reflect_connection_state`) and `Child::try_wait` (via `reap_if_exited`)
  from that background thread, not Tauri's own command-dispatch thread pool.
  Tauri's window handles are documented as safe to call from any thread
  (internally proxied to the main event loop), so this is expected to be
  fine — but that expectation is unverified here. If `cargo tauri dev`
  shows title updates not applying, or a panic/hang originating from
  `spawn_monitor`, route the window-mutating calls through
  `AppHandle::run_on_main_thread` instead.
- **Fully verified**: the shell-agnostic decision logic
  (`beamtalk-desktop-shell`'s `attach`/`empty_state` modules) — that crate has
  no GUI dependency, is a normal workspace member, and its full test suite
  passes under `just test`.
- **A second adversarial review pass found, and this crate's source now
  fixes, several issues verified against the exact pinned Tauri source in
  `Cargo.lock`** (read directly from `~/.cargo/registry/src/`, not just
  documented API surface, since this environment still can't compile the
  crate to check any of it against `rustc`): every command is now
  `#[tauri::command(async)]` — a plain (non-`async`) command's body runs
  inline on the thread that delivered the IPC message, which desktop
  platforms bind to the webview's UI/main thread, so `attach`'s up-to-30s
  readiness wait would otherwise have frozen the whole app; `detach_internal`
  now uses `WebviewWindow::destroy()` instead of `.close()`, since `.close()`
  re-emits `CloseRequested` and this function is itself that event's handler
  (an unbounded reentrant loop on every workspace-window close); a claim a
  concurrent `quit` clears mid-attach can no longer produce a permanently
  stuck ghost `AttachManager` entry (`AttachManager::record_attached_if_claiming`);
  OS-level quit (`Cmd-Q`, `SIGTERM`, …) now runs the same detach-all cleanup
  as the in-app Quit button via a `RunEvent::ExitRequested` handler in
  `main.rs`; and the picker's free-text "create a workspace" field is
  validated (`empty_state::validate_new_workspace_id`) before the typed
  string ever reaches the CLI subprocess as a positional argument. None of
  this is a substitute for actually building and running the app — see
  "Before this ships" below — but it closes the gaps a real compiler and a
  real webview would otherwise have been the first to catch.
- **BT-3244 (the ⌘W/menu change) was written in a sandbox that, unlike every
  session above, *does* have a working `cargo`/`rustc` toolchain and Xcode
  Command Line Tools** — macOS needs no `pkg-config`/`glib-2.0` (that's a
  Linux/WebKitGTK-only build dependency), so `cargo check`, `cargo clippy
  --all-targets`, `cargo fmt --check`, and `cargo test` all ran for real
  against this crate (not just resolved dependencies) and passed, including
  `menu.rs` and its `main.rs` wiring. This also enabled real test coverage
  beyond what earlier sessions in this file could get: `menu.rs` builds a
  real `tauri::menu::Menu` against `tauri::test::mock_app`'s headless
  `MockRuntime` and asserts the custom "Close Window" item — not the native
  `PredefinedMenuItem` it replaces — is what actually ends up in the tree.
  That specific assertion needs a real OS main thread (`muda`, Tauri's menu
  backend, panics building native items off it, and the standard `#[test]`
  harness runs each test on its own worker thread) — see
  `tests/menu_main_thread.rs`'s own doc comment for how that's worked around
  (`harness = false`, `#[path]`-including `src/menu.rs` into a plain `fn
  main`) without turning this bin-only crate into a library. Still **not**
  verified: `cargo tauri dev` itself (no display server here either), so the
  menu's actual on-screen shape, and whether ⌘W really reaches the LiveView
  page once the native binding is gone, remain to be confirmed by hand on a
  real Mac — same as everything else under "Not verified at all" above. The
  `bundle.resources` path in `tauri.conf.json` also still needs a real
  `dist-liveview/` (from `just dist-liveview`) to build past the
  resource-copy step; there's none committed here, so a fresh `cargo tauri
  build`/`bundle` will fail on that alone until one is produced locally.
- **BT-3244's own first-pass implementation was adversarially reviewed
  (fresh Opus subagent) and two real issues it found were fixed before this
  landed, not just noted:** the menu handler originally used
  `Manager::get_focused_window`, gated behind Tauri's `unstable` cargo
  feature — enabling that feature turned out to silently change how *every*
  webview in the app gets created (`WebviewKind::WindowChild` instead of
  `WindowContent`, plus manual bounds tracking) and enabled the
  `create_webview` IPC command app-wide, far beyond what the feature name
  suggested for this one call site; replaced with the stable
  `Manager::webview_windows` + `WebviewWindow::is_focused`, which is exactly
  what the unstable method does internally anyway. Separately, `.menu(...)`
  was originally called unconditionally — but Tauri only auto-installs a
  default menu on macOS in the first place, so that would have *added* a
  File/Edit/Window/Help menu bar to Windows/Linux windows that never had
  one; now gated to `#[cfg(target_os = "macos")]` in `main.rs`. A
  lower-severity finding (the desktop picker window has no `CloseRequested`
  handler, so closing it — now more directly reachable via the global ⇧⌘W
  binding — can leave the app running with no visible window) was
  pre-existing, not introduced by this change; filed as BT-3252 and fixed
  there — see "Closing the picker" below.

**Before this ships**, someone with a real Linux/macOS/Windows desktop needs
to: install the Tauri prerequisites (`https://v2.tauri.app/start/prerequisites/`),
run `cargo tauri dev` from this directory, fix whatever `rustc` errors surface
(expect some — nontrivial Tauri API surface written without compiler
feedback almost always needs at least minor fixes), and manually exercise the
picker end-to-end against a real `beamtalk workspace create --background
--persistent` workspace.

## Local development (once you have the Tauri toolchain)

```bash
# Point the broker at a from-source dist-liveview build instead of the
# resource-bundled one CI packages (BT-2987) — simplest for local iteration,
# since it skips a full `cargo tauri build`:
just dist-liveview   # from editors/liveview/, produces bin/server
export BEAMTALK_ATTACH_LAUNCHER=/path/to/dist-liveview/bin/server

cd desktop
cargo tauri dev
```

On Windows (BT-2988), the equivalent points at `bin\bt_attach.bat` instead —
there is no `bin/server` there for the broker to shell out to, so it resolves
`BT_WORKSPACE_NODE`/`BT_WORKSPACE_COOKIE`/`SECRET_KEY_BASE` itself (see the
"Windows-specific gaps" section above):

```powershell
just dist-liveview   # from editors/liveview/, produces bin\bt_attach.bat
$env:BEAMTALK_ATTACH_LAUNCHER = "C:\path\to\dist-liveview\bin\bt_attach.bat"

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

## Closing the picker (BT-3252)

Unlike each per-workspace window — which `commands::attach_and_open_window`
gives its own `on_window_event`/`CloseRequested` handler, wired to
`detach_internal` — the picker (`tauri.conf.json`'s static `"picker"` window)
had no such handler at all before BT-3252. Since the picker is the app's only
always-present window, and there is no tray icon, dock-click handler, or
other affordance to bring it back once gone, closing it (the traffic-light
button, or — since BT-3244 — the app-wide "Close Window" menu item/⇧⌘W, which
targets whichever window has OS focus) just destroyed the window with no
app-level follow-up: if any workspace window was still attached, the app kept
running with no visible window and no way back in short of killing the
process from a terminal.

**Decision: closing the picker is treated as equivalent to quitting the whole
app.** `main.rs`'s `.setup()` registers a `CloseRequested` handler on the
picker window that calls `commands::quit_app` — the exact same
detach-every-tracked-workspace-then-exit path the in-app "Quit" button
(`commands::quit`) and the OS-level-quit `RunEvent::ExitRequested` handler
already used, not a separate/duplicated implementation. Considered and
rejected:

- **Recreate/show the picker on the next single-instance relaunch even with
  no window open.** Would keep the app alive with literally nothing visible
  and no menu-bar/dock affordance most users would think to use to bring it
  back — worse discoverability than just quitting, for no real benefit over
  option (b) below.
- **Hide instead of destroy (macOS "no visible window, dock icon still
  works" pattern), revealed again via a dock-click handler.** The most
  "native-feeling" option, but needs new dock-click-handler and/or tray-icon
  infrastructure this app has none of today, and would leave every attached
  workspace's front process running invisibly forever if the user's actual
  intent in closing the picker was to quit — a real footgun for a feature
  this app doesn't otherwise need.

Quitting is simplest, most predictable (no hidden windows, no surprising
still-running state), needs no new UI affordance, and matches what most
users intuitively expect from closing what looks like the app's main window.
Reopening the picker is always just relaunching the app from the Dock/
Finder/CLI, same as day one.

Tested via `commands::quit_cleanup` (the detach-all-and-flush portion of
`quit_app`, split out from the final `AppHandle::exit` call it can't reach
under a live event loop): `cargo test -p beamtalk-desktop` (well,
`cd desktop/src-tauri && cargo test`, since this crate is deliberately not a
workspace member — see `Cargo.toml`'s header comment) exercises it against
`tauri::test::mock_app()`. The picker's actual `on_window_event` wiring
itself is not (and, given `MockRuntime`'s `on_window_event` is a
never-stores-the-callback stub, cannot be) exercised this way — the same
pre-existing gap `attach_and_open_window`'s own per-workspace handler has.
