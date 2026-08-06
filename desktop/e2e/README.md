# Desktop attach client — E2E validation (BT-2989)

ADR 0097 Phase 5. Per project convention, the last issue in an epic needs an
end-to-end test exercising the *real* flow — not just the unit/component
tests BT-2985/2986/2987/2988 already added — plus the user-facing docs a new
adopter needs (`docs/deployment/desktop-attach-client.md`).

## What this covers, and why it isn't "launch the actual Tauri app"

The literal ADR flow is: `beamtalk workspace create` → launch the desktop app
→ attach → confirm the window loads and an eval round-trips through the
LiveView UI → detach → confirm the front process exits, plus a negative path
(dead/nonexistent workspace surfaces the `/readiness` failure taxonomy rather
than hanging or crashing).

Every session that has touched `desktop/src-tauri` so far (BT-2986, BT-2987,
BT-2988 — see `desktop/README.md`'s "What was and wasn't verified") has done
so **without** a working Tauri toolchain: no display server, no `cargo-tauri`
CLI, no `webkit2gtk`/`glib`/`gtk3` dev packages, and — new finding from this
session, on an otherwise-capable Windows sandbox — a `mix release`'s
`assets.deploy` step failing to spawn `npm.cmd` via Erlang's `open_port`
(`:eacces`, reproducible outside any project sandboxing) before `cargo tauri
build` is even reached, blocking `dist-liveview/` (the resource `desktop/
src-tauri/tauri.conf.json` bundles) from being built here at all. So: no
compiled `beamtalk-desktop` binary has existed in any development sandbox to
date. This E2E suite validates everything that **is** independently
verifiable without one, split across two test surfaces:

1. **The broker's real process lifecycle** —
   `crates/beamtalk-desktop-broker/tests/live_front.rs`'s
   `detach_kills_the_front_and_it_exits_cleanly` and
   `dead_workspace_readiness_resolves_to_dead_workspace_not_a_hang` (added by
   BT-2989) call the *exact* library functions
   `desktop/src-tauri/src/commands.rs`'s `attach`/`detach` Tauri commands call
   (`spawn_front_with_port_retry`, `wait_ready`, `Child::kill`/`.wait()`) —
   the Tauri command handlers are thin wrappers over this crate, so this is a
   faithful stand-in for "the desktop app attaches/detaches" minus the actual
   window chrome. Run those via `cargo test -p beamtalk-desktop-broker --test
   live_front -- --ignored --test-threads=1` (see that file's module doc for
   full setup).
2. **The eval-round-trips-through-the-UI half** — `attach-cycle.sh` +
   `eval-roundtrip.mjs` here. A workspace window's content is *exactly* the
   LiveView IDE page (`http://127.0.0.1:<port>/`, per ADR 0097's Decision —
   "a window loads `http://localhost:<port>`") — the same page
   `editors/liveview/test/bt_attach_web/workspace_browser_test.exs` already
   validates in a real Chromium via Playwright, just normally reached through
   `mix phx.server` (dev mode) rather than the packaged `bin/server` release
   with the desktop broker's specific env-var contract
   (`BT_ATTACH_BIND_IP`/`BT_ATTACH_NODE_SUFFIX`/ephemeral `PORT`,
   `crates/beamtalk-desktop-broker/src/spawn.rs`'s `build_env`). `attach-
   cycle.sh` spawns the front with that exact contract (`PORT=<port>
   BT_ATTACH_BIND_IP=127.0.0.1 BT_ATTACH_NODE_SUFFIX=<id>_<pid>
   RELEASE_DISTRIBUTION=none bin/server <id>` — copied from `spawn.rs`'s own
   module doc, not re-derived), polls `/readiness` the way the broker does,
   then drives `eval-roundtrip.mjs` (raw Playwright, reusing the browser
   dependency `editors/liveview/assets/package.json` already declares) at the
   real URL to confirm an eval genuinely round-trips through the connected
   LiveView page, then kills the front and confirms the OS process exits.

Neither of these opens a **Tauri window** or clicks the **picker UI**
(`desktop/ui/main.js`) — that would need the compiled binary this session
still can't produce. What *is* exercised, end to end, is every piece of logic
between the workspace and the browser-visible page: workspace creation, the
broker's exact spawn contract, the two-stage readiness probe, a real eval
over the LiveView socket, and process teardown. The picker-UI-specific
wiring (`commands.rs`'s `Err(format!(...))` → `ui/main.js`'s
`statusEl.textContent`) is covered by reading the source (see `commands.rs`'s
and `main.js`'s doc comments) and by `beamtalk-desktop-shell`'s own unit
suite (`just test`), not re-verified against a live webview here.
**Before shipping**, someone with a real desktop still needs to do what
`desktop/README.md`'s "Before this ships" section already says: install the
Tauri prerequisites, `cargo tauri dev`, and manually exercise the picker.

## Prerequisites

```bash
just build                                                  # beamtalk CLI
just dist-liveview                                          # from editors/liveview/, produces dist-liveview/bin/server
cd editors/liveview
npm --prefix assets install                                 # installs the `playwright` devDependency
npx --prefix assets playwright install chromium --with-deps
cd ../..
```

## Running

```bash
BEAMTALK_BIN="$PWD/target/debug/beamtalk" \
BT_ATTACH_LAUNCHER="$PWD/dist-liveview/bin/server" \
  desktop/e2e/attach-cycle.sh
```

Optional: set `BT_E2E_WORKSPACE_ID` to change the throwaway workspace name
(default `bt2989_desktop_e2e`) if you need to avoid a collision with an
existing workspace.

The script is `bash`, POSIX-only (matching `bin/server` itself, per ADR 0097
Implementation §5b — Windows has no `bin/server` for the broker to invoke
this way; BT-2988's Windows path resolves everything itself and calls
`bin\bt_attach.bat start` directly, which this script does not attempt to
reproduce). Run it on Linux or macOS (or under CI's `ubuntu-latest`/
`macos-latest` runners, same as `liveview-e2e.yml`).

It exits non-zero on the first failed step, with a message naming which
acceptance-criterion clause failed (readiness never went Ready, eval result
didn't match, process didn't exit, dead-workspace probe hung past budget or
returned the wrong reason). It always attempts a best-effort cleanup (kill
any front it spawned, stop the workspace it created) even on failure.
