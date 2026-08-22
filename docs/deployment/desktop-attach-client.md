# The Beamtalk desktop app (attach client)

A native picker app that discovers workspaces started with `beamtalk workspace
create --background --persistent` and opens a window per attached workspace —
the same LiveView IDE `editors/liveview` serves, without a `localhost:4000`
URL to remember or a terminal to keep open. Design: [ADR
0097](../ADR/0097-desktop-attach-client-node-per-workspace.md). Source:
[`desktop/`](../../desktop/README.md).

> **Local only.** This app attaches to workspaces on the **same machine** it
> runs on — see [Security posture](#security-posture-local-only) below. For
> attaching to a *remote*, OIDC-authenticated LiveView IDE deployment, see
> [Deploying the LiveView IDE for non-localhost
> access](remote-liveview-ide.md) instead; that is a different topology this
> app does not implement (ADR 0097's "Scope boundary" section).

## Installing the app

**Current status:** the CI release lane
(`.github/workflows/desktop-release.yml`) builds Linux/macOS/Windows
installers but is `workflow_dispatch`-only — it is not yet wired into the
project's tagged-release pipeline, and no development sandbox to date has had
a working Tauri/webview toolchain to run it end to end (see
[`desktop/README.md`](../../desktop/README.md)'s "What was and wasn't
verified"). Until that lane is confirmed and wired in, there are two ways to
get the app:

1. **Trigger the release workflow manually** (`gh workflow run
   "Desktop App Release"` or the Actions tab) and download the resulting
   `beamtalk-desktop-<version>-<platform>` artifact — a 7-day-retention CI
   artifact, not (yet) a GitHub Release asset.
2. **Build it yourself**, on a machine with the [Tauri
   prerequisites](https://v2.tauri.app/start/prerequisites/) installed:

   ```bash
   # from the repo root
   cargo install tauri-cli --version "^2.0.0"
   just dist-liveview                # from editors/liveview/, produces dist-liveview/
   just dist-desktop-platform appimage,deb   # Linux — see Justfile for the macOS/Windows bundle-target names
   ```

   The bundle lands in `desktop/src-tauri/target/release/bundle/`.

Either way, the app bundles its own `bt_attach` ERTS release (Elixir/OTP
included) — it needs **no** Elixir/Mix installed to run. It does **not**
bundle the `beamtalk` compiler/CLI toolchain; that stays a separate install
(see the main [README](../../README.md)) because the desktop app *attaches*
to workspaces the CLI starts, it does not run or supervise them (ADR 0097
"What this is NOT").

## First run: no workspaces yet

A fresh install with zero running workspaces is a designed state, not a
silent blank window. The picker shows a "create a workspace" affordance that
shells out to your installed `beamtalk` CLI (`workspace create <name>
--background --persistent`); if the CLI can't be found on `PATH` — common for
a GUI app launched from a dock/Finder, which does not inherit a shell's
`PATH` — it shows setup instructions instead of failing silently. Type a
workspace name and click **Create**, or create one from a terminal first:

```bash
beamtalk workspace create my-project --background --persistent
```

Either way, the new workspace appears in the picker within a few seconds
(the picker polls `~/.beamtalk/workspaces/` periodically).

## Attach / detach

Click a live workspace in the list to attach: the app spawns a dedicated
`bt_attach` process for **that one workspace**, waits for it to confirm it
can actually reach the workspace (not just that its own web server started —
see [Security posture](#security-posture-local-only) for why that distinction
matters), and opens a window once it's ready. **Window-per-workspace**: each
attached workspace gets its own OS window, so if one workspace crashes or is
stopped, only its window goes dark — the others are unaffected (ADR 0097's
crash-isolation property).

Clicking an already-attached workspace focuses its existing window rather
than opening a second one; attaching is otherwise idempotent per workspace.

**Detach** (the button in that workspace's row, or closing its window) kills
that workspace's `bt_attach` process and closes the window. **Quit** detaches
every attached workspace before exiting — no `bt_attach` process is left
running behind a closed app, whether you quit from the app itself or the OS
kills it (Cmd-Q, `SIGTERM`, the taskbar close button all run the same
cleanup). If the app itself is ever killed uncleanly (a crash, `kill -9`),
its **next** launch sweeps and reaps any `bt_attach` process left over from
the previous run — see `desktop/README.md` and
`crates/beamtalk-desktop-broker/src/reap.rs` for the mechanism.

A workspace that goes away while attached (stopped from the CLI, or the BEAM
node crashes) shows as visibly disconnected — a greyed/marked window and a
status badge in the picker — rather than a wall of connection-error noise or
a silently-hung UI. If the workspace comes back (or the machine wakes from
sleep, which drops the connection), the window reconnects.

## Security posture (local only)

The desktop app is a **single-user, on-this-machine** tool, not a network
service — three things it pins itself, none of which the underlying
`bt_attach` release does on its own by default (its production defaults are
shaped for the *remote*, OIDC-authenticated topology in [the remote deployment
guide](remote-liveview-ide.md#the-shape)):

- **Loopback bind.** Every workspace window's web server binds `127.0.0.1`
  only — never `0.0.0.0` — so nothing on your LAN can reach it. This is
  enforced by the app itself (`BT_ATTACH_BIND_IP`), not left to the release's
  own default.
- **No OIDC / no login.** The app refuses to spawn a front at all if it finds
  OIDC configuration (`~/.beamtalk/ide.toml` or `BT_OIDC_*` env) — that
  configuration means *someone* intends the remote, authenticated topology,
  and silently half-enforcing it locally would be worse than refusing
  outright. Locally-attached windows have no login step: reaching the port at
  all is the only "authentication," which is why loopback-only matters.
- **A workspace's cookie is full eval/RCE access to it, same as running the
  CLI locally.** The desktop app does not change that threat model — it's
  the same trust boundary `beamtalk repl`/`just web` already have on your
  machine (ADR 0058, ADR 0091). What the app adds is *process isolation*:
  each attached workspace's `bt_attach` process holds only that one
  workspace's cookie, so a crash or compromise of one attached window's
  process can't reach a *different* attached workspace's cookie (ADR 0097
  Decision).

**On a shared host, loopback is not the same as single-user.** Any other
local account or process on the same machine can reach `http://127.0.0.1:<port>`
while a workspace window is open and eval as that workspace's owner — the
same as any other loopback-bound dev tool. This is the accepted trusted-
developer-tool stance (ADR 0058), not a gap specific to the desktop app; do
not run it, or any Beamtalk workspace, as a genuinely multi-user service.

## Troubleshooting

- **Logs** — the picker's own attach/spawn/readiness stages are logged to
  `~/.beamtalk/launcher.log`, viewable live from within the app itself via
  the header's **Logs** button, or by tailing the file directly. A spawned
  front's own output (Elixir `Logger`/Phoenix, including exceptions and
  endpoint errors) goes to `~/.beamtalk/workspaces/<id>/attach.log` — check
  this first if a workspace window opens but never finishes connecting
  (stuck on "Connecting to workspace…"), since that means the front itself
  is up but something inside it is failing. Both accumulate across restarts
  the same way `workspace.log` does.
- **"Attach failed: unreachable"** (or similar) — the picker surfaces the
  same failure taxonomy the workspace's `/readiness` endpoint reports:
  `epmd_absent` (no local epmd — the workspace's own boot should have started
  one; check `epmd -names`), `bad_cookie` (the on-disk cookie file no longer
  matches what the workspace node is running with), or `dead_workspace` (the
  workspace process isn't there any more — `beamtalk workspace status
  <name>` will confirm). None of these should hang the picker; if attaching
  seems stuck past ~30s, that itself is worth filing as a bug.
- **CLI not found in the empty-state / create-workspace flow** — the desktop
  app was launched from a dock/Finder/Start-menu shortcut, which on most
  platforms does not inherit your shell's `PATH`. Either launch the app from
  a terminal, or create the workspace from a terminal first (`beamtalk
  workspace create … --background --persistent`) and it will appear in the
  picker once created.

## See also

- [ADR 0097](../ADR/0097-desktop-attach-client-node-per-workspace.md) — full
  design and rationale.
- [`desktop/README.md`](../../desktop/README.md) — source layout, packaging
  internals, and current verification status.
- [`desktop/e2e/README.md`](../../desktop/e2e/README.md) — the BT-2989 E2E
  validation this doc's Attach/detach and Security sections were checked
  against.
- [Deploying the LiveView IDE for non-localhost access](remote-liveview-ide.md)
  — the separate, OIDC-authenticated remote topology.
