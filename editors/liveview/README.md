# editors/liveview — Beamtalk LiveView IDE

A Phoenix LiveView web IDE for Beamtalk, using the **Attach** topology: Phoenix
runs as its own BEAM node and talks to a running Beamtalk workspace over Erlang
distribution (`:rpc` + cross-node messaging). There is no new wire protocol — it
calls the same Erlang functions `beamtalk_ws_handler` uses.

This is the productionised successor to the BT-2394 spike
(`spikes/phoenix_topology`). Background and topology trade-offs are in
[`docs/research/phoenix-topology-spike.md`](../../docs/research/phoenix-topology-spike.md).

## The interesting files

- `lib/bt_attach/workspace.ex` — the entire Attach client. `eval/2` dispatches
  through the BT-2399 term-returning op layer (`beamtalk_repl_ops:dispatch/4`)
  and returns the live Erlang term, not JSON; `render_term/1` reuses the
  workspace's own formatter so display matches the Phase-1 browser; Transcript
  is subscribed through the `beamtalk_repl_subscriptions` facade (no direct
  `{subscribe, self()}` cast at gen_servers).
- `lib/bt_attach_web/live/workspace_live.ex` — the two-pane LiveView (Workspace +
  live Transcript). Each mount gets its own workspace-supervised session;
  eval state persists across evals within the session.
- `test/bt_attach_web/workspace_live_test.exs` — `:workspace`-tagged end-to-end
  tests against a real workspace (eval round-trip, session persistence, live
  Transcript push).

## Toolchain

Erlang and Elixir versions are pinned in the repo-root `.tool-versions` — the
single source of truth shared by mise (dev machines, devcontainer) and CI's
`setup-beam`. Never use the distro `elixir` package, which pins an old
`erlang`. With [mise](https://mise.jdx.dev) (or asdf) installed:

```bash
mise install        # from the repo root; reads .tool-versions
```

The devcontainer (`.devcontainer/Dockerfile`) installs the same pinned
toolchain automatically; `scripts/setup-cloud.sh` installs a working (older)
toolchain for cloud sessions.

Assets are built by a real **esbuild + tailwind** pipeline (no vendored
`*.min.js`). The standalone binaries are downloaded on first use by
`mix assets.setup`, which also runs `npm ci` to fetch the JS bundle's npm
dependency (CodeMirror 6, which powers the code editor — BT-2538). **Node is a
build-time requirement** (provisioned via the repo-root `.tool-versions` / mise);
esbuild bundles CodeMirror straight into `app.js`, so the **shipped release
stays npm-free** — only the build needs node. `phoenix`, `phoenix_html`, and
`phoenix_live_view` JavaScript are still resolved from the Mix deps directory by
esbuild via `NODE_PATH`, version-locked to the hex packages.

The editor highlights Beamtalk with a small regex tokenizer
(`assets/js/hooks/bt_highlight.js`) that paints CodeMirror decorations using the
themed `.tok-*` classes. (An earlier revision ran the VS Code TextMate grammar
via oniguruma WASM; that added ~250 KB gzipped — more than CodeMirror itself —
which was disproportionate for these small buffers. If structural features like
folding or smart indent are ever needed, the CodeMirror-native path is a Lezer
grammar, not WASM.)

## Setup

```bash
# from the repo root
just web-setup                  # mix deps.get (routes via the hex bridge in cloud sessions)

cd editors/liveview
mix assets.setup                # download the esbuild + tailwind binaries (once)
mix assets.build                # bundle JS + CSS into priv/static/assets/
```

## Run

There are three ways to run the IDE, by audience:

| Way | For | Where |
| --- | --- | --- |
| **From source** (`just web` / `just web-remote`) | contributors hacking on the IDE | this section |
| **Release archive** (`beamtalk-ide-<version>-<platform>.tar.gz`) | users who want a self-contained build, no Elixir/Mix | [deployment guide](../../docs/deployment/remote-liveview-ide.md#installing-the-ide) |
| **Docker** (`ghcr.io/jamesc/beamtalk-ide`) | remote / operator deployments (OIDC) | [deployment guide](../../docs/deployment/remote-liveview-ide.md#run-with-docker) |

The archive and image are produced by the IDE's own release lane
(`.github/workflows/liveview-release.yml` and `liveview-docker.yml`); both run the
same release as `just dist-liveview`. The release ships a `bin/server` launcher:
`bin/server <workspace-id>` resolves the workspace's node + cookie from
`~/.beamtalk/workspaces/` exactly like `just web` does (and generates an
ephemeral `SECRET_KEY_BASE` when unset). The rest of this section is the
from-source path.

```bash
# from the repo root: build Beamtalk on OTP 27 and start a workspace
just build
./target/debug/beamtalk workspace create demo --background --persistent

# discover the workspace node + cookie, export them, and start Phoenix on :4000
just web demo
```

`just web <name>` resolves the workspace node via `beamtalk workspace status`,
reads its cookie from `~/.beamtalk/workspaces/<id>/cookie`, exports
`BT_WORKSPACE_NODE` / `BT_WORKSPACE_COOKIE`, then runs `mix phx.server`
(http://localhost:4000).

To run it by hand instead:

```bash
export BT_WORKSPACE_NODE=beamtalk_workspace_demo@localhost
export BT_WORKSPACE_COOKIE=$(cat ~/.beamtalk/workspaces/demo/cookie)
mix phx.server
```

## Test

```bash
mix test
```

The workspace-attach tests are tagged `:workspace` and are **excluded
automatically** unless `BT_WORKSPACE_COOKIE` is set, so a bare `mix test` passes
without a running workspace. To run them, start a workspace and export the node
+ cookie (as in *Run* above) first.

## Browser e2e (Playwright)

`Phoenix.LiveViewTest` renders the LiveView server-side and **never loads
`app.js`**, so the cockpit's client-side JS hooks — `CmEditor` (BT-2538/BT-2539),
`KeyboardShortcuts` (BT-2485) and `TweaksPanel` (BT-2487) —
are invisible to it. The browser e2e suite
(`test/bt_attach_web/workspace_browser_test.exs`) closes that gap:
[PhoenixTest](https://hexdocs.pm/phoenix_test) driving a **real Chromium** via
[Playwright](https://hexdocs.pm/phoenix_test_playwright) against the connected
IDE.

The interesting UI lives behind the *connected* render, which only appears once
the LiveView attaches to a workspace, so these tests are **double-gated**: tagged
both `:workspace` (need a running workspace + cookie) and `:playwright` (need the
browser install + `PHX_PLAYWRIGHT=1`). A bare `mix test` excludes both; the
dedicated `liveview-e2e.yml` CI lane provisions a workspace + Chromium and runs
them.

To run locally (from the repo root):

```bash
just build
./target/debug/beamtalk workspace create e2e --background --persistent

cd editors/liveview
mix deps.get
mix assets.build                       # app.js must exist for the LiveSocket
npm --prefix assets install            # the Playwright CLI
npx --prefix assets playwright install chromium --with-deps

export BT_WORKSPACE_NODE=beamtalk_workspace_e2e@localhost
export BT_WORKSPACE_COOKIE=$(cat ~/.beamtalk/workspaces/e2e/cookie)
export PHX_PLAYWRIGHT=1
mix test --only playwright
```

OIDC is unconfigured in test, so the route is open and the LiveView attaches as
the `:owner` role — every hook + the eval form is in scope. Do **not** set
`BT_IDE_DEV_AUTH` for the e2e run: it switches on the loopback dev-auth gate,
which the headless browser does not satisfy, leaving the IDE on the
"connecting…" screen.
