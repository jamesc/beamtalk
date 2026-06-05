# editors/liveview — Beamtalk LiveView IDE

A Phoenix LiveView web IDE for Beamtalk, using the **Attach** topology: Phoenix
runs as its own BEAM node and talks to a running Beamtalk workspace over Erlang
distribution (`:rpc` + cross-node messaging). There is no new wire protocol — it
calls the same Erlang functions `beamtalk_ws_handler` uses.

This is the productionised successor to the BT-2394 spike
(`spikes/phoenix_topology`). Background and topology trade-offs are in
[`docs/research/phoenix-topology-spike.md`](../../docs/research/phoenix-topology-spike.md).

## The interesting files

- `lib/bt_attach/workspace.ex` — the entire Attach client (connect, eval, transcript).
- `lib/bt_attach_web/live/workspace_live.ex` — the LiveView page.
- `test/bt_attach_web/workspace_live_test.exs` — end-to-end test against a real workspace.

## Toolchain

Requires **Elixir ≥ 1.17 built for OTP 27** — *not* the distro `elixir`
package, which pins `erlang < 26` and conflicts with Beamtalk's OTP 27. Use a
precompiled release or asdf:

```bash
asdf install elixir 1.18.4-otp-27   # or download elixir-otp-27.zip
```

The devcontainer (`.devcontainer/Dockerfile`) and `scripts/setup-cloud.sh`
install a matching Elixir automatically.

Assets are built by a real **esbuild + tailwind** pipeline (no vendored
`*.min.js`). The standalone binaries are downloaded on first use by
`mix assets.setup`; npm is not required. `phoenix`, `phoenix_html`, and
`phoenix_live_view` JavaScript are resolved from the Mix deps directory by
esbuild via `NODE_PATH`, so they stay version-locked to the hex packages.

## Setup

```bash
# from the repo root
just web-setup                  # mix deps.get (routes via the hex bridge in cloud sessions)

cd editors/liveview
mix assets.setup                # download the esbuild + tailwind binaries (once)
mix assets.build                # bundle JS + CSS into priv/static/assets/
```

## Run

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
