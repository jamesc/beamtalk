# phoenix_topology — BT-2394 Attach spike (throwaway)

Minimal Phoenix LiveView app proving the **Attach** topology: Phoenix runs as
its own BEAM node and talks to a running Beamtalk workspace over Erlang
distribution (`:rpc` + cross-node messaging). No new wire protocol — it calls
the same Erlang functions `beamtalk_ws_handler` uses.

See the full write-up: [`docs/research/phoenix-topology-spike.md`](../../docs/research/phoenix-topology-spike.md).

## The interesting files

- `lib/bt_attach/workspace.ex` — the entire Attach client (connect, eval, transcript).
- `lib/bt_attach_web/live/workspace_live.ex` — the LiveView page.
- `test/bt_attach_web/workspace_live_test.exs` — end-to-end test vs a real workspace.

Everything else is stock `mix phx.new` scaffold (no ecto/mailer/dashboard/
gettext/tailwind/esbuild). The Phoenix + LiveView JS is vendored as prebuilt
`*.min.js` into `priv/static/assets/` so no node/esbuild is needed.

## Run

```bash
just build                                   # from repo root: build Beamtalk on OTP 27
./target/debug/beamtalk workspace create spike --background --persistent
export BT_WORKSPACE_NODE=beamtalk_workspace_spike@localhost
export BT_WORKSPACE_COOKIE=$(sed 's/-setcookie //;s/ //g' ~/.beamtalk/workspaces/spike/vm.args)

cd spikes/phoenix_topology
mix deps.get
mix test                                     # or: mix phx.server  (http://localhost:4000)
```

Requires **Elixir ≥ 1.17 built for OTP 27** — not the distro `elixir` package,
which pins `erlang < 26` and conflicts with Beamtalk's OTP 27.
