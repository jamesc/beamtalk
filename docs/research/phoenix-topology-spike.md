# Phoenix LiveView Topology Spike — Embed vs Attach vs Separate

**Issue:** BT-2394
**Date:** 2026-06-04
**Status:** Complete — recommendation: **proceed with Attach**
**Throwaway code:** `spikes/phoenix_topology/` (this branch, not merged to main)

## TL;DR

The **Attach** topology (Livebook-style: Phoenix runs as its own BEAM node and
connects to the workspace node over Erlang distribution) works end-to-end with
**zero changes to the Beamtalk runtime**. All four critical-path checks pass —
both at the raw-distribution level and through a real Phoenix LiveView:

| Check | Result |
| -- | -- |
| Connect to running workspace (dist + shared cookie) | ✅ `net_adm:ping → pong` |
| Eval round-trip (`evaluate` op) | ✅ `3 + 4 → 7` via `:rpc` |
| Transcript stream (live push) | ✅ `Transcript show:` text pushed cross-node to the LiveView |
| Hard thing — two tabs, isolated sessions | ✅ tab1 `x=100`, tab2 `x=999` |

**Recommendation: proceed with Attach.** No pivot needed. The architectural
substrate the spike worried about (a distributed, cookie-protected workspace
node with RPC-reachable eval + Transcript) *already exists* in the shipped CLI.

## Which topology was tried

**Attach**, the recommended starting point. It was chosen because the
distribution substrate already exists: `beamtalk workspace create` launches the
workspace as a **named, distributed BEAM node** with a per-workspace cookie:

- Node name: `beamtalk_workspace_<name>@localhost`
  (`crates/beamtalk-cli/src/commands/workspace/startup_command.rs` — `-name`/`-sname`)
- Cookie: written to `~/.beamtalk/workspaces/<id>/vm.args` as `-setcookie <token>`,
  mode 0600 (BT-726 — kept out of `ps`/`/proc/.../cmdline`)
- Registered with epmd (`epmd -names` shows `beamtalk_workspace_<name>`)

So "attach" is not a new capability — it is the same thing the CLI's own
`beamtalk repl --workspace` and `beamtalk workspace attach` rely on, minus the
fact that those clients happen to use the TCP REPL protocol rather than dist RPC.

## What worked end-to-end

The Phoenix side needs exactly **three** primitives, all of which reuse the
same Erlang functions `beamtalk_ws_handler` already calls — no new wire protocol:

```elixir
# 1. Join the workspace's distribution mesh with the shared cookie.
Node.set_cookie(node, :"<cookie>")
true = Node.connect(node)

# 2. Eval — create a workspace-supervised session, then evaluate.
{:ok, pid}  = :rpc.call(node, :beamtalk_session_sup, :start_session, [session_id])
{:ok, 7, _out, _warn} = :rpc.call(node, :beamtalk_repl_shell, :eval, [pid, ~c"3 + 4"])

# 3. Transcript — subscribe THIS process; pushes arrive natively over dist.
:gen_server.cast({:Transcript, node}, {:subscribe, self()})
# ...later, in the LiveView: handle_info({:transcript_output, text}, socket)
```

Key elegance: because pids and messages are **location-transparent** across
Erlang distribution, the LiveView process subscribes its *own* pid to the
workspace's `Transcript` gen_server and receives `{:transcript_output, Text}`
pushes directly — no polling, no bridge process, no second protocol.

Result formatting reuses the workspace's own `beamtalk_repl_json:format_response/1`
(the same formatter the browser WebSocket protocol uses), so the LiveView shows
byte-identical values to the existing Phase-1 browser. Surface-consistent for free.

### Proof artifacts

- `spikes/phoenix_topology/lib/bt_attach/workspace.ex` — the entire Attach client (~120 LOC).
- `spikes/phoenix_topology/lib/bt_attach_web/live/workspace_live.ex` — minimal LiveView (input → eval → render; live Transcript pane).
- `spikes/phoenix_topology/test/bt_attach_web/workspace_live_test.exs` — two passing tests driving the **real** Phoenix LiveView stack against a **real** running workspace:
  - `eval round-trip renders the workspace result` → asserts `7`
  - `Transcript output streams live into the LiveView` → asserts a `Transcript show:` marker appears

```text
$ mix test test/bt_attach_web/workspace_live_test.exs
2 tests, 0 failures
```

A standalone raw-distribution proof (plain Erlang node, no Phoenix) additionally
confirmed two-tab session isolation: `tab1 x → 100`, `tab2 x → 999`.

## What surprised

1. **Nothing in the runtime needed to change.** The spike budgeted for a "hard
   thing" (distribution security, cookie management, hot-reload conflict). None
   materialised, because the workspace is *already* a distributed node and the
   eval/Transcript functions are *already* plain module functions reachable by RPC.

2. **Toolchain version coupling is the real friction, not the topology.**
   Beamtalk runs on **OTP 27** (the runtime uses EEP-59 `-doc` attributes, which
   do not compile on OTP 25). Ubuntu's packaged Elixir (1.14) **pins
   `erlang < 1:26`** and will *uninstall* an OTP-27 install to satisfy that
   dependency. The working combination was a precompiled **Elixir 1.17.3 built
   for OTP 27** (`elixir-otp-27.zip`) plus `esl-erlang` 27. Distribution between
   the Phoenix node and the workspace node both on OTP 27 was clean. (Erlang
   distribution tolerates ±2 majors, so an OTP-25 Phoenix node *could* in
   principle attach to an OTP-27 workspace, but matching majors is the sane path.)

3. **Attach grants the Phoenix node the *entire* BEAM, not just `evaluate`.**
   A connected node can `:rpc.call/4` *any* function on the workspace — this is
   far more power than the Phase-1 WebSocket protocol's narrow op set. That is
   convenient for the IDE (read-surface ADR 0085 can call navigation/reflection
   functions directly) but it is also the central security consequence (below).

## Recommendation

**Proceed with Attach.** It is the lowest-friction path, it matches the implicit
architecture of ADR 0017, and it makes the read-surface (ADR 0085) and
write-surface (ADR 0082) trivially reachable — they are just more module
functions on a node we are already connected to. Do **not** pivot.

Embed and Separate were not built. Reasoning recorded for the decomposition:

- **Embed** would force Phoenix and the workspace into one VM, where Phoenix's
  dev code reloader and Beamtalk's class hot-patch (ADR 0082) share a code
  server and can fight. Attach keeps them on separate nodes — structurally no
  conflict. Embed also couples release/versioning rigidly.
- **Separate** (WebSocket-only) keeps the narrow protocol surface (a security
  plus) but would force the IDE to re-expose every read/write op as a WS op,
  duplicating what RPC gives for free. Reasonable fallback *if* the security
  surface of Attach proves unacceptable for a given deployment.

## Build-chain implications

What integrating this with the main repo would look like:

- **Repo layout:** a Mix project as a sibling toolchain, mirroring how
  `editors/vscode` (TypeScript/npm) already lives in-repo. Candidate location
  `editors/liveview/` or a top-level `web/`. A **separate repo** is viable under
  Attach (the Phoenix app only needs a node name + cookie, never compiles
  against Beamtalk source) but the monorepo subdir matches existing precedent
  and keeps the LiveView versioned in lockstep with the eval/Transcript surface
  it calls. Recommend monorepo subdir; revisit only if Mix/rebar3 release cadence
  diverges.
- **`just` recipes:** `just web` → discover the running workspace
  (`beamtalk workspace status <name>`), export `BT_WORKSPACE_NODE` +
  `BT_WORKSPACE_COOKIE` from `~/.beamtalk/workspaces/<id>/vm.args`, then
  `mix phx.server`. A `just web-setup` would handle `mix deps.get` via the
  hex bridge.
- **Toolchain pin:** document/pin **Elixir ≥ 1.17 built for OTP 27**. Do *not*
  rely on the distro Elixir package (OTP-version conflict above). asdf or a
  precompiled release is the supported route. This belongs in a future
  "Elixir + Mix integration" ADR.
- **Asset pipeline:** the spike deliberately avoided esbuild/tailwind/npm by
  vendoring the prebuilt `phoenix.min.js` + `phoenix_live_view.min.js` from the
  hex deps into `priv/static/assets/` and connecting `LiveSocket` from a plain
  classic `<script>`. A production IDE will want the real esbuild pipeline; note
  that esbuild's binary download is a separate network dependency to budget for.

## Auth implications

- **Localhost (today):** the Erlang distribution cookie *is* the auth boundary.
  The workspace cookie file is already 0600 and the node binds locally; a
  Phoenix node on the same host reads the cookie and attaches. No new auth code.
- **Past localhost:** two distinct concerns, both real:
  1. **Distribution exposure.** Plain Erlang distribution sends the cookie in a
     challenge/response but the transport is unencrypted and epmd is a network
     service. Remote Attach **must** use TLS distribution (`inet_tls_dist`) or a
     tunnel, per ADR 0020. This is heavier than securing a single WebSocket.
  2. **Capability scope.** Because a connected node can RPC *any* function,
     authentication is all-or-nothing — there is no per-op authorization at the
     dist layer. If the IDE is exposed to less-trusted clients, the Separate
     (WebSocket) topology's narrow op surface becomes attractive *for that edge*,
     even if Attach remains the internal model. Worth an explicit decision in the
     auth ADR: do remote browsers talk to a *trusted* Phoenix node that attaches
     (browser↔Phoenix over HTTPS/auth, Phoenix↔workspace over TLS-dist), rather
     than ever attaching the browser's trust boundary directly to dist? The
     spike suggests **yes** — Phoenix as the authenticated front, dist behind it.

## Hot-reload implications

- Phoenix's dev code reloader operates on the **Phoenix node's** modules only.
  Beamtalk's hot-patch (compile/flush/ChangeLog, ADR 0082) operates on the
  **workspace node's** modules. Because Attach keeps these in *separate* VMs with
  separate code servers, the two reload mechanisms **do not interact** — a clear
  win over Embed, where they would share one code server.
- Editing a Beamtalk *class* from the LiveView is just another `evaluate`/
  `load-source` RPC against the workspace; the result of a subsequent eval
  reflects the new code with no Phoenix-side reload at all. (Exercised
  indirectly via session-bound eval state; a dedicated class-edit→re-eval demo
  is left to the LiveView IDE epic.)

## API shape: return terms, encode JSON only at the WebSocket edge

The spike surfaced a clear recommendation for how the LiveView IDE should talk
to the workspace. Two thin layers, and one explicit decision:

1. **Elixir client module (thin, necessary).** One module (`BtAttach.Workspace`
   in the spike, ~120 LOC) owns node name / cookie / `connect`, and exposes
   idiomatic functions. LiveViews never call `:rpc` directly.

2. **Erlang contract: return terms, not JSON.** The existing curated op layer —
   `beamtalk_repl_server:handle_protocol_request/2` → `handle_op/4` (protocol
   2.0, already used by the browser and *explicitly built for "runtime-attached
   LSP / MCP clients"*) — is the right **dispatch** surface to reuse. But today
   it is also the **JSON boundary**: `beamtalk_repl_ops_eval:handle/4` calls the
   term-returning core (`beamtalk_repl_shell:eval/2 → {ok, Term, Output, Warnings}`)
   and then immediately `beamtalk_repl_json:encode_result/_`.

   Over Erlang distribution, JSON is both overhead and **lossy in the way that
   matters**: `Counter spawn` is a live term —
   `{beamtalk_object, 'Counter', bt@counter, <0.369.0>}` carrying a **real,
   messageable remote pid** — that JSON flattens to the dead string
   `"#Actor<Counter,…>"`. An inspector, "send a message to this object", or
   reference-following all need the term, not its `printString`. That liveness
   is the whole point of a live-image IDE.

   **Decision: the Attach path returns terms.** The op layer should be factored
   so dispatch/validation/error-wrapping returns a structured term
   (`{ok, Value, Output, Warnings} | {error, #beamtalk_error{}}`), and JSON
   encoding moves to the **WebSocket transport edge only**. Then:
     * Browser (WS): term → JSON at the boundary (unchanged on the wire).
     * Phoenix (Attach), LSP, MCP: terms pass through natively.

   The one condition: "terms" must be a *documented, stable* contract (the
   result tuple + the `#beamtalk_error{}` record — Phoenix matches on the
   `:beamtalk_error` tag, no shared `.hrl` needed), not raw internal shapes —
   otherwise it just trades JSON-coupling for internal-shape-coupling.

3. **One genuine gap: push-stream subscription.** `handle_op` covers
   request/response (eval, load-source, inspect, …) but **not** the live push
   streams (Transcript / actors / classes / bindings / flush), which today only
   `beamtalk_ws_handler` wires up by calling `beamtalk_transcript_stream:subscribe/1`
   etc. directly. A dist-attached client needs a small, stable **subscription
   facade** rather than casting `{subscribe, self()}` tuples at gen_servers
   (which is what this spike does as a shortcut).

This "extract a term-returning op layer; JSON only at the WS edge; add a
subscription facade" work is the natural first issue of the LiveView IDE epic.

## What this unblocks

- LiveView IDE epic decomposition can now commit to **Attach**.
- A follow-up **"Elixir + Mix build-chain" ADR** (toolchain pin, repo layout,
  `just` recipes, asset pipeline).
- A follow-up **auth ADR** point: Phoenix-as-authenticated-front + TLS
  distribution behind it; never expose raw dist to the browser trust boundary.
- ADR 0017 Phase 3 can move from "deferred" to "in progress".

## How to reproduce

```bash
# 1. Build Beamtalk (Rust + OTP 27 runtime + stdlib)
just build

# 2. Start a distributed workspace
./target/debug/beamtalk workspace create spike --background --persistent

# 3. Point the Phoenix app at it
export BT_WORKSPACE_NODE=beamtalk_workspace_spike@localhost
export BT_WORKSPACE_COOKIE=$(sed 's/-setcookie //;s/ //g' ~/.beamtalk/workspaces/spike/vm.args)

# 4. Run the end-to-end LiveView test (or `mix phx.server` and open the page)
cd spikes/phoenix_topology
mix deps.get          # via hex bridge: HEX_MIRROR=http://127.0.0.1:18081
mix test test/bt_attach_web/workspace_live_test.exs
```

Requires Elixir ≥ 1.17 built for OTP 27 (not the distro package — see Build-chain).
