# ADR 0097: Desktop Attach Client — One Front Node per Workspace

## Status
Proposed (2026-06-13)

Builds on [ADR 0091 — Connection Security for Remote Workspace Access](0091-remote-workspace-access-phoenix-authenticated-front.md)
(the Attach topology and its cookie boundary) and
[ADR 0017 — Browser Connectivity to Running Workspaces](0017-browser-connectivity-to-running-workspaces.md)
(the LiveView IDE, Phase 3). Reuses the `bin/server` launcher and the
boot-time global-env attach model already shipped for the `bt_attach` release.

## Context

### Problem statement

The LiveView IDE (`editors/liveview`, the `bt_attach` Phoenix app) is a web UI
that attaches to a running Beamtalk workspace over Erlang distribution. We want
to ship it as a **desktop application** — a native window the user opens from
their dock, not a `localhost:4000` URL they have to remember to start by hand.

The shaping discussion converged on two constraints that narrow the design
sharply:

1. **Connect to live workspaces; do not spawn the Rust toolchain.** The desktop
   app should *attach* to workspaces the user has already started
   (`beamtalk workspace create … --background --persistent`), discovered from
   `~/.beamtalk/workspaces/`. It is a client, not a process manager for the
   language runtime. This removes cross-platform bundling and lifecycle
   supervision of the Rust `beamtalk` binary — the most painful packaging slice.

2. **Multiple workspaces, switchable at runtime.** A developer typically has
   more than one workspace alive. The desktop app's reason to exist (over just
   pointing a browser at the front) is precisely the *connection-broker* UX:
   discover live workspaces, attach to one (or several), and manage those
   connections natively.

The crux is **how one desktop process attaches to N workspaces** given how the
attach client is built today.

### Current state

The `bt_attach` front is hardwired to a **single** workspace, fixed at boot:

| Mechanism | Where | Behaviour |
|-----------|-------|-----------|
| Target node | `workspace.ex:54` (`node_name/0`) | reads `BT_WORKSPACE_NODE` env **once**, module-global |
| Cookie | `workspace.ex` `set_cookie/0` | reads `BT_WORKSPACE_COOKIE` env, sets the node-global distribution cookie |
| Every RPC | `workspace.ex:966` | `:rpc.call(node_name(), …)` — always the one global target |
| Launcher | `rel/overlays/bin/server <id>` | resolves `node_name` from `~/.beamtalk/workspaces/<id>/metadata.json` + the sibling `cookie` file, exports the two env vars, runs `bin/bt_attach start` |

So today: **one BEAM node, one cookie, one workspace, for the life of the
process.** A BEAM node has exactly one distribution cookie; attaching to two
workspaces with *different* cookies from a single node is the awkward case.

The release is already self-contained (ERTS-embedded `mix release`), and
`PORT` / `PHX_HOST` pass through `config/runtime.exs`. Discovery data
(`metadata.json` + `cookie`) already lives on disk in a stable layout.

### Constraints

- **Don't regress `workspace.ex`.** It is the attach client shared with the
  from-source `just web` flow; the single-target model works and is tested.
- **Respect the cookie boundary (ADR 0091/0058).** A valid cookie is full RCE
  as the workspace owner. Different workspaces may carry different cookies; the
  design must not weaken or co-mingle them.
- **One shipped artifact.** The desktop app should bundle only the `bt_attach`
  release; the Rust `beamtalk` toolchain is the user's responsibility and is
  assumed already running.

## Decision

Ship a thin **desktop shell** whose main process acts as a **connection
broker**: for each workspace the user attaches to, it spawns **one dedicated
`bt_attach` BEAM node**, bound to that workspace via the existing boot-time
env model, on its own HTTP port, and points a window at it. Disconnecting kills
that node's OS process.

Concretely, attaching to workspace `<id>` is:

```
RELEASE_NODE=bt_attach_<id> PORT=<free-port> bin/server <id>
```

- `bin/server <id>` already resolves the node + cookie from
  `~/.beamtalk/workspaces/<id>/` and boots a fully-attached front. **No change
  to `workspace.ex`.**
- `RELEASE_NODE=bt_attach_<id>` gives each spawned BEAM a unique distribution
  node name (Mix releases otherwise default every instance to `bt_attach`,
  which would collide). This is an env override, not a code change.
- `PORT=<free-port>` is chosen by the broker per instance; the window loads
  `http://localhost:<free-port>`.

Each node holds exactly **one** workspace's cookie — so the multi-cookie
distribution boundary never arises. Cookie isolation, crash isolation, and the
unchanged single-target `workspace.ex` all fall out of "one node per
workspace."

### Broker responsibilities (desktop main process)

1. **Discover** — enumerate `~/.beamtalk/workspaces/*/metadata.json`; liveness
   via `beamtalk workspace status` (or a dist ping).
2. **Attach** — pick a free port, spawn the node as above, readiness-probe the
   port, then open a `BrowserWindow` at `localhost:<port>`.
3. **Detach / quit** — terminate that node's child process; the window closes.

### What this is NOT

- It does **not** spawn or supervise the Rust `beamtalk` workspace runtime.
- It does **not** introduce a new wire protocol — the front still speaks the
  same `:rpc` it does today.
- It does **not** add per-session node targeting to `workspace.ex` (the
  rejected alternative below).

### Decided sub-decisions

- **Shell: Tauri.** The desktop shell is a **Tauri (Rust) app**, not Electron.
  Two reasons. First, **language coherence**: the Beamtalk compiler/CLI is
  already Rust, so a Rust shell keeps the toolchain in one language — the broker
  logic (discovery, spawn, port allocation, child reaping) is ordinary Rust
  process handling, reviewable by the same people who own `beamtalk-cli`.
  Second, **footprint**: we already ship one ERTS per connection, so the
  ~100 MB Electron runtime would be pure overhead on a binary we can't shrink;
  Tauri's system-webview model avoids that. The shell does very little
  (spawn → probe → window), so Tauri's thinner JS ecosystem is not a cost here.

### Open sub-decisions (deferred to implementation spike)

- **Window model: window-per-workspace vs tabbed.** Recommendation:
  **window-per-workspace**. It falls straight out of one-BEAM-per-workspace and
  keeps crash isolation visible to the user (a dead workspace = one greyed
  window, not a dead tab in a shared shell).

## Prior Art

Desktop wrappers over a local server process are well-trodden; the BEAM-native
angle is the interesting part.

- **Livebook (Elixir).** The closest peer: a Phoenix/LiveView app distributed as
  a desktop build (`livebook` Mac/Windows app) that wraps the *same* web server
  the CLI runs. Livebook's desktop shell launches the BEAM and opens a webview
  at the local port — exactly the shell-over-release shape here. Difference:
  Livebook *embeds* its runtime; we deliberately attach to an *external*
  workspace, so our shell spawns the **front**, not the language runtime.
  Livebook also historically ran **one server, many notebooks** (akin to the
  rejected single-node alternative); it can afford that because a notebook
  runtime is in-process, not a separate cookie-bearing dist node.
- **Pharo / Squeak.** The Smalltalk image *is* the desktop app — no
  client/server split, no distribution cookie. There is nothing to broker
  because there is nothing remote. Beamtalk's BEAM-native split (workspace node
  ≠ IDE node, ADR 0017/0091) is the deliberate departure; the cost of that
  departure is exactly the brokering this ADR designs.
- **Erlang/OTP distribution.** One cookie per node is a hard VM property, not a
  policy we can bend. The "spawn a node per cookie" pattern is idiomatic when
  you need to straddle multiple cookie domains (e.g. relays/bridges). We adopt
  it rather than fight it.
- **VS Code (our own `editors/vscode`).** Already a per-workspace-connection
  client model. The desktop broker is the standalone analogue of the VS Code
  sidebar's connection handling (ADR 0046), minus the editor host.

What we adopt: Livebook's "desktop shell wraps a local Phoenix release" shape.
What we reject: Livebook's single-runtime multiplexing — it doesn't fit
cookie-bearing dist nodes.

## User Impact

- **End-user developer (uses the IDE).** Opens a native app, sees a list of
  live workspaces, clicks one, gets a window. No `localhost:4000` to remember,
  no `mix`/Elixir on their machine (ERTS is bundled). Mental model:
  one window = one workspace.
- **IDE contributor.** `workspace.ex` is untouched, so the from-source
  `just web` flow and its tests are unaffected. The new surface is a small
  shell + a `RELEASE_NODE`/`PORT` plumbing tweak to `bin/server` (or a sibling
  script). Low blast radius.
- **Operator / security.** The cookie boundary (ADR 0091) is *strengthened*,
  not weakened: each front node carries exactly one workspace's cookie, never
  co-mingling cookie domains in one process. No new listening surface beyond
  the existing per-front HTTP port (loopback). The desktop app does not
  escalate privilege — it can only reach workspaces whose cookies are already
  readable on disk by this user.
- **BEAM veteran.** "One dist node per cookie domain" reads as obviously
  correct; they'd be suspicious of the alternative (one node hopping cookies).
  `observer`/`recon` work per-front-node exactly as today.

## Steelman Analysis

### Option A — Node per workspace (chosen)

- 🖥️ **End user**: "Window-per-workspace matches how I think — each project gets
  its own window, and if one workspace dies only that window goes dark."
- 🔧 **IDE contributor**: "It ships without touching the attach client. The risky
  refactor (threading a target through every RPC) just… doesn't happen. I reuse
  `bin/server` almost verbatim."
- 🔐 **Operator/security**: "One cookie per OS process is the strongest possible
  story. There is no code path where workspace A's cookie and workspace B's
  cookie are both live in the same VM."
- ⚙️ **BEAM veteran**: "This is the idiomatic way to straddle cookie domains.
  Crash isolation is free; a wedged front for one workspace can't take down the
  others."

### Option B — Single front node, per-session node targeting

- 🖥️ **End user**: "One process sips less RAM; I can keep ten workspaces attached
  without ten BEAMs eating memory."
- 🔧 **IDE contributor**: "The per-session target is arguably *better*
  architecture long-term — even `just web` could switch workspaces at runtime
  instead of rebooting. It pays down the global-env shortcut."
- 🔐 **Operator/security**: "Fewer processes, fewer ports, smaller attack
  surface to reason about."
- ⚙️ **BEAM veteran**: "`:rpc.call/4` already takes the target node per call;
  threading it through is mechanically simple Erlang."

### Tension points

- **Memory vs cookies.** Option B amortizes ERTS across connections (real win at
  many workspaces); Option A gets cookie isolation for free (real win for
  security). At *desktop* scale (a handful of workspaces) the memory cost is
  not felt, so the cookie argument dominates — but a future "attach to 50
  remote workspaces" operator console would flip this, and would want Option B.
- **Refactor now vs later.** Option B's per-session targeting is genuinely
  cleaner attach-client design; the only reason to defer it is risk and the
  fact that Option A doesn't need it. If per-session targeting lands later for
  other reasons, the broker could collapse to a single node — Option A does not
  foreclose that.
- **Where complexity lives.** Option A pushes complexity into the desktop shell
  (process broker); Option B pushes it into `workspace.ex` (per-call target +
  multi-cookie handling). We prefer complexity in the disposable shell over the
  shared, tested attach client.

## Alternatives Considered

### Single front node with per-session node targeting
Refactor `workspace.ex` so the target `{node, cookie}` is carried per LiveView
session/mount rather than read from module-global env, and have one Phoenix node
`Node.connect/1` to several workspaces. **Rejected** for now: forces the
multi-cookie distribution boundary (one VM, one cookie) and a non-trivial
refactor of the shared attach client. Its memory advantage is irrelevant at
desktop scale. Preserved as the natural evolution if a many-workspace operator
surface ever needs it — Option A does not block it.

### Browser, no desktop shell
Just `just web <id>` and open a tab. **Rejected**: provides none of the
connection-broker UX (discovery, native window management) that is the desktop
app's whole justification. If we weren't adding discovery + native connection
management, a desktop wrapper wouldn't earn its keep.

### Desktop app spawns the Rust workspace too
Bundle and supervise `beamtalk workspace create` from the shell. **Rejected**
per the framing constraint: it reintroduces cross-platform bundling of the Rust
toolchain and lifecycle supervision of a second, less-well-behaved process —
the most expensive and platform-fragile slice — for no benefit to the "attach
to what's already running" use case.

## Consequences

### Positive
- **Zero change to `workspace.ex` / the attach client.** Reuses the shipped
  boot-time global-env model and `bin/server` discovery verbatim.
- **Cookie isolation and crash isolation are free** — one workspace per BEAM.
- **One shipped artifact** (the `bt_attach` ERTS release); no Rust toolchain
  bundling.
- **Window-per-workspace** UX falls straight out of the architecture.

### Negative
- **One ERTS per connection** (~30–50 MB+ each). Fine for a handful of
  workspaces, wasteful at dozens. This is the deliberate trade vs Option B.
- **The desktop main process is a process supervisor again** — it must spawn,
  port-allocate, readiness-probe, and reap child nodes. Tamer than supervising
  the Rust toolchain (well-behaved release + known launcher), but non-trivial.
- **Per-instance node-name / port management** is new surface the broker owns.

### Neutral
- Does not foreclose a later move to single-node per-session targeting; the
  broker can collapse if that refactor lands.
- Shell is **Tauri (Rust)**, keeping the desktop broker in the same language as
  the compiler/CLI. The window model (per-workspace vs tabbed) is deferred to
  the implementation spike (recommendation noted above).

## Implementation

1. **`bin/server` plumbing** — confirm/expose `RELEASE_NODE` and `PORT`
   overrides cleanly (sibling script or documented env). (~S)
2. **Broker core (desktop main process)** — discovery of
   `~/.beamtalk/workspaces/*`, free-port selection, spawn with
   `RELEASE_NODE`/`PORT`/cookie env, readiness probe, child-exit reaping. (~M)
3. **Picker / launcher UI** — native list of live workspaces, attach/detach,
   disconnected-state handling. (~M)
4. **Window-per-workspace wiring** — one `BrowserWindow` per attached front. (~S)
5. **Packaging** — bundle the `bt_attach` release into the shell; CI release
   lane mirroring `liveview-release.yml`. (~M)
6. **Spike first**: a minimal Tauri shell that validates `RELEASE_NODE`/`PORT`
   multi-instance boot and the readiness probe end-to-end. (~S)

Rough total: ~2 weeks, low risk — no `workspace.ex` changes.

Affected components: desktop shell (new), `editors/liveview/rel/overlays/bin/`,
CI release lanes. **Not** affected: `workspace.ex`, the wire/RPC layer, the
Rust toolchain.

## References
- Related issues: BT-XXX (to be filed via `/plan-adr`)
- Related ADRs:
  [ADR 0017](0017-browser-connectivity-to-running-workspaces.md) (LiveView IDE),
  [ADR 0091](0091-remote-workspace-access-phoenix-authenticated-front.md) (Attach topology + cookie boundary),
  [ADR 0058](0058-platform-security-model.md) (Trusted Developer Tool stance),
  [ADR 0046](0046-vscode-live-workspace-sidebar.md) (per-connection client precedent),
  [ADR 0020](0020-connection-security.md) (transport/cookie machinery)
- Documentation: `editors/liveview/README.md`,
  `editors/liveview/rel/overlays/bin/server`,
  `docs/research/phoenix-topology-spike.md`,
  `docs/deployment/remote-liveview-ide.md`
