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
| Cookie | `workspace.ex:951` `set_cookie/0` | reads `BT_WORKSPACE_COOKIE` env, calls `Node.set_cookie(node_name(), token)` — the **per-peer** arity (`erlang:set_cookie(Node, Cookie)`), scoped to the one target node |
| Self node name | `workspace.ex:936` `ensure_distributed/0` | the front starts its *own* distribution via `:net_kernel.start([:"bt_attach_#{System.unique_integer([:positive])}@localhost", :shortnames])` — it does **not** use `RELEASE_NODE` |
| Every RPC | `workspace.ex:966` | `:rpc.call(node_name(), …)` — always the one global target |
| Launcher | `rel/overlays/bin/server <id>` | resolves `node_name` from `~/.beamtalk/workspaces/<id>/metadata.json` + the sibling `cookie` file, exports the two env vars, runs `bin/bt_attach start` |

So today: **one front node, bound to one workspace, for the life of the
process** — the `node_name()`/cookie globals are read once and every RPC targets
that single peer.

A correctness nuance worth stating up front, because it shapes the alternatives:
a BEAM node is **not** limited to one cookie. Erlang's `erlang:set_cookie(Node,
Cookie)` (the arity `set_cookie/0` already calls) sets a cookie *per peer node*,
so a single VM can legitimately hold different cookies for workspaces A and B
simultaneously. The single-node-multi-workspace alternative is therefore
*technically viable* — the argument against it (below) is about blast radius and
refactor cost, not impossibility.

The release is already self-contained (ERTS-embedded `mix release`), and `PORT`
(plus `PHX_HOST`) pass through `config/runtime.exs` under `config_env() ==
:prod` — which is how the release runs. Distribution starts lazily on the first
browser mount (`connect/0` → `ensure_distributed/0`), not at boot. Discovery
data (`metadata.json` + `cookie`) already lives on disk in a stable layout.

### Constraints

- **Don't regress `workspace.ex`.** It is the attach client shared with the
  from-source `just web` flow; the single-target model works and is tested.
- **Respect the cookie boundary (ADR 0091/0058).** A valid cookie is full RCE
  as the workspace owner. Different workspaces may carry different cookies; the
  design must not weaken or co-mingle them.
- **One shipped artifact.** The desktop app should bundle only the `bt_attach`
  release; the Rust `beamtalk` toolchain is the user's responsibility and is
  assumed already running.
- **Stay in ADR 0091's localhost zero-config lane.** The desktop broker is a
  single-user, on-host tool; it must *not* re-derive the remote auth surface.
  Two facts about the **prod** release make this an active requirement, not a
  given: (1) `config/runtime.exs:74` binds the endpoint to **all interfaces**
  (`{0,0,0,0,0,0,0,0}`), not loopback — a desktop front left at that default is
  reachable from the LAN; and (2) `runtime.exs:31` runs `IdeConfig.load!()` for
  every non-test boot, so a stray `~/.beamtalk/ide.toml` (or `BT_OIDC_*`) makes
  each spawned front enforce OIDC login. The broker must pin both: bind loopback
  and run the unauthenticated cookie-only path.

## Decision

Ship a thin **desktop shell** whose main process acts as a **connection
broker**: for each workspace the user attaches to, it spawns **one dedicated
`bt_attach` BEAM node**, bound to that workspace via the existing boot-time
env model, on its own HTTP port, and points a window at it. Disconnecting kills
that node's OS process.

Concretely, attaching to workspace `<id>` is:

```
PORT=<free-port> bin/server <id>
```

- `bin/server <id>` already resolves the node + cookie from
  `~/.beamtalk/workspaces/<id>/` and boots a fully-attached front, reusing the
  single-target `node_name()`/cookie model unchanged.
- `PORT=<free-port>` is chosen by the broker per instance; the window loads
  `http://localhost:<free-port>`.
- **Self node name:** each front already self-assigns a unique distribution
  sname (`bt_attach_<unique_integer>@localhost`, `ensure_distributed/0`), so no
  `RELEASE_NODE` override is needed. One caveat (Implementation §1): that
  integer is `System.unique_integer/1`, which is per-VM, so two *separate* front
  processes could in principle generate the same sname and collide on epmd.
  Seeding the name with the workspace id — a one-line change to
  `ensure_distributed/0` (or a `BT_ATTACH_NODE_SUFFIX` env it reads) — makes it
  collision-free across processes. This is the only `workspace.ex` touch the
  design needs.

Each front holds the cookie for exactly **one** workspace. As noted above this
is a *choice*, not a VM constraint — but holding one cookie per process is what
gives the design its two real properties: **blast-radius isolation** (a
compromised or wedged front node can reach only its one workspace, honouring the
ADR 0091/0058 "cookie = full RCE" stance) and **crash isolation** (one front
dying takes down one window, not all attachments). Both fall out of "one node
per workspace," alongside reusing the single-target attach client nearly
untouched.

### Broker responsibilities (desktop main process)

1. **Discover** — enumerate `~/.beamtalk/workspaces/*/metadata.json`; liveness
   via `beamtalk workspace status` (or a dist ping).
2. **Attach** — pick a free port, spawn the node as above pinned to a **loopback
   bind** and the **unauthenticated cookie-only** path (see local-only posture
   below), **attach-health probe** it (not just a bare HTTP 200 — distribution
   starts lazily on first `connect/0`, so the probe must force a workspace round
   trip to surface a bad cookie / dead workspace *before* the window opens),
   then open a window at `localhost:<port>`.
3. **Detach / quit** — terminate that node's child process; the window closes.

**Local-only posture (security).** The broker spawns each front with an explicit
loopback bind (override the prod endpoint's all-interfaces default,
`runtime.exs:74`) and must **refuse to spawn, with a clear error, if OIDC config
is present** (`~/.beamtalk/ide.toml` / `BT_OIDC_*`) — the desktop tool is the
single-user localhost lane (ADR 0091 §"Local dev stays zero-config"), not a place
to silently half-enforce remote auth. It also provisions a **stable
`SECRET_KEY_BASE` per workspace** (persisted under the workspace dir) rather than
inheriting `bin/server`'s ephemeral-per-boot key, so a front crash + respawn
does not invalidate the user's live session cookies.

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
  Caveat: the spike (Implementation §6) builds the no-shell coordinator
  alternative alongside and must confirm the Rust shell earns its keep over it
  before the CI build lane is committed.

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
- **Erlang/OTP distribution.** A node *can* hold per-peer cookies
  (`erlang:set_cookie/2`), so multiplexing workspaces from one node is possible.
  But "one OS process per trust domain" is the idiomatic way to get hard
  isolation on BEAM (process-per-cookie relays/bridges), because the VM offers
  no in-process sandbox once a cookie is in memory (ADR 0058). We adopt
  process-level isolation for blast radius rather than rely on in-VM
  bookkeeping.
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
  shell + a free-`PORT` per instance and (optionally) a one-line self-node-name
  seeding in `ensure_distributed/0`. Low blast radius.
- **Operator / security.** The cookie boundary (ADR 0091) is *strengthened* on
  the isolation axis: each front node carries exactly one workspace's cookie, so
  a compromise or crash is contained to one workspace. (A single node *could*
  hold several cookies via `set_cookie/2` — this design deliberately doesn't.)
  But this only holds **if the broker pins the local-only posture** above: the
  shipped prod release binds **all interfaces** and runs the OIDC fail-closed
  path by default, so a naïve "just run `bin/server`" would *regress* 0091 by
  exposing an unauthenticated IDE to the LAN. The broker must force loopback and
  reject OIDC config. Given that, no new remote surface is added, and the app
  cannot escalate privilege — it only reaches workspaces whose cookies are
  already readable on disk by this user.
- **BEAM veteran.** "One dist node per trust domain" reads as obviously correct;
  they'd be wary of the alternative concentrating several workspaces' cookies in
  one VM. `observer`/`recon` work per-front-node exactly as today.

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

- **Memory vs blast radius.** Option B amortizes ERTS across connections (real
  win at many workspaces); Option A gets process-level isolation (one cookie per
  process — real win for security blast radius). At *desktop* scale (a handful
  of workspaces) the memory cost is not felt, so the isolation argument
  dominates — but a future "attach to 50 remote workspaces" operator console
  would flip this, and would want Option B.
- **Refactor now vs later.** Option B's per-session targeting is genuinely
  cleaner attach-client design; the only reason to defer it is risk and the
  fact that Option A doesn't need it. If per-session targeting lands later for
  other reasons, the broker could collapse to a single node — Option A does not
  foreclose that.
- **Where complexity lives.** Option A pushes complexity into the desktop shell
  (process broker); Option B pushes it into `workspace.ex` (per-call target +
  per-peer cookie handling). We prefer complexity in the disposable shell over
  the shared, tested attach client.

## Alternatives Considered

### Single front node with per-session node targeting
Refactor `workspace.ex` so the target `{node, cookie}` is carried per LiveView
session/mount rather than read from module-global env, and have one Phoenix node
`Node.connect/1` (with per-peer `set_cookie/2`) to several workspaces. This is
**technically viable** — per-peer cookies make it work — so the rejection is on
cost/risk, not feasibility. **Rejected** for now because (a) it concentrates
multiple workspaces' cookies in one process, so a single compromise/crash blasts
all attachments, against the ADR 0091/0058 isolation stance; and (b) it requires
a non-trivial refactor of the shared, tested attach client (threading the target
through `node_name/0`, `set_cookie/0`, and every RPC). Its only edge — amortizing
ERTS across connections — is irrelevant at desktop scale. Preserved as the
natural evolution if a many-workspace operator surface ever needs it; Option A
does not block it.

### Browser, no desktop shell
Just `just web <id>` and open a tab. **Rejected**: provides none of the
connection-broker UX (discovery, native window management) that is the desktop
app's whole justification. If we weren't adding discovery + native connection
management, a desktop wrapper wouldn't earn its keep.

### No-shell coordinator front (+ optional PWA install)
Instead of a Tauri/Rust shell, make the broker itself a small **Phoenix front**:
one always-on `bt_attach`-style node that lists `~/.beamtalk/workspaces/*` and,
on click, shells out to `bin/server` (or opens a window at) the per-workspace
front. The "native app" feel comes from the browser's **Install app** (PWA)
affordance — a dockable, chrome-less `localhost` window with near-zero shell
code. **This is the real 80/20 challenger**: it delivers discovery + a
dock icon + pick-a-workspace without Tauri, a Rust broker, or a new CI build
lane. **Not rejected outright** — instead, the spike (Implementation §6) must
build this *first* and justify the Tauri shell against it. The Tauri case rests
on things the coordinator can't easily do: enforcing the loopback/no-OIDC
posture and `SECRET_KEY_BASE` provisioning *outside* the BEAM (a coordinator
front would have to spawn children with the same care anyway), true per-window
OS integration, and reaping orphaned child processes on crash. If those don't
prove load-bearing in the spike, the coordinator wins and this ADR's
shell choice should flip.

### Desktop app spawns the Rust workspace too
Bundle and supervise `beamtalk workspace create` from the shell. **Rejected**
per the framing constraint: it reintroduces cross-platform bundling of the Rust
toolchain and lifecycle supervision of a second, less-well-behaved process —
the most expensive and platform-fragile slice — for no benefit to the "attach
to what's already running" use case.

## Consequences

### Positive
- **Near-zero change to `workspace.ex` / the attach client.** Reuses the shipped
  boot-time global-env model and `bin/server` discovery; the only possible touch
  is a one-line self-node-name seed for cross-process uniqueness (Impl §1).
- **Blast-radius and crash isolation** — one workspace's cookie per process, one
  window per front.
- **One shipped artifact** (the `bt_attach` ERTS release); no Rust toolchain
  bundling.
- **Window-per-workspace** UX falls straight out of the architecture.

### Negative
- **One ERTS per connection** (~30–50 MB+ each). Fine for a handful of
  workspaces, wasteful at dozens. This is the deliberate trade vs Option B.
- **The desktop main process is a process supervisor again** — it must spawn,
  port-allocate, readiness-probe, and reap child nodes. Tamer than supervising
  the Rust toolchain (well-behaved release + known launcher), but non-trivial.
- **Per-instance port (and sname-uniqueness) management** is new surface the
  broker owns — including reusing a port and re-registering the same sname after
  a front crashes (epmd must have dropped the dead registration first).
- **The broker inherits security-critical responsibilities the BEAM release
  won't enforce for it:** forcing a loopback bind (the prod default is
  all-interfaces), refusing OIDC config, and provisioning a stable
  `SECRET_KEY_BASE`. Get any wrong and you either expose the IDE to the LAN or
  break the user's session on every restart. These are the price of reusing the
  remote-shaped release as a local tool.

### Neutral
- Does not foreclose a later move to single-node per-session targeting; the
  broker can collapse if that refactor lands.
- Shell is **Tauri (Rust)** (language coherence with the compiler/CLI),
  *contingent on the spike* showing the broker's loopback/no-OIDC/secret +
  crash-reaping duties need to live outside the BEAM; if not, the no-shell
  coordinator front wins and the shell choice flips. The window model
  (per-workspace vs tabbed) is likewise deferred to the spike.

## Implementation

1. **Sname uniqueness + `PORT` per instance** — seed `ensure_distributed/0`'s
   sname with the workspace id (or read a `BT_ATTACH_NODE_SUFFIX` env) so two
   front processes can't collide on epmd; confirm `PORT` passthrough. One-line
   `workspace.ex` change + `bin/server` env. (~S)
2. **Broker core (desktop main process)** — discovery of
   `~/.beamtalk/workspaces/*`, free-port selection, spawn with `PORT` + cookie
   env, readiness probe (poll the HTTP port; note distribution starts lazily on
   first mount), child-exit reaping. (~M)
3. **Picker / launcher UI** — native list of live workspaces, attach/detach,
   disconnected-state handling. (~M)
4. **Window-per-workspace wiring** — one window per attached front. (~S)
5. **Packaging** — bundle the `bt_attach` release into the shell; CI release
   lane mirroring `liveview-release.yml`. (~M)
6. **Spike first**, and make it decide the shell question. Validate: (a)
   two-instance boot (distinct snames + ports); (b) **crash → respawn of the
   *same* workspace** — same seeded sname must re-register cleanly after epmd
   drops the dead one, and the freed port must be reusable; (c) the attach-health
   probe catching a dead-workspace/bad-cookie before the window opens; (d) the
   loopback-bind + no-OIDC posture actually takes. Build the **no-shell
   coordinator** (Alternatives) alongside and only commit to the Tauri shell +
   CI lane (§5) if the broker responsibilities prove they need to live outside
   the BEAM. (~M, was ~S — this is the load-bearing spike, not a warm-up.)

Rough total: ~2 weeks, low risk — one-line `workspace.ex` touch at most.

Affected components: desktop shell (new), `editors/liveview/rel/overlays/bin/`,
a one-line seed in `workspace.ex` (`ensure_distributed/0`), CI release lanes.
**Not** affected: the wire/RPC layer, the Rust toolchain.

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
