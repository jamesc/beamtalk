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

1. **Attach to live workspaces; do not bundle or supervise the Rust
   toolchain.** The desktop app *attaches* to workspaces discovered from
   `~/.beamtalk/workspaces/`, typically started with
   `beamtalk workspace create … --background --persistent`. It is a client, not
   a process supervisor for the language runtime: it never ships the Rust
   `beamtalk` binary and never babysits its lifecycle — the most painful
   packaging slice. The constraint is on **bundling, not invoking**: the broker
   already shells out to the *installed* CLI for liveness (`workspace status`),
   and invoking that same CLI for `workspace create`/`stop` costs no additional
   packaging — `--background --persistent` daemonizes the workspace, so no
   supervision follows. In-scope lifecycle is "invoke the user's CLI";
   out-of-scope is shipping or supervising it.

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
| Cookie | `workspace.ex:1966` `set_cookie/0` | reads `BT_WORKSPACE_COOKIE` env, calls `Node.set_cookie(node_name(), token)` — the **per-peer** arity (`erlang:set_cookie(Node, Cookie)`), scoped to the one target node |
| Self node name | `workspace.ex:1951` `ensure_distributed/0` | the front starts its *own* distribution via `:net_kernel.start([:"bt_attach_#{System.unique_integer([:positive])}@localhost", :shortnames])` — it does **not** use `RELEASE_NODE` |
| Every RPC | `workspace.ex:1981` `rpc/3` | `:rpc.call(node_name(), …)` — always the one global target |
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

One invariant to state rather than assume: the front's lazy
`:net_kernel.start/1` — unlike `erl -sname` boot — does **not** auto-start
epmd; it fails if none is listening. The design therefore assumes
"workspace running ⇒ epmd listening on loopback 4369", which holds today
because the workspace's own boot daemonizes epmd. If workspace lifecycle ever
adopts the pinned-dist-port / **epmdless** direction ADR 0091 floats for its
private-network posture, the front's name-resolution path breaks with no
fallback — a known future incompatibility, and why `/readiness` should report
*epmd absent* as its own failure mode (Impl §1c).

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
  reachable from the LAN; and (2) `runtime.exs:32` runs `IdeConfig.load!()` for
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
  Seeding the name with the workspace id **plus per-process entropy** (e.g.
  `bt_attach_<id>_<os_pid>@localhost`, via a `BT_ATTACH_NODE_SUFFIX` env
  `ensure_distributed/0` reads) makes it collision-free across processes.
  An id-*only* seed would invert the bug: two fronts attached to the **same**
  workspace — a second window, a second broker instance, or a crash→respawn
  racing the dying front's epmd deregistration — would collide
  *deterministically*, in exactly the scenarios the spike (§6b) probes.
  This is the smallest of the front-side
  touches; the design also needs a `BT_ATTACH_BIND_IP` hook in
  `config/runtime.exs` and a minimal `/readiness` endpoint (both below) — all
  small and additive, but not literally "one line."

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
   via the installed `beamtalk` CLI (`workspace status`) or a direct epmd query
   (TCP 4369 `NAMES`) — a *dist ping* is **not** available to a non-BEAM broker
   without implementing the distribution handshake, so it is not a fallback.
   Two contracts this surfaces: (a) GUI apps launched from Finder/the dock do
   not inherit the user's shell `PATH`, so the CLI path must be resolved
   explicitly (configurable, with sane defaults) if the CLI route is used;
   (b) the broker parses `metadata.json` with a real JSON parser (not
   `bin/server`'s `sed` extraction), and an independently-updating desktop app
   coupling to a file layout owned by the Rust CLI needs a stated compat
   contract (additive-only fields, or a version field) — today that layout is
   stable by convention, not by contract.
2. **Attach** — pick a free port, spawn the node as above pinned to a **loopback
   bind** and the **unauthenticated cookie-only** path (see local-only posture
   below), then probe for readiness before opening the window. Note the probe is
   **two-stage**, because an external (non-BEAM) broker cannot trigger the lazy
   `connect/0` from outside the VM: (a) poll the HTTP port to confirm Phoenix is
   up; (b) hit a small **attach-health endpoint** — `GET /readiness` — that forces
   `connect/0` + one cheap RPC to the workspace and returns 200 only on success,
   so a bad cookie / dead workspace surfaces *before* the window opens rather than
   on the user's first eval. That endpoint does **not** exist today; it is a
   small, explicit addition to the front's router + a thin controller (see
   Implementation §1) — not free, but contained.
3. **Monitor** — the readiness probe runs once, before the window opens;
   attachment health after that is not free. The broker re-polls `/readiness`
   periodically post-attach to drive window state (the "dead workspace = greyed
   window" UX needs this mechanism — a dead workspace does *not* kill the front,
   whose RPCs just start returning `{:badrpc, :nodedown}`), and the front should
   retry `connect/0` on `:nodedown` so OS sleep/resume — which drops dist
   connections — self-heals.
4. **Detach / quit** — terminate that node's child process; the window closes.
   If the *broker* dies uncleanly (SIGKILL, logout), N cookie-bearing front
   BEAMs are orphaned — the reaping mechanism (process group, parent-death
   watch, or PID-file sweep on next start) must be specified in the spike, since
   crash-reaping is cited below as a load-bearing argument for the Tauri shell.
5. **Create / stop, via the installed CLI** — the picker's empty state and
   lifecycle buttons shell out to the user's installed `beamtalk` CLI
   (`workspace create … --background --persistent`, `workspace stop`) — never a
   bundled copy (constraint 1). If the CLI can't be resolved (GUI-app `PATH`,
   §1), the picker degrades to setup instructions: the first-run experience
   must not be a blank window with an unstated terminal prerequisite.

**Local-only posture (security).** Three things the broker must pin — none of
which the remote-shaped release does for it — and one it deliberately leaves
ephemeral:

- **Loopback bind.** The prod endpoint hardcodes `ip: {0,0,0,0,0,0,0,0}`
  (`runtime.exs:74`) with **no env hook today**, so "the broker passes a bind
  address" is not yet possible — closing this needs a small `config/runtime.exs`
  change: read a `BT_ATTACH_BIND_IP` env (defaulting to the current
  all-interfaces value, so remote deploys are unchanged) and have the broker set
  it to `127.0.0.1`/`::1`. Naming the mechanism here so the spike (§6d) doesn't
  discover it late.
- **No OIDC.** Refuse to spawn, with a clear error, if OIDC config is present
  (`~/.beamtalk/ide.toml` / `BT_OIDC_*`) — the desktop tool is the single-user
  localhost lane (ADR 0091 §"Local dev stays zero-config"), not a place to
  silently half-enforce remote auth. (`runtime.exs:32` runs `IdeConfig.load!()`
  for every non-test boot, so a stray config would otherwise take effect.)
- **Origin pinning.** The front is an *unauthenticated, eval-capable* LiveView
  websocket on a dynamic port — exactly the surface where `check_origin` is the
  remaining defense against drive-by-localhost / DNS-rebinding eval (the threat
  ADR 0091 reasons about for its remote posture). The endpoint's `url` config
  derives from `PORT`, so the origin *likely* matches `http://localhost:<port>`
  already — but it is load-bearing and must be pinned explicitly and validated
  in the spike (§6d), not assumed.
- **Ephemeral `SECRET_KEY_BASE` per boot — deliberately.** The broker inherits
  `bin/server`'s ephemeral-per-boot key rather than provisioning a stable
  per-workspace secret. In the unauthenticated localhost lane there is no login
  session worth preserving across a front restart: LiveView state lives in the
  front's *processes*, which the restart destroyed anyway — the browser
  remounts fresh whether or not the old session cookie still verifies. A
  stable secret would add an RCE-adjacent artifact (a forged session cookie ≈
  eval) with an unspecified writer inside a directory whose lifecycle the Rust
  CLI owns, for no user-visible benefit. If the remote/OIDC topology (future
  ADR) ever needs durable sessions, that ADR owns the decision.

### What this is NOT

- It does **not** bundle or supervise the Rust `beamtalk` workspace runtime —
  creating/stopping workspaces happens by invoking the user's *installed* CLI
  (Broker §5), which daemonizes workspaces itself.
- It does **not** introduce a new wire protocol — the front still speaks the
  same `:rpc` it does today.
- It does **not** add per-session node targeting to `workspace.ex` (the
  rejected alternative below).
- It does **not** attach to **remote, OIDC-authenticated** workspaces — see the
  scope boundary below.

### Scope boundary: remote / OIDC workspaces (out of scope — future, separate ADR)

This ADR covers **local** workspaces only. Attaching to a *remote* workspace —
one already fronted by an OIDC-authenticated Phoenix deployment per ADR 0091 —
is a deliberately separate topology, because it reuses **none** of the broker
designed here:

| | Local (this ADR) | Remote + OIDC (future) |
|---|---|---|
| Who runs the front? | broker **spawns** one `bt_attach` per workspace | already running (Docker, ADR 0091); broker spawns **nothing** |
| Transport | Erlang dist + cookie, loopback | HTTPS to a remote host |
| Auth | none (single-user localhost, ADR 0020) | server-side OIDC in the front |
| Desktop's job | process broker (spawn / port / reap) | a **persistent webview** pointed at `https://host` |
| Cookie / port mgmt | yes | none |

The OIDC flow is entirely **server-side and redirect-driven** in the front
(`oidc_controller.ex`: `/oidc/auth` → 302 to IdP → `/oidc/callback` → session
cookie). A desktop webview therefore implements **no OAuth itself** — it just
follows redirects and persists the session cookie. The one real wrinkle: the IdP
login page is a third-party origin, and several IdPs (notably Google) **reject
OAuth from embedded webviews** (`disallowed_useragent`). The remote path would
need a **system-browser handoff** for the IdP step, with a loopback / custom-
scheme redirect back into the app — the classic desktop-OAuth pattern, which a
Rust shell (Tauri) supports but the no-shell coordinator alternative could not.

The two topologies meet only at the **connection picker**: one list offering
local workspaces (attach = spawn) and remote endpoints (attach = open-webview-
at-URL). That shared surface is the natural seam at which a future
"desktop remote attach + OIDC" ADR would extend this one — it does not require
revisiting any decision here.

### Open sub-decisions (deferred to implementation spike)

- **Shell: Tauri (recommendation, spike-confirmed).** Recommendation: a
  **Tauri (Rust) app**, not Electron — a *recommendation the spike must
  confirm*, not a settled decision; the CI build lane (§5) waits for its
  verdict. Three reasons. First, **language coherence**: the Beamtalk compiler/CLI is
  already Rust, so a Rust shell keeps the toolchain in one language — the broker
  logic (discovery, spawn, port allocation, child reaping) is ordinary Rust
  process handling, reviewable by the same people who own `beamtalk-cli`.
  Second, **footprint**: we already ship one ERTS per connection, so the
  ~100 MB Electron runtime would be pure overhead on a binary we can't shrink;
  Tauri's system-webview model avoids that. The shell does very little
  (spawn → probe → window), so Tauri's thinner JS ecosystem is not a cost here.
  Third, **OS-reserved keybindings**: a native shell owns its window's menu and
  accelerator table, so chords the browser refuses to surrender — `Ctrl/Cmd-W`,
  `Ctrl/Cmd-N`, `Ctrl-T`, `Ctrl-Shift-T` — become bindable IDE actions (e.g.
  `Ctrl/Cmd-W` closing the focused code pane rather than the OS killing the tab).
  In a plain browser tab these chords are handled by the browser chrome before
  the page sees a cancelable event, so `preventDefault()` is silently ignored;
  the `KeyboardShortcuts` hook (`assets/js/hooks/keyboard_shortcuts.js`) can only
  claim the non-reserved chords (⌘S/⌘D/⌘P/⌘I). This is a property the no-shell
  coordinator's **PWA** path cannot match: an installed PWA standalone window
  still lets the browser/OS consume `Ctrl/Cmd-W` (it closes the PWA window), and
  the page still cannot `preventDefault` it — so editor-grade key ownership is a
  concrete capability that tips the spike's shell decision toward Tauri.
  The spike (Implementation §6) builds the no-shell coordinator alternative
  alongside; its exit criteria (a)–(g) — including webview keybinding and
  rendering parity, which cut *against* Tauri on Linux — decide the shell.
- **Window model: window-per-workspace vs tabbed.** Recommendation:
  **window-per-workspace**. It falls straight out of one-BEAM-per-workspace and
  keeps crash isolation visible to the user (a dead workspace = one greyed
  window, not a dead tab in a shared shell).
- **Single-instance policy and attach-twice semantics.** Is the broker a
  single-instance app (Tauri single-instance plugin / lockfile), and what does
  attaching twice to the same workspace mean — focus the existing window, or
  spawn a second front? Both interact with the sname seed (two fronts on one
  workspace must not collide) and port bookkeeping; decide in the spike.

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
  one window = one workspace. First run with no workspaces is a real state:
  the picker offers "create a workspace" via the installed CLI (Broker §5), or
  setup instructions if the CLI isn't found — never a silent empty list.
- **IDE contributor.** `workspace.ex` is untouched, so the from-source
  `just web` flow and its tests are unaffected. The new surface is a small
  shell + a free-`PORT` per instance and a small self-node-name seeding
  (id + entropy) in `ensure_distributed/0`. Low blast radius.
- **Operator / security.** The cookie boundary (ADR 0091) is *strengthened* on
  the isolation axis: each front node carries exactly one workspace's cookie, so
  a compromise or crash is contained to one workspace. (A single node *could*
  hold several cookies via `set_cookie/2` — this design deliberately doesn't.)
  But this only holds **if the broker pins the local-only posture** above: the
  shipped prod release binds **all interfaces** and runs the OIDC fail-closed
  path by default, so a naïve "just run `bin/server`" would *regress* 0091 by
  exposing an unauthenticated IDE to the LAN. The broker must force loopback and
  reject OIDC config. Given that, no new remote surface is added, and the app
  cannot escalate privilege *on a single-user machine* — it only reaches
  workspaces whose cookies are already readable on disk by this user. On a
  **shared host**, loopback ≠ single-user: any local user or process can open
  `http://localhost:<port>` and eval as the workspace owner. That is ADR 0058's
  trusted-developer-tool stance, accepted — but it is a stance, not an absence
  of surface.
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
on things the coordinator can't easily do: enforcing the
loopback/no-OIDC/`check_origin` posture *outside* the BEAM (a coordinator
front would have to spawn children with the same care anyway), true per-window
OS integration, **ownership of OS-reserved keybindings** (`Ctrl/Cmd-W` etc. as
IDE actions — see "Shell: Tauri"; a PWA window still cedes these to the browser),
and reaping orphaned child processes on crash. If those don't
prove load-bearing in the spike, the coordinator wins and this ADR's
shell choice should flip.

### Desktop app bundles and supervises the Rust workspace runtime
Ship the Rust toolchain inside the app and supervise workspace processes from
the shell. **Rejected** per the framing constraint: it reintroduces
cross-platform bundling of the Rust toolchain and lifecycle supervision of a
second, less-well-behaved process — the most expensive and platform-fragile
slice. What is *not* rejected: invoking the user's already-installed CLI to
create/stop workspaces (Broker §5). An earlier "attach-only" framing conflated
bundling with invoking; the rejection applies to bundling only.

### `beamtalk workspace open <id>` — CLI-only launcher, no desktop app
Add a CLI command that spawns a front for the workspace (free port, loopback
bind) and opens the browser/PWA at it; discovery is `beamtalk workspace list`.
Zero new artifacts, no CI lane, no GUI-`PATH` problem (the CLI *is* the entry
point), and it fits ADR 0099's CLI application story. **Rejected as a
replacement, cheap as a complement**: it delivers launch but none of the
connection-broker UX — no persistent picker, no window management, no
post-attach monitoring or orphan reaping (stray fronts accumulate with no
owner) — and it inherits the PWA path's inability to own OS-reserved
keybindings. Worth shipping *independently* of this ADR as a CLI affordance;
the spike's no-shell coordinator comparison effectively subsumes it.

## Consequences

### Positive
- **Small, additive front changes — the attach client's core is untouched.**
  Reuses the shipped boot-time global-env model and `bin/server` discovery; the
  RPC/eval path is unchanged. The new front-side work is three small, additive
  pieces (Impl §1): an id+entropy sname seed, a `BT_ATTACH_BIND_IP` env hook in
  `config/runtime.exs`, and a minimal `/readiness` endpoint.
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
- **The desktop artifact introduces version skew for the first time.** Today
  `just web` builds front and workspace from the same tree, so skew cannot
  occur. A bundled `bt_attach` (+ its ERTS/OTP) updates on the app's cadence
  while the workspace runs whatever runtime the user installed — and the front
  RPCs into workspace-internal modules by name (`beamtalk_repl_ops` etc.), a
  private surface with no versioned protocol; the dist protocol itself must
  also stay OTP-compatible across the gap. The `/readiness` handshake (Impl
  §1c) is the mitigation: the workspace reports a runtime/protocol version and
  the front refuses or warns on mismatch.
- **The broker inherits security-critical responsibilities the BEAM release
  won't enforce for it:** forcing a loopback bind (the prod default is
  all-interfaces), refusing OIDC config, and pinning `check_origin`. Get one
  wrong and you expose an unauthenticated eval surface to the LAN (or to
  drive-by-localhost pages). These are the price of reusing the remote-shaped
  release as a local tool.

### Neutral
- Does not foreclose a later move to single-node per-session targeting; the
  broker can collapse if that refactor lands.
- Shell **recommendation is Tauri (Rust)** (language coherence with the
  compiler/CLI) — an open sub-decision the spike settles: it must show the
  broker's loopback/no-OIDC/`check_origin` + crash-reaping duties need to live
  outside the BEAM; if not, the no-shell coordinator front wins and the shell
  choice flips. The window model (per-workspace vs tabbed) and single-instance
  policy are likewise deferred to the spike.

## Implementation

1. **Front-side hooks (three small, additive changes).** (a) seed
   `ensure_distributed/0`'s sname with the workspace id **plus per-process
   entropy** (e.g. `bt_attach_<id>_<os_pid>`, via a `BT_ATTACH_NODE_SUFFIX`
   env) so two front processes can't collide on epmd — id-only would collide
   deterministically when two fronts attach to the same workspace;
   (b) read `BT_ATTACH_BIND_IP` in `config/runtime.exs` (default = today's
   all-interfaces) so the broker can pin loopback; (c) add a `/readiness`
   endpoint (router + thin controller) that forces `connect/0` + one cheap RPC
   and returns 200 only when the workspace is actually reachable — including a
   **version handshake** (workspace reports its runtime/protocol version; the
   front refuses or warns on mismatch) and a failure taxonomy that
   distinguishes *epmd absent* from *bad cookie* from *dead workspace*.
   Confirm `PORT` passthrough. (~S)
2. **Broker core (desktop main process)** — discovery of
   `~/.beamtalk/workspaces/*`, free-port selection, spawn with `PORT` +
   `BT_ATTACH_BIND_IP` + cookie env, **two-stage readiness probe** (HTTP port up,
   then `GET /readiness` for true workspace reachability — distribution starts
   lazily, so HTTP-up alone is not enough), child-exit reaping. (~M)
3. **Picker / launcher UI** — native list of live workspaces, attach/detach,
   disconnected-state handling, the first-run empty state, and create/stop via
   the installed CLI (Broker §5). (~M)
4. **Window-per-workspace wiring** — one window per attached front. (~S)
5. **Packaging** — bundle the `bt_attach` release into the shell; CI release
   lane mirroring `liveview-release.yml`. Three named costs the "reuse
   `bin/server`" framing hides: (a) an ERTS-embedded release is per-OS/per-arch,
   so this is a **build matrix** (macOS arm64 + x86_64, Linux x86_64, Windows —
   ADR 0027 makes Windows a supported platform), not one artifact; (b)
   `bin/server` is a POSIX `sh` script with no Windows counterpart — on Windows
   the broker must set the env and invoke `bin/bt_attach` itself; (c) macOS
   notarization requires signing every nested Mach-O in the release
   (`beam.smp`, NIFs, epmd) — Livebook treats this as a substantial ongoing
   surface, not a one-off. (~L, was ~M)
6. **Spike first**, and make it decide the shell question. Validate: (a)
   two-instance boot (distinct snames + ports); (b) **crash → respawn of the
   *same* workspace** — same seeded sname must re-register cleanly after epmd
   drops the dead one, and the freed port must be reusable; (c) the attach-health
   probe catching a dead-workspace/bad-cookie before the window opens; (d) the
   loopback-bind + no-OIDC + pinned-`check_origin` posture actually takes;
   (e) **webview parity**: OS-reserved keybinding interception (`Cmd/Ctrl-W`
   etc.) actually works in the Tauri webview on all three platforms — the
   keybinding argument for Tauri is asserted from *browser* behavior, not yet
   demonstrated in WebKitGTK/WebView2/WKWebView — and WebKitGTK's
   rendering/websocket performance is adequate for an editor-grade LiveView
   UI (a known Tauri-on-Linux risk that cuts *against* the shell as surely as
   the keybinding argument cuts for it); (f) post-attach lifecycle: workspace
   killed while attached → greyed window (not a wall of LiveView errors), and
   sleep/resume → reconnect; (g) the orphan-reaping mechanism on broker crash,
   concretely. Build the **no-shell coordinator** (Alternatives) alongside and
   only commit to the Tauri shell + CI lane (§5) if the broker responsibilities
   prove they need to live outside the BEAM — the spike's exit criteria are
   (a)–(g), decided per-duty, not vibes. (~M, was ~S — this is the load-bearing
   spike, not a warm-up.)

Rough total: ~2 weeks, low risk — the front changes are small and additive; the
attach client's RPC/eval core is untouched.

Affected components: desktop shell (new), `editors/liveview/rel/overlays/bin/`,
three small front-side additions (`ensure_distributed/0` sname seed,
`config/runtime.exs` `BT_ATTACH_BIND_IP`, a `/readiness` router + controller),
CI release lanes. **Not** affected: the wire/RPC/eval layer, the Rust toolchain.

## References
- Related issues: BT-XXX (to be filed via `/plan-adr`)
- Related ADRs:
  [ADR 0017](0017-browser-connectivity-to-running-workspaces.md) (LiveView IDE),
  [ADR 0091](0091-remote-workspace-access-phoenix-authenticated-front.md) (Attach topology + cookie boundary),
  [ADR 0058](0058-platform-security-model.md) (Trusted Developer Tool stance),
  [ADR 0046](0046-vscode-live-workspace-sidebar.md) (per-connection client precedent),
  [ADR 0020](0020-connection-security.md) (transport/cookie machinery),
  [ADR 0099](0099-cli-application-story.md) (CLI application story — the `workspace open` alternative)
- Documentation: `editors/liveview/README.md`,
  `editors/liveview/rel/overlays/bin/server`,
  `docs/research/phoenix-topology-spike.md`,
  `docs/deployment/remote-liveview-ide.md`
