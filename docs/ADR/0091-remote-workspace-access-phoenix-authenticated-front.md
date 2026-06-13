# ADR 0091: Connection Security for Remote Workspace Access — Phoenix as Authenticated Front

## Status
Accepted (2026-06-06)

Extends [ADR 0020 — Connection Security](0020-connection-security.md). Amends
[ADR 0058 — Platform Security Model](0058-platform-security-model.md) Principle 6
(single-user per workspace) for the remote/multi-user case. Unblocks
[ADR 0017 — Browser Connectivity](0017-browser-connectivity-to-running-workspaces.md)
Phase 3.

## Context

### Problem statement

The LiveView IDE (ADR 0017 Phase 3) is built on the **Attach** topology validated
by the BT-2394 spike (`docs/research/phoenix-topology-spike.md`): Phoenix runs as
its own BEAM node and connects to the workspace node over Erlang distribution +
`:rpc`, Livebook-style. The spike proved Attach works end-to-end with **zero
runtime changes**, but it explicitly left the **production auth/authz design
unresolved**. Localhost development (Waves 1–4 of BT-2398) can proceed under the
existing cookie boundary; taking the IDE **past localhost** (Wave 5, BT-2411)
needs the decisions this ADR makes.

Two findings from the spike drive everything below:

1. **Distribution exposure.** Plain Erlang distribution sends the cookie in an
   MD5 challenge/response, but the transport is **unencrypted** and **epmd is a
   network service**. The cookie is the only secret, and it is symmetric with no
   rotation. Remote Attach **must** use TLS distribution (`inet_tls_dist`) or a
   tunnel, per ADR 0020 Layer 3.

2. **Capability scope.** A connected node can `:rpc.call/4` **any** function on
   the workspace. Authentication at the distribution layer is **all-or-nothing** —
   there is no per-operation, per-user authorization possible at the dist layer.
   This is far more power than the Phase-1 WebSocket protocol's narrow op set.

The spike's recommendation, which this ADR is chartered to validate and make
binding: **Phoenix as the authenticated front.** The browser's trust boundary
terminates at Phoenix (HTTPS + real authentication); Phoenix — and only Phoenix —
speaks Erlang distribution to the workspace, over a hardened link. The browser's
trust boundary is **never** placed directly on Erlang distribution.

### Current state

| Hop | Today | Security today |
|-----|-------|----------------|
| Browser ↔ workspace (Phase 1, Cowboy) | `ws://localhost:{port}` direct | Loopback + cookie handshake (ADR 0020), Origin check |
| Phoenix node ↔ workspace (Attach, spike) | Erlang distribution + `:rpc` | Shared dist cookie, **unencrypted**, **localhost only** |
| Browser ↔ Phoenix | not yet built | — |

ADR 0020 already specifies the transport machinery this ADR composes:
loopback-by-default binding, the cookie handshake, and overlay networks (Layer 3
Option A). It also specced mTLS for distribution via OTP `ssl_dist` (Layer 3
Option B), but that support was **removed in PR #1401** and is not in the tree
(see Decision 5 — this ADR does not depend on it). ADR 0058
establishes the **Trusted Developer Tool** stance: a valid cookie = full RCE as
the workspace owner; there is no sandbox; and (Principle 6) a workspace is
**single-user**. This ADR's hardest job is reconciling remote multi-user access
with that stance.

### Constraints

1. **No runtime changes for the topology itself** — the spike proved Attach needs
   none. Auth/authz must live in Phoenix and in transport/CLI config, not in the
   workspace eval path.
2. **The workspace remains an arbitrary-code-execution endpoint** (ADR 0058). Any
   role that can `eval` has full RCE on the workspace node. No facade changes this.
3. **Local dev must stay zero-config** (ADR 0020 Principle 3). The remote auth
   stack is explicit opt-in; `beamtalk repl --web` on localhost is unaffected.
4. **Reuse the term-op layer** (BT-2399) — the documented, stable
   `{ok, Value, Output, Warnings} | {error, #beamtalk_error{}}` op surface is the
   substrate, not raw internal shapes.
5. **Don't roll our own crypto** (ADR 0020 Principle 2) — OIDC libraries,
   reverse-proxy TLS, and OS/transport primitives are the sanctioned building
   blocks (not a hand-rolled auth scheme).

## Decision

Remote (non-localhost) workspace access uses **Phoenix as the authenticated
front**, with a three-segment trust path and a per-operation authorization facade.
Five sub-decisions:

```
   UNTRUSTED NETWORK              TRUSTED HOST / PRIVATE NETWORK
┌─────────┐  ① HTTPS + OIDC   ┌──────────────┐  ② internal dist     ┌─────────────┐
│ Browser │ ═══════════════╗  │   Phoenix    │     (loopback /       │  Workspace  │
│ (user)  │  session cookie║  │  LiveView    │ ──────────────────── │  BEAM node  │
└─────────┘  (HttpOnly,    ╚═▶│  node        │     private iface,    │  (eval, RCE)│
             Secure,          │  ③ facade    │     never public)     │             │
             SameSite)        │  ④ RBAC      │  ③ curated op facade  └─────────────┘
                              └──────────────┘     + RBAC, never raw
   The ONLY untrusted boundary is ①.   :rpc passthrough
   Phoenix is the ONLY node on the dist mesh; ② stays off untrusted networks.
```

### Decision 1 — Authentication: OIDC/SSO at the Phoenix front

The browser authenticates to Phoenix via **OIDC** (OpenID Connect) against an
external identity provider (Google, Okta, Entra ID, Keycloak, …). On successful
OIDC exchange, Phoenix establishes a **server-side session** carried in an
`HttpOnly; Secure; SameSite=Strict` cookie (Strict, not Lax — this cookie gates
an RCE-bearing tool; the only route that needs Lax is the OIDC callback, scoped to
that handler). The LiveView socket inherits that session — no token in the URL
(avoids the Jupyter/Livebook URL-token leakage class: browser history, Referer,
screen-sharing).

OIDC from the start (rather than starting with passwords/phx.gen.auth) is chosen
because the remote deployment story is inherently a team/operator story: the
people who need non-localhost access already have an IdP, and SSO is where
multi-user lands anyway.

**Session lifecycle (required, not an implementation detail).** A LiveView socket
authorized at mount must **not** outlive the user's authorization. Because a
mounted LiveView process keeps running after its session cookie/OIDC token expires
or is revoked, the implementation must: (a) set an explicit **maximum session TTL**
(operator-configurable, default ≤ 4h for an RCE-bearing tool); (b) **re-validate**
the session/token on a timer and on every reconnect (`on_mount` in a
`live_session`), tearing down the socket and its dist usage when validation fails;
and (c) document the **maximum window** between IdP revocation/role-change and
effective enforcement (a concrete SLA, e.g. ≤ the re-validation interval), since
server-side sessions do not observe IdP de-provisioning in real time.

**IdP-less path is loopback-only.** A local password/dev-token path remains
available for IdP-less setups (CI, air-gapped), but it must be **refused on a
non-loopback interface in code** (not merely discouraged in docs) — otherwise it
is a bypass of the entire OIDC + RBAC stack. It uses the same session handling
(HttpOnly cookie, no URL token, RBAC applies); it is not a static bearer token.
The **documented default for non-localhost is OIDC**.

**Configuration is read once at boot — never re-entered per start, and not on the
workspace.** OIDC config is a *Phoenix-deployment* concern; the workspace node
knows nothing about OIDC (it only trusts the Phoenix node via the dist cookie), so
`beamtalk workspace create`/restart never touches it. Non-secret config **and the
role map** live in a persistent declarative file, `~/.beamtalk/ide.toml`; the
**client secret** is resolved at boot from env or a `chmod 600` file (never inline,
never logged — ADR 0058 Principle 3). Every key may be **overridden by an env var**
for pure-12-factor / k8s deployments. Phoenix reads the file once in `runtime.exs`;
changing config (including roles) means editing the file + reload, not re-launching
with fresh flags.

```toml
# ~/.beamtalk/ide.toml — persistent; read once at Phoenix boot
[oidc]
issuer            = "https://idp.example.com"     # env override: BT_OIDC_ISSUER
client_id         = "beamtalk-ide"                # env override: BT_OIDC_CLIENT_ID
redirect_uri      = "https://ide.example.com/oidc/callback"
groups_claim      = "groups"                      # which OIDC claim carries group membership
client_secret_env = "BT_OIDC_CLIENT_SECRET"       # secret from env (or client_secret_file = "/run/secrets/…")

[oidc.roles]                                       # IdP group value -> Beamtalk role (Decision 4)
owner    = ["beamtalk-owners", "platform-admins"]
observer = ["beamtalk-observers"]
# unmatched -> DENY (fail-closed)
```

Resolution order per key: **env var → `ide.toml` → error/deny**. A missing file is
fine if every required key is supplied by env (the 12-factor path); a present file
is the "configure once on a host" path. Either way it is set-once, not per-start.

```
# Unauthenticated browser hitting the IDE
GET https://ide.example.com/
→ 302 to https://idp.example.com/authorize?... (OIDC)
→ user authenticates with the org IdP
→ 302 back to /oidc/callback?code=...
→ Phoenix mints session cookie; LiveView mounts with %{user: alice, roles: [...]}
```

**Browser-edge hardening** (standard Phoenix, called out so it is not skipped):
the LiveView socket validates the signed session and a **CSRF token** on connect,
and the OIDC flow uses `state`/PKCE. This subsumes the DNS-rebinding / Origin
concern ADR 0020 §Layer 2 raised for the bare-Cowboy browser path — here the
browser never reaches the workspace's WebSocket directly; it only reaches Phoenix
over HTTPS.

**Session vs. shared state.** RBAC governs *operations*; it does not by itself
isolate data. Per-session bindings are isolated (the spike showed tab-isolated
`x`), but **actor state and the Transcript are workspace-shared** (ADR 0010 / ADR
0017) — so an Observer who can `inspect`/watch Transcript sees shared
actor state and any output a privileged user printed. This is intended for the
shared-image use case; operators must understand "Observer" means "can see the
shared world read-only," not "sees nothing sensitive."

### Decision 2 — Authorization: shared workspace + per-operation RBAC

A workspace may be **shared by multiple authenticated users**, and Phoenix
enforces **per-operation role-based access control** before invoking any dist RPC.
This is a deliberate, scoped **amendment to ADR 0058 Principle 6** (see Migration
Path): single-user-per-workspace remains the rule for the *raw cookie/dist
boundary*; multi-user sharing is permitted **only** behind the Phoenix
authenticated front, where every operation passes an RBAC check tied to an
authenticated identity. The shared secret (dist cookie) never reaches a user;
users hold OIDC identities, and the cookie lives only on the (trusted) Phoenix
and workspace hosts.

Authorization is enforced **in Phoenix**, because it is the **only** place it can
be: the dist layer is all-or-nothing (Decision 3 of the problem framing). The
workspace node does not — and is not asked to — know which human triggered an RPC.

### Decision 3 — Curated op facade (never raw `:rpc` passthrough)

Phoenix exposes a **curated, documented op set** to the browser and invokes
**only** that set on the workspace. It **never** proxies an arbitrary
module/function/args triple from the browser into `:rpc.call/4`. The facade is
the same term-returning op layer the WebSocket edge uses (BT-2399). Indicative
op set (the implementation issue owns the authoritative list):

- **Execute** (RCE-bearing): `eval`, `load-source`, method `save`/`flush`
  (write-surface, ADR 0082), `reload`.
- **Read** (no code injection): `info`, `inspect`, `bindings`, `actors`,
  `sessions`, `complete`, plus the **subscribe-only** push facade
  (transcript / actors / classes / bindings — see below).
- **Admin:** `kill`, `rotate-cookie`.

Note `complete` is **inference-only — it does not evaluate** (ADR 0045: "inference
without evaluation"), so it is safe to grant a non-executing role.

**The push facade is subscribe-only over `SystemAnnouncer` (ADR 0093) — it has no
publish capability.** The discrete push channels (actors / classes / bindings /
flush) are **not** bespoke subscribe/broadcast infrastructure built by this
ADR: since BT-2531 they are subscriptions on the shared typed event bus from ADR
0093 — `SystemAnnouncer current` emits `ActorSpawned` / `ActorStopped` /
`ClassLoaded` / `ClassRemoved` / `BindingChanged` / `FlushCompleted`
`Announcement` subclasses, and the facade (`beamtalk_repl_subscriptions`)
forwards the curated subset to the authenticated browser or dist-attached
LiveView. (Transcript line
streaming stays on its own dedicated `beamtalk_transcript_stream` channel — a
high-volume stream, not a discrete typed event — per ADR 0093 §5.) Per ADR 0093
§6, **subscribing to `SystemAnnouncer` is a read capability**, so the facade
exposes it as a named, RBAC-checked *subscribe* op available to the Observer
role; subscribing to *all* events or to internal/infra announcements is gated
like ADR 0092's `system` scope. **Critically, the facade must never expose a
publish capability** (`announce:`): a remote Observer can listen to the curated
system events but can never emit onto `SystemAnnouncer`, otherwise it could forge
`ActorSpawned` / `ClassRemoved` events and mislead other subscribers. This ADR's
implementation issue owns adding the `subscribe` / `unsubscribe` ops (subscribe-
only) to the authoritative facade op list; there is deliberately no `announce` /
`publish` op in the facade.

The facade is the substrate RBAC hangs off: per-op authorization is only
meaningful against a finite, named vocabulary of ops. It is also the natural
audit point (one structured log line per authorized op per user), closing
ADR 0058's acknowledged "no eval audit trail" gap.

**Honest framing — the facade is not a sandbox.** Any role holding `eval` has
arbitrary Beamtalk and therefore full RCE on the workspace (`Erlang os cmd:`,
unchanged from ADR 0058). The facade's value is enabling a **least-privilege role
that lacks `eval`** — read-only observation (the Observer role) — not
constraining the eval role.

**Honest framing — the facade is Phoenix-side discipline, not a workspace-enforced
wall.** Once attached, the Phoenix node *can* technically `:rpc.call/4` anything;
the curated op set is a rule the Phoenix code keeps, not a structural impossibility
at the dist layer. So a Phoenix-app bug that lets an attacker control the RPC
target (LiveView event injection, a facade logic bug, a compromised Elixir dep)
can bypass RBAC. Two responses: (1) treat Phoenix as a high-value target hardened
like the workspace host (see Consequences); (2) **optional defense-in-depth** — a
workspace-side `beamtalk_authorized_ops` entry module that is the only IDE-facing
exported surface, so a *limited* Phoenix bug cannot reach `beamtalk_repl_shell:eval/2`
directly. Note the honest limit: a *full* Phoenix compromise also yields the dist
cookie, so the attacker can open their own dist connection and the workspace-side
allowlist no longer contains them — it raises the bar against app-level bugs, not
against host compromise.

### Decision 4 — RBAC roles (two levels for v1: Owner + Observer)

**Two built-in roles for v1**, mapped from OIDC claims/groups by Phoenix. The
capability columns (execute / read / admin) are kept explicit so a future middle
rung slots in additively (see Open Questions), but v1 ships exactly the one
boundary that matters — **execute vs. read**:

| Role | execute (eval, load-source, save/flush, reload) | read (inspect, bindings, actors, sessions, transcript, complete) | admin (kill, rotate-cookie) | Use case |
|------|:--:|:--:|:--:|----------|
| **Owner** | ✅ | ✅ | ✅ | Driver(s) who own the image. Full RCE — equivalent to ADR 0058's authenticated user. Multiple Owners are allowed (shared workspace); pair programming = give your pair Owner. |
| **Observer** | ❌ | ✅ | ❌ | Read-only audience (review, teaching, screencasting, monitoring). **Cannot inject or evaluate code.** This is the role the facade makes meaningful. |

**Why two roles, not three.** The only security boundary that actually contains
anything is **execute vs. read** ("Observer vs. anyone-with-eval"). A middle
"Collaborator" (eval but not kill/rotate) would sit on the *execute* side of that
line: a user with `eval`/`load-source` can `Erlang os cmd: "..."` or hot-load a
persistent backdoor (ADR 0082) regardless of whether they can also kill actors,
and rotating the cookie does not even evict already-loaded code. A middle rung
therefore adds policy machinery and a name that *sounds* like containment without
providing any — security theater. v1 draws the line where the trust boundary is.
Reintroducing Collaborator later is **purely additive** (a new role value + op
grants), with no migration.

**Claim→role mapping must fail closed.** The mapping is declarative config — the
`[oidc.roles]` table in `~/.beamtalk/ide.toml` (Decision 1), keyed off the
`groups_claim` — so it is set once and edited in place, not re-specified per start.
It is the single most security-critical configuration step, so the implementation
must: (a) default to **no-access (deny)** when no role claim matches — never a
usability fallback to a privileged role; (b) **raise a startup error** when
`groups_claim` points at a nonexistent claim key, or when the file/env supply no
role map at all (no silent fall-through); and (c) log the **resolved role with the
raw claim values** at session mount for audit.

**Observer read-grant is contingent on read-surface safety.** Granting Observer
the read ops assumes each one triggers **no user code** (per the live-image caveat
below). Because the read-surface (ADR 0085) is still Proposed, the
per-op safety of `inspect`/`sessions` under that constraint must be **signed off as
a Phase 2 acceptance criterion**, not assumed by forward reference.

**Live-image caveat on "read":** Observer cannot inject or run its *own* code, but
in a live image a read can trigger code an *already-privileged* user installed —
e.g. rendering a value through a user-defined `displayString`/`printOn:`. The
Inspector must therefore surface state via builtin field reflection (ADR 0035),
**not** by invoking user display methods, or that rendering path becomes an
execution vector reachable by Observers. This is a no-privilege-escalation risk
(an Observer still cannot run code of *their* choosing), but "read = zero
execution" is not literally true and the read-surface (ADR 0085) implementation
must respect it.

### Decision 5 — Transport hardening: keep distribution off untrusted networks (co-locate)

The governing invariant: **Erlang distribution must never traverse an untrusted
network in cleartext.** We satisfy it by **deployment, not by mandating an
encryption dependency** — because the only hop that *has* to cross an untrusted
network is browser → Phoenix, and that is already secured by HTTPS + OIDC
(Decision 1). The Phoenix → workspace dist link is **internal infrastructure**
and is kept internal:

- **Same host (default, recommended):** Erlang distribution stays on **loopback**
  and never leaves the machine. The only network-facing service is Phoenix's HTTPS
  port. Zero extra config, no certs, no overlay.
  - **epmd must be actively constrained, not assumed.** epmd is a *persistent
    per-user daemon* — a workspace node joins whatever epmd is already running,
    which on many developer machines was started by other Erlang tooling and may
    listen on `0.0.0.0`. So loopback epmd is not automatic. The workspace launcher
    must set **`ERL_EPMD_ADDRESS=127.0.0.1`** in the node's environment (alongside
    the existing allowlisted env in `startup_command.rs`) and **preflight-check**
    for a pre-existing promiscuous epmd, warning/refusing if one is bound to a
    non-loopback interface. The Phase 3 "no public bind" test must assert the
    *running* posture, backed by this enforcement — not just intent.
- **Same trusted private network / VPC:** dist binds to the **private interface**,
  never a public one; epmd is bound to that interface or eliminated (pinned dist
  port / epmdless). This is a standard private-cluster posture — no overlay or
  TLS-dist required.

**No transport-encryption dependency is mandated** — neither a network overlay
(Tailscale/WireGuard) nor TLS distribution. Both are *operator options*, not
platform requirements:

- An operator who *insists* on splitting Phoenix and the workspace across an
  **untrusted** network owns securing that hop. Options, roughly in order of
  preference: **keep them co-located instead**; an SSH tunnel; a network overlay
  (ADR 0020 Layer 3 Option A); or TLS distribution (`inet_tls_dist`, ADR 0020
  Layer 3 Option B). **Note:** the `ssl_dist`/`inet_tls_dist` support referenced
  by ADR 0020 was **removed in PR #1401** and is not currently in the tree; using
  it would require reintroducing it. The platform does not depend on it.

The browser's trust boundary is **always** HTTPS-to-Phoenix; raw distribution is
**never** exposed to it, and by default never leaves the trusted host/network.

#### Cookie management/rotation for the Phoenix↔workspace link

The dist cookie is now an **infrastructure secret** shared between two trusted
hosts, not a user credential. It is provisioned to Phoenix the same way the CLI
gets it today (read from `~/.beamtalk/workspaces/{id}/vm.args`, mode 0600), via
the operator's secret-management path (env var / mounted secret), **never** sent
to a browser. Rotation uses `beamtalk workspace rotate-cookie` (ADR 0020 names
this as future work) coordinated with a Phoenix reconnect; because dist
connections are long-lived, rotation requires a brief reconnect window, not a
live re-key. **Caveat (inherited from ADR 0058):** rotating the cookie does not
evict code already hot-loaded into the workspace; a fully compromised workspace
must be stopped and restarted.

### Worked example

```
# Operator (Owner) deploys the IDE in front of a workspace, co-located:
just web                       # discovers workspace node + cookie (loopback dist)
# → Phoenix serves https://ide.internal:8443, OIDC against the org IdP

# alice (mapped to Owner) signs in via SSO, evaluates:
#   > Counter spawn → live #Actor<Counter,...>  (term, not flattened JSON)

# bob (mapped to Observer) signs in to the SAME workspace:
#   > 3 + 4
#   ✗ Not authorized: role 'observer' may not perform 'eval'
#   (bob still sees the live Transcript, can inspect actors, browse bindings)
```

```
# Error: browser tries to reach an op outside the facade (e.g. crafted request)
→ 403 from Phoenix; nothing reaches dist RPC. Logged:
   level=warn event=facade_reject user=bob op=<unknown> role=observer
```

## Prior Art

### Livebook (Elixir)
The closest analogue: a Phoenix app that attaches to BEAM runtimes over Erlang
distribution. Livebook started with a **single URL token** (Jupyter-style) and
added **password auth** and then **Livebook Teams** (SSO, audit, per-user
identity) for shared/cloud deployments. Its runtime-attach model is exactly our
Attach topology. **What we adopt:** Phoenix-as-front over dist; the
authenticated-app-in-front-of-a-trusted-runtime shape. **What we adapt:** we go
to OIDC directly for non-localhost (skipping the URL-token stage, which the spike
flags as leakage-prone) and we add an explicit per-op RBAC facade rather than
all-or-nothing runtime access.

### Jupyter / JupyterHub
Single-user Jupyter uses a loopback token (full execution to whoever holds it).
**JupyterHub** is the multi-user story: an authenticating proxy (OAuth/OIDC) in
front, spawning a **per-user single-user server**. That is the *workspace-per-user*
shape — the alternative we considered and rejected for v1 in favor of shared +
RBAC (see Alternatives). **What we adopt:** the authenticating-proxy-in-front
pattern. **What differs:** we allow a *shared* workspace with role separation,
which JupyterHub does not (it isolates per user).

### Erlang/OTP distribution + epmd
`inet_tls_dist` provides mTLS for distribution (OTP 18+), battle-tested in
RabbitMQ/CouchDB; ADR 0020 Layer 3 Option B specced it, but it was **removed in
PR #1401** and is not in the tree. epmd-as-network-service is a known exposure;
the modern guidance is loopback epmd or epmdless dist with pinned ports.
**What we adopt:** the **loopback/private-interface + loopback-epmd posture** as
the default — keeping distribution off untrusted networks entirely (Decision 5).
**What we reject:** mandating `inet_tls_dist` — it is an *operator option* for the
unusual untrusted-split case (and would require reintroducing the removed support),
not a platform requirement. Keeping dist internal is cheaper and needs no certs.

### nREPL (Clojure) / "trusted dev tool" REPLs
nREPL is explicit: a network REPL grants full environment access; protect it with
loopback + SSH tunnel. This is the ADR 0058 stance. **What we adopt:** the honest
framing that `eval` = full access; the facade does not pretend otherwise. **What
we add over nREPL:** an authenticated front and non-eval roles, so "watch a
session" doesn't require granting execution.

### Tailscale / WireGuard
Identity-bearing encrypted overlays used by Gitpod, VS Code tunnels, Grafana for
remote dev. **What we adopt:** an accepted alternative to TLS-dist for the
cross-host link (ADR 0020 Layer 3 Option A) — the overlay carries dist; identity
is the overlay's.

## User Impact

### Newcomer
No change for the common case. `beamtalk repl --web` on localhost stays
zero-config (cookie handshake, no OIDC). OIDC/RBAC only appears when someone
deliberately deploys the IDE for remote access — at which point the newcomer is
typically a *consumer* of a URL their team set up, and SSO is the login flow they
already know from every other internal tool.

### Smalltalk developer
The shared-workspace + Owner/Observer roles bring something Smalltalk
images never had cleanly: multiple people looking at **one live image** with
differentiated rights — a driver, a navigator, and a read-only audience watching
the Transcript and Inspector update live. This is the Smalltalk "shared world"
dream made safe to expose.

### Erlang/BEAM developer
Recognizable: Phoenix + dist attach is the Livebook pattern; `ssl_dist` and
loopback epmd are standard OTP hygiene. The one thing to internalize: the
workspace node trusts the *Phoenix node*, not the human — per-human authz is a
Phoenix concern by construction, because dist can't do it.

### Production operator
A real deployment story: an authenticating front (OIDC) they can wire to existing
SSO, a dist link they can reason about (co-located/loopback by default, or bound
to a private interface — never public, no overlay or cert dependency imposed), an
audit trail of authorized ops, and a read-only Observer role to hand out
without granting code execution. The honest caveats are documented up front:
keeping dist off untrusted networks is operator-enforced (not code-enforced); and
the Owner role holds full RCE — only Observer is non-executing.

### Tooling developer
The curated facade is a stable, documented contract (term tuples / `#beamtalk_error{}`)
that LSP, MCP, and the LiveView IDE all share — the same surface, with RBAC
layered on at the Phoenix edge. No new wire protocol to learn.

## Steelman Analysis

### Alternative A: Workspace-per-user (JupyterHub model) — *not chosen*
| Cohort | Strongest argument for per-user |
|--------|--------------------------------|
| 🏭 **Operator** | "Isolation by construction. No shared-secret-shared-state. A compromised session blasts only that user's own workspace — exactly ADR 0058 unchanged. No new authz engine to get wrong." |
| ⚙️ **BEAM veteran** | "One node per user is the OTP-natural blast-radius boundary; JupyterHub proved it scales." |
| 🎩 **Smalltalk purist** | "Each developer owns their image. That *is* the Smalltalk model — sharing was always ad hoc." |
| 🧑‍💻 **Newcomer** | "My workspace is mine; nothing another user does can touch my state." |
| 🎨 **Language designer** | "It needs zero amendment to ADR 0058 — the cleanest story." |

**Why we still chose shared + RBAC:** the IDE's headline value (ADR 0017) is the
*shared live image* — pair programming, teaching, a watched Transcript. Per-user
isolation forecloses that. RBAC + the facade lets us offer sharing *and* a
least-privilege ladder (Observer needs no RCE), which per-user can't express
within one image. Per-user remains available (an operator can simply run one
workspace per user and grant each Owner) — it's a deployment choice, not a
foreclosed one.

### Alternative B: Full `:rpc` passthrough (no facade) — *rejected*
- **Best argument (BEAM veteran):** "Attach's whole elegance is location-transparent
  RPC; a facade re-introduces the narrow-op tax the spike said we'd escaped."
- **Counter:** with shared multi-user access, passthrough hands every browser the
  all-or-nothing dist power directly — the exact thing the spike warns against. And
  there'd be nothing to attach per-op RBAC to. The facade is the same op layer the
  WS edge already maintains, so the "tax" is largely pre-paid (BT-2399).

### Alternative C: Session-token / password auth (Livebook v1 style) — *rejected for non-localhost default*
- **Best argument (newcomer/solo):** "An OIDC dependency is heavy for one developer
  who just wants their laptop workspace on a tablet."
- **Counter:** that user is served by localhost + cookie (unchanged) or the
  IdP-less dev-token flag. The moment access is *remote and multi-user*, SSO is the
  right primitive, and URL tokens leak (history/Referer/screen-share). We keep a
  password/dev-token path for air-gapped/CI, but the documented remote default is
  OIDC.

### Tension points
- **Operators split:** isolation-minded operators prefer per-user (A); collaboration-
  minded teams want shared + RBAC. We make shared the *capability* and per-user a
  *deployment option*, so both are reachable.
- **BEAM veterans** dislike the facade's narrowing but concede it's the only RBAC
  substrate and is already built (BT-2399).
- **Security purists** note (correctly) that anyone with `eval` = RCE, so RBAC's
  real boundary is Observer-vs-execute. We state this plainly (and chose a two-role
  model that draws exactly that line) rather than overselling the facade.

## Alternatives Considered

### Workspace-per-user isolation (JupyterHub model)
Phoenix authenticates the user, then attaches to / spawns **that user's own**
workspace; no node is ever shared. **Rejected as the default** because it
forecloses the shared-live-image experience that motivates the IDE. **Retained as
a deployment option** — operators wanting strict isolation run one workspace per
user, each user an Owner.

### Full RPC passthrough (no permission facade)
Phoenix forwards arbitrary `{module, function, args}` from the browser to
`:rpc.call/4`. **Rejected:** re-exposes all-or-nothing dist power to the browser
trust boundary and leaves nothing for per-op RBAC to govern.

### Token / password auth as the remote default
URL token (Jupyter/Livebook v1) or password. **Rejected for non-localhost
default** (leakage class; multi-user wants real identity). **Kept** as an
IdP-less flag for CI/air-gapped and as the unchanged localhost story.

### Enforce authz in the workspace node instead of Phoenix
Push per-user checks down to the workspace. **Rejected:** dist is all-or-nothing
and the workspace cannot know which human triggered an RPC without inventing a
parallel identity channel — which is exactly what putting Phoenix in front avoids.

### Expose raw distribution to the browser
**Rejected outright** (the spike's central warning): the browser trust boundary
must never sit on Erlang distribution. epmd + unencrypted dist + all-or-nothing
cookie is not a browser-facing boundary.

### Separate / WebSocket-only topology (browser → Phoenix WS → workspace WS, never dist)
Distinct from "raw dist to browser": here Phoenix talks to the workspace over the
**WebSocket op protocol** instead of attaching over distribution, so a Phoenix
compromise does **not** immediately yield full `:rpc` on the workspace — it
narrows the blast radius. **Not chosen as the default** (it forfeits Attach's
location-transparent term access — the reason BT-2394 picked Attach, and re-imposes
the duplicate-every-op tax the spike called out). **But documented as a
deployment-tier option:** for a Phoenix front facing an **untrusted internet edge**
(not just an org intranet), operators should consider the Separate topology to cap
Phoenix-compromise blast radius. Attach + facade is the right call for the
intranet/trusted-front common case this ADR targets; the internet-facing tier is a
conscious, documented downgrade in convenience for a smaller blast radius.

### Mandate always-on TLS distribution (Phoenix↔workspace, every deployment)
Require `inet_tls_dist` on the back link unconditionally, for one uniform mode.
**Rejected:** (1) the `ssl_dist`/`inet_tls_dist` support was **removed in PR #1401**
and is not in the tree — mandating it reintroduces deleted machinery against the
project's stated direction; (2) it adds cert lifecycle (generation, rotation,
expiry — explicitly deferred by ADR 0020) to the common co-located solo case; (3)
the motivating intuition that it "helps Windows" does not hold — in the remote
topology the Windows machine is a browser talking HTTPS to Phoenix and never
participates in dist (ADR 0017: remote browser access implies a Linux/macOS-hosted
workspace), and TLS cert/key files share the same filesystem-ACL weakness as the
cookie. The uniformity benefit is captured instead by the Decision 5 invariant
("dist never crosses an untrusted network in cleartext"), satisfied by deployment.

### Mandate a network overlay (Tailscale/WireGuard) for the back link
Require an overlay to carry dist between Phoenix and the workspace.
**Rejected:** an overlay is a heavy operational dependency to impose on every
deployment, and ADR 0020 itself frames overlays as "convenience, not dependency."
Co-locating (loopback / private interface) needs no overlay at all. Overlays
remain an *operator option* for the untrusted-split case, never a requirement.

## Consequences

### Positive
- A complete, opt-in remote story: OIDC front, hardened dist link, audited curated
  ops, least-privilege roles — composed from primitives ADR 0020 already defines.
- Shared live image with role separation: pair programming and read-only audiences
  without granting everyone RCE.
- Closes ADR 0058's "no eval audit trail" gap at the facade.
- Localhost dev is completely unaffected (zero-config preserved).
- No workspace runtime changes for the topology; auth/authz live in Phoenix + config.

### Negative
- **Amends ADR 0058 Principle 6** — shared multi-user workspaces now exist (behind
  Phoenix). The shared-secret-shared-state risk is real; it is mitigated, not
  eliminated, by RBAC + the trusted front.
- **The Owner role is full RCE.** RBAC meaningfully constrains only Observer. The
  facade is not a sandbox; the `eval`-holding role can do anything ADR 0058 allows.
- **Phoenix becomes a high-value target.** It holds the dist cookie and full RPC
  power; a Phoenix compromise = workspace compromise. Its host must be trusted and
  patched like the workspace host.
- **OIDC operational surface** — IdP config, client secrets, callback URLs, group→role
  mapping — is real setup cost for operators (acceptable: it's opt-in, remote-only).
- **The co-locate invariant is operator-enforced, not code-enforced.** Nothing in
  the platform *prevents* an operator from splitting Phoenix and the workspace
  across an untrusted network with bare dist; the safe posture (same host/private
  network, loopback/private epmd, never public) is the documented default and the
  binding's default, but a determined misconfiguration can still expose dist.
- **No built-in encrypted cross-host dist.** `inet_tls_dist` was removed (PR #1401);
  an operator who genuinely needs Phoenix and the workspace on different untrusted
  hosts must reintroduce TLS-dist or supply their own tunnel. We accept this rather
  than mandate an overlay/cert dependency on every deployment.
- **RBAC is two-level (execute vs. read).** This is deliberate — it is the only
  boundary that contains anything (see Decision 4). A finer middle rung
  (Collaborator) and per-op/per-resource policy are deferred, additive future work;
  until then there is no "eval but limited" tier.
- **Authorization can lag IdP state.** Server-side sessions do not observe IdP
  revocation/role-change in real time; enforcement lags by up to the configured
  re-validation interval / session TTL (Decision 1). For an RCE-bearing tool this
  window must be kept short and documented.
- **The facade is Phoenix-side discipline.** A Phoenix-app bug can bypass RBAC; the
  optional workspace-side allowlist mitigates app-level bugs but not host compromise
  (which yields the cookie). Phoenix must be operated as a high-value host.

### Neutral
- Per-user isolation remains achievable as a deployment pattern (one workspace per
  Owner) — this ADR makes sharing *possible*, not *mandatory*.
- The facade is the existing BT-2399 op layer; this ADR adds RBAC + audit at its
  Phoenix edge, not a new protocol.
- Cookie rotation semantics (brief reconnect window) follow ADR 0020's existing
  rotate-cookie plan.
- The read-surface ops lean on ADR 0085 (still **Proposed**), but the facade
  *contract* reuses BT-2399 (shipped), so the dependency is soft — RBAC and the
  facade can land before 0085's richer read views do.

## Implementation

Lands as **BT-2411 (LiveView IDE Wave 5)**, gated on this ADR. Phased:

### Phase 1 — Authenticated front (OIDC) + session-bound LiveView
- OIDC client in `editors/liveview` (issuer/client config via runtime env;
  `state`/PKCE).
- Server-side session, `HttpOnly; Secure; SameSite=Strict` cookie; LiveView mounts
  with `%{user, roles}`. No token in URL. CSRF token validated on socket connect.
- **Session lifecycle:** max session TTL (default ≤ 4h); `on_mount` re-validation
  on a timer and on reconnect; socket teardown on validation failure (Decision 1).
- IdP-less dev-token path **refused on non-loopback interfaces in code**.
- **Components:** `editors/liveview/lib/**` (auth plug/pipeline, `live_session`).
- **Tests:** unauthenticated → redirect; authenticated mount carries identity;
  expired/revoked session tears down an already-mounted socket; dev-token rejected
  on a non-loopback bind.

### Phase 2 — Curated op facade + RBAC
- Facade module: only the documented op set reaches dist RPC; the browser cannot
  supply an arbitrary `{m,f,a}` (enforced Phoenix-side — see the honest-framing
  note in Decision 3). **Optional defense-in-depth:** workspace-side
  `beamtalk_authorized_ops` entry module so app-level Phoenix bugs cannot reach
  `beamtalk_repl_shell:eval/2` directly.
- RBAC: map OIDC claims/groups → Owner/Observer; **fail closed**
  (no match → deny; bad claim path → startup error); enforce per op before RPC;
  structured audit log per authorized op (`user`, `op`, `role`, raw claims at mount).
- **Read-surface sign-off (acceptance criterion):** confirm each Observer-granted
  read op triggers no user code under ADR 0085.
- **Components:** `editors/liveview/lib/**` (facade + policy + audit); optionally
  `runtime/apps/beamtalk_workspace/src/beamtalk_authorized_ops.erl`.
- **Tests:** Observer denied `eval`/`load-source`/`save`/`kill`; Observer allowed
  read ops; Owner allowed all; facade rejects off-list ops with 403 and no dist
  call; unmatched claim → deny (not a default role).

### Phase 3 — Transport posture (keep dist internal)
- **Co-locate default:** `just web` discovers a loopback-dist workspace. Set
  `ERL_EPMD_ADDRESS=127.0.0.1` for the workspace node (in `startup_command.rs`'s
  allowlisted env) and **preflight-check** for a pre-existing promiscuous epmd,
  warning/refusing on a non-loopback bind. Verify dist never leaves the host.
- **Private-network deployment:** document binding dist + epmd to the private
  interface (never `0.0.0.0`); pin/eliminate the epmd port.
- **No mandated encryption dependency:** document the untrusted-split case as
  operator-owned (prefer co-locate; SSH tunnel / overlay / reintroduced
  `inet_tls_dist` as their choice). Do **not** add an overlay or cert dependency
  to the default path.
- Cookie provisioning to Phoenix via secret management; `rotate-cookie` + reconnect.
- **Components:** workspace dist/epmd bind config
  (`crates/beamtalk-cli/src/commands/workspace/**`, `vm.args` generation),
  `Justfile`/release recipe, deployment docs.
- **Tests:** loopback/private-only dist + epmd posture check (assert no public
  bind in the default deployment).

### Phase 4 — Docs
- Extend `docs/security/threat-model.md` (ADR 0058 Phase 0) with the remote/Phoenix
  trust path and the Owner/Observer (execute-vs-read) caveat.
- Non-localhost deployment guide; update `docs/development/surface-parity.md` if the
  facade op set is surfaced anywhere user-visible.

## Migration Path

This ADR is **additive** for existing users: localhost `beamtalk repl --web` and
the CLI cookie path are unchanged. No migration is required to keep working
locally.

**Amendment to ADR 0058 Principle 6.** Principle 6 currently reads that a
workspace is single-user and sharing a workspace between developers "collapses the
auth boundary to a shared secret." This ADR narrows that rule rather than
overturning it:

- **Raw boundary (cookie/dist) stays single-user.** Anyone holding the cookie is
  fully trusted and unaudited — unchanged. Do not share the cookie.
- **Authenticated front (Phoenix) may be multi-user.** Multiple OIDC-authenticated
  users may share one workspace **only** through Phoenix, where every operation is
  RBAC-checked and audited against a real identity and the dist secret never
  reaches a user.

A one-line note will be added to ADR 0058 pointing to this ADR for the remote
multi-user case. ADR 0058's "Trusted Developer Tool" stance is otherwise intact:
the Owner (execute) role still has full RCE; only Observer is non-executing.

## Implementation Tracking

**Epic:** BT-2416 — Remote Workspace Access — Phoenix Authenticated Front (ADR 0091)
**Implements:** ADR 0017 Phase 3 / LiveView IDE Wave 5 (BT-2411 umbrella)
**Status:** Planned

| Phase | Issue | Title | Size | Blocked by |
|-------|-------|-------|------|------------|
| 1 | BT-2417 | OIDC authentication flow | M | — |
| 1 | BT-2419 | Session lifecycle + browser-edge hardening | M | BT-2417 |
| 2 | BT-2420 | Curated op facade (no raw `:rpc` passthrough) | M | BT-2417 |
| 2 | BT-2421 | Two-level RBAC (Owner/Observer) + fail-closed mapping + audit | M | BT-2419, BT-2420 |
| 3 | BT-2418 | epmd/dist loopback hardening | M | — |
| 3 | BT-2422 | Non-localhost deployment recipe + cookie provisioning | M | BT-2417, BT-2418 |
| 4 | BT-2423 | Security docs: remote trust path + deployment guide | S | BT-2421, BT-2422 |
| 4 | BT-2424 | End-to-end auth-boundary test | M | BT-2421, BT-2418 |

Deferred (ADR Open Questions): workspace-side `beamtalk_authorized_ops` allowlist;
middle "Collaborator" RBAC rung; reintroducing `inet_tls_dist` (removed PR #1401).

## Open Questions
- **When (if ever) to introduce a middle "Collaborator" rung.** v1 is two-level
  (Decision 4). A future Collaborator would only be meaningful if it sits on the
  *read* side of the execute boundary (e.g. "eval into a scratch session but cannot
  `load-source`/`save` persistent code") — i.e. a genuinely smaller capability, not
  just "eval minus kill." Revisit once the read-surface (ADR 0085) settles which
  ops trigger user code, and only if a real use case demands an intermediate tier.
- **Workspace-side `beamtalk_authorized_ops` allowlist — ship in v1 or defer?**
  Cheap defense-in-depth against Phoenix app-bugs; does not contain host compromise.

## References
- Related issues: BT-2400 (this ADR), BT-2411 (Wave 5 — auth + non-localhost),
  BT-2398 (LiveView IDE epic, ADR 0017 Phase 3), BT-2394 (Attach spike),
  BT-2399 (term-op layer + push facade)
- Related ADRs: [ADR 0020 — Connection Security](0020-connection-security.md)
  (extends), [ADR 0058 — Platform Security Model](0058-platform-security-model.md)
  (amends Principle 6), [ADR 0017 — Browser Connectivity](0017-browser-connectivity-to-running-workspaces.md)
  (Phase 3), [ADR 0085 — Editor Live-Image Representation](0085-editor-live-image-representation.md)
  (read-surface), [ADR 0082 — Method-Level Edit and Save](0082-method-level-edit-save-and-changelog.md)
  (write-surface), [ADR 0093 — Announcements (Typed Event Substrate)](0093-announcements-event-substrate.md)
  (the push facade is a **subscribe-only** view of `SystemAnnouncer`; this ADR's
  facade op list owns the `subscribe`/`unsubscribe` ops and exposes **no** publish
  capability — ADR 0093 §6)
- Documentation: `docs/research/phoenix-topology-spike.md` ("Auth implications"),
  [REPL Protocol](../repl-protocol.md)
- Erlang `ssl_dist`: https://www.erlang.org/doc/apps/ssl/ssl_distribution.html
- OpenID Connect: https://openid.net/specs/openid-connect-core-1_0.html
- Livebook security: https://github.com/livebook-dev/livebook#security
- JupyterHub authenticators: https://jupyterhub.readthedocs.io/en/stable/reference/authenticators.html
