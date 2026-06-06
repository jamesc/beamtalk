# ADR 0091: Connection Security for Remote Workspace Access — Phoenix as Authenticated Front

## Status
Proposed (2026-06-06)

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
loopback-by-default binding, the cookie handshake, mTLS for distribution via OTP
`ssl_dist` (Layer 3 Option B), and overlay networks (Layer 3 Option A). ADR 0058
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
5. **Don't roll our own crypto** (ADR 0020 Principle 2) — OIDC libraries, OTP
   `ssl_dist`, and reverse-proxy TLS are the sanctioned primitives.

## Decision

Remote (non-localhost) workspace access uses **Phoenix as the authenticated
front**, with a three-segment trust path and a per-operation authorization facade.
Five sub-decisions:

```
┌─────────┐  ① HTTPS + OIDC   ┌──────────────┐  ② TLS-dist (when    ┌─────────────┐
│ Browser │ ───────────────── │   Phoenix    │     crossing hosts)  │  Workspace  │
│ (user)  │  session cookie   │  LiveView    │ ──────────────────── │  BEAM node  │
└─────────┘  (HttpOnly,       │  node        │  ③ curated op facade  │  (eval, RCE)│
             Secure,          │  ④ RBAC      │     + RBAC, never raw │             │
             SameSite)        └──────────────┘     :rpc passthrough  └─────────────┘
                              Phoenix is the ONLY node on the dist mesh
```

### Decision 1 — Authentication: OIDC/SSO at the Phoenix front

The browser authenticates to Phoenix via **OIDC** (OpenID Connect) against an
external identity provider (Google, Okta, Entra ID, Keycloak, …). On successful
OIDC exchange, Phoenix establishes a **server-side session** carried in an
`HttpOnly; Secure; SameSite=Lax` cookie. The LiveView socket inherits that
session — no token in the URL (avoids the Jupyter/Livebook URL-token leakage
class: browser history, Referer, screen-sharing).

OIDC from the start (rather than starting with passwords/phx.gen.auth) is chosen
because the remote deployment story is inherently a team/operator story: the
people who need non-localhost access already have an IdP, and SSO is where
multi-user lands anyway. A local password/dev-token path remains available behind
a config flag for IdP-less setups (CI, air-gapped), but the **documented default
for non-localhost is OIDC**.

```elixir
# editors/liveview/config/runtime.exs (illustrative)
config :bt_ide, :oidc,
  issuer: System.fetch_env!("BT_OIDC_ISSUER"),
  client_id: System.fetch_env!("BT_OIDC_CLIENT_ID"),
  client_secret: System.fetch_env!("BT_OIDC_CLIENT_SECRET"),
  redirect_uri: System.fetch_env!("BT_OIDC_REDIRECT_URI")
```

```
# Unauthenticated browser hitting the IDE
GET https://ide.example.com/
→ 302 to https://idp.example.com/authorize?... (OIDC)
→ user authenticates with the org IdP
→ 302 back to /oidc/callback?code=...
→ Phoenix mints session cookie; LiveView mounts with %{user: alice, roles: [...]}
```

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
the same term-returning op layer the WebSocket edge uses (BT-2399) —
`eval`, `complete`, `info`, `inspect`, `bindings`, `actors`, `sessions`,
`load-source`, `reload`, `kill`, plus the push-subscription facade
(transcript / actors / classes / bindings / flush).

The facade is the substrate RBAC hangs off: per-op authorization is only
meaningful against a finite, named vocabulary of ops. It is also the natural
audit point (one structured log line per authorized op per user), closing
ADR 0058's acknowledged "no eval audit trail" gap.

**Honest framing — the facade is not a sandbox.** Any role holding `eval` has
arbitrary Beamtalk and therefore full RCE on the workspace (`Erlang os cmd:`,
unchanged from ADR 0058). The facade's value is enabling **least-privilege roles
that lack `eval`** — read-only observation, or write-without-admin — not
constraining the eval role.

### Decision 4 — RBAC roles

Three built-in roles, mapped from OIDC claims/groups by Phoenix:

| Role | eval / load-source | read (inspect, bindings, actors, sessions, transcript) | admin (kill, flush/reload, rotate) | Use case |
|------|:--:|:--:|:--:|----------|
| **Owner** | ✅ | ✅ | ✅ | The workspace owner / driver. Full RCE — equivalent to ADR 0058's authenticated user. |
| **Collaborator** | ✅ | ✅ | ❌ | Pair programming. Can evaluate and read, cannot kill actors, flush code, or rotate secrets. |
| **Observer** | ❌ | ✅ | ❌ | Read-only audience (review, teaching, monitoring). **No code execution.** This is the role the facade makes meaningful. |

Roles are coarse and few on purpose; a finer policy engine is explicitly future
work (see Consequences). The Observer↔Collaborator↔Owner ladder is the minimum
that makes "shared workspace" safe to offer.

### Decision 5 — Transport hardening: co-locate by default, TLS-dist when remote

- **Default — co-locate.** Phoenix and the workspace run on the **same trusted
  host** (or the same private/overlay network), and Erlang distribution stays on
  **loopback** — it never leaves the host. epmd is bound to loopback. The only
  network-facing service is Phoenix's HTTPS port. This is the simplest secure
  deployment and the recommended one.
- **When the link must cross hosts — TLS distribution.** If Phoenix and the
  workspace are on different hosts across an untrusted network, the dist link
  **must** use `inet_tls_dist` (ADR 0020 Layer 3 Option B), and epmd exposure
  must be eliminated (ERL_DIST_PORT / `-erl_epmd_port` pinning, or epmdless dist).
  A network overlay (Tailscale/WireGuard, ADR 0020 Layer 3 Option A) is an
  accepted alternative that carries dist over an encrypted, identity-bearing
  tunnel.

The browser's trust boundary is **always** HTTPS-to-Phoenix; raw distribution is
**never** exposed to it.

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

### Erlang/OTP `ssl_dist` + epmd
`inet_tls_dist` provides mTLS for distribution (OTP 18+), battle-tested in
RabbitMQ/CouchDB. epmd-as-network-service is a known exposure; the modern
guidance is loopback epmd or epmdless dist with pinned ports. **What we adopt:**
`ssl_dist` for cross-host links and loopback/epmdless posture — straight from
ADR 0020 Layer 3, now made mandatory for remote Attach.

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
The shared-workspace + Observer/Collaborator roles bring something Smalltalk
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
SSO, a hardened dist link they can reason about (co-located/loopback by default,
TLS-dist or overlay when crossing hosts), an audit trail of authorized ops, and
roles to hand out read-only access without granting RCE. The honest caveat is
documented up front: Owner/Collaborator = RCE; only Observer is non-execution.

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
- **Security purists** note (correctly) that Owner/Collaborator = RCE, so RBAC only
  truly contains Observer. We state this plainly rather than overselling the facade.

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
- **Owner/Collaborator roles are full/near-full RCE.** RBAC meaningfully constrains
  only Observer. The facade is not a sandbox; an `eval`-holding role can do anything
  ADR 0058 allows.
- **Phoenix becomes a high-value target.** It holds the dist cookie and full RPC
  power; a Phoenix compromise = workspace compromise. Its host must be trusted and
  patched like the workspace host.
- **OIDC operational surface** — IdP config, client secrets, callback URLs, group→role
  mapping — is real setup cost for operators (acceptable: it's opt-in, remote-only).
- **Cross-host TLS-dist adds cert management** (mitigated by co-locate-by-default,
  where dist never leaves loopback).
- **RBAC is coarse (3 roles).** Finer per-op/per-resource policy is future work.

### Neutral
- Per-user isolation remains achievable as a deployment pattern (one workspace per
  Owner) — this ADR makes sharing *possible*, not *mandatory*.
- The facade is the existing BT-2399 op layer; this ADR adds RBAC + audit at its
  Phoenix edge, not a new protocol.
- Cookie rotation semantics (brief reconnect window) follow ADR 0020's existing
  rotate-cookie plan.

## Implementation

Lands as **BT-2411 (LiveView IDE Wave 5)**, gated on this ADR. Phased:

### Phase 1 — Authenticated front (OIDC) + session-bound LiveView
- OIDC client in `editors/liveview` (issuer/client config via runtime env).
- Server-side session, `HttpOnly; Secure; SameSite` cookie; LiveView mounts with
  `%{user, roles}`. No token in URL.
- IdP-less dev-token flag for CI/air-gapped.
- **Components:** `editors/liveview/lib/**` (auth plug/pipeline), config.
- **Tests:** unauthenticated → redirect; authenticated mount carries identity.

### Phase 2 — Curated op facade + RBAC
- Facade module: only the documented op set reaches dist RPC; arbitrary
  `{m,f,a}` from the browser is impossible by construction.
- RBAC: map OIDC claims/groups → Owner/Collaborator/Observer; enforce per op
  before RPC; structured audit log per authorized op (`user`, `op`, `role`).
- **Components:** `editors/liveview/lib/**` (facade + policy + audit).
- **Tests:** Observer denied `eval`; Collaborator denied `kill`/`flush`; Owner
  allowed all; facade rejects off-list ops with 403 and no dist call.

### Phase 3 — Transport hardening
- **Co-locate default:** `just web` discovers a loopback-dist workspace; epmd
  bound to loopback. Document that dist never leaves the host.
- **Cross-host:** wire `inet_tls_dist` for the Phoenix↔workspace link (reuse
  ADR 0020 Layer 3 cert plumbing); pin/eliminate epmd port; document overlay
  (Tailscale/WireGuard) as the alternative.
- Cookie provisioning to Phoenix via secret management; `rotate-cookie` + reconnect.
- **Components:** workspace dist/TLS config
  (`crates/beamtalk-cli/src/commands/workspace/**`, `vm.args` generation),
  `Justfile`/release recipe, deployment docs.
- **Tests:** TLS-dist smoke test (Phoenix↔workspace across hosts); loopback-epmd
  posture check.

### Phase 4 — Docs
- Extend `docs/security/threat-model.md` (ADR 0058 Phase 0) with the remote/Phoenix
  trust path and the Owner/Collaborator/Observer caveat.
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
Owner/Collaborator roles still have full RCE; only Observer is non-executing.

## References
- Related issues: BT-2400 (this ADR), BT-2411 (Wave 5 — auth + non-localhost),
  BT-2398 (LiveView IDE epic, ADR 0017 Phase 3), BT-2394 (Attach spike),
  BT-2399 (term-op layer + push facade)
- Related ADRs: [ADR 0020 — Connection Security](0020-connection-security.md)
  (extends), [ADR 0058 — Platform Security Model](0058-platform-security-model.md)
  (amends Principle 6), [ADR 0017 — Browser Connectivity](0017-browser-connectivity-to-running-workspaces.md)
  (Phase 3), [ADR 0085 — Editor Live-Image Representation](0085-editor-live-image-representation.md)
  (read-surface), [ADR 0082 — Method-Level Edit and Save](0082-method-level-edit-save-and-changelog.md)
  (write-surface)
- Documentation: `docs/research/phoenix-topology-spike.md` ("Auth implications"),
  [REPL Protocol](../repl-protocol.md)
- Erlang `ssl_dist`: https://www.erlang.org/doc/apps/ssl/ssl_distribution.html
- OpenID Connect: https://openid.net/specs/openid-connect-core-1_0.html
- Livebook security: https://github.com/livebook-dev/livebook#security
- JupyterHub authenticators: https://jupyterhub.readthedocs.io/en/stable/reference/authenticators.html
