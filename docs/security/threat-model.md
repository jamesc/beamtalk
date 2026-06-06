# Beamtalk Threat Model

This document is the security-facing companion to
[`docs/beamtalk-security.md`](../beamtalk-security.md) (component-level design).
It states the trust boundaries, what each is assumed to contain, and the honest
caveats — for both the default localhost posture and the remote authenticated
front.

Related ADRs: [ADR 0020 — Connection Security](../ADR/0020-connection-security.md),
[ADR 0058 — Platform Security Model](../ADR/0058-platform-security-model.md),
[ADR 0091 — Remote Workspace Access](../ADR/0091-remote-workspace-access-phoenix-authenticated-front.md).
Deployment recipe: [`docs/deployment/remote-liveview-ide.md`](../deployment/remote-liveview-ide.md).

## Stance: Trusted Developer Tool

Beamtalk is a **Trusted Developer Tool** (ADR 0058). A connected REPL/workspace
client can evaluate arbitrary Beamtalk, which is arbitrary code execution (RCE)
on the workspace host (`Erlang os cmd: "…"`). There is **no sandbox**: anyone who
can `eval` has full power as the workspace owner. The security model is about
*who can reach the eval boundary*, not about constraining eval.

## Default posture: localhost only

| Boundary | Transport | Protection |
|----------|-----------|------------|
| Browser / client ↔ workspace | `127.0.0.1` TCP / Unix socket | Loopback only; filesystem perms; dist cookie |

By default every interactive component binds loopback and requires no
authentication — only local processes can connect, the same model as `erl`,
`iex`, `node`. A workspace is **single-user** (ADR 0058 Principle 6). Do not put
raw distribution or the REPL port on a non-loopback interface.

## Remote posture: Phoenix as the authenticated front (ADR 0091)

Taking the LiveView IDE **past localhost** introduces exactly one untrusted hop
and terminates it at Phoenix:

```
   UNTRUSTED NETWORK              TRUSTED HOST / PRIVATE NETWORK
┌─────────┐  ① HTTPS + OIDC  ┌──────────────┐  ② internal dist  ┌─────────────┐
│ Browser │ ───────────────▶ │   Phoenix    │ ────────────────▶ │  Workspace  │
│ (user)  │  session cookie  │  ③ facade    │  (loopback /      │  BEAM node  │
└─────────┘  (HttpOnly,      │  ④ RBAC      │   private iface,  │  (eval, RCE)│
             Secure, Strict)  └──────────────┘   never public)   └─────────────┘
```

### ① Browser ↔ Phoenix — the only untrusted boundary

- **HTTPS + OIDC.** Users authenticate via an external IdP (OIDC
  authorization-code + `state` + PKCE + `nonce`). No password store, no URL
  token (avoids the Jupyter/Livebook URL-leakage class: history, Referer,
  screen-share).
- **Server-side session.** Verified claims live in a signed, `HttpOnly`,
  `Secure`, `SameSite=Strict` cookie. The only Lax cookie is the short-lived,
  encrypted, `path=/oidc` handshake cookie used solely for the IdP redirect.
- **Lifecycle.** A maximum session TTL (default ≤ 4h) plus re-validation on a
  timer (default ≤ 60s) and on every reconnect; an expired/revoked session
  tears down the live socket. **Enforcement lag:** server-side sessions do not
  observe IdP de-provisioning in real time — the window between an IdP
  revocation/role change and effective teardown is bounded by the re-validation
  interval (default ≤ 60s), and for a never-reconnecting socket by the TTL
  (default ≤ 4h). Tighten both for a stricter SLA.
- **IdP-less dev path.** A local dev-auth path exists for CI/air-gapped use; it
  uses the same session handling and is **refused on a non-loopback interface in
  code**, so it cannot become a remote bypass of OIDC + RBAC.

### ② Phoenix ↔ workspace — internal, never public

Erlang distribution is **never** placed on an untrusted network. By default
Phoenix and the workspace are co-located and dist stays on loopback; on a
private network, dist + epmd bind the private interface, never `0.0.0.0`. epmd
is actively constrained (`ERL_EPMD_ADDRESS=127.0.0.1`) and the launcher warns if
a pre-existing epmd is promiscuous. The dist **cookie is an infrastructure
secret** shared between the two trusted hosts and provisioned to Phoenix via
secret management — it **never reaches a browser**. No overlay or TLS-dist
dependency is imposed (see ADR 0091 Decision 5 for the untrusted-split case).

### ③ Curated op facade

Phoenix exposes a **finite, named op vocabulary** to the browser and invokes
only that set on the workspace — it never proxies an arbitrary
`{module, function, args}` into `:rpc.call/4`. An off-vocabulary request is
refused (403) with no dist call. **Honest framing:** the facade is Phoenix-side
discipline, not a workspace-enforced wall — a Phoenix-app bug or compromise
*could* reach more. Operate Phoenix as a high-value host (it holds the cookie
and full RPC power). The facade's value is enabling a non-`eval` role, not
sandboxing `eval`.

### ④ Two-level RBAC — execute vs. read

Per-op authorization runs in Phoenix (the dist layer is all-or-nothing) before
any dist call, keyed to the OIDC identity and audited.

| Role | execute (eval, load-source, save/flush, reload) | read (inspect, bindings, actors, sessions, transcript, complete) | admin (kill, rotate-cookie) |
|------|:--:|:--:|:--:|
| **Owner** | ✅ | ✅ | ✅ |
| **Observer** | ❌ | ✅ | ❌ |

**The Owner role is full RCE** — equivalent to the ADR 0058 authenticated user.
RBAC meaningfully constrains **only Observer**; "Observer vs. anyone-with-eval"
is the one boundary that contains anything. Hand out Owner as you would shell
access. Claim→role mapping **fails closed**: an unmatched group is denied (never
a privileged fallback), and a missing role map stops boot.

> **Observer ≠ "sees nothing sensitive".** Per-session bindings are isolated, but
> actor state and the Transcript are workspace-shared — an Observer sees the
> shared live world read-only, including output a privileged user printed. This
> is intended for the shared-image use case; operators must understand it.

#### Read-surface safety sign-off (ADR 0091 Decision 4)

Granting Observer the read ops assumes each triggers **no user code**. In a live
image a "read" can otherwise run code an *already-privileged* user installed
(e.g. a user-defined `displayString`/`printOn:`). The Inspector therefore surfaces
state via **builtin field reflection** (ADR 0035), not by invoking user display
methods. v1 ships the ops the IDE actually uses under this constraint:

| Observer-granted op | Triggers user code? | Notes |
|---------------------|:--:|-------|
| `inspect` | No | Builtin field reflection only (ADR 0035), not user `printOn:` |
| `bindings` | No | Reads the session binding map; values rendered via the builtin formatter |
| `complete` | No | Inference-only, no evaluation (ADR 0045) |
| transcript / bindings subscriptions | No | Receive pushes; do not execute on the Observer's behalf |

The richer read views of ADR 0085 (still Proposed) must preserve this property as
they land; any read op that would invoke a user display method must not be
granted to Observer until it routes through builtin reflection.

## Residual risks (accepted)

- **Owner = RCE.** The facade is not a sandbox; only Observer is non-executing.
- **Phoenix is a high-value target.** It holds the cookie and full RPC power; a
  Phoenix compromise = workspace compromise. Patch/operate it like the workspace
  host. (Optional defense-in-depth — a workspace-side `beamtalk_authorized_ops`
  allowlist — is deferred; it mitigates app-level bugs, not host compromise.)
- **Co-locate invariant is operator-enforced.** Nothing prevents an operator from
  splitting Phoenix and the workspace across an untrusted network with bare dist;
  the safe posture is the documented default, not a code-enforced impossibility.
- **Cookie rotation does not evict hot-loaded code.** A fully compromised
  workspace must be stopped and restarted, not merely re-keyed (ADR 0058).
- **Authorization lags IdP state** by up to the re-validation interval / TTL.
