# Deploying the LiveView IDE for non-localhost (remote) access

This guide covers running the Beamtalk LiveView IDE **past localhost** — the
production auth/authz topology decided in
[ADR 0091](../ADR/0091-remote-workspace-access-phoenix-authenticated-front.md).
For the security model and trust path, see
[`docs/security/threat-model.md`](../security/threat-model.md).

> **Local development is unchanged.** `beamtalk repl --web` and `just web <ws>`
> on localhost stay zero-config (cookie handshake, no OIDC). Everything here is
> opt-in and applies only when you expose the IDE beyond the local machine.

## The shape

```
   UNTRUSTED NETWORK              TRUSTED HOST / PRIVATE NETWORK
┌─────────┐  HTTPS + OIDC    ┌──────────────┐  internal dist    ┌─────────────┐
│ Browser │ ───────────────▶ │   Phoenix    │ ────────────────▶ │  Workspace  │
│ (user)  │  session cookie  │  LiveView    │  (loopback /      │  BEAM node  │
└─────────┘  (HttpOnly,      │  front       │   private iface,  │  (eval, RCE)│
             Secure, Strict)  └──────────────┘   never public)   └─────────────┘
   The ONLY untrusted hop is browser → Phoenix.   Cookie is an infra secret;
   Phoenix is the only node on the dist mesh.     it never reaches a browser.
```

The browser's trust boundary terminates at Phoenix (HTTPS + OIDC). Phoenix — and
only Phoenix — speaks Erlang distribution to the workspace, over a link kept off
untrusted networks **by deployment** (no overlay or TLS-dist dependency is
imposed).

## 1. Authentication (OIDC)

Configure OIDC once at boot via `~/.beamtalk/ide.toml` (every key has a
`BT_OIDC_*` env override; the client secret comes from env or a `chmod 600`
file, never inline):

```toml
# ~/.beamtalk/ide.toml
[oidc]
issuer            = "https://idp.example.com"     # env: BT_OIDC_ISSUER
client_id         = "beamtalk-ide"                # env: BT_OIDC_CLIENT_ID
redirect_uri      = "https://ide.example.com/oidc/callback"
groups_claim      = "groups"
client_secret_env = "BT_OIDC_CLIENT_SECRET"       # or client_secret_file = "/run/secrets/oidc"

[oidc.roles]                                       # IdP group -> role (fail-closed)
owner    = ["beamtalk-owners", "platform-admins"]  # execute + read + admin
observer = ["beamtalk-observers"]                  # read-only, no eval
# unmatched group -> DENY
```

Boot fails closed if OIDC is requested but a required key — or the role map — is
missing (it will not start with broken auth). Roles may also be supplied by env
(`BT_OIDC_ROLES_OWNER`, `BT_OIDC_ROLES_OBSERVER`, space/comma-separated) for a
12-factor deployment with no file.

**Owner = full RCE** (equivalent to the ADR 0058 authenticated user). Only
Observer is non-executing. Hand out Observer freely; hand out Owner as you would
shell access.

## 2. Transport posture — keep distribution internal

### Co-located (default, recommended)

Run Phoenix and the workspace on the **same host**. Distribution stays on
loopback and never leaves the machine; the only network-facing service is
Phoenix's HTTPS port.

The workspace launcher pins `ERL_EPMD_ADDRESS=127.0.0.1` for an epmd it starts,
and **warns** at launch if it detects a pre-existing epmd already bound to a
non-loopback interface (epmd is a shared per-user daemon — loopback is not
automatic). If you see that warning, stop the stray epmd (`epmd -kill` when no
other Erlang nodes need it) and restart the workspace.

### Same trusted private network / VPC

Bind distribution and epmd to the **private interface**, never `0.0.0.0`:

```sh
export ERL_EPMD_ADDRESS=10.0.0.5        # the private-interface address
beamtalk workspace create ws --background --persistent
```

This is a standard private-cluster posture — no overlay or TLS-dist required.

### Untrusted split (discouraged)

If you *must* run Phoenix and the workspace across an **untrusted** network, you
own securing that hop. In order of preference: **keep them co-located instead**;
an SSH tunnel; a network overlay (Tailscale/WireGuard); or reintroduce TLS
distribution (`inet_tls_dist`, removed in PR #1401 — not in the tree). The
platform does not depend on any of these.

## 3. Cookie provisioning (infrastructure secret)

The dist cookie is shared between the two **trusted hosts**, not a user
credential. It is read from `~/.beamtalk/workspaces/{id}/cookie` (mode 0600) and
provisioned to Phoenix via env — `BT_WORKSPACE_NODE` and `BT_WORKSPACE_COOKIE` —
through your secret-management path. **It is never sent to a browser** (browser ↔
Phoenix is HTTPS only; the cookie is used solely for the Phoenix ↔ workspace dist
link).

### Run it

```sh
export SECRET_KEY_BASE="$(cd editors/liveview && mix phx.gen.secret)"
export PHX_HOST=ide.example.com          # behind your TLS terminator
just web-remote ws                       # discovers node+cookie, starts prod Phoenix
```

`just web-remote` discovers the node + cookie, exports them as the infra secret,
and starts Phoenix in prod mode (server on `:8443` by default; put a TLS
terminator in front, or add `https:` to the endpoint). OIDC must be configured
(step 1) or boot fails closed.

### Cookie rotation

Rotate with `beamtalk workspace rotate-cookie` (future CLI work, ADR 0020),
coordinated with a Phoenix reconnect: dist connections are long-lived, so
rotation needs a brief reconnect window (re-provision `BT_WORKSPACE_COOKIE` and
restart/reconnect Phoenix), not a live re-key.

> **Caveat (ADR 0058):** rotating the cookie does **not** evict code already
> hot-loaded into the workspace. A fully compromised workspace must be stopped
> and restarted, not merely re-keyed.

## Checklist

- [ ] HTTPS terminates at Phoenix; `PHX_HOST` set; `SECRET_KEY_BASE` set.
- [ ] OIDC `[oidc]` + `[oidc.roles]` configured; secret from env/file, not inline.
- [ ] Workspace + Phoenix co-located (or dist bound to a private iface, never `0.0.0.0`).
- [ ] No promiscuous-epmd warning at workspace launch.
- [ ] `BT_WORKSPACE_COOKIE` delivered via secret management; never in a browser surface.
- [ ] Observer handed out for read-only audiences; Owner treated as shell access.
