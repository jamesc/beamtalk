# ADR 0020: Connection Security — mTLS, Proxies, and Network Overlays

## Status
Proposed (2026-02-12)

## Context

Beamtalk has several TCP communication channels that need security consideration:

| Channel | Transport | Current Security | Users |
|---------|-----------|-----------------|-------|
| **REPL ↔ Workspace** | TCP `127.0.0.1:49152` | Loopback-only binding | `beamtalk repl` |
| **CLI ↔ Compiler Daemon** | Unix socket `~/.beamtalk/daemon.sock` | Filesystem permissions | `beamtalk build`, LSP |
| **Distributed Erlang** | Erlang distribution protocol | Cookie file (`chmod 600`) | Workspace-to-workspace |
| **Web terminal** | Not yet implemented | — | Browser-based REPL |
| **Remote attach** | Not yet implemented | — | `beamtalk attach prod@host` |

### Threat Model

**Local development** (single machine, single user):
- REPL bound to `127.0.0.1` — only local processes can connect
- Daemon uses Unix socket — filesystem ACL provides auth
- Erlang cookie — prevents accidental cross-workspace connections
- **Threat level: low.** The OS provides adequate isolation.

**Local development** (shared machine, CI runner, Docker `--net=host`):
- Multiple users/processes share the loopback interface
- Any local process can connect to `127.0.0.1:49152` — TCP has no caller identity
- The REPL is an **arbitrary code execution endpoint** — a connection = full RCE as the workspace owner
- Erlang cookie protects distributed Erlang but **not** the REPL TCP protocol
- **Threat level: medium.** Loopback binding is necessary but not sufficient on shared machines.

**Web terminal** (browser on same or different machine):
- Browser cannot connect to raw TCP — needs an HTTP/WebSocket proxy
- If proxy listens on `0.0.0.0` or a non-loopback interface, the workspace is network-exposed
- CSRF, session hijacking, DNS rebinding attacks become real threats
- Token leakage via URL (browser history, Referer headers, screen sharing)
- A stolen token = full RCE on the host, not just "session hijack"
- **Threat level: high.** The REPL executes arbitrary code — standard web security is the minimum, not the ceiling.

**Remote attach** (production debugging, multi-machine clusters):
- Distributed Erlang cookies are symmetric shared secrets with no rotation
- Erlang distribution protocol is unencrypted by default
- Anyone who can reach the port and knows the cookie has full control
- **Threat level: high.** Network encryption + strong identity required.

### Why TCP (Not Unix Socket) for the REPL

The compiler daemon uses a Unix socket (`~/.beamtalk/daemon.sock`) — why doesn't the REPL?

1. **The workspace is an Erlang/OTP process.** Erlang's `gen_tcp` is native and well-supported. Unix socket support in Erlang requires workarounds (`afunix` or NIF-based solutions) and isn't portable to Windows.
2. **Remote attach needs TCP.** The REPL protocol must work over the network for `beamtalk attach`. Using Unix sockets locally and TCP remotely would mean two transport implementations for the same protocol.
3. **The daemon is Rust-native.** Unix sockets are trivial in Rust (`std::os::unix::net`). The daemon runs on the same machine as the CLI by definition — it never needs network transport.

**Trade-off acknowledged:** TCP on localhost is less secure than Unix sockets on shared machines. We mitigate this with cookie-based authentication on the REPL protocol (see Layer 1 below).

### Design Principles

1. **Local stays local.** `127.0.0.1` binding is the default and cannot be overridden by accident. No "listen on all interfaces" flag.
2. **Security at the right layer.** Don't reinvent TLS inside the beamtalk protocol. Use the transport layer (OTP `ssl`, network overlays, reverse proxies).
3. **Zero-config for dev, explicit opt-in for remote.** Local REPL should Just Work™. Remote access requires deliberate setup.
4. **Composable with existing infrastructure.** Support Tailscale, WireGuard, SPIFFE, and standard reverse proxies — don't mandate any single approach.

## Decision

### Layer 1: Local connections (`127.0.0.1` + cookie handshake)

Local REPL and daemon connections remain loopback-only. The REPL protocol uses **WebSocket** as its sole transport, with a **cookie handshake** on connection to authenticate the client on shared machines.

```
┌──────────────┐   WS ws://127.0.0.1:49152   ┌──────────────────┐
│  beamtalk    │ ──────────────────────────── │  Workspace Node  │
│  repl (CLI)  │  1. WebSocket connect        │  (BEAM + cowboy)  │
│              │  2. Send cookie message       │                  │
│              │  3. Server validates          │                  │
│              │  4. JSON protocol begins      │                  │
└──────────────┘                              └──────────────────┘
```

**Single transport: WebSocket.** The workspace uses `cowboy` (standard Erlang HTTP/WebSocket server) instead of `gen_tcp`. The REPL protocol is the same JSON messages, carried over WebSocket frames instead of newline-delimited TCP. This means:

- **CLI** connects via `ws://127.0.0.1:49152` (using `tungstenite` crate in Rust)
- **Browser** connects via `ws://localhost:49152` directly — no proxy needed for local dev
- **Remote/TLS** — standard reverse proxy (Caddy, nginx) in front, zero custom code

**Why WebSocket only (not TCP + WebSocket):** One transport means one implementation to test and maintain. WebSocket is HTTP-upgrade-compatible, so standard proxies (Caddy, nginx, envoy) can terminate TLS, handle auth headers, and forward without custom bridging code. This follows nREPL's principle of transport-agnostic protocol design — the JSON messages are the protocol; WebSocket is the transport.

**Cookie handshake protocol:**
```json
→ {"type": "auth", "cookie": "<workspace cookie>"}
← {"type": "auth_ok"}
```
or on failure:
```json
← {"type": "auth_error", "message": "Invalid cookie"}
→ [connection closed by server]
```

The workspace cookie already exists at `~/.beamtalk/workspaces/{id}/cookie` with `chmod 600`. The CLI reads it from disk; the workspace validates it on first WebSocket message. This adds one message to connection setup.

**Why this matters:** On single-user machines, loopback binding is sufficient. On shared machines (CI runners, Docker `--net=host`, multi-user servers), any local process can connect to the port. The cookie handshake ensures only processes with read access to the cookie file can eval code.

**Rationale for cookie (not mTLS):** Any process that can read cert files can also read the cookie file. Cookie auth is simpler and provides equivalent security to mTLS for the localhost threat model. The OS filesystem ACL (`chmod 600`) is the actual authentication boundary.

### Layer 2: Web terminal via standard reverse proxy

Since the workspace speaks WebSocket natively, the browser can connect directly for local dev. For remote access or TLS, use a **standard reverse proxy** (Caddy, nginx, envoy) — zero custom proxy code.

**Local dev (no proxy needed):**
```
┌────────────┐    ws://localhost:49152    ┌────────────────────┐
│  Browser   │ ────────────────────────── │  Workspace Node    │
│  (xterm.js)│    (WebSocket, direct)     │  (cowboy)          │
└────────────┘                            └────────────────────┘
```

The browser loads a static xterm.js page (served by `beamtalk web` or from disk) and connects to `ws://localhost:49152`. The cookie handshake authenticates the connection.

**Remote/TLS (standard proxy):**
```
┌────────────┐    wss://              ┌───────┐    ws://127.0.0.1    ┌────────────┐
│  Browser   │ ────────────────────── │ Caddy │ ────────────────── │  Workspace │
│            │  (TLS, encrypted)      │       │  (loopback)         │  (cowboy)   │
└────────────┘                        └───────┘                     └────────────┘
```

```bash
# Caddyfile — entire proxy config
localhost:8443 {
    reverse_proxy localhost:49152
}

# Or with Tailscale:
my-laptop.tail12345.ts.net:8443 {
    reverse_proxy localhost:49152
}
```

**Authentication options:**

| Method | When | How |
|--------|------|-----|
| **Cookie handshake** | Default (local) | Browser sends workspace cookie in first WebSocket message (same as CLI). User provides cookie via the xterm.js page UI. |
| **Session token** | Convenience | `beamtalk web` generates a one-time URL token, exchanged for `HttpOnly; Secure; SameSite=Strict` cookie. Prevents leakage via browser history. |
| **Proxy-level auth** | Remote/team | Caddy handles OAuth/OIDC, mTLS, or basic auth before forwarding to workspace. |

**DNS rebinding mitigation:** The cowboy WebSocket handler in the workspace must:
1. Validate the `Origin` header on WebSocket upgrade — reject cross-origin connections
2. Require the cookie handshake before accepting any eval commands

**The workspace always binds to `127.0.0.1` by default.** Network exposure is handled by the reverse proxy, not by changing the workspace bind address. For overlay networks (Tailscale), the workspace can optionally bind to the overlay IP:

```bash
# Default: local only (browser connects directly, no proxy needed)
beamtalk repl
# → Workspace listens on ws://127.0.0.1:49152

# Serve xterm.js web terminal
beamtalk web
# → Opens browser to static page that connects to ws://localhost:49152

# Remote via Caddy (workspace stays on loopback)
caddy reverse-proxy --from :8443 --to localhost:49152

# Remote via Tailscale (workspace binds to overlay IP)
beamtalk run server.bt --bind tailscale
# → ws://100.64.x.x:49152, only reachable by Tailscale peers

# Safety check for non-loopback binding
beamtalk run server.bt --bind 0.0.0.0
# ERROR: Binding to all interfaces exposes the workspace to the network.
#        Use --confirm-network to proceed, or use --bind tailscale for secure remote access.
```

### Layer 3: Remote attach via mTLS or network overlay

For `beamtalk attach prod@host`, two approaches are supported. The user chooses based on their infrastructure.

#### Option A: Network overlay (Tailscale, WireGuard, VPN)

The simplest path. The overlay provides encryption and identity. Beamtalk treats the overlay network as "local":

```
┌──────────────┐     WireGuard tunnel      ┌──────────────────┐
│  Developer   │ ────────────────────────── │  Production      │
│  100.64.0.1  │     (encrypted)            │  100.64.0.2      │
└──────────────┘                            └──────────────────┘
                                                     │
                                              beamtalk repl
                                              (connects to 100.64.0.2:49152)
```

**Changes needed:**
- Workspace can bind to a Tailscale/WireGuard IP instead of `127.0.0.1`
- No protocol changes — same TCP, same JSON, same everything
- Authentication relies on the overlay's identity (Tailscale ACLs, WireGuard keys)

```bash
# Start workspace listening on Tailscale interface
beamtalk run server.bt --bind tailscale
# → Binds to Tailscale IP (100.64.x.x), only reachable by Tailscale peers

# Connect from another machine on same tailnet
beamtalk attach my-server.tail12345.ts.net
```

**Tailscale-specific integration:**
- Auto-detect Tailscale IP via `tailscale status --json`
- Use Tailscale's MagicDNS for discovery (`beamtalk attach my-server`)
- Leverage Tailscale ACLs for authorization (who can attach to what)
- `--bind tailscale` as shorthand for "bind to my Tailscale IP"

#### Option B: mTLS on Erlang distribution

For environments without a network overlay, enable OTP's built-in TLS for distributed Erlang:

```
┌──────────────┐     TLS (mTLS)            ┌──────────────────┐
│  Developer   │ ────────────────────────── │  Production      │
│  BEAM node   │  (client+server certs)     │  BEAM node       │
└──────────────┘                            └──────────────────┘
```

**Erlang's `ssl_dist` module** provides mTLS for the Erlang distribution protocol with minimal configuration:

```erlang
%% vm.args (or generated by beamtalk CLI)
-proto_dist inet_tls
-ssl_dist_optfile ~/.beamtalk/tls/ssl_dist.conf
```

```erlang
%% ssl_dist.conf
[{server, [
    {certfile, "server.pem"},
    {keyfile,  "server-key.pem"},
    {cacertfile, "ca.pem"},
    {verify, verify_peer},
    {fail_if_no_peer_cert, true}
]},
 {client, [
    {certfile, "client.pem"},
    {keyfile,  "client-key.pem"},
    {cacertfile, "ca.pem"},
    {verify, verify_peer}
]}].
```

**Certificate management** — auto-generated per workspace:
```bash
~/.beamtalk/tls/
├── ca.pem              # Self-signed CA (generated once)
├── ca-key.pem          # CA private key
├── workspaces/
│   └── {id}/
│       ├── server.pem      # Workspace server cert (signed by CA)
│       ├── server-key.pem  # Workspace server key
│       ├── client.pem      # Client cert (for attaching)
│       └── client-key.pem  # Client key
```

```bash
# Auto-generate CA on first use
beamtalk tls init
# → Creates ~/.beamtalk/tls/ca.pem (self-signed, long-lived)

# Auto-generate workspace certs
beamtalk workspace create my-feature --tls
# → Creates per-workspace server + client certs signed by CA

# Attach with mTLS
beamtalk attach prod@host --tls
# → Uses client cert from ~/.beamtalk/tls/workspaces/{id}/
```

#### Option C: SPIFFE/SPIRE (future — cloud-native deployments)

For Kubernetes and service-mesh environments, SPIFFE provides automatic workload identity:

```
┌──────────────┐                           ┌──────────────────┐
│  SPIRE Agent │ ─── issues x509-SVID ──── │  Workspace Node  │
│  (per node)  │     (auto-rotated)         │  SPIFFE ID:      │
└──────────────┘                            │  spiffe://bt/    │
                                            │  workspace/prod  │
                                            └──────────────────┘
```

This is deferred to a future ADR when Kubernetes deployment becomes a priority. The mTLS infrastructure from Option B provides a natural integration point — SPIRE-issued certs can replace the self-signed CA certs with zero protocol changes.

### Summary: What secures what

| Scenario | Transport Security | Identity/Auth | Config |
|----------|-------------------|---------------|--------|
| `beamtalk repl` (local) | Loopback binding | Cookie handshake | Default, zero config |
| Browser (local) | Direct WebSocket | Cookie handshake | `beamtalk web` serves xterm.js |
| Browser (remote) | Caddy/nginx TLS | Cookie + proxy auth (OAuth, mTLS) | Standard reverse proxy |
| `beamtalk attach` via Tailscale | WireGuard | Tailscale identity + cookie | `--bind tailscale` |
| `beamtalk attach` via mTLS | TLS (`ssl_dist`) | Client certificate | `--tls` flag |
| Future: K8s/SPIFFE | mTLS (SVID) | SPIFFE workload identity | SPIRE agent |

## Prior Art

### Jupyter Notebook
- Local server generates a random token, printed to terminal
- User accesses `http://localhost:8888/?token=abc123`
- No TLS for localhost by default; relies on loopback binding
- JupyterHub adds OAuth/OIDC for multi-user

### Erlang/OTP `ssl_dist`
- Built-in mTLS for distributed Erlang since OTP 18
- Battle-tested in production Erlang systems (RabbitMQ, CouchDB)
- `-proto_dist inet_tls` flag + SSL config file
- Replaces the cookie as primary auth (cookie becomes secondary)

### Tailscale / WireGuard
- Network-level encryption transparent to applications
- Identity tied to device/user, not certificates the app manages
- Tailscale ACLs provide authorization rules
- Used by Grafana, Gitpod, VS Code tunnels for remote dev

### SPIFFE/SPIRE
- CNCF project for workload identity
- x509-SVIDs are short-lived mTLS certs (auto-rotated ~1hr)
- Used by Istio, Linkerd, SPIRE for service mesh identity
- No existing Erlang integration (would need custom Workload API client)

### LiveBook (Elixir)
- Similar architecture: Phoenix app connecting to BEAM node
- Uses Erlang distribution for node-to-node communication
- Authentication via token in URL (like Jupyter)
- Supports clustering via `dns_cluster` or manual node connection

### nREPL (Clojure)
- Network REPL with pluggable transports (TCP bencode, EDN, WebSocket via community)
- Protocol is transport-agnostic — messages are the contract, transport is plumbing
- No built-in TLS/auth — relies on localhost binding
- Influenced our decision to use a single transport (WebSocket) with the protocol independent of framing

## User Impact

### Newcomer
- **No friction.** `beamtalk repl` works exactly as before — cookie handshake is automatic (CLI reads cookie from disk transparently).
- **Web terminal** provides a familiar browser-based experience. `beamtalk web` prints a clickable URL.
- **Potential confusion:** "Why do I need a token for the web terminal but not the CLI?" — document that the CLI reads the cookie file directly.

### Smalltalk developer
- **Familiar model.** Smalltalk images are single-user by design. The local-first, zero-config approach matches their expectations.
- **Web terminal** fills the gap left by Smalltalk's native IDE — browser-based code editing and inspection.

### Erlang/BEAM developer
- **Recognizable patterns.** Cookie auth mirrors Erlang's `--setcookie` pattern. `ssl_dist` is standard OTP.
- **`beamtalk attach`** parallels `erl -remsh` with better security defaults.
- **May expect:** Distributed Erlang features (connecting nodes, `:rpc`) — need docs on how security applies to node-to-node communication.

### Operator (production)
- **Clear upgrade path.** Start with Tailscale (zero code change), graduate to mTLS for stricter environments.
- **`beamtalk attach`** requires explicit `--bind tailscale` or `--tls` — no accidental exposure.
- **Audit logging** (see Consequences) enables compliance and incident response.
- **Team environments:** OAuth/OIDC on web terminal for shared workspaces with SSO integration.

## Steelman Analysis

### "Just use mTLS everywhere, including localhost"
- **Best argument (security engineer):** Defense in depth. On shared machines, a compromised local process can connect to the REPL port and execute arbitrary code. mTLS ensures only cert-holding processes can connect.
- **Counter:** The cookie handshake provides equivalent authentication — any process that can read the cert can also read the cookie. The actual security boundary is filesystem permissions (`chmod 600`), not the transport layer. Jupyter, LiveBook, and VS Code all use unencrypted localhost with similar reasoning.
- **Tension point:** Security engineers prefer defense in depth; developers prefer zero-config simplicity. We side with simplicity for local dev, with the cookie handshake as a middle ground.

### "Use Unix sockets for local REPL, TCP only for remote"
- **Best argument (BEAM developer):** The daemon already uses Unix sockets. Filesystem permissions on Unix sockets provide stronger isolation than TCP + cookie. This eliminates port conflicts and shared-machine risks entirely.
- **Counter:** Erlang's `gen_tcp` is native and well-supported; Unix socket support requires `afunix` or NIFs and isn't portable. Maintaining two transports (UDS local, TCP remote) doubles the protocol surface. The cookie handshake on TCP achieves comparable security with a single transport.
- **Tension point:** Simplicity of implementation vs. security purity. If Erlang had first-class Unix socket support, we'd likely use it.

### "Tailscale is a vendor dependency"
- **Best argument (operator):** Relying on `tailscale status --json` couples the CLI to a proprietary tool. Open-source WireGuard exists without Tailscale.
- **Counter:** We support any WireGuard/overlay network. `--bind tailscale` is a convenience shortcut, not a requirement. `--bind <ip>` works with any network. SPIFFE support is planned for vendor-neutral cloud identity.

### "SPIFFE should be the primary approach"
- **Best argument (platform engineer):** It's the CNCF standard for workload identity. Building custom cert management now creates migration debt when SPIFFE is adopted later.
- **Counter:** SPIFFE requires infrastructure (SPIRE agents on every machine). It's the right answer for Kubernetes deployments but overkill for a developer running `beamtalk repl` on their laptop. The mTLS cert infrastructure we build now is SPIFFE-compatible — SPIRE-issued SVIDs can replace self-signed certs with no protocol changes.

## Alternatives Considered

### Do nothing (status quo)
- Keep loopback binding with no authentication on the REPL protocol
- Defer web terminal and remote attach security until those features are built
- **Rejected:** The REPL is an arbitrary code execution endpoint. On shared machines, any local process can connect and eval code. Adding cookie auth now is low-cost and prevents a real attack vector. Deferring web terminal security is acceptable; deferring local auth is not.

### Unix sockets for local REPL
- Switch REPL transport from TCP to Unix socket (`~/.beamtalk/workspaces/{id}/repl.sock`)
- Filesystem permissions provide auth without any protocol changes
- **Rejected:** Erlang lacks first-class Unix socket support (requires `afunix` driver or NIF). Maintaining two transports (UDS local, TCP remote) doubles protocol surface. Cookie auth on TCP provides comparable security. See "Why TCP" in Context section.

### SSH tunneling for remote attach
- `beamtalk attach` could SSH to the remote host and forward the REPL port
- **Rejected:** Adds SSH as a dependency, doesn't help with web terminal, doesn't integrate with Erlang distribution. Fine as a user-level workaround but not a platform feature.

### Token-only auth (no TLS)
- Simple bearer token in first JSON message
- **Rejected for remote:** Token transmitted in cleartext over the network. Acceptable for localhost (where the OS prevents sniffing), but insufficient for any network-exposed scenario. For local connections, the cookie handshake is effectively this approach.

### Keep TCP + add WebSocket (dual transport)
- Maintain existing `gen_tcp` for CLI backward compatibility, add `cowboy` WebSocket for browsers
- **Rejected:** Two transports doubles the protocol surface, testing matrix, and bug surface. The WebSocket protocol carries the same JSON messages. The CLI migration from `TcpStream` to `tungstenite` is straightforward. One transport is simpler.

### Custom encryption protocol
- Roll our own encryption for the JSON protocol
- **Rejected:** "Don't roll your own crypto." OTP's `ssl` module, Tailscale, and SPIFFE are battle-tested. We gain nothing by implementing our own.

## Consequences

### Positive
- Local dev remains zero-config and fast (cookie handshake is one WebSocket message)
- Web terminal works with direct WebSocket — no custom proxy needed for local dev
- Remote/TLS handled by standard proxies (Caddy, nginx) — zero custom proxy code
- Clear, graduated security model from local → remote → cloud
- Composable with existing infrastructure (Tailscale, VPN, SPIFFE, any reverse proxy)
- mTLS via `ssl_dist` is an OTP built-in — minimal implementation effort
- Cookie handshake closes the shared-machine attack vector on the REPL
- Single transport (WebSocket) simplifies testing and maintenance

### Negative
- WebSocket migration is a breaking change to the REPL protocol (Phase 0)
- `cowboy` dependency added to workspace OTP app
- `tungstenite` dependency added to CLI Rust crate
- Certificate management (even auto-generated) adds user-facing complexity for Phase 3
- Multiple security approaches (cookie, proxy auth, mTLS) mean more documentation

### Neutral
- Tailscale integration is convenience, not dependency — works without it
- SPIFFE deferred to future ADR — no current implementation cost
- Audit logging (connection events, auth failures) is recommended for production deployments but not mandated for local dev
- Cert rotation/revocation deferred to follow-up issue — acceptable for v1

## Implementation

### Phase 0: WebSocket transport + cookie handshake
- Replace `gen_tcp` in `beamtalk_repl_server.erl` with `cowboy` WebSocket handler
- Update `crates/beamtalk-cli/src/commands/protocol.rs` from `TcpStream` to `tungstenite` WebSocket client
- Add cookie handshake as first WebSocket message (validate before accepting eval)
- Update `docs/repl-protocol.md` with WebSocket transport and auth handshake spec
- **Components:** `beamtalk_repl_server.erl` (cowboy), `protocol.rs` (tungstenite), `repl-protocol.md`
- **Dependencies:** `cowboy` (Erlang, already common in BEAM ecosystem), `tungstenite` (Rust crate)
- **Tests:** Runtime unit tests for auth accept/reject, E2E test for REPL connection with cookie

### Phase 1: Web terminal (browser-based REPL)
- `beamtalk web` command that serves static xterm.js page
- Browser connects directly to `ws://localhost:49152` — no proxy needed for local dev
- xterm.js page reads cookie from user input or `beamtalk web --token` convenience URL
- **Components:** Static HTML/JS (xterm.js), new CLI command
- **Tests:** Manual browser testing, WebSocket Origin header validation

### Phase 2: Network binding + standard proxy documentation
- `--bind tailscale` / `--bind <ip>` for workspace
- Tailscale auto-detection (`tailscale status --json`)
- Safety checks for non-loopback binding (`--confirm-network`)
- Document Caddy/nginx reverse proxy configuration for TLS and remote access
- **Components:** CLI argument handling, `beamtalk_repl_server.erl` bind config, docs
- **Tests:** CLI argument validation, error on `--bind 0.0.0.0` without `--confirm-network`

### Phase 3: mTLS for Erlang distribution
- `beamtalk tls init` — generate self-signed CA
- Auto-generate per-workspace certs on `workspace create`
- `--tls` flag for `beamtalk attach` and `beamtalk run`
- Wire up OTP `ssl_dist` configuration
- **v1 scope:** Auto-generated certs with no rotation or revocation. Cert lifecycle management is a follow-up issue.
- **Components:** CLI commands, cert generation, `vm.args` templating
- **Tests:** mTLS connection acceptance/rejection, cert generation and validation

### Phase 4: SPIFFE/SPIRE integration (future)
- Workload API client for fetching SVIDs
- Feed SPIRE-issued certs to `ssl_dist` configuration
- Kubernetes deployment manifests with SPIRE sidecar
- **Components:** New Erlang module, deployment docs

## Scope Boundaries

This ADR does **not** cover:
- **Web IDE security** (Phoenix LiveView) — `docs/beamtalk-ide.md` should have its own security section referencing this ADR's proxy model
- **Distributed actor security** (workspace-to-workspace messaging) — deferred until multi-workspace communication is designed
- **Audit logging implementation** — recommended but not specified; a separate issue for production readiness
- **Code deployment authorization** (who can hot-reload code) — separate from REPL eval authorization

## References
- Related ADRs: [ADR 0004 — Persistent Workspace Management](0004-persistent-workspace-management.md), [ADR 0009 — OTP Application Structure](0009-otp-application-structure.md)
- Related docs: [REPL Protocol](../repl-protocol.md), [Beamtalk IDE Design](../beamtalk-ide.md)
- Erlang `ssl_dist`: https://www.erlang.org/doc/apps/ssl/ssl_distribution.html
- SPIFFE specification: https://spiffe.io/docs/latest/spiffe-about/overview/
- Tailscale: https://tailscale.com/kb/
- LiveBook security: https://github.com/livebook-dev/livebook#security
- Jupyter token auth: https://jupyter-notebook.readthedocs.io/en/stable/security.html
- DNS rebinding attacks: https://en.wikipedia.org/wiki/DNS_rebinding
