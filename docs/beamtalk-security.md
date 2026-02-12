<!-- Copyright 2026 James Casey -->
<!-- SPDX-License-Identifier: Apache-2.0 -->

# Beamtalk Security Model

This document describes the security design of the Beamtalk runtime, focusing on the REPL server, compiler daemon, and workspace management.

## Overview

Beamtalk follows a **localhost-only by default** security model. All interactive components (REPL, daemon, workspace) are designed for local development and use local-only IPC mechanisms (TCP loopback and Unix domain sockets). No authentication is required because only local processes can connect.

This is the same security model used by `erl` (the Erlang shell), `iex` (Elixir), `irb` (Ruby), and `node` (Node.js).

## Components

### REPL TCP Server

The REPL server (`beamtalk_repl_server`) listens on a TCP port bound to the IPv4 loopback address `127.0.0.1`. This is enforced in the `init/1` function:

```erlang
ListenOpts = [binary, {packet, line}, {active, false}, {reuseaddr, true},
              {ip, {127, 0, 0, 1}}],
```

**Security properties:**
- Only processes on the local machine can connect
- No authentication or authorization is performed
- The port is explicitly selected (default `49152` via the CLI) unless overridden
- Each workspace gets its own REPL server instance

### Compiler Daemon

On Unix-like platforms, the compiler daemon communicates via a **Unix domain socket** at `~/.beamtalk/sessions/<session>/daemon.sock`. Unix sockets are inherently localhost-only and protected by filesystem permissions. On non-Unix platforms (e.g., Windows), the compiler daemon is currently disabled and the CLI runs compilation directly without a long-lived daemon process.

**Security properties:**
- Socket file is created with the user's filesystem permissions
- Only the owning user can connect (standard Unix permission model)
- Session isolation: each terminal session gets its own daemon socket
- On Unix platforms, the `BEAMTALK_DAEMON_SOCKET` environment variable can override the socket path

### Workspace Isolation

Beamtalk uses two isolation layers:

**Daemon sessions** — Each terminal session gets an isolated daemon directory under `~/.beamtalk/sessions/`:

```text
~/.beamtalk/sessions/shell-1234/daemon.sock   # Auto session (terminal PPID 1234)
~/.beamtalk/sessions/my-test/daemon.sock      # Named session (BEAMTALK_WORKSPACE=my-test)
```

**Workspace metadata** — Each workspace stores persistent state under `~/.beamtalk/workspaces/`:

```text
~/.beamtalk/workspaces/<workspace_id>/metadata.json   # Project path, loaded modules
~/.beamtalk/workspaces/<workspace_id>/cookie           # Erlang distribution cookie
```

Both layers are isolated from each other. A session's daemon and REPL server only serve that session, and workspace metadata is scoped to its project.

## Threat Model

### In Scope

| Threat | Mitigation |
|--------|------------|
| Remote network access to REPL | Loopback binding (`127.0.0.1`) prevents remote connections |
| Remote access to daemon | Unix domain socket prevents remote connections |
| Cross-user access to daemon | Filesystem permissions on socket file |
| Session interference | Session-specific directories and sockets |

### Out of Scope (Current)

| Threat | Status |
|--------|--------|
| Local privilege escalation | Trusted local environment (same as `erl`, `iex`) |
| Malicious local processes | Not mitigated (standard for development tools) |
| Code injection via REPL protocol | REPL executes arbitrary code by design |

## Future: Remote Development Support

If remote REPL access is added (e.g., `beamtalk repl --remote`), the following safeguards **must** be implemented:

1. **Default to localhost** — Remote binding must be opt-in, never default
2. **Authentication required** — Use API tokens, mutual TLS, or equivalent before accepting remote connections
3. **Configuration flag** — Require explicit `--bind 0.0.0.0` or `--remote` flag
4. **Warning on startup** — Log a clear warning when remote binding is enabled:
   ```
   ⚠️  REPL server bound to 0.0.0.0:PORT — accepting remote connections.
       Ensure network access is restricted.
   ```
5. **Documentation** — Update this document with remote access configuration and risks

## References

- [BT-184](https://linear.app/beamtalk/issue/BT-184): REPL security documentation
- PR #122 code review (comment 2753831629): Original security observation
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_server.erl`: REPL server implementation
- `crates/beamtalk-cli/src/paths.rs`: Daemon socket path resolution
- `crates/beamtalk-cli/src/commands/daemon/mod.rs`: Daemon architecture
