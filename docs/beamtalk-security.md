# Beamtalk Security Model

This document describes the security design of the Beamtalk runtime, focusing on the REPL WebSocket server and workspace management. Beamtalk has no separate compiler daemon: inside a workspace the compiler is an OTP Port owned by the BEAM node (see [architecture](beamtalk-architecture.md#compiler-port)), and batch commands run the compiler in-process.

## Overview

Beamtalk follows a **localhost-only by default** security model. The REPL server and workspace are designed for local development and bind to the loopback interface. Connections are authenticated with the workspace's Erlang cookie, and browser-originated connections are checked against loopback Origin/Host rules (below).

This is the same trust model as `erl` (the Erlang shell), `iex` (Elixir), `irb` (Ruby), and `node` (Node.js): anything that can reach the REPL and holds the cookie can run arbitrary code.

## Components

### REPL WebSocket Server

The REPL server (`beamtalk_repl_server`) is a cowboy WebSocket listener (ADR 0020). By default it binds the IPv4 loopback address `127.0.0.1`; `beamtalk repl --bind <addr>` changes that (an IP address, `0.0.0.0`, or `tailscale`). A non-loopback `--bind` prints a safety warning (`--confirm-network` suppresses that one) and always prints a warning that the `ws://` transport is unencrypted, so the cookie and all evaluated code cross the network in cleartext. Use it only on a network you trust (e.g. Tailscale).

```erlang
socket_opts => [{ip, BindAddr}, {port, Port}]
```

**Security properties:**
- The first message on a connection must authenticate with the workspace cookie. Before authentication the connection has a 10 second deadline and a 4 KiB frame limit; both are relaxed after a successful login.
- A cookie of `nocookie` is never accepted. `beamtalk repl --foreground` starts a distributed node (so it needs `epmd`) under a random per-process cookie. The CLI never puts a cookie on a node's command line: `-setcookie`, `ERL_AFLAGS` and `-args_file` all expand into the BEAM's argv, which any local user can read with `ps`. Instead the cookie is set in the child's environment (`BEAMTALK_ERL_COOKIE`, readable only by its owner), the node starts with a random throwaway `-setcookie`, and its first `-eval` adopts the real cookie and clears the variable. Detached workspace nodes, the `beamtalk run` probes and the release launcher (`bin/<name>`, via `RELEASE_COOKIE`) work the same way. Tests start real nodes and check `ps` and `/proc/<pid>/cmdline`. One exception is outside the CLI's control: `beamtalk attach --port <p> --cookie <c>` takes the cookie as an argument of the `beamtalk` process itself, so it is visible in `ps` while attached. Prefer `beamtalk attach <name>`, which reads the cookie from the workspace's `cookie` file (mode 0600).
- If the upgrade request carries an `Origin` header it must name a loopback host, and the `Host` header must be a loopback name, the bound address, or (for a wildcard bind) an IP literal. Failures get HTTP `403`. Clients without an `Origin` (CLI, MCP, LSP, VS Code) are unaffected. A browser UI behind a reverse proxy must have the proxy rewrite `Host` to a loopback name.
- At most 256 concurrent connections.
- The port is OS-assigned unless `--port` (or `BEAMTALK_REPL_PORT`) is given.
- Each workspace gets its own REPL server instance.

### MCP server

The MCP server's file-based tools (`lint`, `diagnostic_summary`, `load_file`, `load_project`, `test` on a file, `export_traces`) are confined to the project root; symlinks and `..` components are resolved before the check. The server refuses to start confined to the filesystem root, the user's home directory (`HOME` on Unix, the profile folder on Windows) or any directory containing it, such as `/home`. `--allow-outside-project` removes the confinement. Every MCP tool declares `readOnlyHint`, and every non-read-only tool a `destructiveHint`.

### LiveView IDE front

The LiveView front binds loopback by default (`BT_ATTACH_BIND_IP` unset). A non-loopback bind with no authentication gate (OIDC or dev-auth) is refused at boot; set `BT_IDE_ALLOW_UNAUTHENTICATED=1` to opt in explicitly. See [remote LiveView IDE](deployment/remote-liveview-ide.md).

### Workspace Isolation

Each workspace stores persistent state under `~/.beamtalk/workspaces/<workspace_id>/` (or under `$BEAMTALK_HOME` when set), in directories created with mode 0700:

```text
~/.beamtalk/workspaces/<workspace_id>/metadata.json   # Project path, loaded modules
~/.beamtalk/workspaces/<workspace_id>/cookie           # Erlang distribution cookie
```

Workspace metadata is scoped to its project.

## Threat Model

### In Scope

| Threat | Mitigation |
|--------|------------|
| Remote network access to REPL | Loopback binding (`127.0.0.1`) prevents remote connections |
| Cross-site WebSocket hijacking / DNS rebinding from a browser | `Origin` and `Host` must be loopback (or the bound address); cookie required |
| Unauthenticated connections tying up the server | 10 s auth deadline, 4 KiB pre-auth frame limit, 256 connection cap |
| Cross-user access to workspace state | Per-workspace directories created with mode 0700 |

### Out of Scope (Current)

| Threat | Status |
|--------|--------|
| Local privilege escalation | Trusted local environment (same as `erl`, `iex`) |
| Malicious local processes | Not mitigated (standard for development tools) |
| Code injection via REPL protocol | REPL executes arbitrary code by design |

## Remote access

Remote REPL access is opt-in through `beamtalk repl --bind`; it is never the default. The WebSocket transport is not encrypted, so for anything beyond a trusted private network use the authenticated LiveView front ([remote LiveView IDE](deployment/remote-liveview-ide.md), ADR 0091) instead of exposing the REPL port.

## References

- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_server.erl`: REPL server implementation
- `runtime/apps/beamtalk_workspace/src/beamtalk_ws_handler.erl`: cookie authentication, Origin/Host checks, auth deadline
- `runtime/apps/beamtalk_workspace/src/beamtalk_ws_cookie.erl`: cookie validation
- [ADR 0020](ADR/0020-connection-security.md): connection security
