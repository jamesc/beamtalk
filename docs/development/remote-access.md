# Remote Access

This guide covers how to access a Beamtalk REPL workspace from another machine or network.

## Overview

By default, the REPL WebSocket server binds to `127.0.0.1` (localhost only), which means only local processes can connect. For remote access, use the `--bind` flag to bind to a different address.

## Options

### SSH Tunneling (Simplest)

The easiest way to access a remote REPL — no `--bind` flag needed:

```bash
# On the remote machine (default localhost binding):
beamtalk repl

# On your local machine, create an SSH tunnel:
ssh -L 49152:127.0.0.1:49152 user@remote-host

# Then connect locally as if the REPL were running on your machine.
```

This works with the default `127.0.0.1` binding and requires no additional configuration.

### Tailscale (Recommended for Teams)

[Tailscale](https://tailscale.com/) provides a zero-config WireGuard mesh VPN. The `--bind tailscale` flag auto-detects your Tailscale IPv4 address:

```bash
# Bind to Tailscale overlay network:
beamtalk repl --bind tailscale --confirm-network

# The REPL is now reachable by any peer on your Tailscale network.
# Example output: Workspace started on ws://100.64.x.x:49152
```

**Prerequisites:**
- Tailscale installed and running: `tailscale status`
- The `tailscale ip -4` command must return a valid IPv4 address

### Custom IP Address

Bind to a specific network interface:

```bash
# Bind to a specific IP:
beamtalk repl --bind 192.168.1.5 --confirm-network

# Bind to all interfaces (use with caution):
beamtalk repl --bind 0.0.0.0 --confirm-network
```

### Safety Warning

Non-loopback binding requires `--confirm-network` to acknowledge the security implications:

```bash
# This will ERROR without --confirm-network:
beamtalk repl --bind 0.0.0.0
# ⚠️  Binding to 0.0.0.0 exposes the workspace to the network.
#    Use --confirm-network to proceed, or use --bind tailscale for secure remote access.

# Acknowledge the risk:
beamtalk repl --bind 0.0.0.0 --confirm-network
```

## Reverse Proxy Setup

For production or shared environments, a reverse proxy can provide TLS termination and access control in front of the REPL WebSocket server.

### Caddy (Recommended)

[Caddy](https://caddyserver.com/) provides automatic HTTPS with minimal configuration.

```
# Caddyfile
repl.example.com {
    reverse_proxy localhost:49152
}
```

Caddy automatically provisions TLS certificates via Let's Encrypt. For WebSocket support, no additional configuration is needed — Caddy handles `Upgrade: websocket` headers automatically.

**With Tailscale:**

```
# Caddyfile — bind Caddy to Tailscale interface
repl.example.com {
    bind 100.64.x.x
    reverse_proxy localhost:49152
}
```

### nginx

```nginx
server {
    listen 443 ssl;
    server_name repl.example.com;

    ssl_certificate     /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;

    location /ws {
        proxy_pass http://127.0.0.1:49152;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_read_timeout 3600s;
        proxy_send_timeout 3600s;
    }
}
```

**Important nginx notes:**
- `proxy_http_version 1.1` is required for WebSocket upgrade
- `proxy_read_timeout` / `proxy_send_timeout` should be set high to avoid premature disconnects during long evaluations
- The `Upgrade` and `Connection` headers must be forwarded for WebSocket connections

## Security Considerations

- **Cookie authentication** is always active regardless of bind address. The WebSocket connection requires a valid cookie handshake before any REPL operations (see [ADR 0020](../ADR/0020-connection-security.md)).
- **Localhost binding** (`127.0.0.1`, the default) limits the attack surface to local processes only.
- **Tailscale binding** is preferred for remote access because traffic is encrypted and authenticated by the Tailscale mesh.
- **All-interfaces binding** (`0.0.0.0`) should only be used behind a firewall or reverse proxy. The `--confirm-network` flag is required to prevent accidental exposure.
- Consider **TLS termination** via a reverse proxy (Caddy, nginx) for any non-Tailscale remote access.
