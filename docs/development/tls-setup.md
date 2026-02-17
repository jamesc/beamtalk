# TLS Setup for Erlang Distribution

Beamtalk supports mutual TLS (mTLS) for securing Erlang distribution
connections between workspaces. This is part of
[ADR 0020: Connection Security](../ADR/0020-connection-security.md) Phase 2.

## Quick Start

```bash
# 1. Generate TLS certificates for the current workspace
beamtalk tls init

# 2. Start the REPL with TLS-secured distribution
beamtalk repl --tls
```

## Overview

When `--tls` is enabled, Erlang distribution uses the `inet_tls` protocol
instead of plain TCP. This means:

- All inter-node communication is encrypted
- Nodes must present valid certificates signed by the workspace CA
- Non-TLS nodes **cannot** connect to TLS-enabled workspaces

## Certificate Generation

The `beamtalk tls init` command generates a self-signed CA and node
certificate pair for the current workspace:

```bash
beamtalk tls init
# Generating TLS certificates for workspace: abc123def456
#   ✓ CA certificate:  ~/.beamtalk/workspaces/abc123def456/tls/ca.pem
#   ✓ Node certificate: ~/.beamtalk/workspaces/abc123def456/tls/node.pem
#   ✓ Node key:         ~/.beamtalk/workspaces/abc123def456/tls/node-key.pem (mode 600)
#   ✓ Distribution config: ~/.beamtalk/workspaces/abc123def456/tls/ssl_dist.conf
```

### Generated Files

| File | Description | Permissions |
|------|-------------|-------------|
| `ca.pem` | Self-signed CA certificate | 644 |
| `node.pem` | Node certificate (signed by CA) | 644 |
| `node-key.pem` | Node private key | 600 |
| `ssl_dist.conf` | Erlang ssl_dist configuration | 644 |

### Certificate Location

Certificates are stored under the workspace's TLS directory:

```text
~/.beamtalk/workspaces/{workspace-id}/tls/
├── ca.pem
├── node.pem
├── node-key.pem
└── ssl_dist.conf
```

### Specifying a Workspace

By default, `tls init` uses the workspace auto-detected from the current
directory. To target a specific workspace:

```bash
beamtalk tls init --workspace my-project
```

## Using TLS

### REPL

```bash
# Start with TLS distribution
beamtalk repl --tls

# Combine with other flags
beamtalk repl --tls --bind tailscale
beamtalk repl --tls --foreground
```

### How It Works

When `--tls` is specified, Beamtalk adds these Erlang VM arguments:

```text
-proto_dist inet_tls -ssl_dist_optfile <path>/ssl_dist.conf
```

The `ssl_dist.conf` file configures both server and client sides of the
TLS connection:

```erlang
[{server, [
   {certfile, "/path/to/node.pem"},
   {keyfile, "/path/to/node-key.pem"},
   {cacertfile, "/path/to/ca.pem"},
   {verify, verify_peer},
   {fail_if_no_peer_cert, true}
]},
{client, [
   {certfile, "/path/to/node.pem"},
   {keyfile, "/path/to/node-key.pem"},
   {cacertfile, "/path/to/ca.pem"},
   {verify, verify_peer}
]}].
```

## Security Properties

- **Mutual authentication**: Both connecting and accepting nodes must
  present valid certificates
- **Encryption**: All distribution traffic is encrypted with TLS
- **Isolation**: Nodes with different CAs (different workspaces) cannot
  connect to each other
- **Key protection**: Private keys are created with mode 600 (owner
  read/write only)

## Limitations (v1)

- **Self-signed CA only**: No integration with external CAs
- **No certificate rotation**: To rotate, delete the `tls/` directory
  and run `beamtalk tls init` again
- **No certificate revocation**: Compromised certs require regeneration
- **localhost SAN only**: Node certificates include `localhost` as the
  Subject Alternative Name

## Troubleshooting

### "TLS certificates not found"

Run `beamtalk tls init` before using `--tls`.

### Connection refused between TLS and non-TLS nodes

TLS-enabled nodes cannot communicate with non-TLS nodes. All nodes in a
cluster must use the same TLS configuration.

### Certificate verification failure

If certificates were regenerated, all nodes in the cluster must be
restarted with the new certificates.

### Checking certificate details

```bash
openssl x509 -in ~/.beamtalk/workspaces/{id}/tls/node.pem -text -noout
openssl x509 -in ~/.beamtalk/workspaces/{id}/tls/ca.pem -text -noout
```

## References

- [ADR 0020: Connection Security](../ADR/0020-connection-security.md)
- [Erlang ssl_dist documentation](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html)
- [OTP Using TLS for Erlang Distribution](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html)
