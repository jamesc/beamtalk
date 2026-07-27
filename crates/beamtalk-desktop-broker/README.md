# beamtalk-desktop-broker

Desktop attach connection-broker core (ADR 0097, BT-2985): discovery of live
workspaces under `~/.beamtalk/workspaces/`, per-instance spawn of the
LiveView front (`bin/server <id>`) with the loopback/no-OIDC/entropy-seeded-
sname posture, two-stage readiness probing (`GET /readiness`, BT-2983),
post-attach connection monitoring, and orphan reaping.

This is a plain Rust library — no GUI dependency. It implements the
process-supervision logic a desktop shell (picker UI: BT-2986; packaging:
BT-2987/BT-2988) builds a window/event-loop layer on top of.

See `docs/ADR/0097-desktop-attach-client-node-per-workspace.md` and
`docs/research/desktop-shell-spike.md` for the design and the spike that
validated it.

## Modules

| Module | Responsibility |
|---|---|
| `discovery` | Enumerate `metadata.json`, check epmd liveness |
| `port` | Free port allocation with conflict retry |
| `sname` | `BT_ATTACH_NODE_SUFFIX` seeding / node-name prediction |
| `oidc_guard` | Refuse to spawn when OIDC is configured |
| `spawn` | Spawn a front with the required env vars and posture |
| `readiness` | Two-stage probe: HTTP up, then `/readiness` |
| `monitor` | Post-attach periodic `/readiness` re-poll |
| `reap` | PID-file orphan sweep, hardened against PID reuse |
| `cli_ops` | Create/stop workspaces via the installed `beamtalk` CLI |

## What this crate does not do

- Bundle or supervise the Rust `beamtalk` toolchain — workspace
  create/stop shells out to the user's already-installed CLI.
- Build a picker UI or wire an actual Tauri shell.
