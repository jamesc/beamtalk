# Desktop Attach Shell Spike (BT-2984)

Throwaway code for the ADR 0097 shell-decision spike. Findings and verdict are
in `docs/research/desktop-shell-spike.md` — this README is the "how to
reproduce" companion.

**Not shipped code.** `broker/broker.sh` stands in for the eventual Rust
broker (BT-2985); `coordinator/coordinator.exs` is a throwaway prototype of
the ADR's "no-shell coordinator" alternative. Both drive the **real**
`dist-liveview` release against **real** `beamtalk workspace` processes — no
criterion in the write-up is asserted from reading code alone.

## Prerequisites

```bash
just build            # builds target/debug/beamtalk (real workspaces)
just dist-liveview     # builds dist-liveview/bin/server (the real front release)
```

Both are one-time (cached) unless the front or CLI source changes.

## `broker/broker.sh` — bash stand-in for the desktop broker

```
broker.sh discover                          # list ~/.beamtalk/workspaces/*, epmd liveness
broker.sh attach <ws_id> [port]              # spawn a front, print the port
broker.sh attach-bad-cookie <ws_id> [port]   # negative path: corrupted cookie
broker.sh attach-dead <fake_node> [port]     # negative path: no backing workspace
broker.sh attach-with-env <ws_id> <port> K=V...  # e.g. inject BT_OIDC_* to test the no-OIDC posture
broker.sh detach <ws_id_or_port>             # graceful stop
broker.sh sweep                              # orphan-reaping: kill+clear any pidfile'd front
                                              #   still alive from a prior broker lifetime (g)
broker.sh status                             # list tracked fronts + liveness
broker.sh readiness <port>                   # one GET /readiness, raw response
broker.sh wait-ready <port> [timeout_s]      # two-stage probe (HTTP up, then /readiness 200)
```

State (pidfiles/logs) lives under `~/.beamtalk/desktop-broker-spike/`, separate
from `~/.beamtalk/workspaces/` (owned by the Rust CLI).

### Example: reproduce (a) two-instance boot, including two fronts on one workspace

```bash
./target/debug/beamtalk workspace create spike-a --background --persistent
./target/debug/beamtalk workspace create spike-b --background --persistent

broker/broker.sh attach spike-a          # → port P1
broker/broker.sh attach spike-b          # → port P2
broker/broker.sh attach spike-a          # → port P3, SAME workspace as the first

broker/broker.sh wait-ready P1
broker/broker.sh wait-ready P2
broker/broker.sh wait-ready P3           # all three succeed simultaneously

epmd -names   # three distinct bt_attach_<id>_<pid> registrations
```

### Example: reproduce (g) orphan reaping

```bash
broker/broker.sh attach spike-a          # → port P; note the pid in status
broker/broker.sh status                  # shows it alive, tracked

# Simulate the broker crashing: the front was spawned via setsid, so it is
# already independent of this shell — ps -o pid,ppid,pgid,sid on its pid
# confirms it re-parented to init (a genuine orphan).

broker/broker.sh sweep                   # a "fresh broker start": detects the
                                          # still-alive, untracked front, kills it
broker/broker.sh status                  # empty; ps confirms the OS process is gone
```

### Negative-path caveat (criterion c)

`bin/server <id>` **re-resolves** `BT_WORKSPACE_NODE`/`BT_WORKSPACE_COOKIE`
from `~/.beamtalk/workspaces/<id>/` whenever given a workspace-id argument —
right for the happy path, but it means `attach-bad-cookie`/`attach-dead`
deliberately invoke `bin/server` with **no** positional argument (the script's
own documented "environment as-is" mode) so the broker's override actually
reaches the front. See the comment above `_spawn` in `broker.sh` for the full
explanation — this is a real footgun worth carrying into BT-2985.

## `coordinator/coordinator.exs` — the no-shell coordinator prototype

```bash
elixir spikes/desktop-attach-spike/coordinator/coordinator.exs
# first run fetches deps via Mix.install (network required once, then cached)
# → http://127.0.0.1:4500/
```

Lists live workspaces; click **Attach** to spawn a real front and get
redirected to it. Re-clicking Attach on an already-attached workspace reuses
the existing front (see "Single-instance policy" in the write-up) rather than
spawning a duplicate. Install the page as a PWA (browser menu → Install app)
for the dockable-window feel the ADR credits this alternative with.

## Cleanup

```bash
broker/broker.sh sweep
./target/debug/beamtalk workspace stop spike-a
./target/debug/beamtalk workspace stop spike-b
pkill -f coordinator.exs   # if still running
```
