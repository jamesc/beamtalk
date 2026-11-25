# cluster — Multi-Node Distribution (ADR 0126)

A small but real app that runs across three named nodes on one host and uses
Beamtalk's location-transparent actors the way a user would. A coordinator
farms jobs out to two worker nodes, loses one of them mid-call, notices, and
reassigns the work.

It exists to answer one question: **does ADR 0126 hold up when it is used end
to end?** Remote spawn is "frozen" (kept, not grown) until something uses it;
this is that something. The [PR that added it](https://github.com/jamesc/beamtalk/pulls?q=BT-5143)
records which features held up and which did not.

## What It Exercises

| ADR 0126 surface | Where |
|---|---|
| `Node named:`, `connect`, `isConnected`, `Node connected`, `disconnect` | `Demo` phase 1 |
| `NodeUp` / `NodeDown` announcements | `Demo` phase 1 and 7, `ClusterMonitor` |
| `spawnOn:`, `spawnWith:on:`, `spawnAs:on:`, `named:on:`, `allRegisteredOn:` | `Demo` phase 2 |
| Sync `.` sends, casts `!` | `Demo` phase 3, `Worker`, `Farm` |
| Value objects on the wire (`Job`, `JobResult`) | `Demo` phase 3 |
| Remote **async** (`Future`) sends | `Demo` phase 4: **unsupported by design** (ADR 0126, BT-4989); the example checks that they fail loudly |
| `scope: #global` registration, looked up from another node | `Demo` phase 5, `Ledger`, `Worker>>reportToLedger:` |
| `shapeSkew` | `Demo` phase 6 |
| A node dying mid-call, work reassigned | `Demo` phase 7, `Farm` |

### Supervision of remote work

ADR 0126 §3: **remote spawn creates no link.** A `Worker` started with
`spawnOn:` is not a child of any supervisor and nothing restarts it. If its
node dies, it is gone. The coordinator therefore does the supervising itself:
`Farm` catches the `node_down` error from a failed send, drops the dead worker
and retries the job on a survivor. Each node's own `NodeSupervisor` supervises
the `Worker` it starts locally, as usual.

## Classes

| Class | Kind | Role |
|---|---|---|
| `Job`, `JobResult` | Value | The unit of work and its answer; cross the wire by copy |
| `Worker` | Actor | Does a job; runs on every node |
| `Ledger` | Actor | Collects results; registered with `scope: #global` |
| `Farm` | Actor | Round-robin dispatch with retry when a worker's node dies |
| `ClusterMonitor` | Actor | Records `NodeUp` / `NodeDown` announcements |
| `Report` | Actor | Collects the scenario's `PASS` / `FAIL` lines |
| `NodeSupervisor` | Supervisor | Root supervisor of every node |
| `Demo` | Object | The coordinator scenario (class-side) |

## Running It

The nodes are three instances of one OTP release (ADR 0125). The release
launcher takes the node name from `RELEASE_NODE` and the cookie from
`RELEASE_COOKIE`, which is how several instances run on one host.

```bash
cd examples/cluster
beamtalk release --output /tmp/cluster-release --force-output --no-include-erts

export RELEASE_COOKIE=some-throwaway-cookie     # any value; all nodes must share it
R=/tmp/cluster-release/bin/cluster

# Terminals 1-3 (or background them): one node each. `foreground` is the only
# start verb; supervision of the OS process is your shell's or systemd's job.
RELEASE_NODE=worker1 $R foreground
RELEASE_NODE=worker2 $R foreground
RELEASE_NODE=coord   $R foreground

# Terminal 4: run the scenario inside the coordinator node.
RELEASE_NODE=coord $R rpc "Demo main: worker1@localhost worker2@localhost"
```

Expected output, one line per check, ending with `RESULT 33 ok, 0 failed`.
`worker2` goes down during the run (phase 7); `worker1` and `coord` stay up.

Stop what is left:

```bash
RELEASE_NODE=worker1 $R stop
RELEASE_NODE=coord   $R stop
```

Or let the end-to-end script do all of it, with unique node names and cleanup
of every node it started:

```bash
examples/cluster/scripts/e2e.sh
```

CI runs that script (`just test-cluster`).

### Single-node tests

```bash
cd examples/cluster
beamtalk test
```

These cover the classes on one node. The multi-node behaviour is `e2e.sh`'s.

## Same Host Only

Everything here uses `localhost` node names and loopback-bound distribution.
Release nodes pin distribution to `127.0.0.1` in their generated `vm.args`
(`inet_dist_use_interface`), and the launcher's `stop` / `ping` / `rpc` /
`remote_console` client nodes are pinned to loopback too. The launcher also
forces `ERL_EPMD_ADDRESS=127.0.0.1`, so an exported value has no effect here.
Connecting to a node on another host is refused with `kind =
insecure_distribution` unless the node runs TLS distribution
(`-proto_dist inet_tls`, ADR 0091). That setup is not built here: it needs
certificates and `vm.args`, and is the next step for a real deployment. The
cookie is a shared secret: anyone holding it has full control of every node
(ADR 0020/0058), so use a real one anywhere beyond a throwaway run.

## Things To Know

- **`global` needs a moment after nodes connect.** OTP's `global` module syncs
  names in the background after a connection is made, so a `scope: #global`
  lookup from a peer straight after `connect` can answer `name_not_registered`
  for a fraction of a second. `Demo` calls `Erlang global sync` first. See the
  PR notes for BT-5143.
- **`halt` is not a crash.** `Worker>>dieMidCall:` uses `System halt:` to take
  the node down with a call in flight: no `terminate:`, no reply. The caller
  sees `node_down`.
- **Timeouts.** A real partition (not a halted node) is detected after
  `net_ticktime` (60 s by default); a sync send raises `timeout` before
  `node_down` there.
