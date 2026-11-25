# cluster — Agent Guide

Multi-node example for ADR 0126 (location-transparent actors): a coordinator
farms jobs out to worker nodes on one host and survives a worker node dying
mid-call.

## Workspace Structure

```text
cluster/
├── beamtalk.toml        # Package manifest ([application] supervisor = NodeSupervisor)
├── src/
│   ├── job.bt               # Job value (crosses the wire)
│   ├── job_result.bt        # JobResult value
│   ├── worker.bt            # Worker actor (runs on every node)
│   ├── ledger.bt            # Ledger actor (scope: #global)
│   ├── farm.bt              # Farm actor (dispatch + retry on node_down)
│   ├── cluster_monitor.bt   # NodeUp / NodeDown recorder
│   ├── report.bt            # PASS/FAIL collector
│   ├── node_supervisor.bt   # Root supervisor
│   └── demo.bt              # The coordinator scenario (Demo run / Demo main:)
├── test/                # BUnit tests (single node)
├── scripts/e2e.sh       # Multi-node end-to-end run (CI: `just test-cluster`)
└── README.md            # The exact commands to start the nodes
```

## Running

Single-node tests: `cd examples/cluster && beamtalk test`.

The multi-node scenario cannot run in `beamtalk test` or one REPL: it needs
three nodes. Run `examples/cluster/scripts/e2e.sh`, or follow the README.

## Rules For Changing It

- Every ADR 0126 surface listed in the README table must stay used by `src/`,
  not only by tests. If a feature stops working, do not delete the check:
  fix the runtime or file an issue and say so in the README.
- A check is one `report check: "…" is: …` line in `Demo`. `scripts/e2e.sh`
  asserts the key lines by their exact text; change both together.
- Remote async sends are unsupported (ADR 0126 amendment BT-4989). The
  example checks that they fail loudly; it must not start relying on them.
- Do not use cross-file `>>` extension methods (ADR 0152).
- The e2e script must leave no node process and no epmd registration behind.
