# otp-tree — Agent Guide

OTP supervision tree example: static supervisor, dynamic worker pool, and
fault isolation between actors.

## Workspace Structure

```
otp-tree/
├── beamtalk.toml        # Package manifest — [application] supervisor = "AppSupervisor"
├── src/
│   ├── app_supervisor.bt    # Root Supervisor (one_for_one)
│   ├── event_logger.bt      # Permanent Actor — event log
│   ├── worker_pool.bt       # DynamicSupervisor — manages TaskWorkers
│   └── task_worker.bt       # Transient Actor — processes tasks
├── test/
│   ├── otp_tree_test.bt         # Supervisor config (strategy, policies)
│   ├── event_logger_test.bt     # EventLogger actor behaviour
│   └── task_worker_test.bt      # TaskWorker actor behaviour and crash
├── AGENTS.md            # This file
├── .mcp.json            # MCP server config
├── .github/
│   └── copilot-instructions.md
└── .gitignore
```

## Starting the REPL

```bash
cd examples/otp-tree
beamtalk repl
```

All source files in `src/` load automatically when you start the REPL.

## Key OTP Concepts Demonstrated

- **`Supervisor subclass:`** — static supervision tree; children are known at
  startup and declared by overriding `class children`.
- **`DynamicSupervisor subclass:`** — dynamic pool; children are added at
  runtime via `pool startChild` / `pool startChild: args`.
- **`class supervisionPolicy`** — per-actor restart strategy (`#permanent`,
  `#transient`, `#temporary`). Read by the supervisor when building child specs.
- **`supervise`** — starts the supervisor process (or returns the already-running
  instance if called again).
- **`current`** — returns the running supervisor or actor instance looked up by
  class name. Use this from any actor to find a named peer without holding a
  direct reference.
- **Fault isolation** — a crash in `TaskWorker` does not affect `EventLogger`
  or the `WorkerPool` supervisor itself; only the crashed worker is restarted.

## Live Workspace (MCP)

The `.mcp.json` in this project configures the `beamtalk` MCP server, which gives
you live access to a running REPL. Claude Code starts it automatically via
`beamtalk-mcp --start` — no manual `beamtalk repl` required.

**Prefer MCP tools over guessing.** If you're uncertain what a method returns or
whether code is correct, evaluate it directly rather than inferring from source.

| Tool | When to use |
|------|-------------|
| `evaluate` | Test expressions, explore values, prototype code snippets |
| `load_file` | Load a `.bt` file into the workspace before evaluating it |
| `reload_module` | Hot-reload a module after editing — migrates live actors |

## Common Pitfalls

- `current` returns `nil` if the supervisor/actor has not been started yet.
  Always call `supervise` before `current`.
- `DynamicSupervisor` has no `class children` — only `class childClass`.
  Defining `children` on a `DynamicSupervisor` subclass has no effect.
- A `#temporary` worker that crashes is **not restarted** — that is intentional.
  Use `#transient` if you want crash-only restarts, `#permanent` for always.
- Calling `process: 0` on `TaskWorker` intentionally crashes it (for the demo).
  The `WorkerPool` supervisor restarts a fresh worker for the next `startChild`.
