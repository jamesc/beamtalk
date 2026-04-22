# otp-tree — Agent Guide

OTP supervision tree example: static supervisor, dynamic worker pool, and
fault isolation between actors.

## Workspace Structure

```text
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
  runtime via `pool startChild unwrap` / `(pool startChild: args) unwrap`.
- **`class supervisionPolicy`** — per-actor restart strategy (`#permanent`,
  `#transient`, `#temporary`). Read by the supervisor when building child specs.
- **`supervise`** — starts the supervisor process (or returns the
  already-running instance if called again). Returns `Result(Self, Error)`
  (ADR 0080); use `unwrap` for boot-style "crash on failure", or
  `ifOk:ifError:` / `andThen:` for recoverable start flows.
- **`startChild`** / **`startChild:`** — spawns a new child on a
  `DynamicSupervisor`. Returns `Result(C, Error)` (the child type is narrowed
  via the `DynamicSupervisor(C)` parameter).
- **`terminate:`** / **`terminateChild:`** — returns `Result(Nil, Error)` and
  is idempotent on `not_found` (already-gone children yield `Result ok: nil`).
- **`current`** — returns the running supervisor instance looked up by class
  name. Available on `Supervisor` and `DynamicSupervisor` subclasses. Regular
  actors do not have `current`; reach them via the supervisor using `which:`.
- **Fault isolation** — an error raised by `TaskWorker` propagates to the
  caller only; `EventLogger` and `WorkerPool` are completely unaffected.
  If a `TaskWorker` process truly crashes (abnormal exit), `WorkerPool` will
  restart it automatically because `#transient` workers are restarted on crash.

## Development Workflow

The `.mcp.json` MCP server provides a persistent REPL session. Use it as
your primary development environment — not CLI commands.

**Session startup:**

1. Call `describe` to discover available operations
2. Call `load_project` with `include_tests: true` to load all source + tests
3. On a new codebase, read the language guide at https://www.beamtalk.dev/docs/language-features

**Edit → Reload → Test → Debug loop:**

1. Edit a `.bt` source file
2. `evaluate: 'Workspace load: "path"'` or `evaluate: "ClassName reload"`
   — or `load_project` again after multi-file edits
3. `test` with class name or file path — fast, no recompile
4. `evaluate` to debug failures — bindings preserved from prior calls
5. Only use CLI `beamtalk test` as a final full-suite check before committing

## Live Workspace (MCP)

The `.mcp.json` in this project configures the `beamtalk` MCP server, which gives
you live access to a running REPL. Claude Code starts it automatically via
`beamtalk-mcp --start` — no manual `beamtalk repl` required.

**Prefer MCP tools over guessing.** If you're uncertain what a method returns or
whether code is correct, evaluate it directly rather than inferring from source.

| Tool | When to use |
|------|-------------|
| `describe` | First call — discover operations and protocol version |
| `load_project` | Session startup — load all source + test files |
| `evaluate` | Test expressions, debug, call Workspace/Beamtalk APIs |
| `test` | Run tests by class name or file path |
| `complete` | Autocompletion suggestions |
| `search_examples` | Find patterns and working code (offline) |
| `show_codegen` | Inspect generated Core Erlang |
| `inspect` | Examine a live actor's state |

## Common Pitfalls

- `current` returns `nil` if the supervisor has not been started yet.
  Always call `supervise` (and `unwrap` the Result, or branch on it) before
  `current`. Note: `current` is only available on `Supervisor` /
  `DynamicSupervisor` subclasses, not on plain `Actor` subclasses.
- `supervise`, `startChild`, `startChild:`, `terminate:`, and `terminateChild:`
  all return `Result` (ADR 0080). Chaining sends directly on the return value
  — e.g. `AppSupervisor supervise stop` — is a bug; the receiver is a
  `Result`, not the supervisor. Insert `unwrap` (or branch with `ifOk:ifError:`)
  before the chained send: `(AppSupervisor supervise) unwrap stop`.
- `DynamicSupervisor` has no `class children` — only `class childClass`.
  Defining `children` on a `DynamicSupervisor` subclass has no effect.
- A `#temporary` worker that crashes is **not restarted** — that is intentional.
  Use `#transient` if you want crash-only restarts, `#permanent` for always.
- Calling `process: 0` on `TaskWorker` raises a `user_error` to the caller via
  `self error:`. This does NOT crash the actor process — the gen_server stays
  alive. The demo illustrates error isolation (other actors unaffected), not
  supervisor restart. Use `w1 stop` to terminate a worker cleanly.
