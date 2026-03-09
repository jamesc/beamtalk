# OTP Supervision Tree — Fault-Tolerant Service

This example demonstrates Beamtalk's **OTP supervision tree** support: how to
declare a static supervisor, a dynamic worker pool, and per-actor restart
policies — and how the tree keeps a service running when individual actors crash.

## What You'll Learn

| Concept | Where |
|---------|-------|
| **Static supervisor** | `AppSupervisor` — `Supervisor subclass:` with a fixed child list |
| **Dynamic supervisor** | `WorkerPool` — `DynamicSupervisor subclass:` for runtime-spawned workers |
| **Restart policies** | `#permanent`, `#transient`, `#temporary` on each actor class |
| **Fault isolation** | A crashed worker does not disturb the logger or other workers |
| **`current` lookup** | Finding a running supervisor by class name without holding a reference |
| **OTP application** | `[application] supervisor =` in `beamtalk.toml` for `beamtalk run` |

## Supervision Tree

```
AppSupervisor  (one_for_one)
├── EventLogger   (permanent)   — accumulates log entries
└── WorkerPool    (permanent)   — DynamicSupervisor
    └── TaskWorker…             — transient workers, started on demand
```

`one_for_one` strategy: if one child crashes, only that child is restarted.
The other children continue running unchanged.

## Classes

| Class | Type | Restart | Role |
|-------|------|---------|------|
| `AppSupervisor` | Supervisor | — | Root supervisor; declares `EventLogger` and `WorkerPool` as children |
| `EventLogger` | Actor | `#permanent` | Appends log messages; always restarted if it crashes |
| `WorkerPool` | DynamicSupervisor | `#permanent` | Pool of `TaskWorker` actors; started/stopped at runtime |
| `TaskWorker` | Actor | `#transient` | Doubles an input value; raises an error to the caller on `process: 0` |

## Starting the Workspace

```bash
cd examples/otp-tree
beamtalk repl
```

All source files in `src/` load automatically.

## REPL Walkthrough

### 1. Start the supervision tree

```text
> sup := AppSupervisor supervise
```

`supervise` starts `AppSupervisor` as a named OTP supervisor process.
Its two children — `EventLogger` and `WorkerPool` — start automatically.

```text
> sup count
2
```

### 2. Reach named supervisors without a direct reference

`current` looks up a running supervisor by its class name.  Use `which:` to
reach a worker actor managed by the supervisor:

```text
> pool := WorkerPool current
> logger := sup which: EventLogger
```

### 3. Log some events

```text
> logger log: "system started"
> logger log: "ready"
> logger events
#("system started", "ready")
```

### 4. Spawn workers dynamically

`startChild` asks `WorkerPool` to start a new `TaskWorker` and returns it:

```text
> w1 := pool startChild
> w2 := pool startChild
> pool count
2
```

### 5. Process tasks

```text
> w1 process: 21
42

> w2 process: 7
14

> w1 jobCount
1

> w2 jobCount
1
```

### 6. Trigger an error — observe fault isolation

`process: 0` raises an error back to the caller via `self error:`.  The worker
actor itself stays alive; the error propagates to whoever called `process: 0`:

```text
> w1 process: 0
ERROR: TaskWorker: refusing zero — crashing now
```

The error is isolated to the caller.  `w1` is still running, and the rest of
the tree is completely unaffected:

```text
> logger log: "error handled — other actors unaffected"
> logger events
#("system started", "ready", "error handled — other actors unaffected")

> w2 process: 5
10

> w1 process: 3
6
```

`w2` kept running throughout.  `w1` kept running throughout.  The logger kept
running throughout.

### 7. Verify the pool still has two children

```text
> pool count
2
```

Both workers are still alive — no restart was needed because neither process
crashed.

### 8. Tear down cleanly

```text
> sup stop
```

## How It Works

`AppSupervisor` is a `Supervisor subclass:` that overrides two class-side
methods:

```beamtalk
Supervisor subclass: AppSupervisor
  class strategy -> Symbol => #oneForOne
  class children => #(EventLogger, WorkerPool)
```

`class children` returns a list of classes.  The Beamtalk runtime reads each
class's `supervisionPolicy` to build the OTP child spec.  `EventLogger`
declares `#permanent`; `WorkerPool` is a `DynamicSupervisor` (treated as
`#permanent` automatically).

`WorkerPool` is a `DynamicSupervisor subclass:` that names the worker class:

```beamtalk
DynamicSupervisor subclass: WorkerPool
  class childClass => TaskWorker
```

Children are added at runtime with `pool startChild`.  The supervisor watches
each worker independently.

`TaskWorker` declares `#transient` — restarted on abnormal exit (crash), not on
clean stop:

```beamtalk
Actor subclass: TaskWorker
  class supervisionPolicy -> Symbol => #transient
  ...
```

## OTP Application

`beamtalk.toml` declares `AppSupervisor` as the application root:

```toml
[application]
supervisor = "AppSupervisor"
```

This lets `beamtalk run` start the entire tree as a proper OTP application
rather than running an imperative script.  `Workspace supervisor` returns the
live root supervisor instance.

## Key Takeaways

- **`Supervisor subclass:`** for static trees — children known at compile time
- **`DynamicSupervisor subclass:`** for pools — children added and removed at
  runtime
- **`supervisionPolicy`** on each actor class controls restart behaviour
- **`supervise`** starts the supervisor; **`current`** finds the live supervisor
  instance (Supervisor/DynamicSupervisor only); **`which:`** reaches a specific
  child actor managed by the supervisor
- **Fault isolation is automatic** — OTP's one_for_one strategy contains crashes
  to the failing child only
- **`[application] supervisor`** in `beamtalk.toml` wires the tree into the OTP
  application lifecycle
