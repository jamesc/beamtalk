# ADR 0061: Program Entry Points and Run Lifecycle

## Status
Accepted (2026-03-10)

## Context

### The Problem

`beamtalk run` has accumulated three separate entry-point mechanisms with inconsistent behaviour and a critical class-loading bug:

1. **`[package] start = "module"`** — calls `bt@pkg@module:start/0` directly after starting `beamtalk_runtime`. Added early; never widely used.
2. **`[run] entry = "Main run"`** — a Beamtalk expression evaluated at REPL startup. A poor-man's entry point added before proper OTP integration existed. Recognised by `beamtalk repl` but **silently ignored by `beamtalk run`** (sicp fails with "No start module defined").
3. **`[application] supervisor = "AppSup"`** — starts the package as an OTP application. Works structurally but **crashes at startup** because child classes (`WorkerPool`, `EventLogger`) are not registered in the class registry when the root supervisor's `init/1` calls `class_children`. The `beamtalk_workspace` bootstrap that topologically sorts and registers all project classes is never started in the `beamtalk run` path.

The root cause of the class-loading bug: `beamtalk run` only starts `beamtalk_runtime`. The workspace — which contains `beamtalk_workspace_bootstrap` — is only started by `beamtalk repl`. Any program referencing more than one class is therefore fragile under `beamtalk run`.

### Two Distinct Program Kinds

Real-world packages fall into two categories with different lifecycle needs:

**Scripts**: batch programs that run to completion and exit. Compilers, test harnesses, code generators. Operator expectation: `beamtalk run ClassName selector` starts the program in the foreground and the shell returns when it finishes.

**Services** (`[application]`): long-running OTP applications with supervision trees. Operator expectation: `beamtalk run .` starts the service in the background and the shell returns immediately. The service stays running and can be inspected or controlled later by connecting a REPL.

The existing workspace registry (`~/.beamtalk/workspaces/`) and `--ephemeral` flag already provide the right lifecycle primitives for services.

## Decision

### 1. Entry-point model

Scripts and services are invoked differently:

```bash
# Script: explicit class and selector on the CLI
beamtalk run Main run
beamtalk run Database migrate

# Service: supervisor declared in toml, started by beamtalk run .
# beamtalk.toml:
[application]
supervisor = "AppSup"
```

**`[run]` is removed.** It was a workaround — a way to express "run this" before proper OTP integration existed. The right place for a script entry point is the CLI, not config. **`[package] start` is also removed** — superseded by the CLI form.

**Future:** `[scripts]` as a named-shortcut table (like npm scripts or Justfile) is planned but deferred. It will allow `beamtalk run migrate` as a shorthand for `beamtalk run Database migrate`, documented in the project manifest.

### 2. Operator mental model

The workspace is the runtime environment for all Beamtalk programs. It bootstraps all project classes before any user code runs, hosts singletons like `TranscriptStream`, and supervises actors. There are two workspace modes:

**Run mode** (scripts): workspace starts, program runs, workspace exits with the program. Not registered in `~/.beamtalk/workspaces/`, no REPL listener — not connectable. Use `beamtalk repl .` if interactive access is needed.

**Persistent mode** (services and all `beamtalk repl` sessions): workspace registers in `~/.beamtalk/workspaces/`, starts a REPL server on a free port, remains alive until explicitly stopped or `--ephemeral` session disconnects.

The rule: **if `beamtalk run` returns your shell, the workspace persists. If it blocks, the workspace exits with the program.**

### 3. Command behaviour

| Command | Script | Service (`[application]`) |
|---|---|---|
| `beamtalk run Main run` | Run mode workspace → call `Main>>run` → **blocks, exit on return** | — |
| `beamtalk run Main run` (workspace running) | Connect to existing workspace → call `Main>>run` → exit | — |
| `beamtalk run .` | — | Persistent workspace → start OTP app → **shell returns, service running** |
| `beamtalk run .` (service running) | — | Print existing port → exit 0 (idempotent) |
| `beamtalk repl .` (no workspace) | Persistent workspace → open REPL | Persistent workspace → start OTP app → open REPL |
| `beamtalk repl .` (workspace running) | Connect to existing workspace | Connect to existing workspace |
| `beamtalk repl . --ephemeral` | Persistent workspace → REPL → **tears down on disconnect** | Persistent workspace → OTP app → REPL → **tears down on disconnect** |

What an operator sees when running a service:
```text
$ beamtalk run .
Building...
Started otp_tree v0.1.0
  Supervisor : AppSup
  REPL port  : 4001   (connect with: beamtalk repl)
$
```

Running a script against a live service:
```text
$ beamtalk run .          # service already running
otp_tree v0.1.0 is already running (REPL port 4001)
$ beamtalk run Database migrate
Running Database>>migrate in otp_tree workspace...
Migrated 3 schemas.
$
```

### 4. Workspace-first startup (structural fix)

Both modes start the workspace before executing any user code. This guarantees all project classes are registered before any method dispatch occurs, fixing the OTP supervisor class-loading crash.

**Script startup:**
```text
beamtalk run Main run
  → build project
  → start beamtalk_runtime
  → start beamtalk_workspace (run mode: no REPL server, not registered)
      → bootstrap loads + registers all bt@*.beam in topo order
      → TranscriptStream, actor_sup, singletons all running
  → resolve class 'Main', call run/0
  → BEAM node exits when run/0 returns
```

**Script against running service:**
```text
beamtalk run Database migrate
  → detect existing workspace for project path in ~/.beamtalk/workspaces/
  → connect to REPL server on existing workspace port
  → eval Database>>migrate in live workspace
  → disconnect, exit
```

**Service startup:**
```text
beamtalk run .
  → build project
  → start beamtalk_runtime
  → start beamtalk_workspace (persistent: REPL server, registered in ~/.beamtalk/workspaces/)
      → bootstrap loads + registers all bt@*.beam in topo order
  → application:ensure_all_started(app_name)
      → OTP supervisor tree starts; class registry fully populated
  → print "Started <app> — REPL port <N>"
  → CLI process exits; BEAM node continues running
```

### 5. Workspace run mode

`beamtalk_workspace_sup` gains a `repl => boolean()` config key (default `true`). When `repl = false` (run mode):

**Omitted:**
- `beamtalk_repl_server` — no TCP REPL listener
- `beamtalk_idle_monitor` — no auto-shutdown on inactivity
- `beamtalk_session_sup` — no per-connection shell processes

**Always present:**
- `beamtalk_workspace_meta` — workspace state
- `beamtalk_class_events` — class-loaded pub/sub
- `beamtalk_workspace_bootstrap` — loads and registers all project classes
- Singleton actors (`TranscriptStream`, `WorkspaceInterface`, etc.)
- `beamtalk_actor_sup` — supervises user-spawned actors

Run-mode workspaces are **not registered** in `~/.beamtalk/workspaces/` — there is no REPL server to connect to.

## Prior Art

### Elixir / Mix

`mix run` executes a script and exits. `mix phx.server` starts a long-running service. Releases (`mix release`) produce `bin/app start` (daemon) and `bin/app console` (interactive). Script/service distinction is in separate commands. Beamtalk uses one command (`beamtalk run`) dispatching on whether a class/selector is given or `.` is used.

### Erlang / rebar3

`rebar3 shell` starts an Erlang shell with the project loaded — analogous to `beamtalk repl`. No single equivalent of `beamtalk run` for ad-hoc execution; developers use `erl -eval`. Beamtalk improves on this with a first-class run command handling class loading automatically.

### Node.js / npm

`node index.js` runs a script. `npm run migrate` executes named scripts from `package.json`. Long-running services use external process managers. The npm named-scripts model is the inspiration for the planned `[scripts]` table (future work).

### Go

`go run .` compiles and runs in the foreground. Single entry point (`func main()`). No built-in daemon mode. Simpler than Beamtalk's model but less expressive for supervised services.

### What Beamtalk adopts

- Explicit CLI args for script entry (Go / `go run`) rather than config-file expressions
- Named scripts as a convenience layer over CLI args (npm / `package.json` scripts) — future
- OTP supervision tree as a first-class citizen, not bolted on via an external process manager
- The workspace-as-persistent-environment model is specific to Beamtalk's interactive-development philosophy

## User Impact

### Newcomer
Scripts are invoked explicitly: `beamtalk run Main run`. No separate entry-point config is needed for one-off runs; the command still runs within a Beamtalk project context (a `beamtalk.toml` manifest is still required). Services are declared in toml and started with `beamtalk run .`. Error messages guide migration from `[run]` and `[package] start`.

### Smalltalk developer
Familiar with image-based persistence and interactive environments. The persistent workspace model maps naturally — the workspace is an image-like environment that survives the REPL session. `TranscriptStream` works as expected in both modes.

### Erlang/OTP developer
Services use standard OTP application and supervisor mechanics. The workspace is an OTP application sitting above `beamtalk_runtime`. The generated OTP app callback follows standard OTP patterns. Note: `beamtalk_workspace_sup` appears in the OTP supervision tree — operators using `observer` need to know it is the Beamtalk runtime layer, not user code.

### Operator
`beamtalk run .` on a service returns the shell with a REPL port printed. `beamtalk repl` connects. `beamtalk run ClassName selector` runs scripts, connects to existing workspace if one is running. `--foreground` (planned) for CI/systemd.

## Steelman Analysis

### "Keep `[run]` — explicit entry point documentation"
The `[run]` section tells anyone reading the project what the entry point is, without running the code. `beamtalk run Main run` on the CLI has no discoverable default for the project. Counter: `[scripts]` (planned) solves this better — named, structured shortcuts documented in toml without the `[run]` string-expression limitation. The absence of `[run]` is temporary, not permanent.

### "Always use full workspace — simpler mental model"
Two workspace modes (run vs persistent) mean operators must learn when each applies. Counter: short-lived script workspaces polluting `~/.beamtalk/workspaces/` and binding TCP ports is real operational noise. The two-mode model maps to a real distinction in program kind.

### "Services should block by default — principle of least surprise"
`beamtalk run .` blocking until exit is what Node, Go, Python all do. Detach-by-default is surprising, especially in CI. Counter: local development (the primary target) wants the shell back. `--foreground` covers CI. The tension is real and `--foreground` is planned.

### "Config-driven entry point is better — no magic CLI incantation"
Forgetting `beamtalk run Main run` is easy; the toml documents what to run. Counter: this is exactly what `[scripts]` solves, without the `[run]` string-expression footgun. The right fix is structured named scripts, not a free-form expression in config.

## Alternatives Considered

### Keep `[run]` with structured fields

```toml
[run]
class = "Main"
selector = "run"
```

Structured (Anders-friendly), no magic string. But still config-driven, still only one entry point per project, still requires a toml change for every new script. The CLI form (`beamtalk run Main run`) is strictly more flexible with no downsides for the common case.

### Blocking foreground mode for services (`--foreground`)

Keep `beamtalk run .` blocking for `[application]`. CI environments, Docker, and systemd require foreground. **Partially accepted:** `--foreground` is planned as a follow-up flag. Default remains detach-and-persist for local development.

### OTP releases

OTP releases are the canonical solution for supervised BEAM applications as daemons. Rejected for this ADR: heavier than needed for local development, do not integrate with the workspace/REPL model. Planned as a future ADR. See also: Future Release Mode section.

### Named scripts now (`[scripts]`)

```toml
[scripts]
migrate = { class = "Database", selector = "migrate" }
```

The right long-term answer. Deferred: the CLI form covers the immediate need and `[scripts]` can be added without breaking changes later.

## Consequences

### Positive
- Class-loading bug in OTP supervisor startup fixed structurally, not patched
- `[run]` string-expression footgun removed entirely
- Scripts composable with running services: `beamtalk run Main migrate` connects to live workspace if present
- `beamtalk run` for services gives a clean startup message and returns the shell
- Single remaining toml entry-point concept (`[application]`) — unambiguous

### Negative
- `[run]` and `[package] start` removed — existing packages need migration
- `beamtalk_workspace_sup` needs `repl` flag and `tcp_port` made optional
- No discoverable default script entry point until `[scripts]` is implemented — operators must know the class/selector
- `beamtalk repl` auto-eval of `[run] entry` on startup is removed — projects relying on this need to update
- Services started with `beamtalk run` need explicit stopping — `beamtalk stop` is follow-up work
- `--foreground` for CI/Docker/systemd is deferred

### Neutral
- `beamtalk repl` lifecycle behaviour unchanged; only internal startup path gains workspace-first class loading

## Implementation

### Phase 1 — Workspace run mode (`beamtalk_workspace_sup.erl`) — BT-1317
- Add `repl => boolean()` to `workspace_config()` type (default `true`)
- `tcp_port` becomes optional when `repl = false`
- Skip `beamtalk_repl_server`, `beamtalk_idle_monitor`, `beamtalk_session_sup` when `repl = false`
- Skip workspace registration in `~/.beamtalk/workspaces/` when `repl = false`

### Phase 2 — OTP app callback starts full workspace before supervisor (`build.rs`, `run.rs`) — BT-1319
- Update generated `beamtalk_{app}_app.erl` eval to start workspace before `ensure_all_started`
- All `bt@*.beam` classes registered before root supervisor `init/1` runs

### Phase 3 — Unified `beamtalk run` (`run.rs`, `manifest.rs`) — BT-1320
- Remove `run_package_as_otp_application` as a separate path
- Add `beamtalk run ClassName selector` CLI form (positional args, build-time class resolution)
- Script path: detect running workspace → connect and eval, or start run-mode workspace
- Service path: reuse the existing detached-workspace launcher that `beamtalk repl` and workspace management already use. `beamtalk run` starts the package via a workspace in persistent mode (`repl = true`) and relies on the same cross-platform mechanism (Erlang `-detached` with `-noshell` plus `setsid`/umask on Unix, and process-group flags on Windows) to detach the BEAM node. The CLI reads startup output (REPL port sentinel line) from the bootstrap process on stdout and then exits. If the bootstrap process exits before writing the sentinel line, `beamtalk run` reports the failure and exits non-zero.
- Add `--foreground` flag to run without detaching (blocks until the top-level supervisor exits and preserves its exit status)
- Remove `[run]` and `[package] start` from manifest parsing; remove REPL auto-eval of `[run]`
- Update error messages to guide migration

## Migration Path

**`[package] start = "module"`:**
```bash
# Before: beamtalk.toml had start = "main"
# After:
beamtalk run Main start
```

**`[run] entry = "Main run"`:**
```bash
# Before: beamtalk.toml had [run] entry = "Main run"
# After:
beamtalk run Main run
```

**This is a breaking change.** The new CLI contract is `beamtalk run <ClassName> <selector>` — two positional arguments only. The old `[run] entry` field accepted arbitrary Beamtalk expressions (including keyword selectors with arguments, e.g. `entry = "App start: 'production'"`); the new CLI does not. Keyword-selector and multi-argument entry expressions must be refactored into a unary entry point or wrapped behind a named script (see planned `[scripts]` table). Update `beamtalk.toml` and all CI scripts to use the positional form.

**`beamtalk repl` auto-eval of `[run] entry`:** Remove `[run]` from toml. If the auto-eval was used to warm up the REPL, start the service with `beamtalk run .` first and connect with `beamtalk repl`.

## Future: Release Mode

`beamtalk release` (planned, separate ADR) produces a self-contained OTP release for production deployment. This introduces a third operational mode:

| Mode | Bootstrap | REPL server | Workspace registry | Use case |
|---|---|---|---|---|
| Run mode (`repl=false`) | ✓ | ✗ | ✗ | Scripts via `beamtalk run` |
| Full workspace | ✓ | ✓ | ✓ | Dev: `beamtalk run` (services) and `beamtalk repl` |
| Release | ✗ (pre-loaded) | ✓ | ✗ | Production deployments |

**Class loading in releases:** All `bt@*.beam` files bundled and pre-loaded at boot. `beamtalk_workspace_bootstrap` not needed — classes registered before supervisor starts. Sidesteps the class-loading problem ADR 0061 fixes for dev.

**REPL against a release:** `beamtalk repl --host prod.example.com --port 4001` connects via the same protocol as dev. The release includes a minimal REPL server (`beamtalk_repl_server` + `beamtalk_session_sup`) without the full workspace. Security defaults stricter: TLS + auth required (see ADR 0058).

**Design constraint for the release ADR:** `beamtalk_workspace_sup` must support release mode cleanly — either via a third config variant or by extracting `beamtalk_repl_server` + `beamtalk_session_sup` into a standalone OTP application releases can include without the full workspace.

## Design Notes

**Multiple `beamtalk run .` on a running service:** Idempotent. Scans `~/.beamtalk/workspaces/` for a live workspace matching the project path. If found, prints "Already running (REPL port N) — connect with: beamtalk repl" and exits 0. Consistent with `systemctl start` / `docker start` conventions.

**`beamtalk test` class loading:** Not affected. The test runner explicitly calls `code:ensure_loaded/1` on each package module before running EUnit, triggering `-on_load` → `register_class/0`. Classes registered before any dispatch.

**BT-1318 cancelled:** The generated entry module for `[run]` is no longer needed — `[run]` is removed entirely. BT-1320 absorbs the CLI arg parsing work.

## References
- Related ADRs: ADR 0004 (Persistent Workspace Management), ADR 0026 (Package Manifest), ADR 0059 (Supervision Tree Syntax)
- Affected examples: `examples/otp-tree`, `examples/sicp`
- Runtime: `beamtalk_workspace_sup.erl`, `beamtalk_workspace_bootstrap.erl`, `beamtalk_workspace_meta.erl`
- CLI: `crates/beamtalk-cli/src/commands/run.rs`, `build.rs`, `manifest.rs`
- Implementation issues: BT-1317 (workspace run mode), BT-1319 (OTP app callback workspace-first), BT-1320 (unified beamtalk run — absorbs BT-1318)
- Follow-up work: `[scripts]` named shortcuts, `beamtalk stop`, `--foreground` flag, `beamtalk release`
