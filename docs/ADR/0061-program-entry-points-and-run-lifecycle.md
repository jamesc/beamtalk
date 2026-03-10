# ADR 0061: Program Entry Points and Run Lifecycle

## Status
Accepted (2026-03-10)

## Context

### The Problem

`beamtalk run` has accumulated three separate entry-point mechanisms with inconsistent behaviour and a critical class-loading bug:

1. **`[package] start = "module"`** — calls `bt@pkg@module:start/0` directly after starting `beamtalk_runtime`. Added early; never widely used.
2. **`[run] entry = "Main run"`** — a Beamtalk expression evaluated at REPL startup. Recognised by `beamtalk repl` but **silently ignored by `beamtalk run`** (sicp fails with "No start module defined").
3. **`[application] supervisor = "AppSup"`** — starts the package as an OTP application. Works structurally but **crashes at startup** because child classes (`WorkerPool`, `EventLogger`) are not registered in the class registry when the root supervisor's `init/1` calls `class_children`. The `beamtalk_workspace` bootstrap that topologically sorts and registers all project classes is never started in the `beamtalk run` path.

The root cause of the class-loading bug: `beamtalk run` only starts `beamtalk_runtime`. The workspace — which contains `beamtalk_workspace_bootstrap` — is only started by `beamtalk repl`. Any program referencing more than one class is therefore fragile under `beamtalk run`.

### Two Distinct Program Kinds

Real-world packages fall into two categories with different lifecycle needs:

**Scripts** (`[run]`): batch programs that run to completion and exit. Compilers, test harnesses, code generators. Operator expectation: `beamtalk run .` starts the program, it runs in the foreground, and the shell returns when it finishes. No long-lived process left behind.

**Services** (`[application]`): long-running OTP applications with supervision trees. Operator expectation: `beamtalk run .` starts the service in the background and the shell returns immediately. The service stays running and can be inspected or controlled later by connecting a REPL.

The existing workspace registry (`~/.beamtalk/workspaces/`) and `--ephemeral` flag already provide the right lifecycle primitives for services.

### Current `beamtalk repl` behaviour with `[run]`

`beamtalk repl` already reads `[run] entry` and auto-evaluates it after bootstrap — the REPL starts with the entry expression live. This is correct and is preserved.

## Decision

### 1. Two entry-point kinds in `beamtalk.toml`

```toml
# Script: run to completion, shell returns when done
[run]
entry = "Main run"

# Service: supervised OTP application, stays running in background
[application]
supervisor = "AppSup"
```

`[package] start` is **removed**. Packages using it migrate to `[run] entry = "ModuleName start"`.

### 2. Operator mental model

The workspace is the runtime environment for all Beamtalk programs. It bootstraps all project classes before any user code runs, hosts singletons like `TranscriptStream`, and supervises actors. There are two workspace modes:

**Run mode** (scripts): workspace starts, program runs, workspace exits with the program. The workspace is not registered in `~/.beamtalk/workspaces/` and has no REPL listener — it is not connectable. Use `beamtalk repl .` if interactive access is needed.

**Persistent mode** (services and all `beamtalk repl` sessions): workspace registers in `~/.beamtalk/workspaces/`, starts a REPL server on a free port, and remains alive until explicitly stopped or the `--ephemeral` session disconnects.

The rule is simple: **if `beamtalk run` will return your shell, the workspace persists. If `beamtalk run` blocks, the workspace exits with the program.**

### 3. Command behaviour

| Command | `[run] entry` (script) | `[application] supervisor` (service) |
|---|---|---|
| `beamtalk run .` | Workspace (run mode) → eval entry → **blocks until done, shell returns on exit** | Workspace (persistent) → start OTP app → **shell returns immediately, service running in background** |
| `beamtalk repl .` (no workspace running) | Workspace (persistent) → auto-eval entry → open REPL | Workspace (persistent) → start OTP app → open REPL |
| `beamtalk repl .` (workspace running) | Connect to existing workspace | Connect to existing workspace |
| `beamtalk repl . --ephemeral` | Workspace (persistent) → auto-eval entry → REPL → **tears down on disconnect** | Workspace (persistent) → start OTP app → REPL → **tears down on disconnect** |

What an operator sees when running a service:
```
$ beamtalk run .
Building...
Started otp_tree v0.1.0
  Supervisor : AppSup
  REPL port  : 4001   (connect with: beamtalk repl)
$
```

### 4. Workspace-first startup (structural fix)

Both modes start the workspace before executing any user code. This guarantees all project classes are registered before any method dispatch occurs, fixing the OTP supervisor class-loading crash.

**Script startup:**
```
build
  → start beamtalk_runtime
  → start beamtalk_workspace (run mode: no REPL server, not registered)
      → bootstrap loads + registers all bt@*.beam in topo order
      → TranscriptStream, actor_sup, singletons all running
  → call beamtalk_{app}_entry:run/0
  → BEAM node exits when run/0 returns
```

**Service startup:**
```
build
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

### 6. Generated entry module for `[run]`

`beamtalk build` compiles `[run] entry = "Main run"` into a small Erlang module:

```erlang
%% Generated by beamtalk build. Do not edit.
-module(beamtalk_sicp_entry).
-export([run/0]).

run() ->
    ClassPid = beamtalk_class_registry:whereis_class('Main'),
    ClassTag = beamtalk_class_registry:class_object_tag('Main'),
    ClassSelf = {beamtalk_object, ClassTag, 'bt@sicp@main', ClassPid},
    'bt@sicp@main':'run'(ClassSelf, #{}).
```

Errors in the entry expression (unknown class, wrong arity) are caught at **build time**, not at startup. This is consistent with how the OTP application callback is generated.

**Limitation:** `[run] entry` supports unary message sends only (`"Main run"`, `"App start"`). Multi-keyword selectors (`"Server startOn: 8080"`) are not supported as entry points — use a zero-argument wrapper method instead.

## Prior Art

### Elixir / Mix

`mix run` executes a script and exits. `mix phx.server` starts a long-running service in the foreground. Releases (`mix release`) produce a self-contained OTP release with `bin/app start` (daemon) and `bin/app console` (interactive). The script/service distinction is encoded in separate commands, not a single entry-point field. Beamtalk unifies these under one command (`beamtalk run`) dispatching on program kind.

### Erlang / rebar3

`rebar3 shell` starts an Erlang shell with the project loaded — analogous to `beamtalk repl`. Running an OTP application in production uses releases (`rebar3 as prod release`). There is no single equivalent of `beamtalk run` for ad-hoc execution; developers use `erl -eval` directly. Beamtalk improves on this with a first-class run command that handles class loading automatically.

### Node.js / npm

`node index.js` runs a script in the foreground. Long-running services use process managers (pm2, systemd) externally. npm scripts in `package.json` (`"start": "node server.js"`) are the closest analogue to `[run] entry`. No native detach — operators reach for external tools. Beamtalk's `[application]` mode provides native detach without an external process manager.

### Go

`go run .` compiles and runs in the foreground — analogous to `beamtalk run .` for scripts. No built-in daemon/background mode; operators use systemd or Docker. Go's single entry point (`func main()`) is simpler than Beamtalk's two-kind model but less expressive for supervised services.

### What Beamtalk adopts

- Mix's clean script/service distinction, but unified under one command
- OTP's supervision tree as a first-class citizen of the run model (not bolted on via an external process manager)
- The workspace-as-persistent-environment model has no direct analogue in the above; it is specific to Beamtalk's interactive-development philosophy

## User Impact

### Newcomer
Scripts just work: `beamtalk run .` runs and exits. The distinction between scripts and services is declared in `beamtalk.toml`, not inferred from the code. Error messages guide migration from `[package] start`.

### Smalltalk developer
Familiar with image-based persistence and interactive environments. The persistent workspace model maps naturally — the workspace is an image-like environment that survives the REPL session. `TranscriptStream` works as expected in both modes.

### Erlang/OTP developer
Services use standard OTP application and supervisor mechanics. The workspace is an OTP application sitting above `beamtalk_runtime`. The generated OTP app callback and entry module follow the same patterns as manually written OTP code. One concern: the workspace adds an additional supervision layer not present in a plain OTP release — operators familiar with `observer` and crash reports need to understand `beamtalk_workspace_sup` is in the tree.

### Operator
`beamtalk run .` on a service returns the shell with a REPL port printed. `beamtalk repl` connects. `beamtalk run . --foreground` (planned) for CI/systemd. The absence of `beamtalk stop` at initial implementation is a gap — manual `kill` or `beamtalk repl` + actor termination is required.

## Steelman Analysis

### "Keep `[package] start` — one less concept"
A newcomer arriving from Node.js or Python sees `start = "main"` in a config file and immediately understands it. `[run] entry = "Main run"` requires knowing Beamtalk's message-send syntax to read the config file. The config file should not require language knowledge to understand. Counter: the entry expression is a single unary send; the doc comment in `beamtalk.toml` templates can explain it. The symmetry gain (`[run]` mirrors `[application]`) outweighs the readability cost.

### "Always use full workspace — simpler mental model"
Two workspace modes (run vs persistent) mean operators must learn when each applies. A single mode eliminates this. The idle monitor would clean up script workspaces automatically. Counter: short-lived script workspaces polluting `~/.beamtalk/workspaces/` and binding TCP ports create real operational noise. The two-mode model maps to a real distinction in program kind, not an arbitrary implementation detail.

### "Services should block by default — principle of least surprise"
`beamtalk run .` blocking until the service exits is what every other tool does. Node, Go, Python — all block. Detach-by-default is the surprising behaviour. Operators in CI will hit this immediately. Counter: this is strongly cohort-dependent. Local development operators (the primary target) want their shell back. CI operators use `--foreground`. The default should match the majority use case, with `--foreground` for the minority. The tension here is real and the `--foreground` alternative is partially accepted.

### "Generated entry module is over-engineering — just use eval"
Runtime string eval of `"Main run"` is four lines of code vs. a build-time codegen pipeline. The simplicity benefit is large. Counter: build-time errors are worth the investment for a feature that runs on every `beamtalk run .`. The generated module pattern is already established (OTP app callback). Consistency with existing patterns outweighs the implementation simplicity of eval.

## Alternatives Considered

### Keep `[package] start`

Redundant with `[run] entry = "ModuleName start"`. Two mechanisms for the same thing creates documentation burden and confusion. Removed.

### Runtime string eval for `[run] entry`

Pass the raw `entry` string to `beamtalk_workspace_eval:eval/1` at startup. Simpler build step, but errors surface at runtime. Inconsistent with the generated OTP app callback pattern. Rejected.

### Always register run-mode workspaces

If scripts registered in `~/.beamtalk/workspaces/` with a REPL server, `beamtalk repl .` could attach to a running script. Rejected: scripts are short-lived by definition, the workspace would almost always be gone before you could connect, and the overhead (port binding, registry entry, idle monitor) is wasteful for batch programs. Use `beamtalk repl .` directly if interactive access to a script's environment is needed.

### Blocking foreground mode for services (`--foreground`)

Keep `beamtalk run .` blocking for `[application]` — monitor root supervisor, exit on DOWN — or accept this as a `--foreground` flag. CI environments, Docker containers, and systemd unit files all require the process to stay in the foreground; detaching breaks these use cases. **Partially accepted:** a `--foreground` flag on `beamtalk run` that keeps the CLI blocking (and omits the REPL server) is a planned follow-up. The default remains detach-and-persist, matching the local development use case that is the primary target for this ADR.

### OTP releases

OTP releases (`mix release`-style) are the canonical solution for running supervised BEAM applications as daemons — `bin/app start` (daemon), `bin/app console` (interactive), `bin/app stop`. Beamtalk could generate a release instead of a workspace-backed daemon. Rejected for this ADR: releases require additional tooling (`rebar3 as prod release`, `mix release`), are heavier than needed for local development, and do not integrate with the workspace/REPL connectivity model. Release support is a future concern for production deployments.

## Consequences

### Positive
- Class-loading bug in OTP supervisor startup is fixed structurally, not patched
- `[run] entry` works consistently in both `beamtalk run` and `beamtalk repl`
- `beamtalk run` for services gives a clean startup message and returns the shell
- Build-time validation of entry expressions catches errors early
- Single entry-point mechanism (`[run]`) replaces two overlapping ones

### Negative
- `[package] start` removed — existing packages need one-line migration
- `beamtalk_workspace_sup` needs `repl` flag and `tcp_port` made optional (small Erlang change)
- Services started with `beamtalk run` need explicit stopping — `beamtalk stop` is follow-up work; until then, operators must use `kill` or connect via REPL
- `beamtalk run . --foreground` for CI/Docker/systemd is deferred — those environments will see `beamtalk run .` detach unexpectedly until `--foreground` is implemented
- Service startup failure window: the CLI detaches after reading the startup port line; a crash after that point is silent to the CLI (visible only in workspace logs and REPL)

### Neutral
- `beamtalk repl` behaviour is unchanged from an operator perspective; only the internal startup path gains workspace-first class loading

## Implementation

### Phase 1 — Workspace run mode (`beamtalk_workspace_sup.erl`)
- Add `repl => boolean()` to `workspace_config()` type (default `true`)
- `tcp_port` becomes optional when `repl = false` (currently required)
- Skip `beamtalk_repl_server`, `beamtalk_idle_monitor`, `beamtalk_session_sup` when `repl = false`
- Skip workspace registration in `~/.beamtalk/workspaces/` when `repl = false`

### Phase 2 — Build: generated entry module (`build.rs`)
- Parse `[run] entry` string, resolve class name and selector
- Emit `beamtalk_{app}_entry.erl` alongside other generated modules
- Fail build with a clear diagnostic if class or selector is unknown

### Phase 3 — CLI: unified `beamtalk run` (`run.rs`)
- Remove `run_package_as_otp_application` as a separate path
- Unify into single `run_package` dispatching on entry kind
- Script path: workspace eval with `repl = false`, blocking (`child.wait()`) until BEAM node exits
- Service path: workspace eval with `repl = true`; CLI reads startup output (port line) from child stdout, then drops the child handle (BEAM node is reparented to PID 1 / init and continues running). The BEAM node must not use `-noshell` in detached mode so it can receive signals; use `-noinput` instead. Add `--foreground` flag to override detach behaviour (blocks until supervisor exits).
- Startup failure detection: the service BEAM node must write a sentinel line to stdout on successful start (the port line); if the child exits before writing it, the CLI reports the failure and exits non-zero.

### Phase 4 — Remove `[package] start`
- Remove `start` field from `PackageManifest`
- Update error message to guide migration to `[run]`
- Update tests

## Migration Path

Packages using `[package] start = "module"`:

```toml
# Before
[package]
start = "main"

# After
[run]
entry = "Main start"
```

The class name is the PascalCase of the source file name (`main.bt` → `Main`).

## Future: Release Mode

`beamtalk release` (planned, separate ADR) produces a self-contained OTP release for production deployment. This introduces a third operational mode alongside run mode and full workspace:

| Mode | Bootstrap | REPL server | Workspace registry | Use case |
|---|---|---|---|---|
| Run mode (`repl=false`) | ✓ | ✗ | ✗ | Scripts via `beamtalk run` |
| Full workspace | ✓ | ✓ | ✓ | Dev: `beamtalk run` (services) and `beamtalk repl` |
| Release | ✗ (pre-loaded) | ✓ | ✗ | Production deployments |

**Class loading in releases:** All project `bt@*.beam` files are bundled in the release image and pre-loaded at boot via the release boot script. `beamtalk_workspace_bootstrap` is not needed — classes are already registered before the root supervisor starts. This sidesteps the class-loading ordering problem that ADR 0061 fixes for dev mode.

**REPL against a release:** Production nodes should be connectable via `beamtalk repl --host prod.example.com --port 4001`, using the same protocol as dev workspace connections. The release includes a minimal REPL server (`beamtalk_repl_server` + `beamtalk_session_sup`) without the rest of the workspace machinery. Security defaults are stricter than dev: TLS + authentication required (see ADR 0058).

**Design constraint for the release ADR:** `beamtalk_workspace_sup` must support release mode cleanly — either via a third config variant or by extracting `beamtalk_repl_server` + `beamtalk_session_sup` into a standalone OTP application that releases can include without pulling in the full workspace.

## Design Notes

**Multiple `beamtalk run .` on a running service:** Idempotent. Before starting, `beamtalk run .` scans `~/.beamtalk/workspaces/` for a live workspace matching the project path. If found, prints "Already running (REPL port N) — connect with: beamtalk repl" and exits 0. No rebuild, no duplicate process. Consistent with `systemctl start` / `docker start` conventions.

**`beamtalk test` class loading:** Not affected by this ADR. The test runner explicitly calls `code:ensure_loaded/1` on each package module before running EUnit, triggering each module's `-on_load` → `register_class/0`. Classes are registered before any dispatch occurs.

## References
- Related ADRs: ADR 0004 (Persistent Workspace Management), ADR 0026 (Package Manifest), ADR 0059 (Supervision Tree Syntax)
- Affected examples: `examples/otp-tree`, `examples/sicp`
- Runtime: `beamtalk_workspace_sup.erl`, `beamtalk_workspace_bootstrap.erl`, `beamtalk_workspace_meta.erl`
- CLI: `crates/beamtalk-cli/src/commands/run.rs`, `build.rs`, `manifest.rs`
- Implementation issues: BT-1317 (workspace run mode), BT-1318 (generated entry module), BT-1319 (OTP app callback workspace-first), BT-1320 (unified beamtalk run)
- Follow-up work: `beamtalk stop` command, `--foreground` flag, multi-package workspace support
