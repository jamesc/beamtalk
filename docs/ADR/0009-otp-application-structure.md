# ADR 0009: OTP Application Structure — Split Workspace from Runtime

## Status
Implemented (2026-02-07)

## Context

### Problem

The beamtalk Erlang runtime is currently structured as two OTP applications:

- **`beamtalk_runtime`** — A monolithic 36-module application containing everything: primitive type implementations, object system, dispatch, bootstrap, actors, futures, hot reload, REPL server, REPL evaluation, workspace management, session supervision, and idle monitoring.
- **`beamtalk_stdlib`** — Compiled `.bt` → `.beam` files (no hand-written Erlang).

This monolithic structure creates several problems:

1. **Coupling:** `beamtalk_actor.erl` (core runtime) directly calls `beamtalk_repl_actors:register_actor/4` and `beamtalk_workspace_meta:register_actor/1`. Core runtime code depends on REPL code.

2. **Deployment inflexibility:** `beamtalk build` (batch compilation) loads the entire REPL infrastructure even though it doesn't need it. Future production deployments would include REPL code unnecessarily.

3. **Test isolation:** Runtime unit tests require REPL infrastructure to be running, making tests slower and more fragile.

4. **Unclear boundaries:** The 36 modules span at least 4 distinct concerns (primitives, object system, REPL, workspace management) with no structural separation.

### Current State

```
runtime/apps/
├── beamtalk_runtime/     # 36 modules — everything
│   └── src/
│       ├── beamtalk_integer.erl          # Primitive
│       ├── beamtalk_string.erl           # Primitive
│       ├── beamtalk_boolean.erl          # Primitive
│       ├── beamtalk_block.erl            # Primitive
│       ├── beamtalk_nil.erl              # Primitive
│       ├── beamtalk_float.erl            # Primitive
│       ├── beamtalk_tuple.erl            # Primitive
│       ├── beamtalk_dispatch.erl         # Object system
│       ├── beamtalk_object.erl           # Object system
│       ├── beamtalk_object_class.erl     # Object system
│       ├── beamtalk_object_instances.erl # Object system
│       ├── beamtalk_dynamic_object.erl   # Object system
│       ├── beamtalk_compiled_method.erl  # Object system
│       ├── beamtalk_primitive.erl        # Object system
│       ├── beamtalk_extensions.erl       # Object system
│       ├── beamtalk_bootstrap.erl        # Bootstrap
│       ├── beamtalk_stdlib.erl           # Stdlib loader
│       ├── beamtalk_actor.erl            # Actor lifecycle ← calls repl_actors!
│       ├── beamtalk_actor_sup.erl        # Actor supervision
│       ├── beamtalk_future.erl           # Async futures
│       ├── beamtalk_hot_reload.erl       # Hot code loading
│       ├── beamtalk_error.erl            # Error formatting
│       ├── beamtalk_repl.erl             # REPL main
│       ├── beamtalk_repl_eval.erl        # REPL evaluation
│       ├── beamtalk_repl_server.erl      # TCP server
│       ├── beamtalk_repl_shell.erl       # Shell session
│       ├── beamtalk_repl_state.erl       # Session state
│       ├── beamtalk_repl_actors.erl      # Actor registry
│       ├── beamtalk_repl_modules.erl     # Module tracking
│       ├── beamtalk_repl_protocol.erl    # JSON-RPC protocol
│       ├── beamtalk_workspace_sup.erl    # Workspace supervisor
│       ├── beamtalk_workspace_meta.erl   # Workspace metadata
│       ├── beamtalk_session_sup.erl      # Session supervisor
│       ├── beamtalk_idle_monitor.erl     # Idle timeout
│       ├── beamtalk_runtime_app.erl      # OTP app callback
│       └── beamtalk_runtime_sup.erl      # Root supervisor
└── beamtalk_stdlib/      # Compiled .bt (no .erl source)
    └── ebin/
```

### Coupling Analysis

The REPL/workspace modules form a clean layer on top of the core runtime, with **exactly one coupling point**:

```erlang
%% In beamtalk_actor.erl, register_spawned/4:
ok = beamtalk_repl_actors:register_actor(RegistryPid, Pid, Class, Module),
beamtalk_workspace_meta:register_actor(Pid),
```

No other core runtime modules reference REPL or workspace modules. The dependency is one-directional (core → REPL) at a single well-defined point via an application env callback.

### Constraints

- Must remain a rebar3 umbrella project (existing build infrastructure).
- `beamtalk_stdlib` app must be unchanged (compiled `.bt` output, no hand-written Erlang).
- Actor registration is required for any context managing live actors (REPL workspace tracking, hot reload safety, lifecycle cleanup). Batch compilation (`beamtalk build`) does not spawn actors and needs no registration. Production use (`beamtalk run`) would need its own registration for hot reload safety.
- Future workspace management (ADR 0004) will significantly expand the REPL/workspace layer.

## Decision

Split `beamtalk_runtime` into two OTP applications, creating a 3-app umbrella:

```
runtime/apps/
├── beamtalk_runtime/     # Core language runtime (23 modules)
│   └── src/
│       ├── beamtalk_integer.erl          # Primitive types
│       ├── beamtalk_string.erl
│       ├── beamtalk_boolean.erl
│       ├── beamtalk_block.erl
│       ├── beamtalk_nil.erl
│       ├── beamtalk_float.erl
│       ├── beamtalk_tuple.erl
│       ├── beamtalk_dispatch.erl         # Object system
│       ├── beamtalk_object.erl
│       ├── beamtalk_object_class.erl
│       ├── beamtalk_object_instances.erl
│       ├── beamtalk_dynamic_object.erl
│       ├── beamtalk_compiled_method.erl
│       ├── beamtalk_primitive.erl
│       ├── beamtalk_extensions.erl
│       ├── beamtalk_bootstrap.erl        # Bootstrap & lifecycle
│       ├── beamtalk_stdlib.erl
│       ├── beamtalk_actor.erl
│       ├── beamtalk_future.erl
│       ├── beamtalk_hot_reload.erl
│       ├── beamtalk_error.erl
│       ├── beamtalk_runtime_app.erl      # OTP plumbing
│       └── beamtalk_runtime_sup.erl
│
├── beamtalk_workspace/    # Workspace & interactive development (13 moved + 2 new) — NEW
│   └── src/
│       ├── beamtalk_repl.erl             # REPL main
│       ├── beamtalk_repl_eval.erl        # Expression evaluation
│       ├── beamtalk_repl_server.erl      # TCP server
│       ├── beamtalk_repl_shell.erl       # Shell session
│       ├── beamtalk_repl_state.erl       # Session state
│       ├── beamtalk_repl_actors.erl      # Actor registry
│       ├── beamtalk_repl_modules.erl     # Module tracking
│       ├── beamtalk_repl_protocol.erl    # JSON-RPC protocol
│       ├── beamtalk_workspace_sup.erl    # Workspace supervisor (root)
│       ├── beamtalk_workspace_meta.erl   # Workspace metadata
│       ├── beamtalk_session_sup.erl      # Session supervisor
│       ├── beamtalk_idle_monitor.erl     # Idle timeout
│       ├── beamtalk_actor_sup.erl        # Actor supervision (workspace-scoped)
│       ├── beamtalk_workspace_app.erl    # OTP app callback (new)
│       └── beamtalk_workspace_app_sup.erl # App-level supervisor (new)
│
└── beamtalk_stdlib/      # Compiled .bt → .beam (unchanged)
    └── ebin/
```

### Dependency Graph

```
beamtalk_workspace
    ↓ depends on
beamtalk_runtime
    ↓ depends on
beamtalk_stdlib
    ↓ depends on
kernel, stdlib
```

Dependencies flow strictly downward. `beamtalk_runtime` never imports `beamtalk_workspace`.

### Decoupling the Actor Registration

The one coupling point (`beamtalk_actor.erl` → `beamtalk_repl_actors`) is resolved with **optional callback registration**:

```erlang
%% In beamtalk_actor.erl — use application env for optional callback
%% register_spawned/4 registers an already-spawned actor with the REPL.
%% (Replaced the former spawn_with_registry/3,4 which both spawned and
%% registered, bypassing the module's initialize protocol.)
register_spawned(RegistryPid, ActorPid, ClassName, Module) ->
    case application:get_env(beamtalk_runtime, actor_spawn_callback) of
        {ok, CallbackMod} ->
            CallbackMod:on_actor_spawned(RegistryPid, ActorPid, ClassName, Module);
        undefined ->
            ok
    end.
```

```erlang
%% In beamtalk_workspace_app:start/2 — register the callback on startup
application:set_env(beamtalk_runtime, actor_spawn_callback, beamtalk_repl_actors).
```

The `beamtalk_repl_actors` module must export `on_actor_spawned/4`, which wraps the existing `register_actor/4` and `beamtalk_workspace_meta:register_actor/1` calls:

```erlang
%% In beamtalk_repl_actors.erl — new callback function
-spec on_actor_spawned(pid(), pid(), atom(), atom()) -> ok.
on_actor_spawned(RegistryPid, Pid, ClassName, Module) ->
    register_actor(RegistryPid, Pid, ClassName, Module),
    beamtalk_workspace_meta:register_actor(Pid),
    ok.
```

This pattern:
- Core runtime has zero compile-time dependency on REPL
- REPL registers itself at startup via application env
- Batch compilation (`beamtalk build`) works without REPL loaded
- Testable in isolation — mock or omit the callback

### Supervision Trees

**beamtalk_runtime** (unchanged from current core):

```
beamtalk_runtime_sup (one_for_one)
├── beamtalk_bootstrap (worker)
├── beamtalk_stdlib (worker)
└── beamtalk_object_instances (worker)
```

**beamtalk_workspace** (extracted from current workspace tree):

```
beamtalk_workspace_app_sup (one_for_one)
└── [per-workspace children started dynamically]
    └── beamtalk_workspace_sup (one_for_one)
        ├── beamtalk_workspace_meta (worker)
        ├── beamtalk_repl_actors (worker)
        ├── beamtalk_repl_server (worker)
        ├── beamtalk_idle_monitor (worker)
        ├── beamtalk_actor_sup (simple_one_for_one)
        └── beamtalk_session_sup (simple_one_for_one)
```

Note: `beamtalk_actor_sup` moves to `beamtalk_workspace` because it supervises user-spawned actors within a workspace context. The core `beamtalk_actor.erl` module (actor gen_server implementation) stays in runtime.

When a non-REPL use case needs actor supervision (e.g., `beamtalk run server.bt` from ADR 0004), it will start its own `beamtalk_actor_sup` under a production supervisor — the module is reusable, only its supervision placement is workspace-scoped for now. This is a natural extension point: a future `beamtalk_server` app could supervise actors without the workspace layer.

### Rust CLI Startup

The Rust CLI currently boots the Erlang node with `-eval` commands that directly call `beamtalk_repl:start_link()` or `beamtalk_workspace_sup:start_link(#{...})` — it does **not** use `application:ensure_all_started/1`. Code paths are in `-pa` flags only.

After the split, the `-eval` startup must change to:

```erlang
%% Current (REPL mode):
{ok, _} = beamtalk_repl:start_link(), ...

%% After split (REPL mode):
ok = application:ensure_all_started(beamtalk_workspace), ...
```

This ensures both `beamtalk_runtime` and `beamtalk_workspace` start in correct dependency order via their `.app.src` declarations. The Rust CLI must also add `-pa` for the new `beamtalk_workspace/ebin` directory.

For batch compilation (`beamtalk build`), only `beamtalk_runtime` needs to start — no REPL infrastructure.

### Header Files

`beamtalk_repl_eval.erl` and `beamtalk_repl_server.erl` include `beamtalk.hrl` from `beamtalk_runtime/include/`. In a rebar3 umbrella, included applications' header files are automatically available via `-include_lib("beamtalk_runtime/include/beamtalk.hrl")`. The current `-include("beamtalk.hrl")` syntax works because rebar3 adds dependent app include paths, but updating to `-include_lib` form is cleaner and more explicit. This should be done during the migration.

### Test Organization

14 REPL/workspace test files move to `beamtalk_workspace/test/`:

| Test File | Tests |
|-----------|-------|
| `beamtalk_repl_tests.erl` | Main REPL coordinator |
| `beamtalk_repl_eval_tests.erl` | Expression evaluation |
| `beamtalk_repl_server_tests.erl` | TCP server |
| `beamtalk_repl_shell_tests.erl` | Session shell |
| `beamtalk_repl_state_tests.erl` | State management |
| `beamtalk_repl_actors_tests.erl` | Actor registry |
| `beamtalk_repl_modules_tests.erl` | Module tracking |
| `beamtalk_repl_protocol_tests.erl` | Message protocol |
| `beamtalk_repl_integration_tests.erl` | Integration tests |
| `beamtalk_workspace_meta_tests.erl` | Workspace metadata |
| `beamtalk_workspace_sup_tests.erl` | Workspace supervisor |
| `beamtalk_session_sup_tests.erl` | Session supervisor |
| `beamtalk_session_tests.erl` | Session behavior |
| `beamtalk_idle_monitor_tests.erl` | Idle monitoring |

### Test Fixtures

The existing `test_fixtures/compile_fixtures.escript` compiles `counter.bt` and `logging_counter.bt` for runtime tests (codegen simulation, super keyword). These fixtures stay with `beamtalk_runtime` — they test core actor behavior, not REPL features. If workspace tests need compiled fixtures in the future, `beamtalk_workspace` can add its own `test_fixtures/` with a similar compile hook.

```erlang
%% beamtalk_runtime.app.src
{application, beamtalk_runtime, [
    {description, "Beamtalk core language runtime"},
    {mod, {beamtalk_runtime_app, []}},
    {applications, [kernel, stdlib, beamtalk_stdlib]}
]}.
```

```erlang
%% beamtalk_workspace.app.src
{application, beamtalk_workspace, [
    {description, "Beamtalk workspace and interactive development environment"},
    {mod, {beamtalk_workspace_app, []}},
    {applications, [kernel, stdlib, beamtalk_runtime]}
]}.
```

## Prior Art

### Erlang/OTP Ecosystem

OTP itself follows this pattern extensively. The `kernel` application provides core services (process management, error logging, code loading), while `stdlib` provides library modules, and `sasl` provides release handling. Interactive tools (`debugger`, `observer`, `wx`) are separate applications entirely.

### Elixir/Phoenix

Phoenix splits into `phoenix` (HTTP framework), `phoenix_live_view` (interactive UI), and `phoenix_pubsub` (messaging). Each is a separate application with clear dependency direction. The runtime (`phoenix`) has no dependency on live development tools.

### Gleam

Gleam separates `gleam_stdlib` (core types), `gleam_erlang` (BEAM interop primitives), and `gleam_otp` (actor framework). Each is independently usable.

### Rebar3 Umbrella Convention

The standard rebar3 pattern is `apps/` directory with each subdirectory being a full OTP application. This is exactly our current structure — we're just under-utilizing it by putting everything in one app.

## User Impact

### Newcomer
No visible change. The REPL works identically. `beamtalk repl` starts both applications transparently.

### Smalltalk Developer
Conceptually familiar — Pharo separates the VM (runtime) from development tools (browsers, inspectors, debugger). The REPL is a development tool, not part of the language runtime.

### Erlang/BEAM Developer
This is idiomatic OTP design. They would expect this separation. "Why was it ever one app?" would be the natural question.

### Production Operator
Can deploy `beamtalk_runtime` + `beamtalk_stdlib` without REPL code for batch compilation. For production services (`beamtalk run`), the workspace app is still needed for actor supervision and hot reload, but the REPL TCP server and shell sessions could be disabled via configuration. A future refinement could split the workspace app further into workspace-core (actors, hot reload) and workspace-repl (TCP server, shell, eval) — but that's premature until `beamtalk run` is implemented (ADR 0004).

### Tooling Developer
Clearer boundaries make it easier to understand which modules to modify. LSP work touches neither app; REPL protocol work is isolated to `beamtalk_workspace`.

## Steelman Analysis

### Alternative: Keep Monolithic (Status Quo)

| Cohort | Best argument for keeping one app |
|--------|----------------------------------|
| 🧑‍💻 **Newcomer** | "Simpler mental model — one app, one thing to understand. No confusion about which app a module belongs to." |
| 🎩 **Smalltalk purist** | "In Smalltalk, the image IS the IDE — there's no separation between runtime and tools. The REPL is integral to the language." |
| ⚙️ **BEAM veteran** | "36 modules in one app is small by Erlang standards. RabbitMQ's rabbit app has 200+ modules. Don't split prematurely." |
| 🏭 **Operator** | "One app means one thing to configure, monitor, and restart. No inter-app dependency issues." |
| 🎨 **Language designer** | "Beamtalk is interactive-first — the REPL isn't optional tooling, it's core to the language experience. Separating it sends the wrong message." |

### Alternative: 4-App Split (Actors Separated)

| Cohort | Best argument for separating actors |
|--------|-------------------------------------|
| 🧑‍💻 **Newcomer** | "Actors are their own concept — separating them makes the codebase match the mental model." |
| ⚙️ **BEAM veteran** | "Actor supervision is a distinct concern. Future features (named actors, supervision strategies, clustering) need room to grow without polluting the core object system." |
| 🏭 **Operator** | "I could deploy object system without actors for simple scripting use cases." |
| 🎨 **Language designer** | "Actors = processes = concurrency. Object system = dispatch = data. These are orthogonal." |

### Tension Points

- **Smalltalk purists** prefer monolithic (tools are integral) but **BEAM veterans** expect separation (OTP convention).
- **Language designer** could argue either way: interactive-first suggests keeping REPL close, but clean architecture suggests separation.
- **4-app split** has merit for the future but actors and the object system are currently tightly coupled (`gen_server` state IS the object, `beamtalk_actor.erl` uses `beamtalk_dispatch.erl` for message handling). Splitting them would require significant interface work for little immediate benefit.

**Resolution:** The 3-app split resolves the real friction (core→REPL coupling) without premature separation of tightly-coupled modules. The actor system can be extracted later if/when it grows independently (named actors, clustering, supervision policies).

## Alternatives Considered

### Keep Monolithic (Status Quo)

Rejected because the coupling is a real problem today. `beamtalk_actor.erl` cannot be tested without REPL infrastructure. Production deployments include unnecessary REPL code. The 36-module monolith will only grow as ADR 0004 workspace features are implemented.

### 4-App Split (Separate Actors)

Rejected as premature. Actors are implemented as `gen_server` processes whose state IS the Beamtalk object. `beamtalk_actor.erl` depends heavily on `beamtalk_dispatch.erl`, `beamtalk_object_class.erl`, and `beamtalk_error.erl`. Extracting actors would require a substantial interface layer for no immediate benefit. Can be revisited when actor-specific features (named actors, supervision policies, clustering) create natural pressure to separate.

### 5-App Split (Separate Workspace from REPL)

Rejected as premature. Workspace management (ADR 0004) is only ~10% implemented. Splitting workspace from REPL now would create two tiny apps that need to evolve together. Better to keep them together and split when the workspace layer grows substantial enough to warrant its own supervision tree and lifecycle.

## Consequences

### Positive

- **Clean dependency direction.** Core runtime has zero compile-time dependency on workspace/REPL code. Dependencies flow strictly downward: `beamtalk_workspace` → `beamtalk_runtime` → `beamtalk_stdlib`.
- **Independent testing.** Runtime unit tests don't need REPL infrastructure. REPL tests can mock or substitute runtime components.
- **Production deployments.** Batch compilation (`beamtalk build`) can run with just `beamtalk_runtime` + `beamtalk_stdlib`. Production services will need `beamtalk_workspace` for actor lifecycle, but the REPL server can be conditionally disabled. Full REPL-free production is a future refinement when `beamtalk run` is implemented.
- **Clearer ownership.** ADR 0004 workspace features go in `beamtalk_workspace`. New primitive types go in `beamtalk_runtime`. No ambiguity.
- **Aligns with DDD.** Matches the bounded contexts in `docs/beamtalk-ddd-model.md`: "REPL Context" vs "Actor System Context" / "Object System Context".
- **Future-proof.** When workspace management (ADR 0004) or debugging tools grow, they naturally live in `beamtalk_workspace` or become new apps — without touching `beamtalk_runtime`.

### Negative

- **Migration effort.** Need to create new app boilerplate (`beamtalk_workspace_app.erl`, `beamtalk_workspace_app_sup.erl`, `.app.src`), move 13 modules, update rebar.config, update all test paths.
- **Slightly more complex build.** Three apps to configure in rebar.config, Dialyzer, CI.
- **Actor supervisor migration.** `beamtalk_actor_sup` moves to `beamtalk_workspace` (workspace-scoped). A future non-REPL use case (e.g., `beamtalk run server.bt`) would start its own `beamtalk_actor_sup` instance under a production supervisor — the module is reusable, only its supervision placement is workspace-scoped for now.

### Neutral

- **Module naming unchanged.** All modules keep their current names (`beamtalk_repl_*`, `beamtalk_workspace_*`). No code changes needed in callers within the REPL layer.
- **No user-visible changes.** `beamtalk repl` works identically. The Rust CLI starts both apps transparently.
- **rebar3 umbrella structure unchanged.** Still `apps/` directory with per-app subdirectories.

## Implementation

### Phase 1: Create `beamtalk_workspace` App Skeleton

1. Create `runtime/apps/beamtalk_workspace/` directory structure
2. Create `beamtalk_workspace.app.src` with dependency on `beamtalk_runtime`
3. Create `beamtalk_workspace_app.erl` (OTP application callback)
4. Create `beamtalk_workspace_app_sup.erl` (app-level supervisor)

### Phase 2: Decouple Actor Registration

1. Replace direct calls in `beamtalk_actor.erl` with optional callback via `application:get_env`
2. Register callback in `beamtalk_workspace_app:start/2`
3. Verify actors still register correctly when REPL is loaded
4. Verify actors spawn without error when REPL is not loaded

### Phase 3: Move Modules and Tests

1. Move 12 modules from `beamtalk_runtime/src/` to `beamtalk_workspace/src/`
2. Move `beamtalk_actor_sup.erl` to `beamtalk_workspace/src/` (workspace-scoped)
3. Move 14 related test files to `beamtalk_workspace/test/`
4. Test fixtures (`counter.bt`, `logging_counter.bt`) stay in `beamtalk_runtime/test_fixtures/` — they test core actor behavior

### Phase 4: Update Build Configuration and Rust CLI

1. Update `runtime/rebar.config` — add `beamtalk_workspace` to project apps
2. Update Dialyzer configuration
3. Update Justfile commands if needed
4. Update CI configuration
5. Update Rust CLI `-eval` commands to use `application:ensure_all_started(beamtalk_workspace)` in `crates/beamtalk-cli/src/commands/repl/process.rs` and `workspace/mod.rs`
6. Add `-pa` flag for `beamtalk_workspace/ebin` directory in Rust CLI
7. Update `-include("beamtalk.hrl")` to `-include_lib("beamtalk_runtime/include/beamtalk.hrl")` in moved modules

### Phase 5: Verify

1. `just test-runtime` — all existing tests pass
2. `just test-repl-protocol` — REPL integration works
3. `just ci` — full CI green
4. Manual REPL test: `:load`, `spawn`, `increment` flow

### Affected Components

| Component | Change |
|-----------|--------|
| `runtime/apps/beamtalk_runtime/` | Remove 13 modules (12 REPL + actor_sup), decouple actor registration |
| `runtime/apps/beamtalk_workspace/` | New app with 13 moved modules + 2 new (app, sup) + 14 test files |
| `runtime/rebar.config` | Add beamtalk_workspace app configuration, update Dialyzer |
| `beamtalk_actor.erl` | Replace direct REPL calls with optional callback |
| `crates/beamtalk-cli/src/commands/repl/process.rs` | Update `-eval` to use `application:ensure_all_started`, add `-pa` |
| `crates/beamtalk-cli/src/commands/workspace/mod.rs` | Update `-eval` to use `application:ensure_all_started`, add `-pa` |
| `AGENTS.md` | Update repository structure documentation |

## Migration Path

This is an internal restructuring — no user-facing changes. All module names remain the same, all APIs remain the same. The migration requires:

1. **Rust CLI changes:** Update `-eval` commands to use `application:ensure_all_started(beamtalk_workspace)` instead of directly calling `beamtalk_repl:start_link()`. Add `-pa` flag for `beamtalk_workspace/ebin`. Affected files: `crates/beamtalk-cli/src/commands/repl/process.rs`, `crates/beamtalk-cli/src/commands/workspace/mod.rs`.
2. **Header includes:** Update `beamtalk_repl_eval.erl` and `beamtalk_repl_server.erl` to use `-include_lib("beamtalk_runtime/include/beamtalk.hrl")` instead of `-include("beamtalk.hrl")`.
3. **rebar.config:** Add `beamtalk_workspace` app configuration, update Dialyzer exclude list.

## Implementation Tracking

**Epic:** [BT-378](https://linear.app/beamtalk/issue/BT-378)
**Status:** ✅ Done (implemented as BT-351)

| Phase | Issue | Title | Size | Status |
|-------|-------|-------|------|--------|
| 1 | BT-383 | Create beamtalk_workspace OTP app skeleton | S | Done |
| 2 | BT-379 | Decouple actor registration from REPL with optional callback | M | Done |
| 3a | BT-382 | Move REPL/workspace modules to beamtalk_workspace app | M | Done |
| 3b | BT-380 | Move REPL/workspace test files to beamtalk_workspace | S | Done |
| 4 | BT-381 | Update Rust CLI startup for workspace app split | M | Done |
| 5 | BT-384 | Verify OTP split and update documentation | S | Done |

## References

- Related issues: [BT-351](https://linear.app/beamtalk/issue/BT-351/otp-application-structure-split-repl-from-runtime-adr-0009)
- Related ADRs: [ADR 0004](0004-persistent-workspace-management.md) (Persistent Workspace Management — future REPL/workspace growth)
- Related ADRs: [ADR 0007](0007-compilable-stdlib-with-primitive-injection.md) (Compilable Standard Library — stdlib app structure)
- Documentation: `docs/beamtalk-ddd-model.md` (Bounded contexts: REPL Context vs Runtime Context)
- Documentation: `docs/development/architecture-principles.md` (Layered architecture principles)
- Erlang best practices: [Adopting Erlang — Umbrella Projects](https://adoptingerlang.org/docs/development/umbrella_projects/)
