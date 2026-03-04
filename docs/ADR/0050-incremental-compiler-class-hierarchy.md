# ADR 0050: Incremental Compiler ClassHierarchy via BEAM Metadata Streaming

## Status

Proposed (2026-03-04)

## Context

The OTP compiler port (ADR 0022) is stateless — it constructs `ClassHierarchy::with_builtins()` per request and has no knowledge of user-defined classes from the REPL session. This causes two problems:

1. **Completion gaps**: ADR 0045 Option A (Erlang runtime chain resolution) handles type completion for annotated methods on user-defined classes. But when a method lacks a return-type annotation, the chain silently breaks. ADR 0045 Option C (Erlang fast path + compiler fallback for unannotated methods) would close this gap, but requires the compiler to know about user-defined classes.

2. **Diagnostic blindness**: The TypeChecker cannot validate method calls on user-defined REPL classes — it only sees stdlib. Cross-class type errors, "did you mean" hints, and protocol conformance checks are all silently skipped for anything the user has defined.

The solution does not require source code or whole-world recompilation. The generated BEAM module for every Beamtalk class already exposes `__beamtalk_meta/0` — a zero-process reflection function (BT-942). This ADR extends that function to carry the full metadata the compiler needs, and extends the existing `beamtalk_compiler_server` gen_server to cache class metadata and inject it into each port request.

### Baseline: How Good Is "Do Nothing"?

The stdlib has ~100% return-type annotation coverage — all ~500 annotated methods resolve fully via ADR 0045 Option A (runtime chain resolution). Stdlib-to-stdlib chains already work. The gap is **user-defined classes only**: when a user writes an unannotated method that returns a custom class, the chain breaks and completion stops. This is the gap this ADR addresses, and it grows as users write more cross-class code in REPL sessions.

### Current Architecture

```
REPL request                   Compiler Port (Rust)
    │                               │
    ├─ source code ────────────────►│ ClassHierarchy::with_builtins()
    ├─ class_superclass_index ─────►│   (stdlib only — user classes invisible)
    └─ class_module_index ─────────►│
                                    │
                                    └─ compile → Core Erlang
```

### What __beamtalk_meta/0 Currently Returns

```erlang
%% BT-942: zero-process reflection — exists today
Module:'__beamtalk_meta'() ->
    #{
        class      => 'Counter',
        superclass => 'Actor',
        fields     => [count],
        methods    => [{increment, 1}, {value, 0}],
        class_methods => [{new, 0}]
        %% Missing: return types, param types, flags, field types
    }
```

The same metadata also flows from codegen through `register_class/0` into the class gen_server via a `BuilderState` map — duplicating static metadata across two paths. The return-type maps (`method_return_types`, `class_method_return_types`), method signatures, flags, and field types all live in `BuilderState` but not in `__beamtalk_meta/0`.

### Dependency

BT-989 adds `method_return_types` and `class_method_return_types` to `class_state` and the codegen. This ADR depends on that infrastructure being in place.

## Decision

### 1. Extend `__beamtalk_meta/0` to Full ClassInfo

`__beamtalk_meta/0` becomes the **single canonical metadata source** on every compiled Beamtalk class module, carrying the full `ClassInfo` the compiler needs:

```erlang
%% Extended __beamtalk_meta/0 — the canonical class metadata payload
Module:'__beamtalk_meta'() ->
    #{
        %% Identity
        class             => 'Counter',
        superclass        => 'Actor',

        %% Class flags
        is_sealed         => false,
        is_abstract       => false,
        is_value          => false,
        is_typed          => false,

        %% Fields and their declared types
        fields            => [count],
        field_types       => #{count => 'Integer'},

        %% Instance methods: selector => #{arity, param_types, return_type}
        methods => #{
            increment => #{arity => 1, param_types => ['Integer'], return_type => 'Integer'},
            value     => #{arity => 0, param_types => [], return_type => 'Integer'}
        },

        %% Class-side methods
        class_methods => #{
            new => #{arity => 0, param_types => [], return_type => 'Counter'}
        },

        %% Class variables
        class_variables   => []
    }
```

`param_types` and `return_type` are `none` (atom) when unannotated, a class-name atom when annotated. This matches the Rust `Option<EcoString>` in `MethodInfo`.

**Simplification**: Static metadata (return types, flags, field types, param types) is removed from the `BuilderState` passed through `register_class/0`. `beamtalk_object_class:init/1` reads from `Module:__beamtalk_meta()` instead, and `register_class/0` carries only what `__beamtalk_meta/0` cannot: method closures (the actual compiled functions). This eliminates the duplication between the two paths.

**Hot-patching caveat**: `__beamtalk_meta/0` is a compiled function — it is NOT updated when methods are hot-patched via `put_method/3`. The class gen_server retains a **live metadata view** that diverges from `__beamtalk_meta/0` after hot-patching. When a hot-patched class notifies the compiler server, it sends the gen_server's live metadata (which has the hot-patched method with its return type cleared), not the stale `__beamtalk_meta/0`. Crash recovery reads `__beamtalk_meta/0` and therefore loses hot-patch deltas — this is acceptable because hot-patched methods already lose their return-type annotations (they are cleared in `put_method/3`), and the class will be re-registered by its gen_server on next use.

### 2. Extend `beamtalk_compiler_server` with Class Cache

The existing `beamtalk_compiler_server` gen_server (which already owns the port, handles crash recovery, and routes all compilation requests) gains a `classes` field in its state record. No new gen_server is needed. The compiler port itself **remains stateless** per ADR 0022.

```erlang
%% Extended beamtalk_compiler_server state
-record(state, {
    port        :: port(),
    classes     :: #{atom() => map()}   %% class name → metadata map
    %% ... existing fields ...
}).

%% New API
register_class(ClassName, MetaMap) ->
    gen_server:cast(?MODULE, {register_class, ClassName, MetaMap}).
```

On each compile request, the server injects the `classes` map into the port request alongside source code:

```erlang
%% Existing port request (ADR 0022 + BT-907):
#{command => compile_expression,
  source  => Source,
  class_superclass_index => ...,    %% existing, kept for compat
  class_module_index => ...}        %% existing, kept for compat

%% Extended with BT-993:
#{...,
  class_hierarchy => Classes}       %% full __beamtalk_meta maps, keyed by class name
```

The Rust port deserializes `class_hierarchy` and populates `ClassHierarchy` with full `ClassInfo` entries (not stubs) for every user-defined class.

### 3. Registration Hook

`beamtalk_object_class:init/1` notifies the compiler server after class registration:

```erlang
%% In beamtalk_object_class:init/1, after pg:join:
Meta = Module:'__beamtalk_meta'(),
beamtalk_compiler_server:register_class(ClassName, Meta),
```

The call is a cast (fire-and-forget). If the compiler server is not running (e.g. non-REPL compilation), the cast is silently dropped.

Hot-patching via `put_method/3` also notifies the compiler server. Since `__beamtalk_meta/0` is stale after hot-patching, the notification sends the gen_server's live metadata view instead — built from the current `class_state` record. The compiler server replaces the class entry wholesale — no partial updates.

### 4. Crash Recovery

On compiler server restart (port crash), the server scans all currently loaded Beamtalk BEAM modules and repopulates its class cache. **Stdlib classes are excluded** — they are already in `ClassHierarchy::with_builtins()` on the Rust side with richer data (full method info, return types, param types) than `__beamtalk_meta/0` currently provides:

```erlang
recover_from_beam_modules() ->
    BuiltinClasses = beamtalk_class_hierarchy_table:all_builtins(),
    AllModules = code:all_loaded(),
    lists:foldl(fun({Module, _Path}, Acc) ->
        case catch Module:'__beamtalk_meta'() of
            Meta when is_map(Meta) ->
                ClassName = maps:get(class, Meta),
                case lists:member(ClassName, BuiltinClasses) of
                    true  -> Acc;  %% skip — Rust builtins are richer
                    false -> maps:put(ClassName, Meta, Acc)
                end;
            _ ->
                Acc
        end
    end, #{}, AllModules).
```

No persistent state, no disk writes. Like Dialyzer's PLT recovery, the BEAM modules themselves are the durable store.

**Limitation**: Hot-patch deltas are lost on crash recovery, since `__beamtalk_meta/0` reflects compile-time metadata only. This is acceptable — hot-patched methods already have their return types cleared, so the compiler falls back to dynamic for those methods. The class gen_server processes (which survive port crashes) will re-notify the compiler server with their live metadata on next class registration or compilation.

**Timing**: Recovery runs synchronously in the compiler server's `init/1`. Compile requests arriving before recovery completes are queued by the gen_server mailbox, not served with empty state.

### 5. Rust ClassHierarchy Extension

The Rust `ClassHierarchy` gains a method to deserialize a full `class_hierarchy` map from the port request:

```rust
pub fn add_from_beam_meta(&mut self, meta: &BTreeMap<String, Term>) {
    // Deserialize each class entry from ETF terms
    // Creates full ClassInfo (not stubs) with methods, return_types, flags
}
```

The existing `add_external_superclasses` (which creates stubs) remains for backward compatibility with the existing `class_superclass_index` path.

## Prior Art

**Dialyzer PLT**: Erlang's type analyzer maintains a Persistent Lookup Table of type signatures accumulated from BEAM bytecode. On restart it re-reads all modules. This ADR uses the same recovery model: BEAM modules are the durable store, in-memory state is reconstructed on demand. Dialyzer's PLT is file-based; this ADR uses in-memory gen_server state because the REPL session is ephemeral.

**Pharo Smalltalk `SystemDictionary`**: Pharo maintains a live class registry (`Smalltalk`) that the compiler consults for every compilation. All class definitions — including ones typed at the REPL — are immediately visible. This ADR achieves the same property for Beamtalk: classes registered in the REPL session become immediately visible to the compiler on the next request, via the gen_server cache injection.

**Elixir Language Server (ElixirLS)**: Uses incremental Dialyzer to avoid full re-analysis on each keystroke. The pattern of "accumulate knowledge in a session, inject into each analysis request" is the same approach taken here.

**Gleam**: Stateless batch compilation only. No REPL, no incremental session. Not comparable to this use case.

## User Impact

**Newcomer**: No visible change initially. The improvement is in completion quality — chains through user-defined classes will resolve further, and "method not found" errors on user classes will start appearing. The feature is transparent.

**Smalltalk developer**: This brings Beamtalk closer to Pharo's experience — the compiler sees all live classes, not just the stdlib. Defining a class and immediately getting type-aware completions for it is the expected Smalltalk behaviour.

**Erlang/BEAM developer**: The gen_server session is a familiar OTP pattern. `__beamtalk_meta/0` follows the same convention as Erlang's `module_info/0`. Crash recovery via module scanning is idiomatic.

**Operator**: The compiler session is a supervised gen_server. Crashes are isolated (per ADR 0022), observable via standard OTP tools (`sys:get_state/1`), and self-healing via BEAM module recovery. No new persistence layer.

**Tooling developer**: `__beamtalk_meta/0` becomes a richer metadata API for tools that need class information without going through a class gen_server process. Note: the LSP runs in pure Rust without a BEAM runtime; its path to user-class awareness is through file-system scanning and the Rust `ClassHierarchy` directly, not through this ADR's runtime session. Debugger and documentation tools running in the BEAM benefit directly.

## Steelman Analysis

### Option A: Erlang gen_server session (this ADR — Recommended)

| Cohort | Strongest argument |
|--------|--------------------|
| **Newcomer** | "Completions and errors just work for my classes, no configuration" |
| **Smalltalk purist** | "Erlang owns the class registry; it's correct for Erlang to own the compiler's view of it too. State belongs in processes." |
| **BEAM veteran** | "gen_server state is observable, supervised, and hot-upgradeable. Crash recovery by scanning loaded modules is exactly what Dialyzer does. This is idiomatic OTP." |
| **Operator** | "`sys:get_state(SessionPid)` shows the full class cache. No opaque Rust state, no disk files to manage." |
| **Language designer** | "Clean boundary: Erlang manages session state, Rust compiles. Each does what it does best. The port stays testable in isolation." |

### Option B: Stateful Rust port (rejected)

| Cohort | Strongest argument |
|--------|--------------------|
| **Newcomer** | "Doesn't matter either way" |
| **Smalltalk purist** | "The compiler should be self-contained, seeing all classes natively — like a Smalltalk image's compiler" |
| **BEAM veteran** | "Avoids ETF serialization of the full class cache on every request. State lives where it's consumed." |
| **Operator** | "Fewer processes, simpler supervision tree" |
| **Language designer** | "TypeChecker accesses ClassHierarchy directly without ETF round-trips" |

**Tension**: The strongest argument for Option B is eliminating per-request serialization overhead. The counter: a REPL session has tens of user-defined classes at most; their metadata maps are small (kilobytes of ETF). The port already serializes source code (potentially larger) and the existing index maps. The marginal overhead is negligible against the 10–500ms compilation time (ADR 0022). Operational transparency and OTP idiomatics tip decisively to Option A.

**Richer metadata does not require a stateful compiler**: whether `ClassInfo` contains only superclass names or also includes return types and param types, all of it is injected per-request by the gen_server. The gen_server IS the stateful component. The compiler remains a pure function: inputs → Core Erlang.

### Option C: Separate stateful Rust daemon (rejected)

Premature. Adds a second Rust process, complicates deployment, and gains nothing over Option A at current scale. ADR 0022 Phase 6 (NIF backend) is the correct future path if sub-millisecond overhead becomes necessary.

## Alternatives Considered

### Alternative: Minimal metadata (name + superclass only)

Pass only enough for superclass chain resolution, not full TypeChecker use.

**Rejected**: Does not unlock ADR 0045 Option C (compiler fallback for unannotated methods), which requires param types. Does not enable cross-class diagnostics. Saves negligible size.

### Alternative: Separate `class_info/0` function, keep `__beamtalk_meta/0` unchanged

Add a new exported function alongside the existing one.

**Rejected**: `__beamtalk_meta/0` already serves zero-process reflection and is the natural extension point. Two overlapping functions would create maintenance divergence. Extending the existing function is simpler.

### Alternative: Enrich existing `class_superclass_index` without session state

Instead of adding a `classes` cache to the compiler server, extend the existing `class_superclass_index` (already assembled per-request by the workspace) to include full method metadata alongside superclass info. No new state management, no crash recovery needed.

**Rejected**: The workspace already assembles this index per compilation — but it only knows about classes loaded through the workspace. Classes loaded via direct `code:load_file`, supervision tree restarts, or other packages would be invisible. The compiler server cache + registration hook captures ALL class registrations regardless of origin. Additionally, enriching a per-request index means no accumulation across requests — each request must rediscover all classes.

### Alternative: Do nothing — rely on ADR 0045 Option A

Keep the current runtime-only chain resolution. Stdlib has ~100% return-type coverage, so stdlib chains work. Accept that user-defined unannotated classes break the chain.

**Rejected**: The gap is small today but grows as users write more cross-class code. Cross-class diagnostics ("method not found" on user classes, "did you mean" hints) are completely blocked without this. The engineering cost is moderate (extending an existing gen_server, extending an existing metadata function) and the infrastructure enables multiple future features (ADR 0045 Option C, protocol conformance checking, dead code detection).

### Alternative: Workspace layer as registration hook

Notify compiler server from `beamtalk_workspace` after each compilation, rather than from `beamtalk_object_class`.

**Rejected**: Misses classes loaded outside the workspace (direct `code:load_file`, supervision tree restarts). The object class init hook is the only place that fires for every class registration regardless of origin.

## Consequences

### Positive

- ADR 0045 Option C (hybrid Erlang chain resolution + compiler fallback) becomes implementable
- Cross-class type diagnostics enabled for user-defined REPL classes
- TypeChecker can validate method calls across user-defined class boundaries
- `__beamtalk_meta/0` becomes a complete, stable metadata API for all tooling
- Static metadata duplication between `BuilderState` and `__beamtalk_meta/0` is eliminated
- Crash recovery is automatic and stateless — no PLT files, no disk management

### Negative

- Every compile request now includes the full class cache in the port payload (bounded growth: tens of classes per session, kilobytes of ETF)
- `beamtalk_object_class:init/1` gains a conditional notification cast
- `__beamtalk_meta/0` codegen is more complex (additional fields)

### Neutral

- Port protocol gains a new optional `class_hierarchy` key alongside existing optional indices (backward compatible)
- Compiler server API gains a `register_class/2` cast — existing compile API unchanged
- When v0.2 adds namespaces (ADR 0031), the session key may need to change from bare class name to qualified name

## Implementation

### Phase 1: Extend `__beamtalk_meta/0` (backward-compatible)

- Extend codegen to emit new keys (`is_sealed`, `is_abstract`, `is_value`, `is_typed`, `field_types`, per-method `param_types` and `return_type`) in `__beamtalk_meta/0` while keeping existing keys (`methods` list of tuples, `class_methods` list of tuples) for backward compatibility during transition
- Add new `method_info` and `class_method_info` map keys alongside existing tuple lists
- Update all consumers of `__beamtalk_meta/0` (`beamtalk_behaviour_intrinsics.erl`, reflection tests)
- Update ~21 snapshot tests in `test-package-compiler/tests/snapshots/`
- Files: `crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs`, `runtime/apps/beamtalk_runtime/src/beamtalk_behaviour_intrinsics.erl`

### Phase 2: Compiler Server Class Cache + Registration Hook

- Add `classes` field to `beamtalk_compiler_server` state record
- Add `register_class/2` cast handler
- Add registration hook in `beamtalk_object_class:init/1` and `put_method/3`
- Implement BEAM module recovery in server init
- Add `clear_classes/0` API for test isolation
- Files: `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_server.erl`, `runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl`

### Phase 3: Rust ClassHierarchy Deserialization

- Add `ClassHierarchy::add_from_beam_meta` to deserialize the `class_hierarchy` port payload
- Wire into port request handling — skip builtin classes (already in `with_builtins()`)
- Files: `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/mod.rs`, `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_port.erl`

### Phase 4: BuilderState Simplification

- Once Phase 1 consumers are migrated, remove redundant static metadata keys from `BuilderState` / `register_class/0`
- `beamtalk_object_class:init/1` reads metadata from `Module:__beamtalk_meta()` instead of from BuilderState
- Remove deprecated tuple-list `methods` / `class_methods` keys from `__beamtalk_meta/0`
- Files: `crates/beamtalk-core/src/codegen/`, `runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl`

### Phase 5: ADR 0045 Option C

- Implement compiler fallback for unannotated methods in REPL completion
- Tracked separately.

## Migration Path

The `__beamtalk_meta/0` format change is managed across two phases:

**Phase 1 (additive, backward-compatible)**: New keys are added alongside existing keys. The current `methods => [{atom(), integer()}]` tuple-list format is preserved. New `method_info => #{atom() => #{...}}` map is added in parallel. All existing consumers (`beamtalk_behaviour_intrinsics.erl`, reflection tests, snapshot tests) continue working unchanged and are migrated to the new keys.

**Phase 4 (breaking, after consumers migrated)**: Old tuple-list format removed. By this point all consumers read from the new map keys. This is an internal change — `__beamtalk_meta/0` is not part of the public BEAM interop API (ADR 0028).

## References

- Linear: BT-993
- Blocks: ADR 0045 Option C
- Depends on: BT-989 (method_return_types infrastructure)
- ADR 0022 — Embedded compiler via OTP Port (stateless port design)
- ADR 0025 — Gradual typing and protocols (ClassHierarchy, TypeChecker)
- ADR 0028 — BEAM interop strategy
- ADR 0045 — REPL expression-level completion (Option C upgrade path)
- [Dialyzer incremental mode](https://www.erlang.org/doc/apps/dialyzer/dialyzer_chapter.html)
- [ElixirLS incremental Dialyzer](https://medium.com/@JakeBeckerCode/elixirls-0-2-better-builds-code-formatter-and-incremental-dialyzer-be70999ea3e7)
- BT-942 — `__beamtalk_meta/0` zero-process reflection
