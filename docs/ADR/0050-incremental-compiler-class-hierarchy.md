# ADR 0050: Incremental Compiler ClassHierarchy via BEAM Metadata Streaming

## Status

Proposed (2026-03-04)

## Context

The OTP compiler port (ADR 0022) is stateless — it constructs `ClassHierarchy::with_builtins()` per request and has no knowledge of user-defined classes from the REPL session. This causes two problems:

1. **Completion gaps**: ADR 0045 Option A (Erlang runtime chain resolution) handles type completion for annotated methods on user-defined classes. But when a method lacks a return-type annotation, the chain silently breaks. ADR 0045 Option C (Erlang fast path + compiler fallback for unannotated methods) would close this gap, but requires the compiler to know about user-defined classes.

2. **Diagnostic blindness**: The TypeChecker cannot validate method calls on user-defined REPL classes — it only sees stdlib. Cross-class type errors, "did you mean" hints, and protocol conformance checks are all silently skipped for anything the user has defined.

The solution does not require source code or whole-world recompilation. The generated BEAM module for every Beamtalk class already exposes `__beamtalk_meta/0` — a zero-process reflection function (BT-942). This ADR extends that function to carry the full metadata the compiler needs, and establishes a gen_server session that streams it to the compiler port.

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
            <<"increment">> => #{arity => 1, param_types => ['Integer'], return_type => 'Integer'},
            <<"value">>     => #{arity => 0, param_types => [], return_type => 'Integer'}
        },

        %% Class-side methods
        class_methods => #{
            <<"new">> => #{arity => 0, param_types => [], return_type => 'Counter'}
        },

        %% Class variables
        class_variables   => []
    }
```

`param_types` and `return_type` are `none` (atom) when unannotated, a class-name atom when annotated. This matches the Rust `Option<EcoString>` in `MethodInfo`.

**Simplification**: Static metadata (return types, flags, field types, param types) is removed from the `BuilderState` passed through `register_class/0`. `beamtalk_object_class:init/1` reads from `Module:__beamtalk_meta()` instead, and `register_class/0` carries only what `__beamtalk_meta/0` cannot: method closures (the actual compiled functions). This eliminates the duplication between the two paths.

### 2. Compiler Session Gen_Server

A new `beamtalk_compiler_session` gen_server owns the port and maintains an Erlang-side incremental ClassHierarchy as a map of class name → metadata. The compiler port itself **remains stateless** per ADR 0022.

```erlang
%% beamtalk_compiler_session.erl

-record(session_state, {
    port        :: port(),
    classes     :: #{atom() => map()}   %% class name → __beamtalk_meta/0 result
}).

%% Public API (replaces direct port calls)
compile(SessionPid, Source, Opts) ->
    gen_server:call(SessionPid, {compile, Source, Opts}).

register_class(SessionPid, ClassName, MetaMap) ->
    gen_server:cast(SessionPid, {register_class, ClassName, MetaMap}).
```

On each compile request, the session injects the full `classes` map into the port request alongside source code:

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

`beamtalk_object_class:init/1` notifies the compiler session after class registration:

```erlang
%% In beamtalk_object_class:init/1, after pg:join:
Meta = Module:'__beamtalk_meta'(),
case beamtalk_compiler_session:whereis_session() of
    undefined -> ok;
    SessionPid -> beamtalk_compiler_session:register_class(SessionPid, ClassName, Meta)
end,
```

The call is a cast (fire-and-forget). If no session exists (e.g. non-REPL compilation), it's a no-op.

Hot-patching via `put_method/3` also notifies the session, sending an updated meta snapshot. The session replaces the class entry wholesale — no partial updates.

### 4. Crash Recovery

On compiler session restart (port crash or session restart), the session scans all currently loaded Beamtalk BEAM modules and repopulates its class cache:

```erlang
recover_from_beam_modules() ->
    AllModules = code:all_loaded(),
    lists:foldl(fun({Module, _Path}, Acc) ->
        case catch Module:'__beamtalk_meta'() of
            Meta when is_map(Meta) ->
                ClassName = maps:get(class, Meta),
                maps:put(ClassName, Meta, Acc);
            _ ->
                Acc
        end
    end, #{}, AllModules).
```

No persistent state, no disk writes. Like Dialyzer's PLT recovery, the BEAM modules themselves are the durable store.

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

**Tooling developer**: `__beamtalk_meta/0` becomes the stable, versionable metadata API for any tool that needs class information without going through a class gen_server process. LSP, debugger, and documentation tools can all use it.

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

### Alternative: Workspace layer as registration hook

Notify compiler session from `beamtalk_workspace` after each compilation, rather than from `beamtalk_object_class`.

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
- `beamtalk_compiler_session` becomes the point of entry for all compiler calls (existing callers updated)

## Implementation

### Phase 1: Extend `__beamtalk_meta/0`

- Extend codegen to emit `is_sealed`, `is_abstract`, `is_value`, `is_typed`, `field_types`, and per-method `param_types` and `return_type` in `__beamtalk_meta/0`
- Update `beamtalk_object_class:init/1` to read class metadata from `Module:__beamtalk_meta()` instead of from separate `BuilderState` keys
- Remove now-redundant static metadata keys from `BuilderState` / `register_class/0`
- Files: `crates/beamtalk-core/src/codegen/`, `runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl`

### Phase 2: Compiler Session Gen_Server

- Implement `beamtalk_compiler_session.erl` as a supervised gen_server
- Wire up `beamtalk_compiler_port` calls through the session
- Implement BEAM module recovery in `init/1`
- Files: `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_session.erl` (new), `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_port.erl`

### Phase 3: Registration Hook

- Add notification cast to `beamtalk_object_class:init/1` and `put_method/3`
- Files: `runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl`

### Phase 4: Rust ClassHierarchy Deserialization

- Add `ClassHierarchy::add_from_beam_meta` to deserialize the `class_hierarchy` port payload
- Wire into port request handling
- Files: `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/mod.rs`, `runtime/apps/beamtalk_compiler/src/beamtalk_compiler_port.erl`

### Phase 5: ADR 0045 Option C

- Implement compiler fallback for unannotated methods in REPL completion
- Tracked separately.

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
