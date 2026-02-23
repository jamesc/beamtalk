# ADR 0006: Unified Method Dispatch with Hierarchy Walking

## Status
Implemented (2026-02-08) — Epic BT-278

## Context

Beamtalk currently has **three different dispatch mechanisms** for method calls:

### Current State

1. **Compiled Classes** (Counter, user-defined actors)
   - Each class generates its own `dispatch/4` function with hardcoded case clauses
   - Only dispatches methods **defined in that class**
   - Falls through to `doesNotUnderstand:args:` if method not found
   - **No hierarchy walking** - doesn't check superclass methods

2. **Dynamic Classes** (created at runtime via `create_subclass`)
   - Methods stored as closures in `__methods__` field
   - Dispatch via `beamtalk_dynamic_object:dispatch/4` using `apply/2`
   - Hierarchy support depends on if `__methods__` includes inherited closures

3. **Primitives** (Integer, String, Boolean)
   - Compiled to direct Erlang function calls
   - No gen_server, no dispatch function
   - Sealed (no subclassing), so no hierarchy walking needed

### The Problems

**Problem 1: Incomplete hierarchy walking**
```beamtalk
Counter methods         %=> [increment, decrement, getValue]
                        %   Missing: class, respondsTo:, perform:, etc. from Object
```

The `Counter` class only lists methods defined in Counter, not inherited methods from Actor or Object.

**Problem 2: Inconsistent dispatch behavior**

Compiled classes:
```erlang
%% Counter dispatch - NO hierarchy walking
'dispatch'/4 = fun (Selector, Args, Self, State) ->
    case Selector of
        <'increment'> -> ...
        <'getValue'> -> ...
        <'class'> -> ...       %% Hardcoded in every class!
        <'respondsTo:'> -> ... %% Hardcoded in every class!
        <OtherSelector> -> call doesNotUnderstand handler
    end
```

Dynamic classes:
```erlang
%% Dynamic dispatch - maybe walks hierarchy?
dispatch(Selector, Args, Self, State) ->
    Methods = maps:get('__methods__', State),
    case maps:find(Selector, Methods) of
        {ok, Fun} -> apply(Fun, [Self, Args, State]);
        error -> ...  %% What happens here? Walk to super?
    end
```

**Problem 3: Code duplication**

Common reflection methods (`class`, `respondsTo:`, `instVarNames`) are **code-generated into every class's dispatch function**. (Note: `instVarNames`/`instVarAt:`/`instVarAt:put:` were later renamed to `fieldNames`/`fieldAt:`/`fieldAt:put:` — see [ADR 0035](0035-field-based-reflection-api.md).) This bloats generated code and makes changes difficult.

**Problem 4: Cannot add methods to Object at runtime**

If we want to add a method to Object (via hot code reload or extension), **every compiled class's dispatch function** would need to be regenerated. This breaks the live coding experience.

### Design Constraints

1. **Performance**: Method dispatch is on the hot path - must be fast
2. **Distribution**: Objects may be on remote nodes - can't rely on local ETS
3. **Separate compilation**: Each module compiles independently
4. **Hot code reload**: Must support adding/changing methods at runtime
5. **Erlang interop**: Must work seamlessly with existing Erlang gen_servers

## Decision

**Implement a two-tier unified dispatch mechanism with runtime hierarchy walking.**

### Fast Path Policy (Evolvable Boundary)

- **Fast path**: selectors known at compile time to be defined on the current class (local method table).
- **Optional inline core**: a small, stable set of Object/Actor methods may be inlined (e.g., `class`, `respondsTo:`) if we accept recompilation when they change.
- **Everything else**: use runtime hierarchy lookup before DNU.
- **Super sends**: runtime lookup must start at the immediate superclass (avoid re-walking the current class).
- **Perform**: `perform:` always routes through the same runtime lookup path as normal sends.

This boundary is intentionally **shiftable over time** to balance performance and flexibility.

### Method Combination Ordering

- **Before**: run from superclass -> subclass (setup flows down).
- **Primary**: run only the most specific method (no chain).
- **After**: run from subclass -> superclass (cleanup flows up).

This ordering keeps method combinations predictable and mirrors common Smalltalk/Flavors expectations.

Before/after methods are collected from the **entire superclass chain**, not just the defining class. The dispatch service walks the full chain, collects all before/after funs for the selector, and runs them in the specified order around the primary method.

### Method Invocation Strategy

When the hierarchy walk finds a method on a superclass, invocation depends on class type:

- **Compiled class**: call `Module:dispatch(Selector, Args, Self, State)` — the compiled module already has the method body in its `dispatch/4` function.
- **Dynamic class**: call `apply(Fun, [Self, Args, State])` — the closure is stored in the class process's `dynamic_methods` map.

The class process knows the module name (`beamtalk_object_class:module_name/1`), so the dispatch service can resolve which strategy to use.

### Value Types

User-defined value types (Object subclasses like Point, Color) use the **same dispatch pattern** as actors — codegen generates a `dispatch/4` function with local fast path + runtime fallback. The only difference is *entry point*: value types are called directly (no gen_server), actors go through `gen_server:call`. The hierarchy walk is identical for both.

### Bootstrap Ordering

Object and Actor classes must be registered in `beamtalk_bootstrap` **before** any user modules load. The bootstrap sequence is:

1. Register ProtoObject (no superclass)
2. Register Object (superclass: ProtoObject)
3. Register Actor (superclass: Object)
4. Load user modules (can now hierarchy-walk to Object/Actor)

### Reflection Method Implementations

Core reflection methods (`class`, `respondsTo:`, `instVarNames`, `perform:`, `instVarAt:`, `instVarAt:put:`) are implemented **once** in the Object class's compiled module (e.g., `beamtalk_object.erl`). They are found via normal hierarchy walking — no need to duplicate them in every class's codegen. (Note: `instVarNames`/`instVarAt:`/`instVarAt:put:` were later renamed to `fieldNames`/`fieldAt:`/`fieldAt:put:` — see [ADR 0035](0035-field-based-reflection-api.md).)

### Domain Service: `beamtalk_dispatch`

A dedicated `beamtalk_dispatch` module serves as the dispatch domain service (DDD). This replaces the existing `beamtalk_object_class:super_dispatch/3` with a cleaner interface:

```erlang
%% Core dispatch entry point (hierarchy walk)
beamtalk_dispatch:lookup(Selector, Args, Self, State, CurrentClass)
    -> {reply, Result, NewState} | {error, not_found}

%% Super send (starts at immediate superclass)
beamtalk_dispatch:super(Selector, Args, Self, State, CurrentClass)
    -> {reply, Result, NewState} | {error, not_found}

%% Method combinations (collects before/after from chain)
beamtalk_dispatch:invoke_with_combinations(Selector, Args, Self, State, CurrentClass)
    -> {reply, Result, NewState}
```

Responsibilities are separated (DDD):
- **`beamtalk_dispatch`**: method lookup, hierarchy walking, method combinations, invocation
- **`beamtalk_object_class`**: class registry, method storage, metadata
- **`beamtalk_actor`**: actor lifecycle (spawn, make_self, supervision)

### Static Class Hierarchy (Compile-Time Model)

**Design principle:** The hierarchy walking algorithm is specified abstractly and implemented twice — once over the runtime class registry (Erlang), once over static AST/source analysis (Rust) — with shared test cases that verify both produce identical results for the same hierarchy.

A `ClassHierarchy` structure is built during semantic analysis from parsed class definitions. It serves three consumers:

| Consumer | Uses it for |
|----------|------------|
| **Codegen** | Fast path decisions, method combination pre-collection, selector validation, arity checking |
| **Language Service** | Completions (including inherited methods), go-to-definition across hierarchy, hover ("inherited from Object"), edit-time diagnostics |
| **Runtime** | Authoritative fallback for dynamic cases (hot reload, dynamic classes, `perform:`) |

**What static analysis enables at compile time:**

1. **Smarter fast path selection** — compiler knows the full inherited method set, can inline sealed/stable superclass methods
2. **Selector validation** — warn if a method won't resolve in any superclass (likely DNU)
3. **Method combination pre-collection** — pre-compute before/after methods for a selector from the full chain
4. **Arity checking** — verify argument counts against inherited method signatures
5. **Sealed class enforcement** — reject `Integer subclass: MyInt` at compile time

**Implementation approach:** Build `ClassHierarchy` in `crates/beamtalk-core/src/semantic_analysis/` as a map from class name → `{superclass, methods, state, sealed}`. ~200-300 lines of Rust for the minimal version. The existing `NameResolver` already manages scopes; this extends it with class-level knowledge.

**Critical invariant:** The static model and the runtime dispatch must agree on method resolution order and lookup semantics. Shared E2E tests verify this (e.g., compile a hierarchy, run it, confirm static predictions match runtime behavior).


### Architecture

```
User code: counter increment
    ↓
gen_server:call(CounterPid, {increment, []})
    ↓
counter:handle_call({increment, []}, From, State)
    ↓
counter:safe_dispatch(increment, [], State)
    ↓
counter:dispatch(increment, [], Self, State)   ← LOCAL LOOKUP (fast path)
    ↓ (not found locally)
    ↓
beamtalk_dispatch:super(increment, [], Self, State, 'Counter')  ← HIERARCHY WALK
    ↓
Look up Actor class in registry
    ↓
Check Actor's method table (ETS or class process)
    ↓ (not found in Actor)
    ↓
Recurse to Actor's superclass (Object)
    ↓
Check Object's method table
    ↓ (found!)
    ↓
Invoke Object's method implementation
```

### DDD Context and Responsibilities

**Bounded contexts:**

- **Compilation Context**: codegen emits fast-path cases + a single runtime fallback call (no runtime knowledge).
- **Runtime Context**: method lookup and hierarchy walking live in a dispatch domain service (`beamtalk_dispatch`) and the class registry (`beamtalk_object_class`).
- **Language Service Context**: completions/reflection reuse the same hierarchy lookup to avoid drift.

**Ubiquitous language:** selector, method table, superclass chain, class registry, dispatch, DNU. Avoid generic “utils”; prefer domain service names in runtime.

### Implementation Strategy

#### Phase 1: Runtime Hierarchy Walking (Immediate)

**Codegen changes:**
1. Keep fast path for local methods only (no change to performance)
2. When method not found, call runtime helper instead of immediate DNU:
   ```erlang
   'dispatch'/4 = fun (Selector, Args, Self, State) ->
       case Selector of
           <'increment'> -> ... %% Fast path - local method
           <'getValue'> -> ...  %% Fast path - local method
           <OtherSelector> when 'true' ->
               %% NEW: Try hierarchy walk before DNU
               case beamtalk_dispatch:super(OtherSelector, Args, Self, State, 'Counter') of
                   {'reply', Result, NewState} -> {'reply', Result, NewState};
                   {error, {not_found, _}} ->
                       %% NOW try doesNotUnderstand
                       ...
               end
       end
   ```

**Runtime helper** `beamtalk_dispatch:super/5`:
```erlang
super(Selector, Args, Self, State, CurrentClass) ->
    %% Look up superclass
    case whereis_class(CurrentClass) of
        undefined -> {error, {class_not_found, CurrentClass}};
        ClassPid ->
            case beamtalk_object_class:superclass(ClassPid) of
                none -> {error, {not_found, Selector}};
                SuperName ->
                    %% Check if super has this method
                    case check_class_has_method(SuperName, Selector) of
                        true ->
                            %% Invoke super's implementation
                            invoke_super_method(SuperName, Selector, Args, State);
                        false ->
                            %% Recurse up the chain
                            super(Selector, Args, Self, State, SuperName)
                    end
            end
    end.
```

**Benefits:**
- ✅ Fast path unchanged (local methods still inline)
- ✅ Supports hot code reload (hierarchy walked at runtime)
- ✅ Works with dynamic classes (same runtime helper)
- ✅ No code duplication (reflection methods in Object once)

**Trade-offs:**
- ❌ Inherited method calls are slower (ETS lookup + recursion)
- ✅ But local methods (99% of calls) are unaffected

#### Phase 2: Class Method Table Optimization (Future)

Store **flattened method table** in class process:
```erlang
%% In beamtalk_object_class state:
-record(class_state, {
    ...
    instance_methods = #{},      %% Methods defined in this class
    flattened_methods = #{},     %% All methods including inherited (cached)
    ...
}).
```

When class registers:
1. Walk to Object, collect all methods
2. Build flattened table (child overrides parent)
3. Cache for fast lookup

**Benefits:**
- ✅ Faster inherited method lookup (no recursion)
- ✅ `Counter methods` returns complete list

**Trade-offs:**
- ❌ More memory (duplicated method info)
- ❌ Invalidation on hot reload (must rebuild flattened tables)

#### Phase 3: Compile-Time Inlining (Future)

For **sealed hierarchies** (Object → Actor is unlikely to change), inline inherited methods at compile time:
```erlang
'dispatch'/4 = fun (Selector, Args, Self, State) ->
    case Selector of
        %% Local methods
        <'increment'> -> ...
        
        %% Inherited from Object (inlined at compile time)
        <'class'> -> ...
        <'respondsTo:'> -> ...
        
        %% Runtime hierarchy walk for unknown
        <OtherSelector> -> beamtalk_dispatch:super(...)
    end
```

**Benefits:**
- ✅ No runtime lookup for common inherited methods
- ✅ Fast path for `class`, `respondsTo:`, etc.

**Trade-offs:**
- ❌ Recompilation needed if Object changes (acceptable for core classes)

### Method Resolution Order (MRO)

Beamtalk uses **simple depth-first left-to-right** traversal:

```
Counter → Actor → Object → ProtoObject
```

No multiple inheritance, so no C3 linearization needed.

### Where Methods Are Stored

| Class Type | Method Definitions | Flattened Table | Dispatch |
|------------|-------------------|-----------------|----------|
| **Compiled** | Class process (`instance_methods`) | Class process (`flattened_methods`) | Generated `dispatch/4` + runtime fallback |
| **Dynamic** | Instance state (`__methods__`) | N/A (computed on demand) | `beamtalk_dynamic_object:dispatch/4` |
| **Primitive** | Compiled Erlang modules | N/A (sealed, no hierarchy) | Direct function calls (no dispatch) |

### Reflection API

**Class methods (complete hierarchy):**
```beamtalk
Counter methods
%=> [increment, decrement, getValue,      %% Defined in Counter
     spawn, spawnWith:,                    %% Defined in Actor  
     class, respondsTo:, perform:, ...]    %% Defined in Object
```

**Implementation:**
```erlang
%% In beamtalk_object_class:
handle_call(methods, _From, #class_state{flattened_methods = Flattened} = State) ->
    {reply, maps:keys(Flattened), State}.
```

## Insights and Gotchas

### Insights

1. **Runtime lookup is the source of truth**: codegen fast paths must be behaviorally identical to the runtime hierarchy walk.
2. **Fast paths are shiftable**: adding/removing selectors from the fast path should never change observable lookup order.
3. **Uniform reflection**: completions, `methods`, and `respondsTo:` must reuse the same hierarchy walk to avoid drift.
4. **Hot reload compatibility**: runtime lookup preserves live coding; flattening or inlining must include invalidation/regen hooks.

### Gotchas

1. **DNU ordering**: always attempt runtime hierarchy lookup *before* `doesNotUnderstand:args:`.
2. **Dynamic classes**: ensure dynamic dispatch uses the same hierarchy rules (even if methods are stored as closures).
3. **Remote nodes**: superclass lookup must work across distributed nodes (no local-only assumptions).
4. **Cache invalidation**: flattened tables and lookup caches must invalidate on method additions, removals, or class reloads.
5. **Error shape**: missing methods should return structured `#beamtalk_error{}` consistently across dispatch paths.

## Consequences

### Positive

1. **Correct Smalltalk semantics** - Hierarchy walking works as expected
2. **Unified dispatch model** - All object types use same resolution algorithm
3. **Efficient fast path** - Local methods unaffected (still inline case clauses)
4. **Hot reload friendly** - Runtime lookup enables method addition to superclasses
5. **Less code generation** - No need to duplicate reflection methods in every class
6. **Correct reflection** - `Counter methods` returns complete list

### Negative

1. **Inherited method overhead** - First call to inherited method requires ETS lookup and recursion
2. **Implementation complexity** - Need runtime helper plus codegen changes
3. **Memory overhead (Phase 2)** - Flattened method tables duplicate method info
4. **Invalidation complexity (Phase 2)** - Hot reload must rebuild flattened tables

### Neutral

1. **Performance characteristics change** - Fast path same, inherited path slower but correct
2. **Debugging** - Easier to trace (hierarchy walk explicit in logs)

## Implementation Plan

### Phase 1a: Static ClassHierarchy

1. Build `ClassHierarchy` struct in `crates/beamtalk-core/src/semantic_analysis/`
2. Populate from parsed class definitions (name → superclass, methods, state, sealed)
3. Expose hierarchy queries: `all_methods(class)`, `resolves_selector(class, selector)`, `superclass_chain(class)`
4. Wire into codegen so it can query inherited methods
5. Wire into LSP completion/hover providers

**Acceptance criteria:**
- [ ] `ClassHierarchy` built from parsed AST
- [ ] Codegen can query full method set (local + inherited)
- [ ] LSP completions include inherited methods
- [ ] Shared test cases verify static model matches expected MRO

### Phase 1b: Runtime Dispatch Service

1. Create `beamtalk_dispatch` module (hierarchy walking, method invocation, method combinations)
2. Bootstrap Object with hand-written `beamtalk_object.erl` runtime module (reflection methods: `class`, `respondsTo:`, `instVarNames`, `perform:`, `instVarAt:`, `instVarAt:put:`) — note: `instVarNames`/`instVarAt:`/`instVarAt:put:` renamed to `fieldNames`/`fieldAt:`/`fieldAt:put:` in [ADR 0035](0035-field-based-reflection-api.md)
3. Modify codegen: local fast path + `beamtalk_dispatch:lookup/5` fallback before DNU
4. Update `beamtalk_object_class:methods/1` to walk hierarchy
5. Remove duplicated reflection methods from per-class codegen
6. Add tests for inherited method dispatch + method combinations

**Bootstrap strategy for Object/Actor:** Hand-written Erlang modules (`beamtalk_object.erl`, `beamtalk_actor.erl`) registered during `beamtalk_bootstrap` — consistent with how primitives (`beamtalk_integer.erl`, `beamtalk_string.erl`, etc.) are already implemented. ADR 0007 will explore a Rust `core`-style approach for compiling the stdlib from Beamtalk source.

**Acceptance criteria:**
- [ ] `Counter increment` works (local method - fast path)
- [ ] `Counter class` works (inherited from Object - runtime fallback)
- [ ] `Counter methods` includes `increment` + `class` + `respondsTo:` + ...
- [ ] `Counter undefinedMethod` calls DNU handler
- [ ] `perform:` routes through same dispatch path
- [ ] `super` sends start at immediate superclass
- [ ] Before/after method combinations fire in correct order across chain
- [ ] Reflection/completions reuse the hierarchy walk (no divergence between dispatch and tooling)
- [ ] Structured `#beamtalk_error{}` for all dispatch failures
- [ ] Performance: local methods ±0%, inherited methods acceptable

### Phase 2: Flattened Method Tables (Future)

1. Add `flattened_methods` field to class state
2. Build flattened table during class registration
3. Update `beamtalk_dispatch:lookup` to use flattened table
4. Add invalidation on hot reload
5. Benchmark memory and performance

### Phase 3: Compile-Time Inlining (Future)

1. Use static `ClassHierarchy` to identify stable inherited methods
2. Inline common inherited methods in dispatch (e.g., `class`, `respondsTo:`)
3. Keep runtime fallback for unknown/dynamic methods

## References

- **Related files:**
  - `crates/beamtalk-core/src/codegen/core_erlang/gen_server.rs` - Dispatch codegen
  - `runtime/src/beamtalk_object_class.erl` - Class registry and method storage
  - `runtime/src/beamtalk_dynamic_object.erl` - Dynamic class dispatch
  - `runtime/src/beamtalk_actor.erl` - Actor support functions
  
- **Linear issues:**
  - BT-278: Epic: Unified Method Dispatch (ADR 0006)
  - BT-279: Build static ClassHierarchy in semantic analysis (Phase 1a)
  - BT-281: Create beamtalk_dispatch runtime module (Phase 1b)
  - BT-282: Bootstrap beamtalk_object.erl with shared reflection methods (Phase 1b)
  - BT-283: Flattened method tables for O(1) inherited dispatch (Phase 2)
  - BT-162: Epic: BEAM Object Model Implementation (ADR 0005) — parent epic
  - BT-216: Optimize message dispatch: sync for value types, async for actors
  
- **Related ADRs:**
  - [ADR 0005](0005-beam-object-model-pragmatic-hybrid.md): BEAM Object Model - Pragmatic Hybrid Approach (establishes class hierarchy)
  
- **Prior art:**
  - [LFE Flavors](https://github.com/rvirding/flavors) - Method combinations and hierarchy
  - [Smalltalk-80](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf) - Method lookup algorithm (Chapter 13)
  - [Python MRO (C3)](https://www.python.org/download/releases/2.3/mro/) - For comparison (we don't need this complexity)

## Open Questions

1. **Caching strategy**: Should we cache method lookups in process dictionary? (Deferred to Phase 2)
2. **Remote dispatch**: How does hierarchy walking work when the class registry is distributed? (Deferred — `pg` groups work across nodes, but latency implications need benchmarking)
3. **Performance target**: What's acceptable overhead for inherited methods? (Deferred to Phase 1b benchmarking)
4. **Hot reload invalidation**: Eager vs lazy rebuild of flattened tables? (Deferred to Phase 2)

## Future ADRs

- **ADR 0007: Stdlib Compilation and Primitive Specification** — How to compile the stdlib from Beamtalk source, including specifying primitives and core classes (analogous to Rust's `core` crate). Replaces hand-written bootstrap modules with compiled Beamtalk using named intrinsic pragmas. Introduces three-kind class routing (Actor/Value Type/Primitive) driven by stdlib metadata instead of `is_actor_class()` heuristic.
