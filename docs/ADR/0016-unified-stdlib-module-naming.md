# ADR 0016: Unified Stdlib Packaging and Module Naming

## Status
Proposed (2026-02-10)

## Context

### The Problem

The Beamtalk standard library packaging has evolved through multiple phases (ADR 0007, ADR 0009, BT-340, BT-411) and accumulated inconsistencies. This ADR consolidates the full stdlib packaging architecture — module naming, app metadata, class registration, and runtime dispatch — into a coherent design.

### Current Architecture: How Stdlib Packaging Works

The stdlib packaging pipeline has five interacting pieces:

#### 1. Build-time: `build_stdlib.rs` compiles `lib/*.bt` → `.beam`

Each `.bt` file in `lib/` is compiled to a BEAM module. The module name is determined by `module_name_from_path()` using a **two-prefix scheme**:

| Category | Count | Prefix | Example | Reason |
|----------|-------|--------|---------|--------|
| Primitive types | 12 | `beamtalk_` | `Integer` → `beamtalk_integer` | Drop-in replacement for old hand-written Erlang dispatch modules |
| Non-primitive types | 7 | `bt_stdlib_` | `Number` → `bt_stdlib_number` | Avoid shadowing Erlang built-in modules on case-insensitive filesystems |
| Bootstrap classes | 3 | `bt_stdlib_` | `Object` → `bt_stdlib_object` | Compiled but skipped during loading (bootstrap registers these with runtime modules instead) |

#### 2. Build-time: `.app.src` env metadata

`build_stdlib.rs` generates class hierarchy metadata in `beamtalk_stdlib.app.src`:

```erlang
{env, [
    {classes, [{'beamtalk_integer', 'Integer', 'Number'},
               {'bt_stdlib_number', 'Number', 'Object'},
               {'beamtalk_set', 'Set', 'Object'},
               {'bt_stdlib_association', 'Association', 'Object'},
               ...]}
]}
```

This embeds `{Module, ClassName, SuperclassName}` tuples so the runtime can load modules in dependency order without filesystem discovery.

#### 3. Boot-time: `beamtalk_bootstrap.erl` registers foundation classes

Before stdlib loads, bootstrap creates class processes for the three irreducible foundation classes — ProtoObject, Object, Actor — with `beamtalk_object` as their dispatch module. These are **not** replaced by compiled stdlib.

#### 4. Boot-time: `beamtalk_stdlib.erl` loads compiled modules

Reads the `{classes, [...]}` env from the `.app.src`, topologically sorts by superclass dependency, and calls `code:ensure_loaded/1` on each module. Loading triggers each module's `on_load` hook → `register_class/0`, which starts a class process via `beamtalk_object_class:start/2`.

Bootstrap classes (ProtoObject, Object, Actor) are explicitly skipped via `is_bootstrap_class/1` to avoid overwriting the runtime registrations.

#### 5. Runtime: `beamtalk_primitive.erl` dispatches to modules

`send/3` pattern-matches on Erlang type guards and dispatches to the compiled module:

```erlang
send(Selector, Args, X) when is_integer(X) ->
    beamtalk_integer:dispatch(Selector, Args, X);    % beamtalk_ prefix
send(Selector, Args, X) when is_binary(X) ->
    beamtalk_string:dispatch(Selector, Args, X);     % beamtalk_ prefix
...
%% Tagged maps — examine $beamtalk_class tag:
    bt_stdlib_association:dispatch(Selector, Args, X);  % bt_stdlib_ prefix (!)
    beamtalk_set:dispatch(Selector, Args, X);           % beamtalk_ prefix
```

For user-defined value types, `class_name_to_module/1` converts CamelCase → snake_case at runtime *without any prefix*, falling back to the module name as compiled by the user.

### What's Wrong

**1. The two-prefix naming convention is vestigial.**

The `beamtalk_` prefix was introduced so compiled `.bt` modules could be **drop-in replacements** for hand-written Erlang dispatch modules (`beamtalk_integer.erl`, `beamtalk_string.erl`, etc.). Those hand-written modules are **all gone now** — deleted during ADR 0007 Phase 4. The naming split has no remaining justification.

**2. Three-way naming logic in codegen, each annotated "must stay in sync".**

The compiler maintains three separate class lists across two crates:

```rust
// build_stdlib.rs
fn is_primitive_type(class_name: &str) -> bool { ... }          // 12 classes

// value_type_codegen.rs  
fn is_primitive_type(class_name: &str) -> bool { ... }          // Same 12 (must match!)
fn is_stdlib_nonprimitive_type(class_name: &str) -> bool { ... } // 9 classes (must match!)
fn superclass_module_name(superclass: &str) -> Option<String> {
    // Three-way branch: primitive? non-primitive stdlib? user-defined?
}
```

Each annotated with `// NOTE: Must stay in sync with ...` — a textbook code smell signaling the design has outgrown its implementation.

**3. Already inconsistent in runtime dispatch.**

`beamtalk_primitive.erl` dispatches Association with the `bt_stdlib_` prefix but Set with the `beamtalk_` prefix — both are tagged-map value types compiled from `lib/*.bt`:

```erlang
bt_stdlib_association:dispatch(Selector, Args, X);  % bt_stdlib_ prefix
beamtalk_set:dispatch(Selector, Args, X);           % beamtalk_ prefix
```

**4. Ambiguous naming convention.**

A developer seeing `beamtalk_set.beam` in ebin cannot tell whether it's:
- Hand-written Erlang (like `beamtalk_actor.erl` in runtime), or
- Compiled from `.bt` (like `lib/Set.bt` in stdlib)

**5. "Primitive" conflates two orthogonal concepts.**

`is_primitive_type()` mixes up:
- **Dispatch mechanism** — native Erlang values dispatched by `beamtalk_primitive:send/3` via type guards (`is_integer`, `is_binary`, etc.). This is a runtime concern.
- **Module naming** — gets the `beamtalk_` prefix. This is a build-system concern.

These are independent. Renaming `beamtalk_integer` to `bt_stdlib_integer` changes zero dispatch logic — `send/3` still matches `is_integer(X)` and calls `SomeModule:dispatch/3`.

## Decision

### 1. Unify all compiled stdlib module names to `bt_stdlib_*`

Every `.bt` file in `lib/` compiles to `bt_stdlib_{snake_case}`, regardless of whether the class is "primitive":

| Class | Before | After |
|-------|--------|-------|
| Integer | `beamtalk_integer` | `bt_stdlib_integer` |
| Float | `beamtalk_float` | `bt_stdlib_float` |
| String | `beamtalk_string` | `bt_stdlib_string` |
| True | `beamtalk_true` | `bt_stdlib_true` |
| False | `beamtalk_false` | `bt_stdlib_false` |
| UndefinedObject | `beamtalk_undefined_object` | `bt_stdlib_undefined_object` |
| Block | `beamtalk_block` | `bt_stdlib_block` |
| Symbol | `beamtalk_symbol` | `bt_stdlib_symbol` |
| Tuple | `beamtalk_tuple` | `bt_stdlib_tuple` |
| List | `beamtalk_list` | `bt_stdlib_list` |
| Dictionary | `beamtalk_dictionary` | `bt_stdlib_dictionary` |
| Set | `beamtalk_set` | `bt_stdlib_set` |
| Object | `bt_stdlib_object` | *(unchanged)* |
| Number | `bt_stdlib_number` | *(unchanged)* |
| Actor | `bt_stdlib_actor` | *(unchanged)* |
| Association | `bt_stdlib_association` | *(unchanged)* |
| ... | `bt_stdlib_*` | *(unchanged)* |

**Resulting naming convention:**

| Pattern | Meaning | Location |
|---------|---------|----------|
| `beamtalk_*_ops.erl` | Low-level Erlang FFI primitives | `runtime/apps/beamtalk_runtime/src/` |
| `beamtalk_*.erl` | Other hand-written Erlang runtime code | `runtime/apps/beamtalk_runtime/src/` |
| `bt_stdlib_*.beam` | Compiled from `.bt` source in `lib/` | `runtime/apps/beamtalk_stdlib/ebin/` |

This makes the two-layer architecture explicit: `bt_stdlib_list` (Beamtalk API compiled from `lib/List.bt`) wraps `beamtalk_list_ops` (Erlang FFI in runtime) via `@primitive` pragmas. The runtime provides the bare-metal operations; the stdlib provides the Beamtalk-level class interface.

### 2. Simplify `module_name_from_path()` to a single rule

```rust
// build_stdlib.rs — AFTER
fn module_name_from_path(path: &Utf8Path) -> Result<String> {
    let stem = path.file_stem().ok_or_else(|| ...)?;
    let snake = to_module_name(stem);
    Ok(format!("bt_stdlib_{snake}"))
}
// is_primitive_type() deleted — no longer needed
```

### 3. Collapse three-way codegen lists into one

```rust
// value_type_codegen.rs — AFTER
fn is_known_stdlib_type(class_name: &str) -> bool {
    matches!(class_name,
        "Integer" | "Float" | "String" | "True" | "False"
        | "UndefinedObject" | "Block" | "Symbol" | "Tuple"
        | "List" | "Dictionary" | "Set"
        | "Object" | "Number" | "Actor" | "File"
        | "Association" | "SystemDictionary" | "TranscriptStream"
        | "Exception" | "Error"
    )
}

fn superclass_module_name(superclass: &str) -> Option<String> {
    if superclass == "ProtoObject" {
        return None;
    }
    let snake = to_module_name(superclass);
    if Self::is_known_stdlib_type(superclass) {
        Some(format!("bt_stdlib_{snake}"))
    } else {
        Some(snake) // user-defined class
    }
}
// is_primitive_type() and is_stdlib_nonprimitive_type() deleted
```

### 4. Update `.app.src` env metadata (automatic)

`build-stdlib` regenerates the env metadata. After the rename all entries use `bt_stdlib_` prefix:

```erlang
{env, [
    {classes, [{'bt_stdlib_integer', 'Integer', 'Number'},
               {'bt_stdlib_number', 'Number', 'Object'},
               {'bt_stdlib_set', 'Set', 'Object'},
               {'bt_stdlib_association', 'Association', 'Object'},
               ...]}
]}
```

No code changes in `beamtalk_stdlib.erl` — it reads `{Module, ClassName, SuperclassName}` tuples generically. The topo-sort, `is_bootstrap_class/1` skip, and `ensure_class_registered/2` all work on class names, not module names.

### 5. Update `beamtalk_primitive.erl` dispatch atoms

Mechanical rename of module atoms in `send/3` and `has_method/1`:

```erlang
%% Before:
send(Selector, Args, X) when is_integer(X) ->
    beamtalk_integer:dispatch(Selector, Args, X);

%% After:
send(Selector, Args, X) when is_integer(X) ->
    bt_stdlib_integer:dispatch(Selector, Args, X);
```

The dispatch logic (type guard matching, tagged-map detection, fallback to `beamtalk_object`) is unchanged. Only the module atoms that appear after the `->` change.

### 6. `class_name_to_module/1` stays as-is for user-defined types

The runtime fallback for user-defined value types (`class_name_to_module/1` in `beamtalk_primitive.erl`) converts CamelCase → snake_case without any prefix. This remains correct — user `.bt` files compile to plain snake_case modules (e.g., `Point` → `point`, `Validator` → `validator`). Only stdlib gets the `bt_stdlib_` prefix.

## Prior Art

### Erlang/OTP

OTP uses application-prefixed module names (e.g., `crypto_ec`, `ssl_cipher`) to avoid collisions between applications. The `bt_stdlib_` prefix follows this convention — namespacing all compiled stdlib modules under the `beamtalk_stdlib` application.

### Gleam

Gleam compiles modules to Erlang using the package name as prefix: `gleam@list`, `gleam@string`. Analogous to our `bt_stdlib_` prefix for the standard library package.

### Elixir

Elixir uses `Elixir.ModuleName` as the Erlang module atom for all compiled modules, providing a uniform prefix that distinguishes Elixir modules from Erlang ones. Same principle: one prefix, no exceptions.

## User Impact

### For Beamtalk Users (No Impact)

This change is entirely internal. Users write `42 + 3`, `Set new`, `'hello' size` — module names are never visible in Beamtalk code. Error messages display class names (`Integer`, `Set`), not module names.

### For Runtime Developers

Clear rule: if a module starts with `beamtalk_`, it's hand-written Erlang you can edit directly. If it starts with `bt_stdlib_`, it's compiled from a `.bt` file — edit the `.bt` source instead.

### For Tooling/CI

No change to build commands or test commands. `just build-stdlib` produces `bt_stdlib_*.beam` files in the same location.

## Steelman Analysis

### Alternative: Keep the Split Naming (Status Quo)

| Cohort | Best argument | Assessment |
|--------|--------------|------------|
| ⚙️ **BEAM veteran** | Renaming 12 modules risks stale `.beam` files or dialyzer PLT corruption | **Weak.** Stdlib is always rebuilt as a unit via `just build-stdlib`. No incremental module builds exist. `just clean` handles stale artifacts. Standard OTP practice. |
| 🏭 **Operator** | Zero risk. Ship features, not renames. ~2 hours spent here is ~2 hours not spent on metaclasses or the test framework | **Moderate, but time-decaying.** Every new stdlib class added pays the "which prefix?" tax. The rename gets more expensive the longer we wait as more code accumulates referencing the old names. Pre-1.0 is the cheapest time to do this. |
| 🎨 **Language designer** | The naming split documents the real distinction between primitive types (native Erlang values) and non-primitive types (tagged maps) | **Weak.** The distinction is real, but module names are the wrong place to encode it. `beamtalk_primitive:send/3` already encodes it precisely via type guards — that's the source of truth. Worse, the boundary can shift: if Set moves from ordsets to ETS-backed processes, the split naming forces a module rename for a pure implementation change. |

### Arguments We Considered But Found No Strong Steelman For

- **Stack traces / crash dumps:** Module atoms appear in Erlang stack traces. `bt_stdlib_integer` is actually *better* than `beamtalk_integer` because it clearly signals "compiled stdlib" rather than being ambiguous with hand-written runtime modules.
- **Third-party packages / FFI:** If external code ever calls stdlib dispatch modules directly, a rename breaks them. But Beamtalk is pre-1.0 — doing the rename now is free. Waiting until after a package ecosystem exists makes it a breaking change.
- **Convention for future packages:** Unified `bt_stdlib_*` establishes the pattern that OTP app name = module prefix. Future user packages would naturally follow: `bt_mylib_*`, `bt_webframework_*`.

### Verdict

No steelman survives scrutiny. The operator timing argument has real weight but argues for "do it soon" not "don't do it." Every other argument either collapses on inspection or actively favours the rename.

## Alternatives Considered

### Alternative A: Move Primitives to `beamtalk_runtime` App

Move the 12 primitive BEAM files into the `beamtalk_runtime` application since the runtime dispatches to them.

**Rejected because:**
- Blurs the line between hand-written Erlang and compiled Beamtalk within the same OTP app
- Complicates the build pipeline — `beamtalk_runtime` is built by rebar3 from `.erl`, not from `.bt`
- Violates the dependency direction from ADR 0009: `beamtalk_workspace → beamtalk_runtime → beamtalk_stdlib`
- All classes live in `lib/*.bt` regardless of whether they're "primitive" — they belong together in `beamtalk_stdlib`

### Alternative B: Unify Everything to `beamtalk_*`

Use `beamtalk_` prefix for all compiled stdlib, removing the `bt_stdlib_` prefix.

**Rejected because:**
- Creates naming conflicts on case-insensitive filesystems (e.g., `error.beam` clashes with Erlang's `error` module)
- Makes it impossible to distinguish compiled Beamtalk from hand-written Erlang by filename
- `beamtalk_object.erl` (hand-written runtime) would clash with compiled `Object.bt` → `beamtalk_object.beam`

### Alternative C: Use a Different Prefix (e.g., `btlib_*`)

Shorter prefix to reduce verbosity.

**Rejected because:**
- `bt_stdlib_` is already established for 10 modules — changing the prefix for all 22 is more churn for marginal gain
- `bt_stdlib_` clearly indicates "beamtalk standard library" — `btlib_` is less descriptive
- These module names are never typed by users

## Consequences

### Positive
- Three separate `is_*_type()` functions collapse into one `is_known_stdlib_type()`
- `module_name_from_path()` becomes a single-line function
- `superclass_module_name()` loses its three-way branch
- Clear, memorable naming convention: `beamtalk_*.erl` = hand-written Erlang, `bt_stdlib_*.beam` = compiled `.bt`
- Eliminates the "must stay in sync" coupling between three match lists across two crates
- `.app.src` env metadata uses uniform prefix — easier to grep, validate, and reason about
- **Implementation changes don't force module renames.** If a class moves between primitive and non-primitive (e.g., Set moves from ordsets to ETS-backed processes), the module name stays `bt_stdlib_set` — only the dispatch logic in `beamtalk_primitive.erl` changes. With the split naming, such a change would require renaming the module, updating every reference, and rebuilding dialyzer PLTs

### Negative
- Mechanical churn across ~8 files (low risk but nonzero)
- `beamtalk_primitive.erl` needs ~24 module atom updates (12 in `send/3`, 12 in `has_method/1`)
- Snapshot tests for codegen need updating (module names appear in generated Core Erlang)
- Dialyzer PLT may need a clean rebuild after the rename

### Neutral
- No user-visible behavior change
- No performance impact (module dispatch is atom comparison either way)
- `beamtalk_stdlib.erl` loading logic unchanged — it reads `{Module, ClassName, Super}` tuples generically
- `is_bootstrap_class/1` skip logic unchanged — it checks class names, not module names
- `class_name_to_module/1` fallback for user-defined types unchanged — no prefix involved
- Test infrastructure (stdlib test runner) already bootstraps class system correctly regardless of naming

## Implementation

### Affected Components

| Component | File(s) | Change |
|-----------|---------|--------|
| **Build stdlib** | `build_stdlib.rs` | Simplify `module_name_from_path()` — always `bt_stdlib_`. Delete `is_primitive_type()` |
| **Codegen** | `value_type_codegen.rs` | Merge `is_primitive_type()` + `is_stdlib_nonprimitive_type()` → `is_known_stdlib_type()`. Simplify `superclass_module_name()` |
| **Runtime dispatch** | `beamtalk_primitive.erl` | Update ~24 module atoms in `send/3` and `has_method/1` |
| **App metadata** | `beamtalk_stdlib.app.src` | Regenerated by `build-stdlib` (automatic — no manual change) |
| **Stdlib loading** | `beamtalk_stdlib.erl` | No change — reads module names from env generically |
| **Bootstrap** | `beamtalk_bootstrap.erl` | No change — registers ProtoObject/Object/Actor with `beamtalk_object`, unrelated to stdlib module names |
| **Snapshot tests** | `tests/snapshots/*.snap` | Update expected module names in codegen snapshots |
| **Codegen simulation tests** | `beamtalk_codegen_simulation_tests.erl` | Update module references if hardcoded |

### Phases

**Single phase** — this is a mechanical rename, not a behavioral change. All changes can be made atomically:

1. Update `build_stdlib.rs`: delete `is_primitive_type()`, simplify `module_name_from_path()`
2. Update `value_type_codegen.rs`: merge type lists, simplify `superclass_module_name()`
3. Run `just build-stdlib` to regenerate BEAM files and `.app.src` with new names
4. Update `beamtalk_primitive.erl` module atoms in `send/3` and `has_method/1`
5. Run `just ci` — fix snapshot tests as needed
6. Clean dialyzer PLT and rebuild: `cd runtime && rebar3 dialyzer`

**Estimated size: M** (mechanical but wide, ~8 files, ~80 lines changed)

## References
- Related ADRs: [ADR 0007](0007-compilable-stdlib-with-primitive-injection.md) — introduced the split naming convention and compilable stdlib
- Related ADRs: [ADR 0009](0009-otp-application-structure.md) — OTP app structure, dependency direction `workspace → runtime → stdlib`
- Related ADRs: [ADR 0006](0006-unified-method-dispatch.md) — unified dispatch, flattened method tables
- Documentation: `docs/development/architecture-principles.md` — layered architecture
- Discovered during: BT-411 (class methods and initialize protocol)
