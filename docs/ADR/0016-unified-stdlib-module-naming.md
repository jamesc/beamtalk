# ADR 0016: Universal Module Naming with @ Separator

## Status
Accepted (2026-02-10), Implemented (BT-454)

**Amendment 2026-02-10:** Changed from underscore-based naming (`bt_stdlib_*`) to at-sign separator (`bt@stdlib@*`) for clearer namespacing and following proven BEAM patterns (Gleam). Extends scope from stdlib-only to all compiled Beamtalk modules.

> **Filename note:** This ADR was originally drafted for "unified stdlib module naming" and later expanded to universal module naming for all compiled Beamtalk modules. The file name `0016-unified-stdlib-module-naming.md` is retained for historical continuity and to avoid breaking existing links.

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

**Note:** This two-prefix scheme is the current implementation being replaced by this ADR.

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

**Note:** This mixed-prefix metadata is the current implementation being replaced.

This embeds `{Module, ClassName, SuperclassName}` tuples so the runtime can load modules in dependency order without filesystem discovery.

#### 3. Boot-time: `beamtalk_bootstrap.erl` registers foundation classes

Before stdlib loads, bootstrap creates class processes for the three irreducible foundation classes — ProtoObject, Object, Actor — with `beamtalk_object` as their dispatch module. These are **not** replaced by compiled stdlib.

#### 4. Boot-time: `beamtalk_stdlib.erl` loads compiled modules

Reads the `{classes, [...]}` env from the `.app.src`, topologically sorts by superclass dependency, and calls `code:ensure_loaded/1` on each module. Loading triggers each module's `on_load` hook → `register_class/0`, which starts a class process via `beamtalk_object_class:start/2`.

Bootstrap classes (ProtoObject, Object, Actor) are explicitly skipped via `is_bootstrap_class/1` to avoid overwriting the runtime registrations.

#### 5. Runtime: `beamtalk_primitive.erl` dispatches to modules

`send/3` pattern-matches on Erlang type guards and dispatches to the compiled module:

```erlang
send(X, Selector, Args) when is_integer(X) ->
    beamtalk_integer:dispatch(X, Selector, Args);    % beamtalk_ prefix
send(X, Selector, Args) when is_binary(X) ->
    beamtalk_string:dispatch(X, Selector, Args);     % beamtalk_ prefix
...
%% Tagged maps — examine $beamtalk_class tag:
    bt_stdlib_association:dispatch(X, Selector, Args);  % bt_stdlib_ prefix (!)
    beamtalk_set:dispatch(X, Selector, Args);           % beamtalk_ prefix
```

**Note:** This inconsistent dispatch is the current implementation being replaced.

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

### 1. Use `bt@` separator for all compiled Beamtalk modules

Following Gleam's proven pattern of using `@` as a namespace separator, all `.bt` files compile to modules with `bt@` prefix:

**Stdlib:** `bt@stdlib@{snake_case}`
**User code:** `bt@{snake_case}` 

The `@` separator provides clear visual distinction from hand-written Erlang runtime modules which use `beamtalk_` with underscore.

| Class/Module | Before | After |
|--------------|--------|-------|
| **Stdlib classes:** |||
| Integer | `beamtalk_integer` | `bt@stdlib@integer` |
| Float | `beamtalk_float` | `bt@stdlib@float` |
| String | `beamtalk_string` | `bt@stdlib@string` |
| True | `beamtalk_true` | `bt@stdlib@true` |
| False | `beamtalk_false` | `bt@stdlib@false` |
| UndefinedObject | `beamtalk_undefined_object` | `bt@stdlib@undefined_object` |
| Block | `beamtalk_block` | `bt@stdlib@block` |
| Symbol | `beamtalk_symbol` | `bt@stdlib@symbol` |
| Tuple | `beamtalk_tuple` | `bt@stdlib@tuple` |
| List | `beamtalk_list` | `bt@stdlib@list` |
| Dictionary | `beamtalk_dictionary` | `bt@stdlib@dictionary` |
| Set | `beamtalk_set` | `bt@stdlib@set` |
| Object | `bt_stdlib_object` | `bt@stdlib@object` |
| Number | `bt_stdlib_number` | `bt@stdlib@number` |
| Actor | `bt_stdlib_actor` | `bt@stdlib@actor` |
| Association | `bt_stdlib_association` | `bt@stdlib@association` |
| ... | `bt_stdlib_*` | `bt@stdlib@*` |
| **User code:** |||
| Counter (example) | `counter` | `bt@counter` |
| Point (example) | `point` | `bt@point` |
| Validator (user) | `validator` | `bt@validator` |

**Resulting naming convention:**

| Pattern | Meaning | Location |
|---------|---------|----------|
| `beamtalk_*_ops.erl` | Low-level Erlang FFI primitives | `runtime/apps/beamtalk_runtime/src/` |
| `beamtalk_*.erl` | Other hand-written Erlang runtime code | `runtime/apps/beamtalk_runtime/src/` |
| `bt@stdlib@*.beam` | Stdlib compiled from `lib/*.bt` | `runtime/apps/beamtalk_stdlib/ebin/` |
| `bt@*.beam` | User code compiled from `*.bt` | User's project `ebin/` |

This makes the two-layer architecture explicit: `bt@stdlib@list` (Beamtalk stdlib API compiled from `lib/List.bt`) wraps `beamtalk_list_ops` (Erlang FFI in runtime) via `@primitive` pragmas. The runtime provides the bare-metal operations; the stdlib provides the Beamtalk-level class interface.

### Why `@` separator?

1. **Visual distinction** - `bt@stdlib@integer` vs `beamtalk_actor` makes compiled vs hand-written instantly clear
2. **Unlikely in user identifiers** - Users rarely choose `@` in names (unlike `_` which is common: `user_service`, `data_store`)
3. **Proven by Gleam** - Gleam successfully uses `gleam@list`, `gleam@string`, `gleam@dict` for its stdlib
4. **Package-ready** - Natural extension to third-party packages: `bt@json@parser`, `bt@web@handler`
5. **Namespace clarity** - `stdlib@` segment provides explicit collision protection

### 2. Simplify `module_name_from_path()` to a single rule

```rust
// build_stdlib.rs — AFTER
fn module_name_from_path(path: &Utf8Path) -> Result<String> {
    let stem = path.file_stem().ok_or_else(|| ...)?;
    let snake = to_module_name(stem);
    Ok(format!("bt@stdlib@{snake}"))  // ✅ Using @ separator
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
        Some(format!("bt@stdlib@{snake}"))  // ✅ Using @ separator
    } else {
        Some(format!("bt@{snake}"))  // ✅ User code also gets bt@ prefix
    }
}
// is_primitive_type() and is_stdlib_nonprimitive_type() deleted
```

### 4. Update `.app.src` env metadata (automatic)

`build-stdlib` regenerates the env metadata. After the rename all entries use `bt@stdlib@` prefix:

```erlang
{env, [
    {classes, [{bt@stdlib@integer, 'Integer', 'Number'},
               {bt@stdlib@number, 'Number', 'Object'},
               {bt@stdlib@set, 'Set', 'Object'},
               {bt@stdlib@association, 'Association', 'Object'},
               ...]}
]}
```

No code changes in `beamtalk_stdlib.erl` — it reads `{Module, ClassName, SuperclassName}` tuples generically. The topo-sort, `is_bootstrap_class/1` skip, and `ensure_class_registered/2` all work on class names, not module names.

### 5. Update `beamtalk_primitive.erl` dispatch atoms

Mechanical rename of module atoms in `send/3` and `responds_to/2` (which in turn calls the per-class `has_method/1` implementations):

```erlang
%% Before:
send(X, Selector, Args) when is_integer(X) ->
    beamtalk_integer:dispatch(X, Selector, Args);

%% After:
send(X, Selector, Args) when is_integer(X) ->
    bt@stdlib@integer:dispatch(X, Selector, Args);  % Unquoted — @ is legal in Erlang atoms
```

The dispatch logic (type guard matching, tagged-map detection, fallback to `beamtalk_object`) is unchanged. Only the module atoms that appear after the `->` change.

### 6. `class_name_to_module/1` stays as-is for user-defined types

The runtime fallback for user-defined value types (`class_name_to_module/1` in `beamtalk_primitive.erl`) currently converts CamelCase → snake_case without any prefix. This needs updating — user `.bt` files now compile to `bt@{snake_case}` modules (e.g., `Point` → `bt@point`, `Validator` → `bt@validator`). The fallback must add the `bt@` prefix.

## Prior Art

### Erlang/OTP

OTP uses application-prefixed module names (e.g., `crypto_ec`, `ssl_cipher`) to avoid collisions between applications. The `bt@stdlib@` naming follows a similar namespace isolation principle — all compiled stdlib modules are clearly distinguished from runtime modules and third-party code.

### Gleam

Gleam compiles modules to Erlang using the package name as `@` separator prefix: `gleam@list`, `gleam@string`, `gleam@dict`. 

**Why Gleam chose `@`:**
- The `@` character is legal in Erlang atoms but unlikely to appear in user-chosen identifiers
- Package names already use it in Gleam's source syntax (`import gleam/list`)
- Avoids collision with Erlang's `:` module separator (used in `module:function` calls)
- No quoting needed — `@` is allowed in unquoted Erlang atoms (along with alphanumerics and `_`), so `gleam@list` works as-is in source and shell

**Gleam's experience:** Successfully used in production since 2019. No reported issues with tooling (rebar3, dialyzer, observer). The community has accepted the convention and it appears throughout the ecosystem (gleam-lang/stdlib, gleam-lang/httpc, etc.).

### Elixir

Elixir uses `Elixir.ModuleName` as the Erlang module atom for all compiled modules (using `.` as separator), providing a uniform prefix that distinguishes Elixir modules from Erlang ones.

**Why Elixir chose `.`:**
- Matches Elixir's source-level module syntax (`Elixir.List`)
- The dot is the natural separator for hierarchical names in many languages
- Requires quoting in Erlang code (`'Elixir.List'`), but Elixir users rarely write raw Erlang

**Trade-off:** More quoting required than `@`, but arguably more familiar to developers from other ecosystems (Java, Python, etc.).

**Our choice:** We use `@` instead of `.` because:
1. Gleam proves `@` works well in the BEAM ecosystem (5+ years of production use)
2. Less visual noise than dots: `bt@stdlib@integer` vs `'bt.stdlib.integer'` (dots require quoting, `@` does not)
3. Clearer separation from Erlang's `:` operator (`Module:function` calls)

## User Impact

### For Beamtalk Users (No Impact)

This change is entirely internal. Users write `42 + 3`, `Set new`, `'hello' size` — module names are never visible in Beamtalk code. Error messages display class names (`Integer`, `Set`), not module names.

### For Runtime Developers

Clear rule: if a module starts with `beamtalk_`, it's hand-written Erlang you can edit directly. If it starts with `bt@`, it's compiled from a `.bt` file — edit the `.bt` source instead.

### For Tooling/CI

No change to build commands or test commands. `just build-stdlib` produces `bt@stdlib@*.beam` files in the same location.

## Steelman Analysis

### Alternative: Keep the Split Naming (Status Quo)

| Cohort | Best argument | Assessment |
|--------|--------------|------------|
| ⚙️ **BEAM veteran** | Renaming 12 modules risks stale `.beam` files or dialyzer PLT corruption | **Weak.** Stdlib is always rebuilt as a unit via `just build-stdlib`. No incremental module builds exist. `just clean` handles stale artifacts. Standard OTP practice. |
| 🏭 **Operator** | Zero risk. Ship features, not renames. ~2 hours spent here is ~2 hours not spent on metaclasses or the test framework | **Moderate, but time-decaying.** Every new stdlib class added pays the "which prefix?" tax. The rename gets more expensive the longer we wait as more code accumulates referencing the old names. Pre-1.0 is the cheapest time to do this. |
| 🎨 **Language designer** | The naming split documents the real distinction between primitive types (native Erlang values) and non-primitive types (tagged maps) | **Weak.** The distinction is real, but module names are the wrong place to encode it. `beamtalk_primitive:send/3` already encodes it precisely via type guards — that's the source of truth. Worse, the boundary can shift: if Set moves from ordsets to ETS-backed processes, the split naming forces a module rename for a pure implementation change. |

### Arguments We Considered But Found No Strong Steelman For

- **Stack traces / crash dumps:** Module atoms appear in Erlang stack traces. `bt@stdlib@integer` is clearer than `beamtalk_integer` because the `@` separator immediately signals "compiled Beamtalk stdlib" rather than being ambiguous with hand-written runtime modules.
- **Third-party packages / FFI:** If external code ever calls stdlib dispatch modules directly, a rename breaks them. But Beamtalk is pre-1.0 — doing the rename now is free. Waiting until after a package ecosystem exists makes it a breaking change.
- **Convention for future packages:** Unified `bt@stdlib@*` establishes the pattern that package name = module namespace. Future user packages would naturally follow: `bt@mylib@*`, `bt@webframework@*`.

### Additional Steelman Arguments

**🆕 Newcomer / Developer Experience perspective:**

| Cohort | Best argument | Assessment |
|--------|--------------|------------|
| 🐍 **Python/Ruby developer** | The `@` separator looks unfamiliar: `bt@stdlib@integer:dispatch()`. Why not just `bt_stdlib_integer`? | **Weak.** Module names rarely appear in user-facing errors (we show class names, not module names). Runtime developers see this daily, but they benefit from the clear compiled-vs-handwritten distinction. The `@` separator is unfamiliar at first but becomes recognizable quickly (Gleam developers report no confusion after initial exposure). No quoting needed — `@` is legal in unquoted Erlang atoms. |
| 📦 **Package author** | I'm publishing `beamtalk-json`. Do I use `bt@json@parser` or `beamtalk_json_parser`? If every package uses `bt@`, how do we avoid collisions? | **Weak.** Package authors would follow the pattern: `bt@json@parser`, `bt@web@handler`. Collisions are prevented by the middle namespace segment (package name). This is exactly how Gleam works: `gleam@json`, `gleam@http`. The pattern is proven and scales. Documentation should make this clear with examples. |
| 🔧 **Erlang FFI author** | I'm writing Erlang code that calls Beamtalk modules: `bt@stdlib@list:foldl/3`. The `@` looks unusual in Erlang code. | **Weak.** Since `@` is legal in unquoted Erlang atoms, no quoting is needed — `bt@stdlib@list:foldl(Fun, Acc, List)` works directly. FFI is rare (most users write pure Beamtalk). The unfamiliarity fades quickly (Gleam FFI authors have no issues). Trade-off: FFI ergonomics vs namespace clarity. We choose clarity because FFI is the exception, not the rule. |

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

### Alternative B: Unify Everything to `beamtalk@*`

Use `beamtalk@` prefix for all compiled code (stdlib and user), without the `stdlib@` namespace segment.

**Rejected because:**
- Collision risk: user creates `beamtalk@set` (custom Set) → clashes with stdlib `beamtalk@set`
- Longer prefix (`beamtalk@` vs `bt@`) with no benefit
- No namespace protection between stdlib and user code

### Alternative C: Use Different Separators (`.` like Elixir, or `-`)

Use `.` (Elixir-style `bt.stdlib.integer`) or `-` (hyphen-separated `bt-stdlib-integer`).

**Rejected because:**
- `.` in atoms requires quoting in some Erlang contexts, less ergonomic than `@`
- `-` is ambiguous (looks like kebab-case, not namespacing)
- `@` is proven by Gleam and visually distinct
- These module names are never typed by users, so brevity is less important than clarity

### Alternative D: Only Rename Stdlib, Keep User Code as Plain Modules

Use `bt@stdlib@*` for stdlib but keep user code as plain `counter`, `point`, etc.

**Rejected because:**
- Creates a two-tier system (stdlib namespaced, user code not)
- User modules can still collide with Erlang/OTP modules on case-insensitive filesystems
- `superclass_module_name()` still needs special-casing for stdlib vs user code
- Doesn't establish a clear pattern for future third-party packages
- Stdlib classes that extend user classes would have inconsistent module naming in the hierarchy

**Counter-argument:** Could defer user code renaming until 0.2.0 or 0.3.0 to observe Gleam ecosystem evolution and reduce breaking change surface area now.

**Rebuttal:** 
- Deferring creates a second breaking change later (worse than one break now)
- Pre-1.0 is the only time we can change fundamental conventions for free
- Gleam's pattern has already proven stable (5+ years, no changes)
- Doing both together establishes clear convention: all `.bt` compiles to `bt@*`
- The incremental cost of including user code is low (same codegen changes, same test updates)

## Consequences

### Positive
- Three separate `is_*_type()` functions collapse into one `is_known_stdlib_type()`
- `module_name_from_path()` becomes a single-line function
- `superclass_module_name()` loses its three-way branch — unified `bt@` prefix for all compiled code
- Clear, memorable naming convention: `beamtalk_*.erl` = hand-written Erlang, `bt@*.beam` = compiled `.bt`
- **User code gets namespacing** — `bt@counter` can't clash with Erlang/OTP modules
- Eliminates the "must stay in sync" coupling between three match lists across two crates
- `.app.src` env metadata uses uniform prefix — easier to grep, validate, and reason about
- **Implementation changes don't force module renames.** If a class moves between primitive and non-primitive (e.g., Set moves from ordsets to ETS-backed processes), the module name stays `bt@stdlib@set` — only the dispatch logic in `beamtalk_primitive.erl` changes
- **Package-ready** — Natural extension to third-party packages: `bt@json@parser`, `bt@web@handler`

### Negative
- **No atom quoting needed** — `@` is legal in unquoted Erlang atoms (along with alphanumerics and `_`), so `bt@stdlib@integer` works without quoting in all contexts (source, shell, observer). This is a key advantage over `.` (which requires quoting: `'bt.stdlib.integer'`).
- **Two-character separator** — `@stdlib@` vs `_stdlib_` is slightly longer (1 extra char)
- **Documentation fragmentation** — All existing examples, tutorials, blog posts using old module names become outdated. Search engines will index both naming schemes during transition period. Requires documentation sweep and redirects.
- Mechanical churn across ~10 files (low risk but nonzero)
- `beamtalk_primitive.erl` needs ~24 module atom updates (12 in `send/3`, 12 in `responds_to/2`)
- Codegen emits `bt@stdlib@*` atoms as unquoted module names in Core Erlang
- Snapshot tests for codegen need updating (module names appear in generated Core Erlang)
- Dialyzer PLT may need a clean rebuild after the rename
- **User code compilation changes** — User `.bt` files now compile to `bt@module` instead of plain `module`. Affects module name lookup and loading.
- **Tooling compatibility risk** — Must verify `@` in atoms works correctly with all BEAM ecosystem tools: rebar3 plugins, observer GUI, dialyzer, cover, etc. Gleam's 5+ years of production use suggests low risk, but worth explicit testing.

### Neutral
- **Performance:** No user-visible behavior change (module names don't appear in Beamtalk code). No performance impact for module dispatch (atom comparison is identical regardless of how the atom was written — the BEAM VM stores all atoms in a global atom table). **Note:** This is a fundamental BEAM VM design property, not an assumption. See Erlang Efficiency Guide: "Atoms are stored in a global atom table and are accessed by an index."
- `beamtalk_stdlib.erl` loading logic unchanged — it reads `{Module, ClassName, Super}` tuples generically
- `is_bootstrap_class/1` skip logic unchanged — it checks class names, not module names
- Test infrastructure (stdlib test runner) already bootstraps class system correctly regardless of naming

## Implementation

### Affected Components

| Component | File(s) | Change |
|-----------|---------|--------|
| **Build stdlib** | `build_stdlib.rs` | Simplify `module_name_from_path()` — always `bt@stdlib@`. Delete `is_primitive_type()`. Emit `bt@stdlib@list` etc. as unquoted atoms (no quoting needed — `@` is legal in Erlang atoms). |
| **User code compiler** | `main.rs` / `compile.rs` | Update module name generation to `bt@{snake_case}` for user `.bt` files |
| **Codegen** | `value_type_codegen.rs`, `module_codegen.rs` | Merge `is_primitive_type()` + `is_stdlib_nonprimitive_type()` → `is_known_stdlib_type()`. Update `superclass_module_name()` to emit `bt@stdlib@` and `bt@` prefixes. No quoting needed — `@` is legal in unquoted Erlang atoms. |
| **Runtime dispatch** | `beamtalk_primitive.erl` | Update ~24 module atoms in `send/3` and `has_method/1` to use `@` separator (unquoted — `@` is legal in Erlang atoms) |
| **class_name_to_module/1** | `beamtalk_primitive.erl` | Update fallback to add `bt@` prefix for user-defined types |
| **App metadata** | `beamtalk_stdlib.app.src` | Regenerated by `build-stdlib` (automatic — no manual change) |
| **Stdlib loading** | `beamtalk_stdlib.erl` | No change — reads module names from env generically |
| **Bootstrap** | `beamtalk_bootstrap.erl` | No change — registers ProtoObject/Object/Actor with `beamtalk_object`, unrelated to stdlib module names |
| **Snapshot tests** | `tests/snapshots/*.snap` | Update expected module names in codegen snapshots (unquoted `bt@stdlib@*` atoms) |
| **Codegen simulation tests** | `beamtalk_codegen_simulation_tests.erl` | Update module references if hardcoded |

### Phases

**Single phase** — this is a mechanical rename, not a behavioral change. All changes can be made atomically:

1. Update `build_stdlib.rs`: delete `is_primitive_type()`, simplify `module_name_from_path()` to emit `bt@stdlib@{snake}`
2. Update user code compiler to emit `bt@{snake}` module names
3. Update `value_type_codegen.rs`: merge type lists, update `superclass_module_name()` to emit `bt@stdlib@` and `bt@` prefixes
4. Update Core Erlang codegen to emit `bt@` prefixed module names
5. Run `just build-stdlib` to regenerate BEAM files and `.app.src` with new names
6. Update `beamtalk_primitive.erl` module atoms in `send/3`, `has_method/1`, and `class_name_to_module/1` to use `@` separator
7. **Verify tooling compatibility**: Test with rebar3 shell, observer, dialyzer, cover to confirm `@` atoms work correctly
8. Run `just ci` — fix snapshot tests as needed
9. Clean dialyzer PLT and rebuild: `cd runtime && rebar3 clean && rebar3 dialyzer`
10. **Documentation sweep**: Update all examples, tutorials, and docs to use new module names

**Estimated size: L** (mechanical but wider scope than stdlib-only, ~10+ files, ~120+ lines changed, affects user code compilation, requires documentation updates)

## Migration Path

**Breaking change level:** Medium — affects module naming for all user code, but no source code changes required.

### For Beamtalk User Code

**What breaks:**
- Existing compiled `.beam` files will have wrong module names (`counter.beam` instead of `bt@counter.beam`)
- Any Erlang FFI code calling user modules directly will break (rare - users typically don't hand-write Erlang dispatch)

**Migration steps:**
1. Recompile all `.bt` files with the updated compiler
2. Clean all `ebin/` directories: `rm -rf ebin/*.beam`
3. Rebuild: `beamtalk build .`

**Source code:** No changes needed — `.bt` source files are unaffected

### For Runtime/Stdlib Developers

**What breaks:**
- All existing `bt_stdlib_*.beam` files
- Codegen snapshot tests
- Any hardcoded module names in tests

**Migration steps:**
1. Run `just clean` to remove all old BEAM files
2. Run `just build-stdlib` to regenerate with new names
3. Run `just ci` to identify and fix snapshot tests
4. Update `runtime/apps/beamtalk_runtime/src/beamtalk_primitive.erl` dispatch clauses
5. **Test tooling**: Verify observer, rebar3 shell, dialyzer work correctly with `@` atoms
6. Rebuild dialyzer PLT: `cd runtime && rebar3 clean && rebar3 dialyzer`
7. **Update documentation**: Sweep all docs, examples, tutorials for old module names

### Backwards Compatibility

**None.** This is a breaking change that requires recompiling all Beamtalk code. Acceptable because:
- Beamtalk is pre-1.0 (breaking changes expected)
- No released packages exist yet
- Migration is mechanical (just recompile)
- No source code changes required

### Rollout Strategy

**Single atomic commit.** All changes can be made together since this is internal module naming. No gradual migration needed.

## Future Considerations

### Hot Code Reload
Erlang's hot code reload mechanism uses the *module atom* as the identity for code replacement. Since `@` is legal in unquoted atoms, `bt@stdlib@integer` is a plain atom that works without quoting. However, `bt@stdlib@integer` and `bt_stdlib_integer` are *different atoms* — this ADR deliberately changes the module atom, so the VM treats them as two distinct modules. Code must be recompiled and loaded under the new name; hot reload will not transparently map old modules to the new naming scheme.

### Class-Side Methods (ADR 0013)
When class-side methods are implemented, they will use the same module names as instance methods. Class-side dispatch will route through the same `bt@stdlib@integer` module, using different function names or metadata to distinguish class-side from instance-side methods. No module naming conflicts expected.

### Namespace Versioning
If breaking stdlib changes require multiple versions to coexist (e.g., for gradual migration), we could use:
- `bt@stdlib@v1@integer` and `bt@stdlib@v2@integer`, or
- `bt@stdlib_v1@integer` and `bt@stdlib_v2@integer`

This is speculative—not a current requirement. The `@` separator allows flexible extension if needed.

### Package Ecosystem Examples
When third-party packages emerge, the pattern extends naturally:
- JSON library: `bt@json@parser`, `bt@json@encoder`
- Web framework: `bt@web@router`, `bt@web@handler`
- Database adapter: `bt@postgres@connection`, `bt@postgres@query`

Package collision prevention: middle namespace segment is the package name, providing clear ownership (same as Gleam's `gleam@json`, `gleam@http` pattern).

## References
- Related ADRs: [ADR 0007](0007-compilable-stdlib-with-primitive-injection.md) — introduced the split naming convention and compilable stdlib
- Related ADRs: [ADR 0009](0009-otp-application-structure.md) — OTP app structure, dependency direction `workspace → runtime → stdlib`
- Related ADRs: [ADR 0006](0006-unified-method-dispatch.md) — unified dispatch, flattened method tables
- Documentation: `docs/development/architecture-principles.md` — layered architecture
- Discovered during: BT-411 (class methods and initialize protocol)
