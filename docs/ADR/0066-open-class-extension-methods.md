# ADR 0066: Open Class Extension Methods (`>>` Syntax)

## Status
Accepted (2026-03-18)

## Context

Beamtalk supports adding methods to existing classes — including sealed primitives — using the `>>` standalone method definition syntax:

```beamtalk
Counter >> increment => self.value := self.value + 1
String >> shout => self asUppercase ++ "!"
Integer >> double => self * 2
Array class >> ofSize: n => Array new: n withAll: 0
```

This feature is **already implemented** across the pipeline: parser (`StandaloneMethodDefinition` AST node), semantic analysis, codegen (runtime registration via `beamtalk_extensions` ETS), REPL, and hot reload. The `>>` token also serves as a binary operator in expression context for method reflection (`Counter >> #increment` returns a `CompiledMethod`).

What was missing was:
1. An ADR documenting the design decisions
2. A file naming convention for extension method files
3. Clear policy on sealed class extensions and conflict resolution
4. **A compile-time analysis model for type checking and conflict detection**

### Current Implementation

**Parser:** `ClassName [class] >> selector => body` is parsed as a `StandaloneMethodDefinition` containing the target class name, an `is_class_method` flag, and the method definition. Standalone methods are stored in `Module.method_definitions`, separate from class bodies.

**Codegen:** Standalone methods are **not** compiled into the target class's static BEAM module. Instead, they are registered at load time via `beamtalk_extensions:register/4`, which stores them in an ETS table keyed by `{ClassName, Selector}`.

**Dispatch:** When a message is not found in the target class's local method table, the dispatcher checks `beamtalk_extensions:lookup/2` before walking the class hierarchy. If an extension is found, it is called directly. This integrates extensions into the normal dispatch chain at each level of the hierarchy walk — extensions on the current class are checked before moving to the superclass.

**Conflict resolution:** Last-writer-wins with warning logging and conflict history tracking.

### The Problem: Type System and Conflict Detection

The ETS-based runtime dispatch works correctly and is O(1) — there is no performance problem. However, the purely dynamic model has two gaps that must be addressed as the language matures:

1. **Type system blind spot:** Extension methods in ETS are invisible to the type checker. A typed call site like `x factorial` (where `x :: Integer`) cannot verify at compile time that `factorial` exists on Integer. This makes extensions second-class citizens in the gradual type system (ADR 0025).
2. **Runtime-only conflict detection:** Two packages defining `String >> json` is only detected at load time via a logged warning. In production deployments, log output during load may be silently discarded.

The solution is **compile-time static analysis** — a build step that collects extension declarations and feeds them to the conflict detector and type checker — without changing the runtime dispatch mechanism.

## Decision

### 1. Syntax

The `>>` syntax for standalone method definitions is confirmed as the standard way to add methods to existing classes:

```beamtalk
// Instance method
String >> reversed => self asArray reversed join: ""

// Class method
String class >> fromCharCodes: codes => codes collect: [:c | c asCharacter] join: ""

// Binary operator
Point >> + other => Point x: self x + other x y: self y + other y

// Keyword method
Array >> chunksOf: n =>
  result := Array new.
  self withIndex do: [:item :i |
    (i % n) = 0 ifTrue: [result add: Array new].
    result last add: item
  ].
  result
```

**REPL usage:**

```beamtalk
>> Integer >> double => self * 2
=> <StandaloneMethodDefinition>
>> 21 double
=> 42
>> (Integer >> #double) source
=> "double => self * 2"
```

**Error on misuse:**

```beamtalk
>> 42 >> increment => self + 1
Error: Expected class name before '>>' (got Integer literal)
```

### 2. File Naming Convention: `ClassName+Feature.bt`

Extension method files follow the **Swift-style** naming convention:

```text
stdlib/src/String+JSON.bt       // String >> json => ..., String >> fromJson: => ...
stdlib/src/Array+Sorting.bt     // Array >> sortBy: => ..., Array >> sorted => ...
myapp/src/Integer+Roman.bt      // Integer >> asRoman => ...
myapp/src/Actor+Logging.bt      // Actor >> logInfo: => ..., Actor >> logError: => ...
```

**Rules:**
- Extension files MUST be named `TargetClass+Feature.bt`
- The `+Feature` suffix describes the capability being added (e.g., `+JSON`, `+Sorting`, `+Logging`)
- Feature names use PascalCase with no spaces, dots, or slashes (e.g., `+JSON`, `+Sorting`, not `+json` or `+sorting.utils`)
- One file may contain multiple `>>` definitions, all targeting the **same class** and related to the same feature
- A file MAY also contain helper classes used only by the extensions (e.g., a private `JsonParser` class inside `String+JSON.bt`)
- A regular class definition file (e.g., `Counter.bt`) MAY include `>>` extensions on other classes when there is tight coupling, but this should be the exception

**What goes where:**
- Methods that are core to the class → `ClassName.bt` (class definition)
- Methods that add a capability from another domain → `ClassName+Feature.bt` (extension file)
- Ad-hoc REPL extensions → no file needed (registered in-memory via ETS)

### 3. Runtime Dispatch: ETS with Hierarchy-Interleaved Lookup

Runtime dispatch uses the existing ETS-based extension registry. Extensions are checked at **each level** of the hierarchy walk, not deferred to a fallback position:

1. Local methods on the receiver's class
2. ETS extension lookup on the receiver's class
3. Inherited methods from superclass
4. ETS extension lookup on superclass
5. ... (continue up hierarchy to `Object`)
6. `doesNotUnderstand:` fallback

This interleaved ordering ensures that:
- Local methods always take priority over extensions
- An extension on `Integer` takes priority over an inherited method from `Number` or `Object`
- Extensions defined in the REPL immediately participate in dispatch at the correct priority level
- Hot reload of an extension file updates the ETS entry, and subsequent dispatches use the new function immediately

An extension cannot override a method defined in the class body — it can only add new selectors or override inherited methods.

### 4. Compile-Time Analysis: Conflict Detection and Type Metadata

A compile-time analysis pass runs during `just build` to provide static guarantees without changing the runtime:

1. The compiler scans all `StandaloneMethodDefinition` nodes across the project and its dependencies
2. Duplicate `{Class, Side, Selector}` registrations — from any source, same-package or cross-package — are **compile errors**. Instance-side and class-side methods are distinct: `String >> json` and `String class >> json` do not conflict (they target different BEAM modules — the class and its metaclass respectively)
3. Extension declarations are written to a **type metadata file** that the gradual type checker reads, making extensions part of a class's typed method surface

```text
error[E0451]: extension conflict on String>>json
  --> myapp/src/String+JSON.bt:3:1
   |
3  | String >> json => ...
   | ^^^^^^^^^^^^^^^^ defined here
   |
  --> myapp/src/String+Serialization.bt:7:1
   |
7  | String >> json => ...
   | ^^^^^^^^^^^^^^^^ also defined here
   |
   = help: rename one of the extensions to avoid the conflict
```

This is a hard error — the build fails. No last-writer-wins ambiguity in compiled projects.

**In the REPL:** Last-writer-wins with provenance tracking remains, as currently implemented. This is intentional — the REPL supports iteratively redefining extensions during development:

- A warning is logged via OTP logger: `Extension String>>json overwritten (was owned by string_json, now owned by my_json_lib)`
- The `beamtalk_extension_conflicts` ETS bag table records every overwrite with `{Class, Selector, Owner, Timestamp}`
- `beamtalk_extensions:conflicts/0` returns all selectors that have been registered by multiple owners

**Why compile-time analysis, not runtime consolidation:**

An earlier revision of this ADR proposed an Elixir-style "protocol consolidation" model that would generate a static dispatch index at build time, replacing ETS for compiled code. This was rejected because:

1. **ETS lookup is already O(1)** — there is no runtime performance problem to solve. Elixir protocol consolidation addresses module enumeration overhead; Beamtalk's ETS-keyed lookup has no equivalent bottleneck.
2. **A consolidated dispatch index introduces a dispatch ordering bug** — consolidated extensions checked before ETS fallback would make REPL-defined extensions unreachable when they conflict with inherited methods, breaking the interactive development model.
3. **Hot code reload becomes complex** — a static index cannot be updated without rebuilding, creating a split between dev and production behavior.
4. **The real goals (type checking, conflict detection) only require compile-time analysis of declarations**, not a new runtime dispatch mechanism. Keeping ETS dispatch unchanged avoids all of these problems.

### 5. Sealed Class Policy

**Sealed classes CAN be extended.** "Sealed" means a class cannot be *subclassed*, not that it cannot receive new methods. Extensions are the **primary** mechanism for adding behaviour to sealed primitives like `Integer`, `String`, `Float`, `Boolean`, and `Array`.

```beamtalk
// This is valid — Integer is sealed but extensible
Integer >> factorial =>
  self <= 1
    ifTrue: [1]
    ifFalse: [self * (self - 1) factorial]
```

This follows Pharo's model where extension methods are regular methods in the class's logical method dictionary.

### 6. Load Order

Extensions are registered when their containing module's `on_load` callback fires (the `register_class/0` function generated by codegen). In compiled `.bt` files, this happens when the BEAM module is loaded by the VM. In the REPL, this happens immediately on evaluation. During hot reload, re-loading a file re-registers its extensions, overwriting previous registrations from the same owner.

Load order within a package follows the order specified in `beamtalk.toml`. Cross-package load order follows dependency declarations. Within these constraints, extension registration order is deterministic. The compile-time conflict detector catches duplicates regardless of load order.

### 7. ETS Table Lifecycle

The `beamtalk_extensions` and `beamtalk_extension_conflicts` ETS tables are created by `beamtalk_extensions:init/0`, called during the `beamtalk_runtime` OTP application startup. Both tables are `public` and `named_table` with `read_concurrency` enabled. They are owned by the runtime supervisor process and survive individual process crashes. If the runtime application itself restarts, extensions are re-registered as modules are re-loaded by the VM.

Extensions are node-local — in a distributed BEAM cluster, each node maintains its own extension registry. This matches the standard BEAM model where code loading is per-node.

## Prior Art

| Language | Mechanism | File Convention | Conflict Resolution | Extend Sealed? |
|----------|-----------|-----------------|---------------------|----------------|
| **Pharo** | Open classes via `*Package` protocols | `Class.extension.st` in package dir | Last loaded wins | Yes (all classes) |
| **Newspeak** | None (by design) | N/A | N/A | No |
| **Ruby** | Open classes / refinements | `core_ext/class_name.rb` (Rails) | Last defined wins / lexical (refinements) | Yes |
| **Swift** | `extension Type { }` | **`Type+Feature.swift`** | Compile error (same module) | Yes |
| **Kotlin** | `fun Type.method()` | `TypeExtensions.kt` | Member wins; ambiguity = error | Yes |
| **C#** | `this Type` parameter | `TypeExtensions.cs` | Member wins; ambiguity = error | Yes |
| **Elixir** | Protocols (type-class-like) | Co-located with protocol | Compile error (duplicate impl) | Yes (all types) |

**Key influences:**
- **Pharo:** Semantic model — extensions are logically part of the class's method dictionary, not a separate mechanism. ETS-based dispatch mirrors this: extensions integrate into the hierarchy walk at each level.
- **Swift:** File naming convention — `Type+Feature.ext` is proven at massive ecosystem scale. Compile-time conflict detection as a hard error.
- **C#/Kotlin:** Compile-time visibility — the type checker must see extensions. Beamtalk achieves this via compile-time metadata extraction rather than static dispatch resolution.

**Rejected influences:**
- **Pharo's last-writer-wins in production:** Acceptable for an image-based system where the developer controls all loaded code. Unacceptable for a package ecosystem where transitive dependencies can silently conflict.
- **Elixir protocol consolidation as a runtime model:** Elixir consolidation solves module enumeration overhead; Beamtalk's ETS lookup is already O(1). The consolidation pattern introduces dispatch ordering and hot reload problems without solving a real performance bottleneck. Beamtalk adopts the *principle* (static analysis for production safety) but not the *mechanism* (replacing runtime dispatch).
- **Newspeak's "no extensions" stance:** Too restrictive for a language with sealed primitives — users need a way to add methods to `Integer` and `String`.
- **Ruby refinements (scoped extensions):** Scoping conflicts with the live development model — in a Smalltalk-style environment, all methods should be globally visible. If cross-package conflicts become a problem despite compile-time detection, scoped extensions could be revisited as an opt-in mechanism.

## User Impact

**Newcomer (from Python/JS/Ruby):**
- `>>` syntax is unfamiliar but learnable — reads as "ClassName gets method"
- `ClassName+Feature.bt` file naming is self-documenting
- Can discover extensions via `Integer methods` or `Integer >> #double` in the REPL
- Ruby developers will recognize open-class semantics immediately
- Conflicts are caught at build time with clear error messages, not silent runtime surprises

**Smalltalk developer:**
- Pharo-compatible semantics — extensions are first-class methods
- `>>` is more explicit than Pharo's protocol-based approach (no hidden `*Package` naming)
- File naming departs from Tonel's `.extension.st` but is more expressive
- Build-time conflict detection is stricter than Pharo — a trade-off for package ecosystem safety

**Erlang/BEAM developer:**
- ETS-backed dispatch is a familiar, battle-tested BEAM pattern
- Compile-time analysis adds safety without changing runtime behaviour
- Load-order semantics follow standard OTP module loading

**Production operator:**
- Conflicts are caught at build time — impossible to deploy conflicting extensions
- Extensions are hot-reloadable — re-evaluating a file updates ETS immediately
- `beamtalk_extensions:conflicts/0` provides runtime visibility into REPL overwrites
- Extensions are node-local, consistent with BEAM code loading semantics

**Tooling developer:**
- `StandaloneMethodDefinition` AST node provides clean structure for LSP
- Compile-time type metadata gives the type checker full visibility into extension methods
- File naming convention enables glob-based tooling (`*+*.bt` finds all extension files)
- LSP must maintain a secondary index of `(Class, Selector) → file location` for go-to-definition on extension methods
- API documentation generation must aggregate across `ClassName+*.bt` files for the full method surface

## Steelman Analysis

### Option B: `ClassName.extension.bt` (Pharo Tonel-style)
- **Smalltalk purist**: "This is exactly how Pharo does it. One file per class is the simplest mental model — I always know where to look for extensions on String."
- **Newcomer**: "Fewer naming decisions — I don't have to think about what 'feature' to name the file."
- **Language designer**: "Minimal surface area. One convention is easier to enforce than a naming pattern."

### Option C: `ClassName_extensions.bt` (Kotlin/C#-style)
- **BEAM veteran**: "No special characters in filenames — plays safe with all filesystems and tools."
- **Newcomer**: "Underscores are familiar from Python and Elixir — no new convention to learn."

### ETS-only with no compile-time analysis
- **Smalltalk purist**: "This is exactly how Pharo works — fully dynamic, fully live. Pharo has thrived for decades without compile-time extension checking. Compile-time analysis adds ceremony to what should be a dynamic language."
- **BEAM veteran**: "ETS is battle-tested. Adding a compile-time pass is more moving parts to break. If conflicts are a real problem, add a linter — don't bake it into the compiler."
- **Language designer**: "The type system doesn't exist yet. Building compile-time infrastructure for a type checker that may be years away is premature — solve real problems now, not hypothetical future ones."

### Runtime consolidation (Elixir-style)
- **Language designer**: "A static dispatch index would be faster than ETS for compiled code — even if ETS is O(1), a case clause is zero overhead."
- **Production operator**: "I want compiled extensions to have no runtime infrastructure dependency at all — no ETS, no supervisor, just BEAM modules."

### Tension Points
- Smalltalk purists prefer fully dynamic extensions (matching Pharo), but runtime-only conflict detection is insufficient for a package ecosystem
- The runtime consolidation model offers theoretical performance benefits but introduces dispatch ordering bugs and hot reload complexity that outweigh the gains
- BEAM veterans see ETS as natural and prefer not adding compile-time machinery, but acknowledge the type system needs static visibility into extensions

## Alternatives Considered

### `ClassName.extension.bt` (Pharo Tonel-style)
One extension file per target class (e.g., `String.extension.bt`). Rejected because it doesn't scale — a popular class like String would accumulate dozens of unrelated extensions in a single file, making the file hard to navigate and causing merge conflicts when multiple developers add extensions to the same class.

### `ClassName_extensions.bt` (Kotlin/C#-style)
Same single-file-per-class problem as Tonel, plus the underscore conflicts with Beamtalk's established PascalCase file naming convention (`String.bt`, `Array.bt`, not `string.bt` or `array_extensions.bt`).

### Mixed convention (A + B)
Allow both `+Feature` and `.extension` patterns. Rejected because having two conventions leads to inconsistency — developers would need to decide which to use, and different packages would make different choices, fragmenting the ecosystem.

### No convention (freeform)
Let developers name extension files however they want. Rejected because consistent naming enables tooling (glob patterns, IDE file browsers) and makes extensions discoverable across packages.

### ETS-only with no compile-time analysis
Keep extensions purely dynamic with no build-time checks. Rejected because:
1. The type checker cannot see ETS-registered methods — extensions would be permanently untyped
2. Conflicts are detected only at runtime, which is too late for production deployments

### Runtime consolidation (Elixir-style protocol consolidation)
Generate a static dispatch index at build time, replacing ETS for compiled code. Rejected because:
1. ETS lookup is already O(1) — there is no runtime performance problem to solve
2. A consolidated dispatch index creates a dispatch ordering bug: REPL-defined extensions placed after the full hierarchy walk become unreachable when they conflict with inherited methods
3. Hot code reload of extensions requires rebuilding the consolidated index, breaking the live development model
4. The "consolidated extension index" BEAM artifact is undefined — no clear implementation path on BEAM without modifying target class modules
5. Compile-time analysis achieves the same goals (type checking, conflict detection) without changing runtime dispatch

### Protocol/typeclass approach (Elixir-style)
Instead of open classes, use protocols — `Serializable` protocol with per-type implementations. This avoids global mutation and last-writer-wins conflicts entirely, and is statically analyzable. Rejected as a *replacement* for open classes, but acknowledged as **complementary** — many practical uses of `>>` (JSON serialization, formatting, logging) are protocol-shaped and would be better expressed as protocols once they exist.

The two mechanisms solve different axes of the expression problem:
- **Open classes (`>>`):** Add new operations to a single type — `Integer >> factorial`. Natural as a message send (`5 factorial`), awkward as a protocol (a `Factorial` protocol with one implementor is ceremony for no benefit).
- **Protocols:** Add a single operation across many types — `Serializable` implemented for Integer, String, Array, etc. Statically verifiable, no conflict risk, but requires invocation syntax that may not feel like a regular message send.

Beamtalk will add protocols (required for the gradual type system, ADR 0025). When that happens, the guidance should be: use protocols for cross-type operations, use `>>` for type-specific additions. Some existing `>>` extensions may migrate to protocol implementations — this is expected and healthy.

## Consequences

### Positive
- Extensions provide the only way to add methods to sealed primitives, completing the object model
- `ClassName+Feature.bt` naming is self-documenting and scales to large codebases
- Compile-time analysis gives the type checker visibility into extensions without changing the runtime
- Conflicts are compile errors in built projects, not silent runtime surprises
- Single dispatch mechanism (ETS) for all modes — no behavioral differences between dev and production
- REPL retains full liveness — define `>>` interactively, use immediately, hot-reload freely
- Dispatch integration means extensions participate in `respondsTo:`, `methods`, and reflection

### Negative
- REPL-defined extensions remain untyped — the type checker only covers extensions from compiled `.bt` files
- The `+` in filenames is unfamiliar to non-Swift developers — mitigated by clear documentation
- LSP must maintain a secondary index for extension method locations, separate from class body index
- API documentation generation must aggregate across `ClassName+*.bt` files for the full method surface
- ETS dispatch has minor overhead vs. local methods (one ETS read per hierarchy level on the slow path) — acceptable given O(1) lookup

### Neutral
- Extensions cannot add instance variables (state) to existing classes — this is by design, matching Swift/Kotlin/C# semantics. Extension methods that depend on target class state must use the class's public API.
- The `>>` token is overloaded (definition syntax AND method reflection operator) — context disambiguates cleanly
- Extensions are node-local in distributed BEAM — each node has its own ETS registry, consistent with per-node code loading
- Test suites share ETS state within the test node — tests should not assume a clean extension registry unless explicitly cleared

## Implementation

### Phase 1a: Current (Implemented)

ETS-based extension registration and dispatch.

| Component | File | Status |
|-----------|------|--------|
| Parser | `crates/beamtalk-core/src/source_analysis/parser/declarations.rs` | Implemented |
| AST | `crates/beamtalk-core/src/ast.rs` (`StandaloneMethodDefinition`) | Implemented |
| Semantic analysis | `crates/beamtalk-core/src/source_analysis/semantic_analysis/` | Implemented |
| Codegen | `crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs` | Implemented |
| Dispatch (ETS) | `crates/beamtalk-core/src/codegen/core_erlang/gen_server/dispatch.rs` | Implemented |
| Runtime registry | `runtime/apps/beamtalk_runtime/src/beamtalk_extensions.erl` | Implemented |
| REPL | Inline `>>` definitions and `:load` of extension files | Implemented |
| Hot reload | Extension re-registration on file reload | Implemented |

**Note:** REPL `>>` definitions work correctly for state mutations because the REPL recompiles the entire class (concatenating existing source with the new method), making the extension a local method with full state threading. File-loaded extensions go through the ETS dispatch path, which has the state threading bug described in Phase 1b.

### Phase 1b: Fix Extension State Threading (Bug)

The ETS dispatch path does not thread state for extension methods. The current generated code:

```erlang
let ExtResult = apply ExtFun(Args, Self) in
{'reply', ExtResult, State}          %% ← returns OLD State, mutations discarded
```

An extension like `Counter >> debugIncrement => self.value := self.value + 1` silently discards the state mutation — the method appears to run but the actor's state is unchanged.

**Fix:** Change the extension closure signature and dispatch to match regular methods:

```erlang
apply ExtFun(Args, Self, State)      %% ← extension receives State
                                     %% ← returns {'reply', Result, NewState} directly
```

| Component | Description | Status |
|-----------|-------------|--------|
| Extension closure codegen | Change signature from `fun(Args, Self) -> Result` to `fun(Args, Self, State) -> {'reply', Result, NewState}` | Not started |
| Dispatch unwrapping | Remove `{'reply', ExtResult, State}` wrapper; use extension's return directly | Not started |
| Extension registry | Update `beamtalk_extensions:register/4` to store new-signature closures | Not started |
| Tests | Add test: extension on actor class with state mutation via ETS path | Not started |

### Phase 2: Compile-Time Analysis

Add a build-time pass that collects extension declarations for conflict detection and type metadata.

| Component | Description | Status |
|-----------|-------------|--------|
| Extension collector | Scan all `StandaloneMethodDefinition` across project + dependencies | Not started |
| Conflict detector | Report duplicate `{Class, Side, Selector}` as compile errors | Not started |
| Type metadata emitter | Write extension declarations to metadata for the type checker | Not started |

### Phase 3: Type System Integration

Make extensions visible to the gradual type checker via the metadata from Phase 2.

| Component | Description | Status |
|-----------|-------------|--------|
| Type checker reads extension metadata | Extensions contribute to a class's typed method surface | Done (BT-1518) |
| Extension type annotations | `Integer >> factorial :: -> Integer => ...` | Done (BT-1519) |

### Documentation (BT-1473)
1. Add `>>` syntax section to `docs/beamtalk-language-features.md`
2. Test and document sealed class extension behaviour explicitly
3. Enforce `ClassName+Feature.bt` naming convention in documentation and examples

## Known Limitations

**Flat namespace assumption:** The ETS key `{ClassName, Selector}` uses bare atoms, assuming a flat class namespace (ADR 0031). If Beamtalk later introduces namespaced classes, the registry key format will need migration.

**REPL extensions are untyped:** Extensions defined interactively in the REPL bypass compile-time analysis and are invisible to the type checker. This is by design — the REPL is the "dynamic zone" of gradual typing — but means REPL-defined extensions won't get type error reporting until they are moved to a `+Feature.bt` file.

**ETS is global mutable state:** All extensions share a single ETS table per node. Test isolation requires explicit cleanup between test suites that register extensions. This is inherent to the Pharo-style open class model.

## Implementation Tracking

**Epic:** [BT-1513](https://linear.app/beamtalk/issue/BT-1513)

| Phase | Issue | Title | Size | Status |
|-------|-------|-------|------|--------|
| 1b | [BT-1512](https://linear.app/beamtalk/issue/BT-1512) | Fix extension state threading — ETS dispatch discards mutations | S | Backlog |
| 2 | [BT-1473](https://linear.app/beamtalk/issue/BT-1473) | Document `>>` syntax in language features docs | M | In Review |
| 2 | [BT-1514](https://linear.app/beamtalk/issue/BT-1514) | Test sealed class extensions via ETS dispatch path | S | Backlog |
| 3 | [BT-1515](https://linear.app/beamtalk/issue/BT-1515) | Extension collector — scan declarations across project | M | Backlog |
| 3 | [BT-1516](https://linear.app/beamtalk/issue/BT-1516) | Compile-time conflict detection for duplicate extensions | S | Backlog |
| 3 | [BT-1517](https://linear.app/beamtalk/issue/BT-1517) | Emit extension method type metadata for the type checker | M | Backlog |
| 4 | [BT-1518](https://linear.app/beamtalk/issue/BT-1518) | Type checker reads extension metadata — typed method surface | M | Backlog |
| 4 | [BT-1519](https://linear.app/beamtalk/issue/BT-1519) | Extension type annotation syntax — return types on `>>` | M | Backlog |

## References
- Related issues: BT-1473, [BT-1512](https://linear.app/beamtalk/issue/BT-1512) (extension state threading bug)
- Related ADRs: [ADR 0005](0005-beam-object-model-pragmatic-hybrid.md) (object model, extension registry design), [ADR 0006](0006-unified-method-dispatch.md) (dispatch chain), [ADR 0025](0025-gradual-typing-and-protocols.md) (typing interaction, protocol complement), [ADR 0031](0031-flat-namespace-for-v01.md) (flat namespace assumption), [ADR 0032](0032-early-class-protocol.md) (flattened table removal)
- Implementation: `beamtalk_extensions.erl`, `StandaloneMethodDefinition` in `ast.rs`
- Prior art: Pharo Tonel format, Swift extension file conventions
