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

This feature is **already partially implemented** across the pipeline: parser (`StandaloneMethodDefinition` AST node), semantic analysis, codegen, REPL, and hot reload. The `>>` token also serves as a binary operator in expression context for method reflection (`Counter >> #increment` returns a `CompiledMethod`).

What was missing was:
1. An ADR documenting the design decisions
2. A file naming convention for extension method files
3. Clear policy on sealed class extensions and conflict resolution
4. **An architectural model for how extensions interact with the type system and cross-package builds**

### Current Implementation (ETS-only, being superseded)

The current implementation uses a purely dynamic, ETS-based model:

**Parser:** `ClassName [class] >> selector => body` is parsed as a `StandaloneMethodDefinition` containing the target class name, an `is_class_method` flag, and the method definition. Standalone methods are stored in `Module.method_definitions`, separate from class bodies.

**Codegen:** Standalone methods are **not** compiled into the target class's static BEAM module. Instead, they are registered at load time via `beamtalk_extensions:register/4`, which stores them in an ETS table keyed by `{ClassName, Selector}`.

**Dispatch:** When a message is not found in the target class's local method table, the dispatcher checks `beamtalk_extensions:lookup/2` before walking the class hierarchy.

**Conflict resolution:** Last-writer-wins with warning logging.

### The Problem with ETS-Only

The purely dynamic model has fundamental limitations:

1. **Type system blind spot:** Extension methods in ETS are invisible to the type checker. A typed call site cannot verify at compile time that an extension method exists. This makes extensions second-class citizens in the gradual type system (ADR 0025).
2. **Runtime-only conflict detection:** Two packages defining `String >> json` is only detected at load time via a logged warning. In production deployments, log output during load may be silently discarded.
3. **Cross-package opacity:** Package A cannot know at build time what extensions Package B adds. The full method surface of a class is unknowable until all packages are loaded.
4. **No separate compilation story:** The target class is never modified — extensions live entirely in global mutable ETS state, which doesn't compose well with BEAM's module-based code loading.

These are the same problems that led Elixir to introduce **protocol consolidation** — and the same solution applies to Beamtalk extensions.

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

```
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

### 3. Compilation Model: Consolidation at Build Time, ETS for REPL

Extensions operate in **two modes**, following the Elixir protocol consolidation model:

**Consolidated mode (production builds via `just build`):**

1. Each `+Feature.bt` file compiles to its own standalone BEAM module containing the extension functions
2. At application build time, the compiler collects ALL `StandaloneMethodDefinition` nodes across all packages in the dependency graph
3. It generates a **consolidated extension index** per target class — a static lookup table separate from the class's own BEAM module
4. Duplicate `{Class, Selector}` registrations across packages are **compile errors**, not runtime warnings
5. The type checker reads the consolidated index, making extensions visible to static analysis

```
Package build:      String+JSON.bt  → bt@string_json.beam (standalone module)
                    Array+Sorting.bt → bt@array_sorting.beam (standalone module)
Application build:  Consolidate all extensions → static extension dispatch index
                    Detect conflicts across all packages → compile error
                    Type checker reads consolidated index → extensions are typed
```

**Unconsolidated mode (REPL and dev mode):**

Extensions are registered dynamically in the `beamtalk_extensions` ETS table, exactly as today. This preserves the interactive-first development experience — you can define `Integer >> double` in the REPL and use it immediately. Extensions defined this way are unchecked by the type system.

```
REPL:  Integer >> double => self * 2  → ETS registration (dynamic, unchecked)
```

**The key principle: extensions are static in production, dynamic in the REPL.** Same syntax, same semantics, different compilation strategy. This mirrors how Elixir handles protocol consolidation — `mix compile` consolidates for production, skips consolidation in dev for faster recompilation.

**Dispatch in consolidated mode:**

1. Local methods (defined in the class body)
2. Consolidated extensions on that class (static lookup from the extension index)
3. Inherited methods from superclass
4. Consolidated extensions on superclass
5. ... (continue up hierarchy to `Object`)
6. Unconsolidated ETS extensions (fallback for REPL-defined extensions)
7. `doesNotUnderstand:` fallback

**Dispatch in unconsolidated mode (REPL):**

1. Local methods (defined in the class body)
2. ETS extension lookup on that class
3. Inherited methods from superclass
4. ETS extension lookup on superclass
5. ... (continue up hierarchy to `Object`)
6. `doesNotUnderstand:` fallback

Local methods always take priority over extensions in both modes. An extension cannot override a method defined in the class body — it can only add new selectors or override inherited methods.

### 4. Sealed Class Policy

**Sealed classes CAN be extended.** "Sealed" means a class cannot be *subclassed*, not that it cannot receive new methods. Extensions are the **primary** mechanism for adding behaviour to sealed primitives like `Integer`, `String`, `Float`, `Boolean`, and `Array`.

```beamtalk
// This is valid — Integer is sealed but extensible
Integer >> factorial =>
  self <= 1
    ifTrue: [1]
    ifFalse: [self * (self - 1) factorial]
```

This follows Pharo's model where extension methods are regular methods in the class's logical method dictionary. In consolidated mode, extensions on sealed classes are part of the static extension index — no need to recompile the sealed class's own BEAM module.

### 5. Conflict Resolution

**Consolidated mode (build time):** Duplicate `{Class, Selector}` registrations from different packages are **compile errors**. The compiler reports which packages conflict and which files define the duplicate:

```
error[E0451]: extension conflict on String>>json
  --> myapp/src/String+JSON.bt:3:1
   |
3  | String >> json => ...
   | ^^^^^^^^^^^^^^^^ defined here
   |
  --> vendor/json_utils/src/String+Serialization.bt:7:1
   |
7  | String >> json => ...
   | ^^^^^^^^^^^^^^^^ also defined here
   |
   = help: rename one of the extensions or remove the conflicting dependency
```

This is a hard error — the build fails. No last-writer-wins ambiguity in production.

**Unconsolidated mode (REPL):** Last-writer-wins with provenance tracking, as currently implemented:

- A warning is logged via OTP logger: `Extension String>>json overwritten (was owned by string_json, now owned by my_json_lib)`
- The `beamtalk_extension_conflicts` ETS bag table records every overwrite with `{Class, Selector, Owner, Timestamp}`
- `beamtalk_extensions:conflicts/0` returns all selectors that have been registered by multiple owners

Last-writer-wins in the REPL is intentional — it supports the interactive workflow of iteratively redefining extensions during development.

### 6. Load Order

**Consolidated mode:** Extension dispatch order is determined at build time by the consolidated index. Load order is irrelevant — all extensions are known statically.

**Unconsolidated mode:** Extensions are registered when their containing module's `on_load` callback fires (the `register_class/0` function generated by codegen). In the REPL, this happens immediately on evaluation. During hot reload, re-loading a file re-registers its extensions, overwriting previous registrations from the same owner.

### 7. ETS Table Lifecycle

The `beamtalk_extensions` and `beamtalk_extension_conflicts` ETS tables are created by `beamtalk_extensions:init/0`, called during the `beamtalk_runtime` OTP application startup. Both tables are `public` and `named_table` with `read_concurrency` enabled. They are owned by the runtime supervisor process and survive individual process crashes.

In consolidated mode, ETS serves only as a fallback for REPL-defined extensions that were added after build time. The consolidated extension index is the primary dispatch path.

Extensions are node-local — in a distributed BEAM cluster, each node maintains its own extension registry and consolidated index. This matches the standard BEAM model where code loading is per-node.

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
- **Pharo:** Semantic model — extensions are logically part of the class's method dictionary, not a separate mechanism.
- **Swift:** File naming convention — `Type+Feature.ext` is proven at massive ecosystem scale. Compile-time conflict detection.
- **Elixir protocol consolidation:** The two-mode compilation model — consolidated for production, unconsolidated for dev/REPL. This is the direct inspiration for Beamtalk's extension consolidation.
- **C#:** Extensions are statically resolved at the call site — informed the principle that the type checker must see extensions at compile time.

**Rejected influences:**
- **Pharo's last-writer-wins in production:** Acceptable for an image-based system where the developer controls all loaded code. Unacceptable for a package ecosystem where transitive dependencies can silently conflict.
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
- Consolidated extensions generate standard BEAM code — debuggable with `:observer`, `:recon`, `:dbg`
- Consolidation model is familiar from Elixir protocol consolidation
- ETS fallback for REPL is a familiar BEAM pattern

**Production operator:**
- No ETS overhead for compiled extensions in consolidated mode
- Conflicts are impossible in production — they're caught at build time
- Extensions are hot-reloadable in unconsolidated mode (REPL/dev)
- Extensions are node-local, consistent with BEAM code loading semantics

**Tooling developer:**
- `StandaloneMethodDefinition` AST node provides clean structure for LSP
- Consolidated extension index gives the type checker full visibility into extension methods
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

### ETS-only (no consolidation)
- **Smalltalk purist**: "This is exactly how Pharo works — fully dynamic, fully live. Build-time consolidation adds C#-style ceremony to what should be a dynamic language."
- **BEAM veteran**: "ETS is battle-tested. Adding a consolidation step is complexity that could have bugs. KISS."
- **Language designer**: "Last-writer-wins is simpler to reason about than a two-mode system. The REPL and production should behave identically."

### Tension Points
- Smalltalk purists prefer fully dynamic extensions (matching Pharo), but this sacrifices type checker visibility and compile-time conflict detection — costs that compound as the package ecosystem grows
- BEAM veterans see ETS as natural, but Elixir developers who've used protocol consolidation understand the value of static resolution for production
- The two-mode model (consolidated vs. unconsolidated) adds complexity, but the same pattern is proven in Elixir and accepted by the BEAM community

## Alternatives Considered

### `ClassName.extension.bt` (Pharo Tonel-style)
One extension file per target class (e.g., `String.extension.bt`). Rejected because it doesn't scale — a popular class like String would accumulate dozens of unrelated extensions in a single file, making the file hard to navigate and causing merge conflicts when multiple developers add extensions to the same class.

### `ClassName_extensions.bt` (Kotlin/C#-style)
Same single-file-per-class problem as Tonel, plus the underscore conflicts with Beamtalk's established PascalCase file naming convention (`String.bt`, `Array.bt`, not `string.bt` or `array_extensions.bt`).

### Mixed convention (A + B)
Allow both `+Feature` and `.extension` patterns. Rejected because having two conventions leads to inconsistency — developers would need to decide which to use, and different packages would make different choices, fragmenting the ecosystem.

### No convention (freeform)
Let developers name extension files however they want. Rejected because consistent naming enables tooling (glob patterns, IDE file browsers) and makes extensions discoverable across packages.

### ETS-only (no consolidation)
Keep extensions purely dynamic with ETS dispatch for all modes. Rejected because:
1. The type checker cannot see ETS-registered methods — extensions would be permanently untyped
2. Conflicts are detected only at runtime, which is too late for production deployments
3. The Elixir ecosystem proved that protocol consolidation is the right model for BEAM — it gives production safety without sacrificing dev-time liveness

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
- Build-time consolidation gives the type checker full visibility into extensions — they are first-class in the type system
- Conflicts are compile errors in production, not silent runtime surprises
- Zero ETS overhead for compiled extensions in consolidated mode
- REPL retains full liveness — define `>>` interactively, use immediately
- Dispatch integration means extensions participate in `respondsTo:`, `methods`, and reflection

### Negative
- Two-mode compilation (consolidated vs. unconsolidated) adds complexity to the build pipeline
- REPL-defined extensions remain untyped — the type checker only covers consolidated extensions
- The `+` in filenames is unfamiliar to non-Swift developers — mitigated by clear documentation
- LSP must maintain a secondary index for extension method locations, separate from class body index
- API documentation generation must aggregate across `ClassName+*.bt` files for the full method surface

### Neutral
- Extensions cannot add instance variables (state) to existing classes — this is by design, matching Swift/Kotlin/C# semantics. Extension methods that depend on target class state must use the class's public API.
- The `>>` token is overloaded (definition syntax AND method reflection operator) — context disambiguates cleanly
- Extensions are node-local in distributed BEAM — each node has its own consolidated index and ETS fallback, consistent with per-node code loading
- Test suites that register REPL-style extensions share ETS state within the test node — tests should not assume a clean extension registry unless explicitly cleared

## Implementation

### Phase 1: Current (Implemented)

ETS-only extension registration and dispatch — the existing implementation.

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

### Phase 2: Build-Time Consolidation

Collect extensions across the project and generate a static extension index.

| Component | Description | Status |
|-----------|-------------|--------|
| Extension collector | Scan all `StandaloneMethodDefinition` across project + dependencies | Not started |
| Conflict detector | Report duplicate `{Class, Selector}` as compile errors | Not started |
| Extension index codegen | Generate consolidated dispatch index per target class | Not started |
| Dispatch (consolidated) | Check consolidated index before ETS fallback | Not started |

### Phase 3: Type System Integration

Make consolidated extensions visible to the gradual type checker.

| Component | Description | Status |
|-----------|-------------|--------|
| Type checker reads extension index | Extensions contribute to a class's typed method surface | Not started |
| Extension type annotations | `Integer >> factorial :: -> Integer => ...` | Not started |

### Documentation (BT-1473)
1. Add `>>` syntax section to `docs/beamtalk-language-features.md`
2. Test and document sealed class extension behaviour explicitly
3. Enforce `ClassName+Feature.bt` naming convention in documentation and examples

## Known Limitations

**Flat namespace assumption:** The extension index keys on bare class name atoms, assuming a flat namespace (ADR 0031). If Beamtalk later introduces namespaced classes, the index key format will need migration.

**REPL extensions are untyped:** Extensions defined interactively in the REPL bypass consolidation and are invisible to the type checker. This is by design — the REPL is the "dynamic zone" of gradual typing — but means REPL-defined extensions won't get type error reporting until they are moved to a `+Feature.bt` file.

## References
- Related issues: BT-1473
- Related ADRs: [ADR 0005](0005-beam-object-model-pragmatic-hybrid.md) (object model, extension registry design), [ADR 0006](0006-unified-method-dispatch.md) (dispatch chain), [ADR 0025](0025-gradual-typing-and-protocols.md) (typing interaction, protocol complement), [ADR 0031](0031-flat-namespace-for-v01.md) (flat namespace assumption), [ADR 0032](0032-early-class-protocol.md) (flattened table removal)
- Implementation: `beamtalk_extensions.erl`, `StandaloneMethodDefinition` in `ast.rs`
- Prior art: Pharo Tonel format, Swift extension file conventions, Elixir protocol consolidation
