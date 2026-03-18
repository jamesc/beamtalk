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

This feature is **already fully implemented** across the entire pipeline: parser (`StandaloneMethodDefinition` AST node), semantic analysis, codegen (runtime-patched via `beamtalk_extensions` ETS registry), REPL, and hot reload. The `>>` token also serves as a binary operator in expression context for method reflection (`Counter >> #increment` returns a `CompiledMethod`).

What was missing was:
1. An ADR documenting the design decisions
2. A file naming convention for extension method files
3. Clear policy on sealed class extensions and conflict resolution

### Current Implementation

**Parser:** `ClassName [class] >> selector => body` is parsed as a `StandaloneMethodDefinition` containing the target class name, an `is_class_method` flag, and the method definition. Standalone methods are stored in `Module.method_definitions`, separate from class bodies.

**Codegen:** Standalone methods are **not** compiled into the target class's static BEAM module. Instead, they are registered at load time via `beamtalk_extensions:register/4`, which stores them in an ETS table keyed by `{ClassName, Selector}`.

**Dispatch:** When a message is not found in the target class's local method table, the dispatcher checks `beamtalk_extensions:lookup/2` before walking the class hierarchy. If an extension is found, it is called directly. This integrates extensions into the normal dispatch chain — they are not a separate mechanism.

**Conflict resolution:** Last-writer-wins. If two files register the same `{Class, Selector}` pair, the later registration overwrites the earlier one. A warning is logged when a different owner overwrites an existing extension. A separate `beamtalk_extension_conflicts` ETS table tracks conflict history for debugging.

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
- Ad-hoc REPL extensions → no file needed (registered in-memory)

### 3. Sealed Class Policy

**Sealed classes CAN be extended.** "Sealed" means a class cannot be *subclassed*, not that it cannot receive new methods. Extensions are the **primary** mechanism for adding behaviour to sealed primitives like `Integer`, `String`, `Float`, `Boolean`, and `Array`.

```beamtalk
// This is valid — Integer is sealed but extensible
Integer >> factorial =>
  self <= 1
    ifTrue: [1]
    ifFalse: [self * (self - 1) factorial]
```

This follows Pharo's model where extension methods are regular methods in the class's logical method dictionary.

### 4. Dispatch Priority

Extension methods integrate into the standard hierarchy walk:

1. Local methods (defined on the class in its `.bt` file)
2. Extension methods on that class (via `beamtalk_extensions` ETS lookup)
3. Inherited methods from superclass
4. Extension methods on superclass
5. ... (continue up hierarchy to `Object`)
6. `doesNotUnderstand:` fallback

Local methods always take priority over extensions. An extension cannot override a method defined in the class body — it can only add new selectors or override inherited methods.

### 5. Conflict Resolution

**Last-writer-wins** with provenance tracking:

- When two extensions register the same `{Class, Selector}`, the later registration wins
- A warning is logged via OTP logger: `Extension String>>json overwritten (was owned by string_json, now owned by my_json_lib)`
- The `beamtalk_extension_conflicts` ETS bag table records every overwrite with `{Class, Selector, Owner, Timestamp}` for debugging
- `beamtalk_extensions:conflicts/0` returns all selectors that have been registered by multiple owners

**Provenance stores:** the owning module atom (derived from the BEAM module name of the file that defined the extension). This is queryable at runtime:
```beamtalk
>> beamtalk_extensions list: #String
=> #((#json, #string_json_lib), (#shout, #my_utils))
```

**What to do when a conflict is detected:**
1. Check `beamtalk_extensions:conflicts/0` to identify which packages collide
2. Rename the conflicting selector to be more specific (e.g., `json` → `asJsonString`)
3. Or control load order explicitly via `beamtalk.toml` dependency declarations

**Limitations:** Last-writer-wins is appropriate for development and single-package deployments. For multi-package production systems, static conflict detection at build time (scanning all packages for duplicate `{Class, Selector}` pairs) would be preferable — this is deferred to future work.

This matches Pharo's behaviour. The community convention to avoid conflicts is to use descriptive method names specific to the feature being added, rather than generic names.

### 6. Load Order

Extensions are registered when their containing module's `on_load` callback fires (the `register_class/0` function generated by codegen). In compiled `.bt` files, this happens when the BEAM module is loaded by the VM. In the REPL, this happens immediately on evaluation. During hot reload, re-loading a file re-registers its extensions, overwriting previous registrations from the same owner.

Load order within a package follows the order specified in `beamtalk.toml`. Cross-package load order follows dependency declarations. Within these constraints, extension registration order is deterministic.

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
- **Pharo:** Semantic model — extensions are logically part of the class's method dictionary, not a separate mechanism. Last-writer-wins conflict resolution with community conventions to avoid collisions.
- **Swift:** File naming convention — `Type+Feature.ext` is proven at massive ecosystem scale and is self-documenting.
- **Ruby refinements:** Informed the decision NOT to add scoping — Beamtalk extensions are global like Pharo, not lexically scoped. Scoped extensions add complexity without clear benefit in a Smalltalk-style live environment.

**Rejected influences:**
- **Newspeak's "no extensions" stance:** Too restrictive for a language with sealed primitives — users need a way to add methods to `Integer` and `String`.
- **C#/Kotlin compile-time resolution:** Beamtalk's live development model requires runtime dispatch, not static resolution.

## User Impact

**Newcomer (from Python/JS/Ruby):**
- `>>` syntax is unfamiliar but learnable — reads as "ClassName gets method"
- `ClassName+Feature.bt` file naming is self-documenting
- Can discover extensions via `Integer methods` or `Integer >> #double` in the REPL
- Ruby developers will recognize open-class semantics immediately

**Smalltalk developer:**
- Pharo-compatible semantics — extensions are first-class methods
- `>>` is more explicit than Pharo's protocol-based approach (no hidden `*Package` naming)
- File naming departs from Tonel's `.extension.st` but is more expressive

**Erlang/BEAM developer:**
- Extensions generate standard BEAM code — debuggable with `:observer`, `:recon`, `:dbg`
- ETS-backed registry is a familiar BEAM pattern
- Load-order semantics follow standard OTP module loading

**Production operator:**
- `beamtalk_extensions:conflicts/0` provides visibility into overwrites
- Extensions are hot-reloadable — re-evaluating a file updates the registered function
- No performance impact on local method dispatch; extension lookup adds one ETS read on the slow path

**Tooling developer:**
- `StandaloneMethodDefinition` AST node provides clean structure for LSP
- File naming convention enables glob-based tooling (`*+*.bt` finds all extension files)
- Extension registry is queryable: `beamtalk_extensions:list/1` returns all extensions on a class

## Steelman Analysis

### Option B: `ClassName.extension.bt` (Pharo Tonel-style)
- **Smalltalk purist**: "This is exactly how Pharo does it. One file per class is the simplest mental model — I always know where to look for extensions on String."
- **Newcomer**: "Fewer naming decisions — I don't have to think about what 'feature' to name the file."
- **Language designer**: "Minimal surface area. One convention is easier to enforce than a naming pattern."

### Option C: `ClassName_extensions.bt` (Kotlin/C#-style)
- **BEAM veteran**: "No special characters in filenames — plays safe with all filesystems and tools."
- **Newcomer**: "Underscores are familiar from Python and Elixir — no new convention to learn."

### Tension Points
- Smalltalk purists prefer the Tonel convention (B) for familiarity, but acknowledge that single files per class become unwieldy for popular types like String
- The `+` character in filenames (Option A) is unusual outside Swift, but is legal on all major filesystems and is visually distinctive
- Language designers are split between A (more expressive) and B (simpler) — A wins because it scales better and avoids the "God extension file" problem

## Alternatives Considered

### `ClassName.extension.bt` (Pharo Tonel-style)
One extension file per target class (e.g., `String.extension.bt`). Rejected because it doesn't scale — a popular class like String would accumulate dozens of unrelated extensions in a single file, making the file hard to navigate and causing merge conflicts when multiple developers add extensions to the same class.

### `ClassName_extensions.bt` (Kotlin/C#-style)
Same single-file-per-class problem as Tonel, plus the underscore conflicts with Beamtalk's established PascalCase file naming convention (`String.bt`, `Array.bt`, not `string.bt` or `array_extensions.bt`).

### Mixed convention (A + B)
Allow both `+Feature` and `.extension` patterns. Rejected because having two conventions leads to inconsistency — developers would need to decide which to use, and different packages would make different choices, fragmenting the ecosystem.

### No convention (freeform)
Let developers name extension files however they want. Rejected because consistent naming enables tooling (glob patterns, IDE file browsers) and makes extensions discoverable across packages.

### Protocol/typeclass approach (Elixir-style)
Instead of open classes, use protocols — `Serializable` protocol with per-type implementations. This avoids ETS global mutation and last-writer-wins conflicts entirely, and is statically analyzable. Rejected as a *replacement* for open classes, but acknowledged as **complementary** — many practical uses of `>>` (JSON serialization, formatting, logging) are protocol-shaped and would be better expressed as protocols once they exist.

The two mechanisms solve different axes of the expression problem:
- **Open classes (`>>`):** Add new operations to a single type — `Integer >> factorial`. Natural as a message send (`5 factorial`), awkward as a protocol (a `Factorial` protocol with one implementor is ceremony for no benefit).
- **Protocols:** Add a single operation across many types — `Serializable` implemented for Integer, String, Array, etc. Statically verifiable, no conflict risk, but requires invocation syntax that may not feel like a regular message send.

Beamtalk will add protocols (required for the gradual type system, ADR 0025). When that happens, the guidance should be: use protocols for cross-type operations, use `>>` for type-specific additions. Some existing `>>` extensions may migrate to protocol implementations — this is expected and healthy.

## Consequences

### Positive
- Extensions provide the only way to add methods to sealed primitives, completing the object model
- `ClassName+Feature.bt` naming is self-documenting and scales to large codebases
- Runtime ETS registry enables hot reload of extensions without recompiling target classes
- Conflict tracking via `beamtalk_extension_conflicts` provides debugging visibility
- Dispatch integration means extensions participate in `respondsTo:`, `methods`, and reflection

### Negative
- Last-writer-wins can silently break code when load order changes — mitigated by conflict logging and future static detection
- Extensions are checked via ETS lookup on the slow dispatch path — minor performance cost vs. local methods
- The `+` in filenames is unfamiliar to non-Swift developers — mitigated by clear documentation
- LSP go-to-definition must maintain a secondary index of `(Class, Selector) → file location` for extensions defined in `+Feature` files, separate from the class body index
- API documentation generation for a class must aggregate across all `ClassName+*.bt` files to present the full method surface — tooling cannot rely on the class definition file alone
- Extension methods registered in ETS are invisible to static analysis and future type checking — a typed call site cannot verify at compile time that an extension method exists

### Neutral
- Extensions cannot add instance variables (state) to existing classes — this is by design, matching Swift/Kotlin/C# semantics. Extension methods that depend on target class state must use the class's public API.
- The `>>` token is overloaded (definition syntax AND method reflection operator) — context disambiguates cleanly
- Extensions are node-local in distributed BEAM — each node in a cluster has its own extension registry, consistent with per-node code loading
- Test suites that register extensions share ETS state within the test node — tests should not assume a clean extension registry unless explicitly cleared

## Implementation

The feature is already implemented across all components:

| Component | File | Status |
|-----------|------|--------|
| Parser | `crates/beamtalk-core/src/source_analysis/parser/declarations.rs` | Implemented |
| AST | `crates/beamtalk-core/src/ast.rs` (`StandaloneMethodDefinition`) | Implemented |
| Semantic analysis | `crates/beamtalk-core/src/source_analysis/semantic_analysis/` | Implemented |
| Codegen | `crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs` | Implemented |
| Dispatch | `crates/beamtalk-core/src/codegen/core_erlang/gen_server/dispatch.rs` | Implemented |
| Runtime registry | `runtime/apps/beamtalk_runtime/src/beamtalk_extensions.erl` | Implemented |
| REPL | Inline `>>` definitions and `:load` of extension files | Implemented |
| Hot reload | Extension re-registration on file reload | Implemented |

**Remaining work (BT-1473):**
1. Add `>>` syntax section to `docs/beamtalk-language-features.md`
2. Test and document sealed class extension behaviour explicitly
3. Enforce `ClassName+Feature.bt` naming convention in documentation and examples

## Known Limitations and Future Work

**Static conflict detection:** The current last-writer-wins model detects conflicts only at runtime (load time). A future compiler pass could scan all packages in `beamtalk.toml` and report duplicate `{Class, Selector}` registrations at build time, before deployment. This is the highest-priority follow-up.

**Scoped extensions:** Ruby refinements and C#/Kotlin import-based visibility demonstrate that scoped extensions can prevent cross-package conflicts. Beamtalk intentionally chose global extensions (matching Pharo) because scoping conflicts with the live development model — in a Smalltalk-style image, all methods are globally visible, and scoping would make REPL exploration unpredictable. If cross-package conflicts become a real problem in practice, scoped extensions could be revisited as an opt-in mechanism without changing the default.

**Gradual typing interaction:** When Beamtalk's gradual type system (ADR 0025) matures, extension methods will need type signatures that the type checker can discover. This likely requires a compile-time extension manifest (generated from `+Feature.bt` files) that the type checker reads alongside the class definition. The runtime ETS registry alone is insufficient for static type checking.

**Flat namespace assumption:** The ETS key `{ClassName, Selector}` uses bare atoms, assuming a flat class namespace (ADR 0031). If Beamtalk later introduces namespaced classes, the registry key format will need migration.

## References
- Related issues: BT-1473
- Related ADRs: [ADR 0005](0005-beam-object-model-pragmatic-hybrid.md) (object model, extension registry design), [ADR 0006](0006-unified-method-dispatch.md) (dispatch chain), [ADR 0025](0025-gradual-typing-and-protocols.md) (typing interaction), [ADR 0031](0031-flat-namespace-for-v01.md) (flat namespace assumption), [ADR 0032](0032-early-class-protocol.md) (flattened table removal)
- Implementation: `beamtalk_extensions.erl`, `StandaloneMethodDefinition` in `ast.rs`
- Prior art: Pharo Tonel format, Swift extension file conventions
