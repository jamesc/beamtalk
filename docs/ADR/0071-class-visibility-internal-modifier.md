# ADR 0071: Class Visibility — `internal` Modifier for Package-Scoped Access Control

## Status

Proposed (2026-03-28)

## Context

ADR 0070 Section 9 explicitly calls for a follow-up ADR on class visibility. Today, **all classes in a package are public**. Library authors cannot hide implementation-detail classes — every class becomes part of the public API the moment a user references it, constraining internal refactoring.

This is the classic Smalltalk trade-off: everything is public, which enables explorability but prevents stable API boundaries in a distributed package ecosystem.

### Constraints

- **One class per file**, each class compiles to one BEAM module
- **Package is the visibility boundary** (ADR 0070)
- **BEAM has no module-level access control** — any loaded module is callable by any process
- **Smalltalk tradition:** everything is public
- **Existing class modifiers:** `sealed`, `abstract`, `typed`
- **Method-level `sealed`** slated for removal per ADR 0049 (inferred from `@primitive`/`@intrinsic`)

### Current Modifier Parsing

The parser already handles a modifier loop for `abstract`, `sealed`, and `typed` before the superclass name:

```rust
// declarations.rs lines 81–94
while let TokenKind::Identifier(name) = self.current_kind() {
    if name == "abstract" {
        is_abstract = true;
        self.advance();
    } else if name == "sealed" {
        is_sealed = true;
        self.advance();
    } else if name == "typed" {
        is_typed = true;
        self.advance();
    } else {
        break;
    }
}
```

Adding `internal` follows the same pattern — one more branch in the modifier loop.

## Decision

### Keyword: `internal`

Introduce an `internal` class modifier that restricts a class to package-scoped visibility. Classes without this modifier remain public (the default).

**Why `internal`:** Beamtalk's core principle is that visibility controls *dependency*, not *knowledge* — you can browse, document, inspect, and send messages to internal classes, you just can't name them in compiled code from outside the package. That's the definition of `internal`, not `private`. A class you can `:doc`, `:source`, `:browse`, and inspect isn't private — it's internal.

Every modern language that distinguishes package-boundary visibility from object-level encapsulation uses different words: Swift uses `internal` (the default!) vs `private`; Kotlin uses `internal` vs `private`; Rust uses `pub(crate)` vs private (default); Scala 3 uses `private[pkg]` vs `private`. Languages that use `private` for the package boundary are the ones that *don't have* class-level encapsulation (Go, Dart, Zig). Using the right word now avoids a painful rename if method-level `private` (self-only access) is ever needed.

Swift is especially instructive — `internal` is the default visibility, and everything is module-internal unless explicitly marked `public`. The concept is natural enough that Swift made it the default.

### Syntax

```beamtalk
// Public (default) — available to any package that depends on this one
Object subclass: Parser

// Internal — only visible within this package
internal Object subclass: ParserState

// Combined with other modifiers (any order)
internal abstract Object subclass: InternalBase
internal sealed Object subclass: CacheImpl
abstract internal Object subclass: AlsoFine
```

### Scope

Class-level only in v1. Method-level visibility is a natural future extension — `internal` for package-scoped methods, potentially `private` for self-only access. Using `internal` at the class level keeps the keyword space clean for this. Deferred until real usage patterns emerge from the package ecosystem.

### Default Visibility

**Public by default**, `internal` to opt out. Consistent with Smalltalk heritage and the principle of least surprise for newcomers.

### Enforcement

**Compile-time only.** Hard error on cross-package reference to an internal class:

```text
error[E0401]: Class 'ParserState' is internal to package 'json' and cannot be referenced from 'my_app'
  --> src/app.bt:5:12
   |
 5 |     json@ParserState new
   |          ^^^^^^^^^^^
   |
   = note: 'ParserState' is declared 'internal' in package 'json'
```

**Leaked visibility** — hard error when an internal class appears in the public signature of a public class (parameter type, return type, state type annotation). This is always a bug: either the class should be public, or the signature is wrong.

```text
error[E0402]: Internal class 'TokenBuffer' appears in public signature of 'Parser >> tokenize:'
  --> src/parser.bt:12:3
   |
12 |   tokenize: input :: String -> TokenBuffer =>
   |                                ^^^^^^^^^^^
   |
   = note: 'TokenBuffer' is declared 'internal' — make it public, or change the return type
```

This follows Rust's model, where `pub fn` returning a non-public type is a compile error.

### Visibility Controls Dependency, Not Knowledge

**Core principle:** `internal` restricts what you can *depend on*, not what you can *know about*. Internal classes are fully visible to browsing, reflection, and documentation tools — you just can't compile code that references them from outside the package. This preserves Smalltalk-style explorability while maintaining API boundaries. The keyword itself conveys this: "internal" means "part of this package's internals" — it doesn't imply hidden or inaccessible.

Concretely, from outside a package you **can**:
- **Browse** — `:browse json` lists all classes, including internal ones (marked as internal)
- **Read docs** — `:doc json@ParserState` shows its documentation
- **Read source** — `:source json@ParserState >> reset` shows the method source
- **Inspect objects** — `obj class`, `obj methods`, `obj respondsTo: #foo` — runtime reflection on values works regardless of visibility
- **Send messages** — if you have a reference to an object, you can send it any message

You **cannot**:
- **Name the class in compiled code** — `json@ParserState new` is a compile error from outside the package
- **Use it in type annotations** — `param :: json@ParserState` is a compile error
- **Subclass it** — `json@ParserState subclass: MyState` is a compile error
- **Extend it** — `json@ParserState >> helper => ...` is a compile error

This is the Smalltalk-native model: the system browser shows everything, but the compiler enforces boundaries. The filesystem analogy is: you can `ls` and `cat` any file, but the build system won't let you import internal types.

### Metadata

Add a `visibility` field to `__beamtalk_meta/0` (value: `public` or `internal`). Zero runtime cost — compile-time constant. Enables tooling and future Erlang→Beamtalk FFI enforcement.

### Modifier Interactions

| Combination | Valid? | Notes |
|-------------|--------|-------|
| `internal sealed` | Yes | Redundant for cross-package subclassing (can't be seen), but useful for intra-package subclass prevention |
| `internal abstract` | Yes | Internal base class, must be subclassed within the package |
| `internal typed` | Yes | Internal class with type annotation requirements |
| Stacking order | Any | `internal` can appear anywhere in the modifier list before the superclass name, like existing modifiers |

### ClassBuilder Protocol

The `subclass:` declaration desugars to `ClassBuilder` (ADR 0038). The `modifier:` method currently accepts `#abstract`, `#sealed`, `#typed`. This ADR extends it to also accept `#internal`. The `classBuilderRegister` intrinsic must propagate the visibility flag to `__beamtalk_meta/0`.

### Semantic Edge Cases

**Subclassing across packages:** A public class may have an internal superclass within the same package. External packages see the public class but cannot name the internal superclass directly. This is valid — the internal class is an implementation detail of the inheritance chain. The compiler does not require the entire superclass chain to be public.

**Internal classes in public signatures:** If a public method's type annotation references an internal class (parameter type, return type), it is a hard error (`E0402` — see Enforcement above). An internal class that appears in a public signature is always a bug. However, a public method *without* a type annotation may return an internal class's instance at runtime — visibility controls naming in source, not data flow. Callers interact with such values via structural typing (protocol conformance) or message sends.

**Protocol conformance:** An internal class can conform to a public protocol structurally (ADR 0025). Instances may escape the package boundary via protocol-typed parameters. This is by design — the visibility boundary controls class naming, not object identity.

**Runtime type checks (`isKindOf:`, `class`):** `isKindOf:` takes a class reference at the source level. Writing `obj isKindOf: json@ParserState` in another package is a cross-package reference to an internal class — this is a compile error, same as any other cross-package reference. However, `obj class` returns the class at runtime without naming it in source — this works fine and is not restricted. Runtime reflection (`class`, `respondsTo:`, `printString`) operates on values, not names, so it is unaffected by visibility.

**Extension methods (ADR 0066):** Extension methods (`>>` syntax) on an internal class are only visible within the defining package. Cross-package extensions targeting an internal class are a compile error (the class name cannot be resolved). The `>>` codegen patches a runtime ETS table — the compile-time check prevents the cross-package extension from ever being compiled.

**Generic type arguments (ADR 0068):** Internal class names may appear in type metadata (`__beamtalk_meta/0`) for generic instantiations. Tooling (LSP, FFI generators) should filter internal names from cross-package surfaces.

### Erlang FFI

| Direction | Enforcement |
|-----------|-------------|
| **Beamtalk → Erlang** | Internal classes can call Erlang freely via existing FFI |
| **Erlang → Beamtalk (current)** | No clean FFI exists — no enforcement needed yet |
| **Erlang → Beamtalk (future)** | Generated API wrappers will exclude internal classes — you can't call what doesn't exist |
| **Raw `apply/3`** | Always possible, always "voided warranty" territory |

### Enforcement Stack

| Call Path | Enforcement | When |
|-----------|-------------|------|
| Beamtalk → Beamtalk | Beamtalk compiler | Compile time |
| Erlang → Beamtalk via FFI | Generated API / parse transform | Erlang compile time (future) |
| Raw `apply/3` to BEAM module | None | Voided warranty |

## Prior Art

### Go

**Mechanism:** Case convention — uppercase exported, lowercase unexported.
**Boundary:** Package.
**Levels:** Binary: exported or unexported. No protected.
**Lesson:** Package as the only visibility boundary works. Nobody seriously misses `protected` in Go.

### Newspeak

**Mechanism:** Lexical scoping — no global namespace, everything dependency-injected.
**Motivation:** Capability security (principle of least authority). If you weren't given access, you can't name it.
**Lesson:** Beautiful but requires owning the whole stack. BEAM can't enforce this — any process can call any module. Would be language-level fiction on BEAM.

### Rust

**Mechanism:** `pub`/private, compile-time. `unsafe` can bypass.
**Boundary:** Module (file) and crate.
**Lesson:** Compile-time enforcement is sufficient. `pub(crate)` ≈ package-private.

### Swift

**Mechanism:** `internal` (default) / `public` / `private` / `fileprivate`. `internal` is module-scoped — visible within the module, hidden from importers.
**Boundary:** Module (≈ package).
**Lesson:** `internal` is so natural at the module boundary that Swift made it the default. Separate from `private` (self-only) — using different words for different concepts. Beamtalk's `internal` maps directly to Swift's `internal`.

### C# / Kotlin

**Mechanism:** `internal` keyword — assembly/module-scoped. Separate from `private` (class-scoped).
**Lesson:** The `internal`/`private` distinction exists because these are genuinely different concepts. `internal` means "part of this module's internals" (accessible for browsing/reflection but not for cross-module dependency). `private` means "you can't see it." Beamtalk's semantics match `internal` — the class is visible, browsable, and inspectable, just not importable.

### Java

**Mechanism:** `private`/`protected`/`public` + JPMS modules. Default (no modifier) = package-private.
**Bypass:** `setAccessible(true)`, `--add-opens`.
**Lesson:** "Package-private" is established terminology. Even Java acknowledges escape hatches exist.

### Erlang

**Mechanism:** `-export()` controls what other modules can call.
**Bypass:** `apply(Mod, Fun, Args)` works on anything.
**Lesson:** BEAM's own visibility is compile-time convention, not runtime enforcement.

### Smalltalk

**Mechanism:** No enforced visibility. Everything public. Private is a category/protocol convention.
**Lesson:** Works for single-image development. Breaks down for distributed packages — no stable API boundary.

## User Impact

**Newcomer** (from Swift/Kotlin/C#): Immediately familiar — `internal` is established terminology for module-scoped visibility. Developers from Swift (where it's the default) and C#/Kotlin will recognize it instantly. Modifier syntax matches existing `sealed`/`abstract` pattern. Error messages are clear and actionable.

**Smalltalk developer:** Departure from Smalltalk purity (everything is public). Justified by package ecosystem needs — Pharo's lack of visibility is a known pain point for library authors. The word `internal` is less jarring than `private` in a Smalltalk context — it says "this is an implementation detail" rather than "you can't see this."

**Erlang/BEAM developer:** Maps naturally to `-export()` concept. Understands compile-time enforcement. Comfortable with BEAM's escape hatches via `apply/3`.

**Production operator:** Reduces API surface. Cleaner dependency boundaries. Would want future `--privileged` lockout for production REPLs.

**Tooling developer:** LSP can filter internal classes from cross-package completions. `__beamtalk_meta/0` visibility field enables tooling without runtime cost.

## Steelman Analysis

### Rejected: Use `private` Instead of `internal`

**Strongest argument:** `private` is the most universally recognized visibility keyword. Developers from Go, Rust, Java, Python, Ruby, and Kotlin will reach for it first. "Package-private" is established Java terminology. Beamtalk has a single visibility boundary (the package), so there is no ambiguity — and if method-level visibility is never needed, `private` would have been fine. Choosing a less familiar keyword to reserve design space for a hypothetical future feature is premature.

**Counter:** The word is wrong for what it describes. A class you can `:doc`, `:source`, `:browse`, and inspect isn't private — it's internal. `private` means "you can't see it"; `internal` means "it's part of this package's internals." Beamtalk's core principle — visibility controls *dependency*, not *knowledge* — is exactly the `internal` concept. Every modern language that has both package-boundary and object-level visibility uses different words (Swift: `internal`/`private`, Kotlin: `internal`/`private`, Rust: `pub(crate)`/private, Scala 3: `private[pkg]`/`private`). Using `private` now forces an awkward rename if method-level encapsulation is ever needed. Using the right word from the start costs minor familiarity but buys semantic honesty and design space.

### Rejected: Method-Level Visibility in v1

**Strongest argument:** Library authors need to hide helper methods on public classes, not just whole classes. A public `HttpClient` with `buildHeaders:`, `retryWithBackoff:`, `parseChunkedResponse:` helper methods exposes its entire implementation surface. Without method-level visibility, the only option is to extract helpers into an internal class — forcing an architectural pattern to work around a missing language feature.

**Counter:** In Beamtalk's one-class-per-file model, the natural pattern is to extract implementation details into dedicated helper classes — making class-level `internal` the primary tool for hiding them. Method-level visibility adds complexity to dispatch (what happens when you send a message to an object whose class has internal methods — `doesNotUnderstand:` or a visibility error?) and FFI generation. There is no ecosystem data yet to quantify the split. Deferring lets real usage patterns from published packages reveal whether method-level visibility justifies the dispatch complexity. Using `internal` at the class level keeps keyword space clean — `internal` for package-scoped methods and `private` for self-only access are natural future extensions.

### Rejected: REPL Always Exempt from Visibility

**Strongest argument:** Smalltalk's "all objects are inspectable" philosophy is core to the live-programming experience. Enforcing visibility in the REPL limits exploration of library internals.

**Counter:** This is a non-issue. Visibility controls dependency, not knowledge (see above). Browsing, docs, source reading, reflection, and message sends all work on internal classes — the only thing blocked is naming an internal class in a compiled expression. The Smalltalk system browser shows everything; the compiler enforces boundaries. Beamtalk works the same way. The word `internal` itself reinforces this — these classes are "internal to the package," not "hidden from the world."

### Rejected: Runtime Enforcement

**Strongest argument:** Compile-time only means Erlang code can bypass visibility via raw BEAM module calls. Real security needs runtime checks.

**Counter:** BEAM fundamentally can't prevent `apply/3` calls — that's the VM's architecture. Runtime checks would add cost to every dispatch and still be bypassable. Java, Rust, Go, and Erlang itself all use compile-time enforcement as their primary visibility mechanism. The future Erlang→Beamtalk FFI provides Erlang-compile-time enforcement — the right level for cross-language calls.

### Tension Points

- Smalltalk purists prefer no visibility at all; every other cohort wants it
- `internal` is less universally recognized than `private` — but it's the *right* word for the semantics, and developers from Swift/Kotlin/C# will recognize it immediately
- BEAM veterans and operators align on compile-time enforcement being sufficient
- Method-level visibility is the strongest deferred request — choosing `internal` keeps the keyword space clean for future `internal` (package-scoped) and `private` (self-only) method modifiers
- The leaked-visibility error (`E0402`) is strict — some developers may want to return "opaque" internal types from public methods, but this is better served by protocols

## Alternatives Considered

### Use `private` Keyword

Use `private` instead of `internal` for package-scoped visibility, as the most universally recognized visibility keyword.

```beamtalk
private Object subclass: ParserState
```

**Rejected because:** `private` means "you can't see it" — but in Beamtalk, you *can* see internal classes (browse, doc, source, inspect). The word is wrong for the semantics. Every modern language that has both package-boundary and object-level visibility uses different words: Swift (`internal`/`private`), Kotlin (`internal`/`private`), Rust (`pub(crate)`/private), Scala 3 (`private[pkg]`/`private`). Using `private` for the package boundary closes off the keyword for future self-only method-level encapsulation. `internal` accurately describes the relationship ("part of this package's internals") rather than implying inaccessibility.

### Include Method-Level Visibility in v1

Allow `internal` (or `private`) as a method modifier to hide individual methods:

```beamtalk
Object subclass: Parser
  internal reset => ...  // only callable within this package
  parse: input => ...    // public
```

**Rejected because:** Adds complexity to dispatch (method resolution must check caller's package), FFI generation, and the conceptual model. Class-level `internal` addresses the primary use case. Defer to v2 based on real ecosystem feedback. Choosing `internal` at the class level keeps the keyword space clean for future method-level modifiers (`internal` for package-scoped, `private` for self-only).

### Package-Level Export List

Declare public classes in `beamtalk.toml` rather than marking internal classes individually:

```toml
[package]
name = "json"
public_classes = ["Parser", "ParseResult", "JsonError"]
# Everything else is internal by omission
```

This is the Erlang-native approach — `-export()` declares what's public in one place. The API surface is visible at a glance without reading every source file.

**Rejected because:** Beamtalk's one-class-per-file model means the modifier is always on the class declaration itself — there's no ambiguity about which file it's in. A separate export list creates a synchronization problem: add a class, forget to add it to the list, and it's silently internal. The per-class modifier is self-documenting — you see `internal` on the declaration. Erlang needs `-export()` because a single file contains many functions; Beamtalk doesn't have that problem.

### No Visibility Controls (Status Quo)

Keep everything public, following Smalltalk tradition.

**Rejected because:** Breaks down for distributed packages — library authors have no way to mark implementation details, every class is public API, and refactoring risks breaking consumers. Pharo's package ecosystem demonstrates this pain point.

### Runtime Enforcement via Dispatch Checks

Check visibility at message dispatch time, preventing cross-package calls even from Erlang:

```erlang
% Would add to every dispatch path:
check_visibility(CallerModule, TargetModule, Visibility)
```

**Rejected because:** BEAM can't prevent `apply/3` calls at the VM level, so runtime checks are always bypassable. They add measurable cost to every dispatch. Compile-time enforcement is the established pattern across BEAM languages (Erlang's `-export()`) and systems languages (Rust, Go).

## Consequences

### Positive

- Library authors can draw stable API boundaries
- Semver becomes meaningful — internal class changes are patch releases
- Compile-time enforcement, zero runtime cost
- Composes cleanly with existing modifiers (`sealed`, `abstract`, `typed`)
- `internal` accurately describes the semantics — "part of this package's internals," not "hidden"
- Keyword space stays clean — `private` remains available for future self-only method-level encapsulation
- Aligns with Swift/Kotlin/C# terminology for the same concept
- `__beamtalk_meta/0` visibility field enables tooling and future FFI enforcement

### Negative

- Departure from Smalltalk "everything is public" tradition
- Compile-time only — Erlang code can bypass via raw BEAM module calls
- `internal` is less universally recognized than `private` — developers from Go/Python/Ruby may not guess it first
- No method-level visibility in v1 — helper methods on public classes remain exposed

### Neutral

- REPL expressions go through compiler, so enforcement is automatic
- No runtime cost — metadata is a compile-time constant
- Existing single-package code requires no changes
- Hot code reloading unaffected — visibility is a compile-time property, not a runtime check; reloading an internal class works identically to reloading a public one. If a dependency changes a class from public to internal, already-compiled downstream code continues to work at runtime (BEAM does not re-check); the error surfaces on next recompilation of the downstream package

## Implementation

### Phase 1: Parser

Add `internal` to the modifier parsing loop.

**Files:**
- `crates/beamtalk-core/src/source_analysis/parser/declarations.rs` — add `internal` branch to modifier loop (alongside `abstract`, `sealed`, `typed`)
- `crates/beamtalk-core/src/ast.rs` — add `is_internal: bool` to `ClassDefinition`

### Phase 2: Semantic Analysis (largest phase)

Enforce cross-package `internal` access. This is the core of the implementation and requires infrastructure that does not yet exist.

**Infrastructure prerequisite:** `ClassInfo` currently has no `package` field. `analyse_with_packages()` exists in `semantic_analysis/mod.rs` but has no non-test callers — it is not wired into the build pipeline. Phase 2 must:

1. Add `package: Option<String>` and `is_internal: bool` to `ClassInfo`
2. Populate `package` during `add_from_beam_meta()` by extracting it from the BEAM module name (`bt@{package}@{class}`, ADR 0016)
3. Thread the current package name into the analysis pipeline (wire `analyse_with_packages` into the build)
4. Add `check_internal_visibility()` — emit `E0401` diagnostic when an internal class is referenced from another package
5. Cover all reference positions: superclass, type annotations, `isKindOf:` argument, extension method target

**Files:**
- `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/mod.rs` — `ClassInfo` fields, `check_internal_visibility()`
- `crates/beamtalk-core/src/semantic_analysis/mod.rs` — wire `analyse_with_packages` into the build pipeline
- `add_from_beam_meta()` — propagate visibility and package from BEAM metadata

### Phase 3: Metadata and Codegen

Emit visibility in `__beamtalk_meta/0` and `.app` metadata.

**Files:**
- `crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs` — add `visibility => public | internal` to meta map
- `.app.src` class registry — add visibility field
- `ClassBuilder` modifier handling — accept `#internal` symbol (ADR 0038)

**Coordination:** Must align with ADR 0050's `meta_version` scheme. If `__beamtalk_meta/0` is already stable, bump `meta_version` when adding the `visibility` field.

### Phase 4: Tooling

Filter internal classes in LSP and REPL completions.

**Files:**
- `crates/beamtalk-lsp/` — filter cross-package completions, show internal classes only for same-package files
- REPL — filter tab completion for cross-package classes

## References

- Related issues: BT-714, BT-716
- Related ADRs:
  - [ADR 0070](0070-package-namespaces-and-dependencies.md) Section 9 (calls for this ADR)
  - [ADR 0049](0049-remove-method-level-sealed.md) (sealed method removal — modifier precedent)
  - [ADR 0050](0050-incremental-compiler-class-hierarchy.md) (`__beamtalk_meta/0` structure — must coordinate `visibility` field)
  - [ADR 0038](0038-subclass-classbuilder-protocol.md) (ClassBuilder protocol — must accept `#internal` modifier)
  - [ADR 0066](0066-open-class-extension-methods.md) (extension methods — internal class extensions scoped to package)
  - [ADR 0068](0068-parametric-types-and-protocols.md) (parametric types — internal names in type metadata)
  - [ADR 0025](0025-gradual-typing-and-protocols.md) (protocols — internal classes can conform to public protocols)
  - [ADR 0016](0016-unified-stdlib-module-naming.md) (BEAM module naming — package extraction)
  - [ADR 0031](0031-flat-namespace-for-v01.md) (flat namespace)
