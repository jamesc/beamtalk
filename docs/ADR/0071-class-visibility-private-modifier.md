# ADR 0071: Class Visibility — `private` Modifier for Package-Scoped Access Control

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

Adding `private` follows the same pattern — one more branch in the modifier loop.

## Decision

### Keyword: `private`

Introduce a `private` class modifier that restricts a class to package-scoped visibility. Classes without this modifier remain public (the default).

**Why `private`:** This is the keyword ADR 0070 Section 9 anticipated. Every mainstream language uses `private` as a visibility modifier — developers from Go, Rust, Java, Python, Ruby, and Kotlin will reach for it first. "Package-private" is established terminology (Java's default access level is literally called this). Beamtalk has a single visibility boundary (the package), so there is no ambiguity — `private` means "private to this package." There is no planned need for finer-grained object-level encapsulation (`private` = self-only, `protected` = hierarchy), so there is no design space to reserve. If method-level visibility is added later, `private` works naturally there too: `private reset => ...` reads clearly.

### Syntax

```beamtalk
// Public (default) — available to any package that depends on this one
Object subclass: Parser

// Private — only visible within this package
private Object subclass: ParserState

// Combined with other modifiers (any order)
private abstract Object subclass: PrivateBase
private sealed Object subclass: CacheImpl
abstract private Object subclass: AlsoFine
```

### Scope

Class-level only in v1. Method-level `private` is a natural future extension — the keyword works at both levels without conflict. Deferred until real usage patterns emerge from the package ecosystem.

### Default Visibility

**Public by default**, `private` to opt out. Consistent with Smalltalk heritage and the principle of least surprise for newcomers.

### Enforcement

**Compile-time only.** Hard error on cross-package reference to a private class:

```text
error[E0401]: Class 'ParserState' is private to package 'json' and cannot be referenced from 'my_app'
  --> src/app.bt:5:12
   |
 5 |     json@ParserState new
   |          ^^^^^^^^^^^
   |
   = note: 'ParserState' is declared 'private' in package 'json'
```

**Leaked visibility** — hard error when a private class appears in the public signature of a public class (parameter type, return type, state type annotation). This is always a bug: either the class should be public, or the signature is wrong.

```text
error[E0402]: Private class 'TokenBuffer' appears in public signature of 'Parser >> tokenize:'
  --> src/parser.bt:12:3
   |
12 |   tokenize: input :: String -> TokenBuffer =>
   |                                ^^^^^^^^^^^
   |
   = note: 'TokenBuffer' is declared 'private' — make it public, or change the return type
```

This follows Rust's model, where `pub fn` returning a private type is a compile error.

### Visibility Controls Dependency, Not Knowledge

**Core principle:** `private` restricts what you can *depend on*, not what you can *know about*. Private classes are fully visible to browsing, reflection, and documentation tools — you just can't compile code that references them from outside the package. This preserves Smalltalk-style explorability while maintaining API boundaries.

Concretely, from outside a package you **can**:
- **Browse** — `:browse json` lists all classes, including private ones (marked as private)
- **Read docs** — `:doc json@ParserState` shows its documentation
- **Read source** — `:source json@ParserState >> reset` shows the method source
- **Inspect objects** — `obj class`, `obj methods`, `obj respondsTo: #foo` — runtime reflection on values works regardless of visibility
- **Send messages** — if you have a reference to an object, you can send it any message

You **cannot**:
- **Name the class in compiled code** — `json@ParserState new` is a compile error from outside the package
- **Use it in type annotations** — `param :: json@ParserState` is a compile error
- **Subclass it** — `json@ParserState subclass: MyState` is a compile error
- **Extend it** — `json@ParserState >> helper => ...` is a compile error

This is the Smalltalk-native model: the system browser shows everything, but the compiler enforces boundaries. The filesystem analogy is: you can `ls` and `cat` any file, but the build system won't let you import private types.

### Metadata

Add a `visibility` field to `__beamtalk_meta/0` (value: `public` or `private`). Zero runtime cost — compile-time constant. Enables tooling and future Erlang→Beamtalk FFI enforcement.

### Modifier Interactions

| Combination | Valid? | Notes |
|-------------|--------|-------|
| `private sealed` | Yes | Redundant for cross-package subclassing (can't be seen), but useful for intra-package subclass prevention |
| `private abstract` | Yes | Private base class, must be subclassed within the package |
| `private typed` | Yes | Private class with type annotation requirements |
| Stacking order | Any | `private` can appear anywhere in the modifier list before the superclass name, like existing modifiers |

### ClassBuilder Protocol

The `subclass:` declaration desugars to `ClassBuilder` (ADR 0038). The `modifier:` method currently accepts `#abstract`, `#sealed`, `#typed`. This ADR extends it to also accept `#private`. The `classBuilderRegister` intrinsic must propagate the visibility flag to `__beamtalk_meta/0`.

### Semantic Edge Cases

**Subclassing across packages:** A public class may have a private superclass within the same package. External packages see the public class but cannot name the private superclass directly. This is valid — the private class is an implementation detail of the inheritance chain. The compiler does not require the entire superclass chain to be public.

**Private classes in public signatures:** If a public method's type annotation references a private class (parameter type, return type), it is a hard error (`E0402` — see Enforcement above). A private class that appears in a public signature is always a bug. However, a public method *without* a type annotation may return a private class's instance at runtime — visibility controls naming in source, not data flow. Callers interact with such values via structural typing (protocol conformance) or message sends.

**Protocol conformance:** A private class can conform to a public protocol structurally (ADR 0025). Instances may escape the package boundary via protocol-typed parameters. This is by design — the visibility boundary controls class naming, not object identity.

**Runtime type checks (`isKindOf:`, `class`):** `isKindOf:` takes a class reference at the source level. Writing `obj isKindOf: json@ParserState` in another package is a cross-package reference to a private class — this is a compile error, same as any other cross-package reference. However, `obj class` returns the class at runtime without naming it in source — this works fine and is not restricted. Runtime reflection (`class`, `respondsTo:`, `printString`) operates on values, not names, so it is unaffected by visibility.

**Extension methods (ADR 0066):** Extension methods (`>>` syntax) on a private class are only visible within the defining package. Cross-package extensions targeting a private class are a compile error (the class name cannot be resolved). The `>>` codegen patches a runtime ETS table — the compile-time check prevents the cross-package extension from ever being compiled.

**Generic type arguments (ADR 0068):** Private class names may appear in type metadata (`__beamtalk_meta/0`) for generic instantiations. Tooling (LSP, FFI generators) should filter private names from cross-package surfaces.

### Erlang FFI

| Direction | Enforcement |
|-----------|-------------|
| **Beamtalk → Erlang** | Private classes can call Erlang freely via existing FFI |
| **Erlang → Beamtalk (current)** | No clean FFI exists — no enforcement needed yet |
| **Erlang → Beamtalk (future)** | Generated API wrappers will exclude private classes — you can't call what doesn't exist |
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

### C# / Kotlin

**Mechanism:** `internal` keyword — assembly/module-scoped. Separate from `private` (class-scoped).
**Lesson:** The `internal`/`private` distinction is necessary in C# because it has both levels. Beamtalk has only one visibility boundary (the package), so a single keyword suffices.

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
**Lesson:** Works for single-image development. Breaks down for distributed packages ��� no stable API boundary.

## User Impact

**Newcomer** (from Python/JS/C#): Immediately familiar — `private` is the first keyword they'd guess. Modifier syntax matches existing `sealed`/`abstract` pattern. Error messages are clear and actionable.

**Smalltalk developer:** Departure from Smalltalk purity (everything is public). Justified by package ecosystem needs — Pharo's lack of visibility is a known pain point for library authors.

**Erlang/BEAM developer:** Maps naturally to `-export()` concept. Understands compile-time enforcement. Comfortable with BEAM's escape hatches via `apply/3`.

**Production operator:** Reduces API surface. Cleaner dependency boundaries. Would want future `--privileged` lockout for production REPLs.

**Tooling developer:** LSP can filter private classes from cross-package completions. `__beamtalk_meta/0` visibility field enables tooling without runtime cost.

## Steelman Analysis

### Rejected: Use `internal` Instead of `private`

**Strongest argument:** `internal` more precisely describes the semantics — "visible within this package, not exported." It reserves `private` for potential future object-level encapsulation (self-only access), enabling a clean three-level model (`internal` = package, `protected` = hierarchy, `private` = self). C#/Kotlin established `internal` as the industry-standard term for assembly/module-scoped visibility.

**Counter:** The three-level model assumes Beamtalk will need object-level `private` and `protected`. In a message-passing language on BEAM — where objects are either processes (actors) or immutable values — the need for self-only visibility is unclear. Reserving design space for a feature that may never arrive costs familiarity now. Every developer from Go, Rust, Java, Python, or Ruby reaches for `private` first. If object-level encapsulation is needed later, Beamtalk can use a different mechanism (e.g., Newspeak-style lexical scoping, or simply not adding it — Go has thrived with only exported/unexported).

### Rejected: Method-Level `private` in v1

**Strongest argument:** Library authors need to hide helper methods on public classes, not just whole classes. A public `HttpClient` with `buildHeaders:`, `retryWithBackoff:`, `parseChunkedResponse:` helper methods exposes its entire implementation surface. Without method-level `private`, the only option is to extract helpers into a private class — forcing an architectural pattern to work around a missing language feature.

**Counter:** In Beamtalk's one-class-per-file model, the natural pattern is to extract implementation details into dedicated helper classes — making class-level `private` the primary tool for hiding them. Method-level `private` adds complexity to dispatch (what happens when you send a message to an object whose class has private methods — `doesNotUnderstand:` or a visibility error?) and FFI generation. There is no ecosystem data yet to quantify the split. Deferring lets real usage patterns from published packages reveal whether method-level visibility justifies the dispatch complexity. The keyword `private` works naturally at both class and method level, so this extension is straightforward when needed.

### Rejected: REPL Always Exempt from Visibility

**Strongest argument:** Smalltalk's "all objects are inspectable" philosophy is core to the live-programming experience. Enforcing visibility in the REPL limits exploration of library internals.

**Counter:** This is a non-issue. Visibility controls dependency, not knowledge (see REPL Behavior above). Browsing, docs, source reading, reflection, and message sends all work on private classes — the only thing blocked is naming a private class in a compiled expression. The Smalltalk system browser shows everything; the compiler enforces boundaries. Beamtalk works the same way.

### Rejected: Runtime Enforcement

**Strongest argument:** Compile-time only means Erlang code can bypass visibility via raw BEAM module calls. Real security needs runtime checks.

**Counter:** BEAM fundamentally can't prevent `apply/3` calls — that's the VM's architecture. Runtime checks would add cost to every dispatch and still be bypassable. Java, Rust, Go, and Erlang itself all use compile-time enforcement as their primary visibility mechanism. The future Erlang→Beamtalk FFI provides Erlang-compile-time enforcement — the right level for cross-language calls.

### Tension Points

- Smalltalk purists prefer no visibility at all; every other cohort wants it
- If Beamtalk later needs object-level encapsulation, `private` is already taken for package scope — but this is deemed unlikely for a BEAM message-passing language
- BEAM veterans and operators align on compile-time enforcement being sufficient
- Method-level `private` is the strongest deferred request — the keyword choice makes this a natural future extension
- The leaked-visibility error (`E0402`) is strict — some developers may want to return "opaque" private types from public methods, but this is better served by protocols

## Alternatives Considered

### Use `internal` Keyword

Use `internal` instead of `private` for package-scoped visibility, following C#/Kotlin terminology.

```beamtalk
internal Object subclass: ParserState
```

**Rejected because:** Reserves design space for object-level `private`/`protected` that Beamtalk is unlikely to need. `internal` is less familiar than `private` — every mainstream language uses `private` as its primary visibility modifier. The C#/Kotlin `internal`/`private` distinction exists because those languages have both levels; Beamtalk has a single visibility boundary (the package), so a single keyword suffices. Choosing the less familiar keyword for a hypothetical future feature is the wrong trade-off for a pre-1.0 language.

### Include Method-Level `private` in v1

Allow `private` as a method modifier to hide individual methods:

```beamtalk
Object subclass: Parser
  private reset => ...  // only callable within this package
  parse: input => ...   // public
```

**Rejected because:** Adds complexity to dispatch (method resolution must check caller's package), FFI generation, and the conceptual model. Class-level `private` addresses the primary use case. Defer to v2 based on real ecosystem feedback. The keyword `private` works at both levels, so this extension is straightforward when needed.

### No Visibility Controls (Status Quo)

Keep everything public, following Smalltalk tradition.

**Rejected because:** Breaks down for distributed packages �� library authors have no way to mark implementation details, every class is public API, and internal refactoring risks breaking consumers. Pharo's package ecosystem demonstrates this pain point.

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
- Semver becomes meaningful — private class changes are patch releases
- Compile-time enforcement, zero runtime cost
- Composes cleanly with existing modifiers (`sealed`, `abstract`, `typed`)
- `private` is immediately familiar — every developer's first guess
- Method-level `private` is a natural future extension (no keyword conflict)
- `__beamtalk_meta/0` visibility field enables tooling and future FFI enforcement

### Negative

- Departure from Smalltalk "everything is public" tradition
- Compile-time only — Erlang code can bypass via raw BEAM module calls
- Adds a keyword to learn
- No method-level `private` in v1 — helper methods on public classes remain exposed
- If object-level encapsulation is ever needed, `private` is already taken for package scope

### Neutral

- REPL expressions go through compiler, so enforcement is automatic
- No runtime cost — metadata is a compile-time constant
- Existing single-package code requires no changes
- Hot code reloading unaffected — visibility is a compile-time property, not a runtime check; reloading a private class works identically to reloading a public one. If a dependency changes a class from public to private, already-compiled downstream code continues to work at runtime (BEAM does not re-check); the error surfaces on next recompilation of the downstream package

## Implementation

### Phase 1: Parser

Add `private` to the modifier parsing loop.

**Files:**
- `crates/beamtalk-core/src/source_analysis/parser/declarations.rs` — add `private` branch to modifier loop (alongside `abstract`, `sealed`, `typed`)
- `crates/beamtalk-core/src/ast.rs` — add `is_private: bool` to `ClassDefinition`

### Phase 2: Semantic Analysis (largest phase)

Enforce cross-package `private` access. This is the core of the implementation and requires infrastructure that does not yet exist.

**Infrastructure prerequisite:** `ClassInfo` currently has no `package` field. `analyse_with_packages()` exists in `semantic_analysis/mod.rs` but has no non-test callers — it is not wired into the build pipeline. Phase 2 must:

1. Add `package: Option<String>` and `is_private: bool` to `ClassInfo`
2. Populate `package` during `add_from_beam_meta()` by extracting it from the BEAM module name (`bt@{package}@{class}`, ADR 0016)
3. Thread the current package name into the analysis pipeline (wire `analyse_with_packages` into the build)
4. Add `check_private_visibility()` — emit `E0401` diagnostic when a private class is referenced from another package
5. Cover all reference positions: superclass, type annotations, `isKindOf:` argument, extension method target

**Files:**
- `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/mod.rs` — `ClassInfo` fields, `check_private_visibility()`
- `crates/beamtalk-core/src/semantic_analysis/mod.rs` — wire `analyse_with_packages` into the build pipeline
- `add_from_beam_meta()` — propagate visibility and package from BEAM metadata

### Phase 3: Metadata and Codegen

Emit visibility in `__beamtalk_meta/0` and `.app` metadata.

**Files:**
- `crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs` — add `visibility => public | private` to meta map
- `.app.src` class registry — add visibility field
- `ClassBuilder` modifier handling — accept `#private` symbol (ADR 0038)

**Coordination:** Must align with ADR 0050's `meta_version` scheme. If `__beamtalk_meta/0` is already stable, bump `meta_version` when adding the `visibility` field.

### Phase 4: Tooling

Filter private classes in LSP and REPL completions.

**Files:**
- `crates/beamtalk-lsp/` — filter cross-package completions, show private classes only for same-package files
- REPL — filter tab completion for cross-package classes

## References

- Related issues: BT-714, BT-716
- Related ADRs:
  - [ADR 0070](0070-package-namespaces-and-dependencies.md) Section 9 (calls for this ADR)
  - [ADR 0049](0049-remove-method-level-sealed.md) (sealed method removal — modifier precedent)
  - [ADR 0050](0050-incremental-compiler-class-hierarchy.md) (`__beamtalk_meta/0` structure — must coordinate `visibility` field)
  - [ADR 0038](0038-subclass-classbuilder-protocol.md) (ClassBuilder protocol — must accept `#private` modifier)
  - [ADR 0066](0066-open-class-extension-methods.md) (extension methods — private class extensions scoped to package)
  - [ADR 0068](0068-parametric-types-and-protocols.md) (parametric types — private names in type metadata)
  - [ADR 0025](0025-gradual-typing-and-protocols.md) (protocols — private classes can conform to public protocols)
  - [ADR 0016](0016-unified-stdlib-module-naming.md) (BEAM module naming — package extraction)
  - [ADR 0031](0031-flat-namespace-for-v01.md) (flat namespace)
