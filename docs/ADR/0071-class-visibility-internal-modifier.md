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

**Why `internal` and not `private`:** ADR 0070 Section 9 anticipated `private` as the likely keyword. This ADR diverges from that direction for two reasons: (1) `private` is the most natural keyword for future object-level encapsulation (self-only access) and `protected` for hierarchy-visible access — using `private` for package scope burns the most valuable design space; (2) `internal` honestly describes the semantics — visible within this package, not exported — and matches established C#/Kotlin terminology for assembly/module-scoped visibility. If Beamtalk never adds object-level `private`, the cost is a less familiar keyword; if it does, the benefit is a clean three-level model (`internal` = package, `protected` = hierarchy, `private` = self).

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

Class-level only in v1. Method-level `internal` is deferred until real usage patterns emerge from the package ecosystem.

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

### REPL Behavior

REPL expressions go through the compiler pipeline, so visibility enforcement is automatic — no special exemption in v1.

**Future direction** (deferred to when remote REPL attach lands): a `--privileged` flag could exempt the REPL from visibility enforcement for debugging. Design details deferred.

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

**Internal classes as return types:** An internal Value class can be returned from a public method. The caller receives an opaque value — they can send messages to it (structural typing) but cannot name the class for instantiation or type annotations. This is intentional: visibility controls naming, not data flow.

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
**Lesson:** Compile-time enforcement is sufficient. `pub(crate)` = `internal`.

### C# / Kotlin

**Mechanism:** `internal` keyword — assembly/module-scoped.
**Lesson:** Exact terminology match. Well-understood by industry developers.

### Java

**Mechanism:** `private`/`protected`/`public` + JPMS modules.
**Bypass:** `setAccessible(true)`, `--add-opens`.
**Lesson:** Even Java acknowledges escape hatches exist. The default path enforces boundaries.

### Erlang

**Mechanism:** `-export()` controls what other modules can call.
**Bypass:** `apply(Mod, Fun, Args)` works on anything.
**Lesson:** BEAM's own visibility is compile-time convention, not runtime enforcement.

### Smalltalk

**Mechanism:** No enforced visibility. Everything public. Private is a category/protocol convention.
**Lesson:** Works for single-image development. Breaks down for distributed packages — no stable API boundary.

## User Impact

**Newcomer** (from Python/JS/C#): Intuitive — `internal` is a familiar concept from C#/Kotlin. Modifier syntax matches existing `sealed`/`abstract` pattern. Error messages are clear and actionable.

**Smalltalk developer:** Departure from Smalltalk purity (everything is public). Justified by package ecosystem needs — Pharo's lack of visibility is a known pain point for library authors.

**Erlang/BEAM developer:** Maps naturally to `-export()` concept. Understands compile-time enforcement. Comfortable with BEAM's escape hatches via `apply/3`.

**Production operator:** Reduces API surface. Cleaner dependency boundaries. Would want future `--privileged` lockout for production REPLs.

**Tooling developer:** LSP can filter internal classes from cross-package completions. `__beamtalk_meta/0` visibility field enables tooling without runtime cost.

## Steelman Analysis

### Rejected: Use `private` Instead of `internal`

**Strongest argument:** Every mainstream language uses `private`. Newcomers would guess `private` before `internal`. Less cognitive load for the most common case. ADR 0070 Section 9 explicitly anticipated `private` — changing the keyword overrides the predecessor's stated direction and creates a terminology debt unless the future `private`/`protected` plan materializes.

**Counter:** `private` in Go, Rust, Java, and Kotlin means "most restrictive" — self-only or file-only. Using it for package scope creates a semantics mismatch with every major language. If Beamtalk later adds object-level encapsulation, `private` at both package and object level would be incoherent. `internal` is less familiar but more honest: it says "visible within this package," which is exactly what it means. The risk is that object-level `private` never ships and the less familiar keyword was chosen for nothing — an acceptable bet given the language is pre-1.0.

### Rejected: Method-Level `internal` in v1

**Strongest argument:** Library authors need to hide helper methods on public classes, not just whole classes. Without method-level `internal`, you expose every method on every public class.

**Counter:** In Beamtalk's one-class-per-file model, the natural pattern is to extract implementation details into dedicated helper classes — making class-level `internal` the primary tool for hiding them. Method-level `internal` adds complexity to dispatch (what happens when you send a message to an object whose class has internal methods — `doesNotUnderstand:` or a visibility error?) and FFI generation. There is no ecosystem data yet to quantify the split. Deferring lets real usage patterns from published packages reveal whether method-level visibility justifies the dispatch complexity.

### Rejected: REPL Always Exempt from Visibility

**Strongest argument:** Smalltalk's "all objects are inspectable" philosophy is core to the live-programming experience. If I can't poke at internals in the REPL, I've lost half of what makes Smalltalk-style development productive.

**Counter:** REPL expressions go through the compiler anyway, so enforcement is free and consistent. A production REPL connecting to live systems *should* respect boundaries by default. The future `--privileged` flag provides the escape hatch when you genuinely need to inspect internals — opt-in, not opt-out.

### Rejected: Runtime Enforcement

**Strongest argument:** Compile-time only means Erlang code can bypass visibility via raw BEAM module calls. Real security needs runtime checks.

**Counter:** BEAM fundamentally can't prevent `apply/3` calls — that's the VM's architecture. Runtime checks would add cost to every dispatch and still be bypassable. Java, Rust, Go, and Erlang itself all use compile-time enforcement as their primary visibility mechanism. The future Erlang→Beamtalk FFI provides Erlang-compile-time enforcement — the right level for cross-language calls.

### Tension Points

- Smalltalk purists prefer no visibility at all; every other cohort wants it
- Newcomers would prefer `private` as the keyword; language designers prefer reserving it
- BEAM veterans and operators align on compile-time enforcement being sufficient
- Method-level `internal` is the strongest deferred request — monitor ecosystem feedback

## Alternatives Considered

### Use `private` Keyword

Use `private` instead of `internal` for package-scoped visibility.

```beamtalk
private Object subclass: ParserState
```

**Rejected because:** `private` is the most natural keyword for future object-level encapsulation (self-only access). Using it for package scope burns design space. C#/Kotlin established `internal` as the industry-standard term for assembly/module-scoped visibility.

### Include Method-Level `internal` in v1

Allow `internal` as a method modifier to hide individual methods:

```beamtalk
Object subclass: Parser
  internal reset => ...  // only callable within this package
  parse: input => ...    // public
```

**Rejected because:** Adds complexity to dispatch (method resolution must check caller's package), FFI generation, and the conceptual model. Class-level `internal` addresses the primary use case. Defer to v2 based on real ecosystem feedback.

### No Visibility Controls (Status Quo)

Keep everything public, following Smalltalk tradition.

**Rejected because:** Breaks down for distributed packages — library authors have no way to mark implementation details, every class is public API, and internal refactoring risks breaking consumers. Pharo's package ecosystem demonstrates this pain point.

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
- Reserves `private`/`protected` for future object-level encapsulation
- `__beamtalk_meta/0` visibility field enables tooling and future FFI enforcement

### Negative

- Departure from Smalltalk "everything is public" tradition
- Compile-time only — Erlang code can bypass via raw BEAM module calls
- Adds a keyword to learn
- No method-level `internal` in v1 — helper methods on public classes remain exposed

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
