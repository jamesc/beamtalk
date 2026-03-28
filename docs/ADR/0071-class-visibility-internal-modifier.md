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

**Why `internal` and not `private`:** Reserves `private` and `protected` for potential future object-level encapsulation (self-only, hierarchy-visible). `internal` honestly describes what it is: visible within this package, not exported. Matches C#/Kotlin terminology.

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

**Future direction** (deferred to when remote REPL attach lands):
- **Development REPL:** Could exempt visibility via a `--privileged` flag
- **Production REPL:** Enforces visibility by default
- **Lockout option:** `beamtalk.toml` → `[repl] allow-privileged = false`

### Metadata

Add a `visibility` field to `__beamtalk_meta/0` (value: `public` or `internal`). Zero runtime cost — compile-time constant. Enables tooling and future Erlang→Beamtalk FFI enforcement.

### Modifier Interactions

| Combination | Valid? | Notes |
|-------------|--------|-------|
| `internal sealed` | Yes | Redundant for cross-package subclassing (can't be seen), but useful for intra-package subclass prevention |
| `internal abstract` | Yes | Internal base class, must be subclassed within the package |
| `internal typed` | Yes | Internal class with type annotation requirements |
| Stacking order | Any | `internal` can appear anywhere in the modifier list before the superclass name, like existing modifiers |

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

**Strongest argument:** Every mainstream language uses `private`. Newcomers would guess `private` before `internal`. Less cognitive load for the most common case.

**Counter:** Burns the most natural keyword for future object-level encapsulation (self-only access). C# proved `internal` works for package/assembly scope — developers learn the distinction quickly. Reserving `private` preserves design space for `private`/`protected` at the method level if the language evolves toward finer-grained encapsulation.

### Rejected: Method-Level `internal` in v1

**Strongest argument:** Library authors need to hide helper methods on public classes, not just whole classes. Without method-level `internal`, you expose every method on every public class.

**Counter:** Class-level `internal` covers ~80% of the need — most implementation details live in dedicated helper classes, not as stray methods on public classes. Method-level `internal` adds complexity to dispatch (what happens when you send a message to an object whose class has internal methods?) and FFI generation. Defer until real usage patterns from the package ecosystem reveal whether the remaining 20% justifies the complexity.

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

## Implementation

### Phase 1: Parser

Add `internal` to the modifier parsing loop.

**Files:**
- `crates/beamtalk-core/src/source_analysis/parser/declarations.rs` — add `internal` branch to modifier loop (alongside `abstract`, `sealed`, `typed`)
- `crates/beamtalk-core/src/ast.rs` — add `is_internal: bool` to `ClassDefinition`

### Phase 2: Semantic Analysis

Enforce cross-package `internal` access.

**Files:**
- `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/mod.rs` — add `check_internal_visibility()`
- `ClassInfo` — add `is_internal: bool`

**Note:** Requires package context in compilation — the compiler must know which package owns each class.

### Phase 3: Metadata

Emit visibility in `__beamtalk_meta/0` and `.app` metadata.

**Files:**
- `crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs` — add `visibility` to meta map
- `.app.src` class registry — add visibility field

### Phase 4: Tooling

Filter internal classes in LSP and REPL completions.

**Files:**
- `crates/beamtalk-lsp/` — filter cross-package completions
- REPL — filter tab completion for cross-package classes

## References

- Related issues: BT-714, BT-716
- Related ADRs: [ADR 0070](0070-package-namespaces-and-dependencies.md) Section 9 (calls for this ADR), [ADR 0049](0049-remove-method-level-sealed.md) (sealed method removal), [ADR 0016](0016-unified-stdlib-module-naming.md) (BEAM module naming), [ADR 0031](0031-flat-namespace-for-v01.md) (flat namespace)
