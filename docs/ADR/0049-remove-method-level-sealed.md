# ADR 0049: Remove Method-Level `sealed` — Infer Protection from `@primitive`/`@intrinsic`

## Status

Deferred (2026-03-15) — revisit post-0.1.0. Originally proposed 2026-03-02.

## Context

Beamtalk supports a `sealed` keyword on method definitions to prevent subclass overriding:

```beamtalk
sealed respondsTo: selector: Symbol -> Boolean => @intrinsic respondsTo
sealed isBehaviour => true
sealed spawn => @intrinsic actorSpawn
```

The compiler enforces this in `ClassHierarchy::add_module_classes()` via
`find_sealed_method_in_ancestors()`: if a subclass defines a method whose selector
matches a `sealed` method in any ancestor, a diagnostic is emitted.

### The `sealed` landscape after ADR 0048

ADR 0048 removes the `class` modifier keyword for class-side method definitions.
After that change, `sealed` is the only remaining method-level modifier keyword in
the grammar. Having a single modifier keyword is an awkward position: it complicates
the grammar (a modifier prefix that applies to some methods but not others), and it
sets a precedent that invites future modifier keywords — each of which risks collision
with valid method names (the problem ADR 0048 was written to solve for `class`).

### What `sealed` actually protects

A survey of all `sealed` methods in the stdlib reveals two distinct use cases:

**Case 1 — Primitive/intrinsic implementations (~39 methods):** Methods whose body
is `@primitive "name"` or `@intrinsic name`. These are backed by runtime primitives;
there is no meaningful Beamtalk implementation that could replace them. Examples:
`respondsTo:`, `fieldNames`, `spawn`, `superclass`, `allSuperclasses`, `doc`, `reload`.

**Case 2 — Semantic invariant methods (~15 methods):** Methods with pure Beamtalk
bodies that define invariants the object model depends on. Examples:
- `isBehaviour => true` / `isMeta => false` / `isMetaclass => false` (Behaviour)
- `isMeta => true` / `isClass => false` (Metaclass)
- `new => self error: "Actors must use spawn, not new"` (Actor guards)
- `yourself => self`, `printString => self name`, `name => ...` (small delegations)

### The compiler already has the information

`@primitive` and `@intrinsic` are `Expression` variants in the AST. The body of every
method is available during semantic analysis. The compiler can detect Case 1 — methods
whose body reduces to a single `@primitive` or `@intrinsic` expression — without any
user annotation.

Currently, `MethodInfo` (the class hierarchy's per-method record) has no field for
primitive/intrinsic status: the method body is discarded when `MethodInfo` is
constructed. Adding one field and propagating it from the AST is a small, contained
change.

### Case 2 without `sealed`

The semantic invariant methods (Case 2) would become overridable. In practice:

- `isBehaviour`, `isMeta`, `isMetaclass`, `isClass` — a subclass overriding these to
  return wrong values would break class hierarchy queries. However, `Behaviour` and
  `Metaclass` are either `abstract` or `sealed` at the class level; user-defined
  subclasses that do this are already in pathological territory. The Smalltalk
  tradition is to trust the programmer here.
- `Actor.new` / `Actor.new:` error guards — a subclass overriding `new` to call
  `super new` instead would bypass the guard. This is the weakest case for dropping
  `sealed`: the guard is a deliberate protocol constraint, not just an optimization.
  However, it is addressable at the class level by sealing `Actor` itself once actors
  are stable.
- `yourself => self` — trivially overridable; no semantic concern.

None of the Case 2 methods are in user-subclassable classes in the current stdlib.
`Behaviour` is `abstract`; `Metaclass` extends `sealed Class`; `Actor` is user-
subclassable but the error guards can be protected by sealing `Actor` if needed.

## Decision

1. **Remove the `sealed` method modifier keyword** from the grammar entirely. No
   Beamtalk source file may use `sealed` as a method prefix after migration.

2. **Infer `is_sealed = true`** in `MethodInfo` for any method whose body is a
   single `@primitive` or `@intrinsic` expression. The semantic analysis already
   constructs `MethodInfo` from the AST; detecting `@primitive`/`@intrinsic` bodies
   at the same point is a one-line extension.

3. **Class-level `sealed`** (preventing subclassing of a class) is unaffected — it
   remains a valid class declaration modifier. This protects `sealed Behaviour
   subclass: Class`, `sealed Class subclass: Metaclass`, etc.

### What this means in practice

```beamtalk
// Before — explicit sealed on each method
sealed respondsTo: selector: Symbol -> Boolean => @intrinsic respondsTo
sealed spawn => @intrinsic actorSpawn
sealed isBehaviour => true
sealed isMeta => false

// After — no sealed keyword; @intrinsic bodies are protected automatically
respondsTo: selector: Symbol -> Boolean => @intrinsic respondsTo
spawn => @intrinsic actorSpawn
isBehaviour => true   // now overridable; acceptable given Smalltalk tradition
isMeta => false       // now overridable; Behaviour is abstract anyway
```

A user attempting to override a `@primitive`/`@intrinsic`-backed method continues
to receive a compiler diagnostic:

```text
error: cannot override method `respondsTo:` — it is implemented as a primitive in `Object`
  --> src/MyClass.bt:5:3
   |
 5 |     respondsTo: selector => true
   |     ^^^^^^^^^^^^^^^^^^^^
   |
   = note: primitive methods cannot be replaced by Beamtalk definitions
```

### Result

`sealed` disappears as a method-level keyword. The grammar has zero method-level
modifiers. Class-side method declarations (ADR 0048) and return type annotations
(ADR 0047) are the only declaration-level additions, and neither is a modifier.

## Prior Art

**Pharo/Smalltalk:** No method sealing concept. All methods are overridable; the
object model relies on programmer convention, not enforcement. Pharo's `<primitive>`
pragma marks methods backed by VM primitives but does not prevent override — the
override simply shadows the primitive. Beamtalk's inference approach is stronger:
`@primitive`/`@intrinsic` bodies are non-overridable by design, without any user
annotation required.

**Java `final` methods:** `final` explicitly prevents override; no inference. The
explicit keyword is required because Java has no concept analogous to `@primitive`.

**Rust:** Methods on structs implementing a trait cannot be "sealed" in the same way;
`#[sealed]` is a community crate pattern, not a language feature. Rust's design
instead uses visibility (`pub(crate)`) and trait bounds to constrain overriding.

**Swift `final`:** Explicit `final` on methods and classes. No inference from body
content. Beamtalk's inference-from-body is novel; the closest analogue is the
compiler inferring that a `@primitive` binding cannot be meaningfully replaced.

**Kotlin `final`/`open`:** Methods are `final` by default; `open` opts into
overriding. This is the inverse of Beamtalk's current approach (overridable by
default, `sealed` to lock). Beamtalk post-ADR 0049 aligns with the Kotlin default
for primitive methods — they are effectively `final` — while keeping Smalltalk's
open default for all other methods.

## User Impact

**Newcomer:** Simpler mental model. Methods are overridable by default. Trying to
override a primitive gets a clear error. No need to learn a `sealed` keyword.

**Smalltalk developer:** Closer to Smalltalk orthodoxy — no modifier keywords on
methods. The `@primitive` protection is a reasonable BEAM-specific addition; Pharo
has analogous (if unenforced) conventions around primitive methods.

**Erlang/BEAM developer:** No runtime change. `@primitive`/`@intrinsic` dispatch is
unchanged; the compiler simply stops accepting overrides rather than waiting for the
programmer to annotate `sealed`.

**Tooling developer:** One fewer token kind to handle in syntax highlighting and LSP.
`@primitive`/`@intrinsic` are already first-class expression forms; their sealed
status is now derivable from the AST without inspecting `is_sealed` flags.

**Production operator:** No runtime impact.

## Steelman Analysis

### Keep `sealed` as an explicit keyword

- 🧑‍💻 **Newcomer**: "I want to write a class library and prevent users from
  overriding certain methods. Without `sealed`, I have no way to express 'this
  method is part of my class's contract and must not change.'"
- 🎩 **Smalltalk developer**: "Smalltalk doesn't have it, but Newspeak has module
  sealing for similar reasons. A general method-level `sealed` is a useful escape
  hatch for class library authors beyond stdlib."
- ⚙️ **BEAM developer**: "Explicit is better than inferred. A `sealed` annotation is
  documentation as well as enforcement — it tells the reader 'the author considered
  this and decided it must not change.'"

  **Why rejected:** The only current users of method-level `sealed` are stdlib
  classes. User-defined classes have zero `sealed` methods. Adding a modifier keyword
  to the language for a use case that exists nowhere outside the stdlib — and which
  the compiler can handle automatically for the primitive case — is over-engineering.
  If a future class library author genuinely needs method sealing, it can be
  reintroduced then; removing a keyword is harder than adding one.

### Infer sealed for all small/pure methods, not just primitives

- 🎨 **Language designer**: "The `isMeta => true` methods are equally invariant to
  `@intrinsic` methods. A compiler that infers `sealed` for literals and identity
  checks would cover all of Case 2 without any annotation."
- ⚙️ **BEAM developer**: "If the compiler can inline `true` / `false` return values,
  it could also enforce they're not overridden — consistent with how Kotlin inlines
  and seals `const val`."

  **Why rejected:** This path leads toward complex body analysis to determine which
  methods "should" be sealed. `@primitive`/`@intrinsic` is a well-defined, explicit
  annotation; literal bodies are not. The Case 2 methods (`isBehaviour => true`) are
  overridable in practice only by subclasses of abstract/sealed classes where it is
  already unusual to do so. Keeping the inference rule simple and tied to the explicit
  `@primitive`/`@intrinsic` annotation avoids overreach.

## Alternatives Considered

### Alternative A: Keep `sealed` but make it optional documentation

Mark `sealed` as a no-op hint — the compiler ignores it for enforcement but treats
it as documentation of intent. Rejected: a no-op keyword is confusing and creates
divergence between annotated and un-annotated primitive methods.

### Alternative B: Seal whole classes that contain only primitives

Instead of per-method inference, seal `Behaviour`, `Object`, `Actor` at the class
level. Rejected: these classes are intentionally subclassable — `Behaviour` is
`abstract` and the entire point of `Actor` is user subclassing.

### Alternative C: Keep `sealed` methods, remove the keyword via sugar

Internally track `sealed` but surface it only in generated documentation and tooling
rather than source syntax — annotate primitives as sealed in `generated_builtins.rs`
only. Rejected: `generated_builtins.rs` is a codegen artifact; semantic constraints
should live in the semantic analysis layer, not generated code.

## Consequences

### Positive
- Zero method-level modifier keywords in the grammar after this ADR and ADR 0048
- Primitive/intrinsic methods are protected without user annotation — the compiler
  derives the constraint from the body, which is where the information lives
- Simpler mental model: one rule ("you cannot override primitives") instead of two
  ("you cannot override `sealed` methods; also, these primitive methods happen to be
  `sealed`")
- ~54 `sealed` prefixes removed from stdlib source files

### Negative
- Case 2 semantic invariant methods (`isMeta`, `isBehaviour`, Actor guards) become
  overridable. This is the correct Smalltalk trade-off but represents a weakening of
  compiler enforcement for edge cases.
- Tooling that currently inspects `is_sealed` on `MethodInfo` for non-primitive
  methods will see `false` for those methods post-migration; any downstream logic
  must be reviewed.

### Neutral
- `MethodInfo` gains an `is_primitive: bool` field (or `is_sealed` is now derived
  from it). The class hierarchy construction changes in one place.
- `generated_builtins.rs` entries for builtin methods that are `@primitive`-backed
  will have `is_sealed` set during stdlib compilation rather than via the keyword.
- The `sealed` keyword remains valid at the class declaration level (e.g.,
  `sealed Behaviour subclass: Class`) — only the method-level use is removed.

## Implementation

**Phase 1 — Semantic analysis (`class_hierarchy/mod.rs`):**
- Add `is_primitive: bool` to `MethodInfo` (alongside existing `is_sealed`)
- In `add_module_classes()`, when constructing `MethodInfo` for a method, inspect
  the method body: if the body is a single `Primitive` or `Intrinsic` expression,
  set `is_primitive = true` and `is_sealed = true`
- The existing `find_sealed_method_in_ancestors()` check continues to work unchanged
  — it reads `is_sealed`; it does not need to know whether that flag came from an
  explicit keyword or body inference

**Phase 2 — Parser (`declarations.rs`):**
- Remove `sealed` as a valid method modifier token
- `sealed` remains valid as a class declaration modifier (before `subclass:`)
- Any source file using `sealed methodName` after this change should produce a parse
  error: "unknown method modifier `sealed` — did you mean to seal the class?"

**Phase 3 — Stdlib migration:**
- Remove all `sealed` prefixes from method definitions in `stdlib/src/*.bt`
- Class-level `sealed` declarations (e.g., `sealed Behaviour subclass: Class`) are
  untouched
- Rebuild stdlib and regenerate `generated_builtins.rs`

**Phase 4 — Diagnostic message:**
- Update the "cannot override sealed method" diagnostic to read:
  "cannot override method `{selector}` — it is implemented as a primitive in `{class}`"
- This is more informative than "sealed" and reflects the actual reason

**Affected components:** `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/mod.rs`,
`crates/beamtalk-core/src/source_analysis/parser/declarations.rs`,
`stdlib/src/*.bt`, `generated_builtins.rs`.

## References

- Related issues: BT-1003 (stdlib annotation audit — context for discovering the sealed landscape)
- Related ADRs: ADR 0013 (introduced `sealed` method modifier), ADR 0048 (removes `class` method modifier — leaves `sealed` as the sole method modifier, motivating this ADR)
- Discovered during: ADR 0048 drafting — noted that `sealed` was the last method modifier and asked whether it could be removed
