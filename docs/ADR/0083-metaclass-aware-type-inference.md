# ADR 0083: Metaclass-Aware Type Inference

## Status
Proposed (2026-05-23)

## Context

### Problem statement

Beamtalk has a full metaclass tower at runtime and in the class hierarchy
(`Metaclass → Class → Behaviour → Object → ProtoObject`, ADR 0036), and the type
annotation syntax for metatypes already exists: `TypeAnnotation::SelfClass`
(`Self class`) and `TypeAnnotation::ClassOf` (`X class`), added in BT-2034.
`Object>>class` is even declared `-> Self class` and `Class>>class -> Metaclass`.

But the **type checker discards this information**. In
`type_checker/type_resolver.rs`, both `Self class` and `X class` resolve to
`Dynamic`:

> `Self class` … returns `Dynamic(Unknown)`. … `<ClassName> class` metatype
> (BT-2034) … like `Self class` … resolves to Dynamic.

The consequence: any expression whose static type is "a class object" loses all
type information. Sends to it cannot be checked, and the type checker cannot route
them to class-side method lookup. Class-side resolution today is decided
*syntactically* — `is_class_side_receiver` is true only when the receiver is a
class literal (`String new`) or `self` inside a class method — never from a
*value's type* being a metaclass.

### Current state

What already exists and works:

- **The tower** (ADR 0036): `Behaviour`, `Class`, `Metaclass` classes with
  class-side method storage (`ClassInfo.class_methods`) and `find_class_method`.
- **A metaclass-tower fallback** in `type_checker/validation.rs`: when an
  instance-side lookup on a *class literal* fails, it re-checks against the
  `Metaclass → Class → Behaviour` chain.
- **The annotation syntax** `Self class` / `X class` (BT-2034), parsed and
  accepted.

What is missing — entirely in the type checker's *type representation*:

- `Self class` / `X class` resolve to `Dynamic`, not a tracked "metaclass-of-X"
  type.
- A method that *returns* a class value (`Collection>>species -> Self class`,
  `Object>>class -> Self class`, the reflection FFI that returns
  `List(Behaviour)`) yields `Dynamic` downstream.
- Sends on a metaclass-typed receiver are not routed through `find_class_method`.

This is the structural reason behind several `@expect` overrides found in the
stdlib audit (see BT-2254 / the ADR 0075 amendment for the FFI-element-type
sibling): for example `self species withAll:` in `Collection.bt` carries
`@expect dnu` purely because `species` returns `Dynamic` and `withAll:` cannot be
resolved as a class-side send.

### Constraints

1. **Additive / gradual** — consistent with ADR 0025. Code that does not annotate
   metatypes keeps working; unresolved metatypes still fall back to `Dynamic`.
2. **Reuse the existing tower** — must route through `find_class_method` and the
   `Metaclass` chain already present, not a parallel mechanism.
3. **No runtime/codegen change** — this is a static-analysis precision change
   only. Dispatch already works at runtime.
4. **Precision increase is opt-in pressure** — turning `X class` into a real type
   *will* surface new diagnostics where code previously rode on `Dynamic`; these
   must be absorbable (fix or annotate), not a hard break.

## Decision

Make metatypes first-class in inference:

1. **Represent a metaclass type.** Introduce a representation for "the class
   object whose instances are `T`" — the metatype of `T`. (Representation choice
   in Implementation below: a flag/marker on `InferredType::Known` vs. a new
   `Metaclass` variant.)

2. **Resolve the annotations.** `type_resolver` resolves
   `TypeAnnotation::SelfClass` to the metatype of the enclosing `Self`, and
   `TypeAnnotation::ClassOf { name }` to the metatype of `name`, instead of
   `Dynamic`.

3. **Route sends on metaclass-typed receivers to class-side lookup.** When a
   receiver's inferred type is the metatype of `T`, resolve the selector via
   `find_class_method(T, …)` (with the existing `Metaclass → Class → Behaviour`
   fallback). This generalizes `is_class_side_send` from a syntactic test to a
   type-driven one.

4. **`self class new` and friends type to `Self`.** `new` / `basicNew` on the
   metatype of `T` returns an instance of `T` (covariant with `Self` when the
   metatype is `Self class`).

### Slicing

- **Slice 1 (this ADR's core):** items 1–4 above for the *non-covariant-return*
  cases. Covers `species`, `self class new`, `obj class <selector>` reflection,
  class values returned from FFI, and the implicit class-side `new` override.
- **Slice 2 (deferred):** covariant `Self` on *class-side* method returns — e.g.
  `class withAll: -> Self` so `Set withAll:` types as `Set` and `List withAll:`
  as `List`. This is the species *return* side and the fiddliest part; it
  interacts with the existing `Self`-return inference and is split out to keep
  Slice 1 shippable.

### Example

```beamtalk
typed Object subclass: Collection(E)
  collect: block :: Block(E, R) -> Self =>
    result := (self inject: #() into: [:acc :each | acc addFirst: (block value: each)]) reversed
    self species withAll: result    // species -> Self class; withAll: resolved class-side — no @expect
```

```beamtalk
// Reflection: a class value flows with type, class-side sends are checked
cls := SystemNavigation default findClass: #Counter   // -> Behaviour (a class value)
instance := cls new                                    // class-side new resolved; -> Counter-ish
```

## Prior Art

- **Smalltalk (Pharo/Squdeak):** every class is an instance of its metaclass; the
  metaclass tower is the canonical model ADR 0036 follows. Smalltalk is
  dynamically typed, so it gets class-side dispatch "for free" but no static
  checking — this ADR adds the static layer.
- **Newspeak:** classes are first-class messages; metaclass-aware but again
  dynamic.
- **TypeScript:** models "the class object" with `typeof ClassName` and
  constructor types (`new () => T`). The metatype-of-`T` representation here is
  the direct analogue, adapted to the Smalltalk tower instead of structural
  constructor types.
- **Gleam:** no metaclasses; rejected as a model since Beamtalk committed to the
  tower in ADR 0036.

## User Impact

- **Smalltalk developer:** the species pattern, `self class new`, and reflective
  class-side sends type-check without `@expect` — matching their expectation that
  classes are real objects.
- **Newcomer:** fewer `@expect` directives to copy/cargo-cult; class-side calls
  on stored class values get the same hints as instance calls.
- **Erlang/BEAM developer:** reflection FFI that returns classes
  (`SystemNavigation`, `Behaviour`) gains downstream type info instead of
  decaying to `Dynamic`.
- **Operator:** no runtime change.

## Steelman Analysis

- **"Keep it Dynamic" (status quo):** class values are rare enough, and `@expect`
  documents the boundary honestly. Strongest where the metatype is genuinely
  unknown (heterogeneous class lists). Countered by the metaprogramming surface
  (SystemNavigation, builders, DSLs) being exactly where Beamtalk leans hardest on
  `Dynamic` — the cost concentrates in the most reflective code.
- **Tension:** covariant `Self` class-side returns (Slice 2) add real complexity
  for a narrow set of methods (`withAll:`, factory methods). Reasonable people
  disagree on whether that slice ever pays for itself; hence it is deferred, not
  committed.

## Alternatives Considered

### Keep metatypes as `Dynamic` (status quo)
Documented via `@expect`. Rejected as the default direction because it caps the
typed-coverage ceiling exactly in reflective/metaprogramming code, but retained as
the fallback for genuinely-unknown metatypes.

### Syntactic-only class-side detection (extend `is_class_side_receiver`)
Special-case more receiver *shapes* (e.g. `<expr> class`) without a real metatype.
Rejected: it does not compose — it cannot follow a class value through a variable,
a collection, or an FFI return, which is where the actual overrides live.

### Full structural metatypes (TypeScript-style constructor types)
Model class-side as arbitrary constructor signatures independent of the tower.
Rejected: duplicates the tower that ADR 0036 already provides; more machinery than
the Smalltalk model needs.

## Consequences

### Positive
- Removes the `species` overrides and subsumes the implicit class-side `new`
  override; unlocks typed reflection/metaprogramming generally.
- Generalizes class-side resolution from syntactic to type-driven, so class values
  flow through variables, collections, and FFI returns.
- Complements BT-2254 (FFI element types): that ADR types the *elements*; this one
  types the *class-side* of those elements.

### Negative
- Precision increase surfaces **new** diagnostics where stdlib code rode on
  `Dynamic` metatypes; must be fixed or annotated deliberately (not purely
  subtractive).
- Slice 2 (`Self` class-side covariance) is genuinely intricate and interacts with
  existing `Self`-return inference.
- Adds a representation decision (variant vs. flag) that touches `InferredType`
  pattern matches broadly.

### Neutral
- No runtime, codegen, or syntax change — static analysis only.
- `Metaclass`/`Class`/`Behaviour` remain valid explicit annotations; the metatype
  representation sits alongside them.

## Implementation

1. **Representation** — prefer a marker on `InferredType::Known` (e.g. an
   `is_meta` flag, or reuse `type_args` with a metatype tag) over a new variant,
   to minimize churn across the many `Known` match sites; evaluate during the
   spike.
2. **`type_resolver`** — resolve `SelfClass` / `ClassOf` to the metatype instead
   of `Dynamic` (`type_resolver.rs:64`, `:128`).
3. **`inference.rs`** — when the receiver type is a metatype, set
   `is_class_side_send` and look up via `find_class_method`; type `new`/`basicNew`
   on a metatype to the instance type.
4. **`validation.rs`** — extend the existing `Metaclass`-chain fallback to
   metatype-typed receivers (not just class literals).
5. **stdlib cleanup** — remove the `species` overrides; verify the implicit
   class-side `new` case; absorb new diagnostics.
6. **Slice 2 (separate issue)** — covariant `Self` class-side returns.

Affected components: type checker only (`type_resolver`, `inference`,
`validation`, `types`). No parser/codegen/runtime changes.

## Migration Path

Additive. Existing code is unchanged except that some sends on class values that
previously inferred `Dynamic` now type-check; any that surface a real warning are
fixed or annotated as part of the stdlib cleanup. Genuinely-unknown metatypes
still fall back to `Dynamic`.

## References
- Related issues: BT-2034 (`Self class` / `X class` annotation syntax), BT-2254
  (typed FFI collection element types — sibling precision work)
- Related ADRs: ADR 0036 (Full Metaclass Tower — the runtime/hierarchy this types),
  ADR 0025 (Gradual Typing and Protocols — the type system this plugs into),
  ADR 0075 (Erlang FFI Type Definitions — class-returning reflection FFI)
- Documentation: `docs/beamtalk-language-features.md` (type annotations,
  `@expect`)
