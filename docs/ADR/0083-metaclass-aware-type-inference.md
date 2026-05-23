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

- `Self class` / `X class` resolve to `Dynamic` (`type_resolver.rs:128–134`), not
  a tracked "metaclass-of-X" type. (`Object>>class` *is* declared `-> Self class`,
  but that annotation resolves to `Dynamic` today.)
- A method that *returns* a class value yields `Dynamic` downstream — and a method
  that returns a class value via a *plain* type loses even more: `Collection>>species`
  is declared `-> Object => self class` (Collection.bt:57), so `self species` infers
  as `Object`, not even a class.
- Sends on a metaclass-typed receiver are not routed through `find_class_method`.

This is the structural reason behind several `@expect` overrides found in the
stdlib audit (see BT-2254 / the ADR 0075 amendment for the FFI-element-type
sibling): for example `self species withAll:` in `Collection.bt` carries
`@expect dnu` because `species` is typed `-> Object`, so `withAll:` cannot be
resolved as a class-side send. Note this override is **not removable by metatype
typing alone** — it additionally requires re-declaring `species -> Self class`
(see Slicing, below).

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

1. **Represent a metaclass type — over a class *name*, not a parameterized class.**
   Introduce a representation for "the class object named `C`" — the metatype of
   `C`. **Critically, the class object is _not_ parameterized** (ADR 0068:511:
   "there's no `Result(Integer, Error)` class object"). The metatype carries a
   class *name* only (`metatype-of-List`, not `metatype-of-List(E)`). Instance
   type arguments are recovered at the *call site* via ADR 0068's existing
   class-method inference ("class method calls on generic classes as implicit
   type application sites") — e.g. `List withAll: aList(Integer)` infers the
   element type from the argument, not from the class object. This keeps 0083 and
   0068 consistent rather than in conflict.

2. **Subtyping into the tower.** `metatype-of-C <: Class <: Behaviour <: Object`,
   so a metatype value still satisfies `:: Class` / `:: Behaviour` parameters and
   FFI returns typed `List(Behaviour)`. This must compose with the existing
   `expected == "Class"` shortcut and class-literal/metaclass compatibility branch
   in `validation.rs` (BT-1877 / BT-2038, `validation.rs:686–700`).

3. **Resolve the annotations.** `type_resolver` resolves
   `TypeAnnotation::SelfClass` to the metatype of the enclosing class, and
   `TypeAnnotation::ClassOf { name }` to the metatype of `name`, instead of
   `Dynamic` (`type_resolver.rs:128–134`).

4. **Route sends on metaclass-typed receivers to class-side lookup.** When a
   receiver's inferred type is a metatype of `C`, resolve the selector via
   `find_class_method(C, …)` (with the existing `Metaclass → Class → Behaviour`
   fallback). This generalizes `is_class_side_send` from a syntactic test to a
   type-driven one, and applies a class-side method's declared return — including
   a `-> Self` return resolved to `C` (the same mechanism `new -> Self` needs).

5. **`self class new` and class-method returns.** `new` / `basicNew` on a metatype
   of `C` returns an instance of `C`. **Soundness caveat:** when `C` is abstract
   (`Collection`, `Behaviour`), `new` must *not* be blessed as a concrete instance
   — guard `infer_constructor_type` (`validation.rs:284`) against abstract
   metatypes, falling back to the abstract type / `Dynamic`.

### Slicing

The clean, self-contained win is **reflection / class-as-value typing**; the
species pattern needs an extra stdlib change and is honestly the harder case.

- **Slice 1 (this ADR's core):** items 1–5 above — metatype representation,
  tower subtyping, annotation resolution, type-driven class-side routing, and
  class-method return typing (`new`/`-> Self` resolved to the metatype's class).
  Cleanly covers: `aConcreteInstance class new` typed as the instance class,
  `obj class <selector>` reflective *resolution*, class values flowing through
  variables/collections, and (subsuming the separate small fix) the implicit
  class-side `new` override.

- **Species (Slice 1, but requires a stdlib change):** removing the
  `self species withAll:` override is *not* free. It requires re-declaring
  `Collection>>species -> Self class` (today `-> Object`). With that, at the
  definition site `self : Collection(E)` ⇒ `self species : metatype-of-Collection`,
  `withAll:` resolves class-side and its `-> Self` return is `Collection(E)`, which
  matches `collect: -> Self`. The DNU disappears. **The runtime species (the
  concrete subclass) remains statically invisible** — at the abstract definition
  site `Self` is the defining class, so the body types as `Collection`, not the
  subclass. That is sound (the declared return is `Self`) but it means metatype
  typing reproduces the *resolution*, not the subtype precision.

- **Slice 2 (deferred — depends on ADR 0068 Stage 2):** *parametric variance*
  precision for concrete class literals — `Set withAll: → Set`, `List withAll: →
  List`. ADR 0068 makes all type parameters invariant in Stage 1 (0068:350) and
  defers variance to Stage 2, so this slice is **blocked on that work**, not
  merely fiddly.

### Example

Today (`species -> Object`, the override is needed):

```beamtalk
typed Object subclass: Collection(E)
  species -> Object => self class      // returns Object — loses class-ness
  collect: block :: Block(E, R) -> Self =>
    result := (self inject: #() into: [:acc :each | acc addFirst: (block value: each)]) reversed
    @expect dnu                        // withAll: not resolvable on Object
    self species withAll: result
```

Proposed (Slice 1 + the `species` re-declaration):

```beamtalk
  species -> Self class => self class   // metatype-of-Self
  collect: block :: Block(E, R) -> Self =>
    result := (...) reversed
    self species withAll: result        // withAll: resolved class-side, -> Self — no @expect
```

Reflection — honest about precision limits:

```beamtalk
// allClasses returns List(Behaviour); the element is a class value but its
// INSTANCE type is statically unknown.
cls := SystemNavigation default allClasses first   // cls :: Behaviour
cls name                                            // class-side resolved (Behaviour>>name) — no @expect
cls new                                             // resolves, but typed Object: the instance type is unknown
```

## Prior Art

- **Strongtalk** (the most relevant prior art): a statically-typed Smalltalk that
  layered a structural type system over exactly this metaclass model. It typed
  class-side protocols separately from instance protocols and used `Self`-types for
  the species/`new` family — the same problem this ADR addresses. Adopted: the
  separation of class-side from instance-side method lookup and `Self`-return
  resolution. Adapted: Beamtalk uses the nominal class hierarchy (ADR 0036) rather
  than Strongtalk's structural protocols.
- **Smalltalk (Pharo/Squeak):** every class is an instance of its metaclass; the
  metaclass tower is the canonical model ADR 0036 follows. Dynamically typed, so it
  gets class-side dispatch "for free" but no static checking — this ADR adds the
  static layer.
- **Newspeak:** classes are first-class messages; metaclass-aware but again
  dynamic.
- **TypeScript:** models "the class object" with `typeof ClassName` and
  constructor types (`new () => T`). Note TypeScript *does* parameterize the
  constructor side; Beamtalk deliberately does **not** (ADR 0068 — the class
  object is unparameterized, instance params are inferred at the call site), so the
  analogue is the unparameterized metatype-of-name, not `typeof` over generics.
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

- **"Keep it Dynamic" (status quo) — newcomer/maintainer:** class values are rare,
  and `@expect` documents the boundary honestly. Strongest where the metatype is
  genuinely unknown (heterogeneous class lists from reflection, where `new` is
  `Object` anyway — see the reflection example). Countered by the metaprogramming
  surface (SystemNavigation, builders, DSLs) being exactly where Beamtalk leans
  hardest on `Dynamic`.
- **BEAM developer:** "class objects are just atoms/modules at runtime; static
  metatypes add checker complexity for something dispatch already handles." Genuine
  — and why this ADR is static-only with zero runtime change; the payoff is purely
  earlier diagnostics, which a BEAM dev may value less than a Smalltalker.
- **Language designer:** the strongest case *against* is the ADR 0068 boundary —
  parameterized metatypes were explicitly rejected. This ADR must stay on the
  unparameterized side of that line (Decision item 1); if a future need for
  `metatype-of-List(E)` precision appears, it reopens 0068, not just 0083.
- **Tension / where reasonable people disagree:** whether re-declaring
  `species -> Self class` is worth it given the runtime species stays statically
  invisible (the def-site only ever sees `Collection`). The win is removing one
  `@expect` and documenting intent; the subtype precision is not actually gained
  until 0068 Stage 2. A reviewer could reasonably say "leave the species override;
  ship Slice 1 for reflection only."

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
- Subsumes the implicit class-side `new` override and unlocks typed
  reflection/metaprogramming (class-side method *resolution* on class values).
- Generalizes class-side resolution from syntactic to type-driven, so class values
  flow through variables, collections, and FFI returns.
- Removes the `species` override **given the `species -> Self class` re-declaration**
  (not for free — see Negative).
- Complements BT-2254 (FFI element types): that ADR types the *elements*; this one
  types the *class-side* of those elements.

### Negative
- The `species` override removal requires a stdlib signature change
  (`species -> Self class`) and still does not recover the runtime species
  statically (def-site `Self` is the defining class).
- Precision increase surfaces **new** diagnostics where stdlib code rode on
  `Dynamic` metatypes; must be fixed or annotated deliberately (not purely
  subtractive).
- Abstract-class `new` is a soundness hazard (`Collection new` must not type as a
  concrete instance) — needs an explicit guard.
- Touches the `InferredType` representation (variant vs. flag) broadly, and must
  compose with the existing `expected == "Class"` / metaclass-compat branch in
  `validation.rs` (BT-1877 / BT-2038).
- Subtype precision for concrete generics (`Set withAll: → Set`) is **blocked on
  ADR 0068 Stage 2** (variance), not deliverable here.

### Neutral
- No runtime, codegen, or syntax change — static analysis only (the `species`
  re-declaration is a type-annotation change; its runtime body is unchanged).
- `Metaclass`/`Class`/`Behaviour` remain valid explicit annotations; the metatype
  representation sits alongside them and subtypes into them.

## Implementation

0. **Spike** — confirm the representation and trace the species case end-to-end on
   one method before broad work (the smallest proof: `Counter class new` infers
   `Counter`, and `self species withAll:` resolves with `species -> Self class`).
1. **Representation** — a marker on `InferredType::Known` (an `is_meta` flag
   carrying the class *name* only — **not** a parameterized class object, per
   ADR 0068) is preferred over a new variant, to minimize churn across the many
   `Known` match sites. Define `metatype-of-C <: Class <: Behaviour`.
2. **`type_resolver`** — resolve `SelfClass` / `ClassOf` to the metatype instead
   of `Dynamic` (`type_resolver.rs:128–134`).
3. **`inference.rs`** — when the receiver type is a metatype, set
   `is_class_side_send` and look up via `find_class_method`; apply class-method
   returns (incl. `-> Self` → the metatype's class, and ADR 0068 call-site param
   inference for `withAll:`-style methods).
4. **`validation.rs`** — extend the `Metaclass`-chain fallback to metatype-typed
   receivers; compose with the `expected == "Class"` shortcut (`:686–700`); guard
   `infer_constructor_type` (`:284`) so abstract metatypes don't yield concrete
   instances.
5. **stdlib** — re-declare `Collection>>species -> Self class`; remove the species
   `@expect dnu` overrides; verify/remove the implicit class-side `new` override;
   absorb new diagnostics.
6. **Tests** — type-checker unit tests (`type_checker/tests/`: a `self_class.rs` /
   metatype suite); stdlib BUnit (`stdlib/test/`) for species/reflection; ensure
   `just test-stdlib` stays warning-clean.
7. **Slice 2 (separate issue, blocked on ADR 0068 Stage 2)** — parametric variance
   for concrete-class-literal precision.

Affected components: type checker (`type_resolver`, `inference`, `validation`,
`types`) plus one stdlib annotation (`Collection.bt`). No parser/codegen/runtime
changes.

## Open Questions

1. **`species` re-declaration vs. body-driven inference.** Re-declaring
   `species -> Self class` is the simplest path (the call site uses the *declared*
   return, so inferring the body `self class` as a metatype does not help unless
   the signature changes). Confirm this is acceptable, or decide to special-case
   `self class` expression inference. *(Recommendation: re-declare — explicit and
   local.)*
2. **Representation:** `is_meta` flag on `Known` vs. a dedicated `Metaclass`
   variant — settle in the spike against the actual breadth of `Known` match sites.
3. **Is the species slice worth it now,** or ship Slice 1 for reflection only and
   leave the species override until 0068 Stage 2 buys real subtype precision?

## Migration Path

Additive. Existing code is unchanged except that some sends on class values that
previously inferred `Dynamic` now type-check; any that surface a real warning are
fixed or annotated as part of the stdlib cleanup. Genuinely-unknown metatypes
still fall back to `Dynamic`.

## References
- Related issues: BT-2034 (`Self class` / `X class` annotation syntax), BT-1952
  (`Self class` historically resolves to `Dynamic`), BT-1877 / BT-2038
  (`expected == "Class"` shortcut + metaclass-tower compatibility in
  `validation.rs`), BT-2254 (typed FFI collection element types — sibling), BT-2255
  (Slice 1 implementation)
- Related ADRs: ADR 0036 (Full Metaclass Tower — the runtime/hierarchy this types),
  ADR 0068 (Parametric Types — **constraint**: class objects are unparameterized
  (§511) and variance is deferred to Stage 2 (§350); this ADR stays inside both
  boundaries), ADR 0025 (Gradual Typing and Protocols — the type system this plugs
  into), ADR 0075 (Erlang FFI Type Definitions — class-returning reflection FFI),
  ADR 0077 (Type Coverage Visibility — interaction with `@expect`)
- Documentation: `docs/beamtalk-language-features.md` (type annotations,
  `@expect`)
