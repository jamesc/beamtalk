# Set-Theoretic Types — North Star

**Status:** Vision / directional. Not a committed decision.
**Relationship to ADRs:** [ADR 0102](../ADR/0102-set-theoretic-type-operators.md)
takes the first pragmatic step toward this vision (intersection + negation as
`InferredType` operators, uniform narrowing, atom exhaustiveness). This document
describes the *full* destination that ADR 0102 was deliberately designed to stay
compatible with — so that reaching it is a **deepening, not a rewrite**.

**Audience:** compiler authors deciding how far to push the type system, and
anyone tempted to add a type-checker feature that would foreclose this path.

---

## The vision in one paragraph

A type **is a set of values**. Subtyping **is set inclusion**, decided
*semantically* (by asking "is this combination of types empty?") rather than by
walking a nominal class hierarchy. The type language is closed under **union**
(`or`), **intersection** (`and`), and **negation** (`not`), plus a first-class
gradual type **`dynamic()`** that lets typed and untyped Beamtalk interoperate
without runtime checks and without `dynamic()` "spreading everywhere". This is
the Castagna/Duboc/Valim design that Elixir (1.17+) adopted; Beamtalk's
singleton-as-`Symbol` convention, `Union` type, and hand-rolled `union_without`
are all partial, nominal approximations of it.

## What "full" adds beyond ADR 0102

ADR 0102 gives us the *operators* and normalisation for the atom/nominal cases
we narrow over today. The north star is five further steps.

### 1. Semantic subtyping via emptiness checking

Today subtyping is `class_name` comparison plus a `superclass_chain` walk.
Semantic subtyping replaces this with a single primitive: `A <: B` iff
`A and not B` is empty (`none()` / `Never`). Everything — union members, atom
complements, function domains — reduces to **one emptiness decision procedure**,
classically implemented over BDDs (binary decision diagrams) of type
constructors.

```
integer() and atom()   ==>  none()      "disjoint — emptiness holds"
#foo <: Symbol          <=>  (#foo and not Symbol) == none()
```

The payoff: the growing pile of special-case predicates
(`type_admits_singleton`, impossible-comparison checks, member-of-union tests)
all become "is this empty?" — no new predicate per shape.

### 2. Structural `dynamic()` replacing the `Dynamic` hole

Beamtalk's `Dynamic(reason)` is a **checking hole**: "skip all checking." The
north-star `dynamic()` is a **type with structure** — a *range* of types kept
"at the root" of any composite (`{ok, dynamic()}` normalises to
`dynamic({ok, term()})`). Intersecting with `dynamic()` makes any type gradual:
only a subset needs to statically hold.

The critical property is that `dynamic()` **does not spread**. When a value of
`dynamic()` reaches a *strong* operation (one Erlang guarantees will error on
bad input — see §3), the checker returns the operation's real codomain
(optionally `dynamic() and codomain`) instead of smearing `dynamic()` across
everything downstream. This is what makes gradual typing usable at scale: adding
one annotation tightens results *locally* instead of being swallowed by dynamic.

```
// today: a hole — inference stops
x :: Dynamic

// north star: a bounded range — inference continues through strong ops
x :: dynamic() and (Integer | Symbol)
x + 1        // Integer  (not dynamic()) — `+` is a strong arrow
```

This is the single biggest upgrade for Beamtalk's stated *gradual-typing* goal
(`type-system-design.md`): untyped code stays unannotated, yet annotations pay
off precisely where added.

### 3. Strong arrows and intersection function types

Function types become first-class. **Note the Beamtalk-specific caveat:** unlike
Erlang/Elixir, Beamtalk has **no multi-clause method heads** — a selector maps
to exactly one method body (the Smalltalk model), and case analysis happens
*inside* the body via `match:` (see §5 / BT-1299 exhaustiveness). So arrow
intersections here do **not** model overloaded clauses; they type a *single*
method whose **result type covaries with its argument type**:

```
// one method body; its result type depends on the input type
abs :: (Integer -> Integer) and (Float -> Float)
```

The intersection is the *signature*; a `match:` (or guard) in the body is what
actually discriminates. This matters most for BEAM interop and numeric-tower
methods, and less than it does in Erlang precisely because Beamtalk lacks clause
heads.

A **strong arrow** is a function statically provable to error on inputs outside
its domain — checked by *negating the domain and type-checking*: if the body
returns `none()` on out-of-domain input, the arrow is strong. Strong arrows are
what let `dynamic()` cross the static/dynamic boundary without runtime checks
(§2), because the underlying BEAM semantics already fail on invalid input.

**Open tension (see Open Questions):** Beamtalk's `doesNotUnderstand:` can make
*any* selector succeed at runtime, so "provably errors out of domain" needs a
careful definition — DNU-overriding receivers may have *no* out-of-domain
inputs at all.

### 4. Full structural products

Tuples, maps, and records typed structurally, with the same operators applying
inside them — `{ok, T} or {error, E}` for tagged returns, map field types,
negation inside products. This subsumes the ad-hoc handling of Erlang tagged
tuples (`{ok, V} | {error, R}`) that Beamtalk interop leans on today.

### 5. Negation everywhere

ADR 0102 introduces negation for atoms/nominal narrowing. The north star allows
`not T` for *any* `T`, which is what makes exhaustiveness, dead-branch
detection, and the strong-arrow check total rather than atom-only.

## Why not now

- **Cost.** A sound emptiness/BDD decision procedure plus normalisation is a
  substantial, subtle engine (Elixir's took years and a PhD). It is disjoint
  from Beamtalk's current nominal comparison.
- **Nominal-core mismatch.** Beamtalk is deliberately nominal — classes, the
  metaclass tower (ADR 0036), structural protocols layered on (ADR 0068 Stage 2).
  Full semantic subtyping is structural; adopting it means embedding nominal
  classes as *basic types* in the lattice with declared inclusions, and
  reconciling the metaclass story.
- **Unspent benefit.** Arrow/intersection-function typing and a structural
  `dynamic()` buy the most when there is a large typed surface to check. Today
  the payoff is narrowing and atom exhaustiveness — exactly ADR 0102's scope.

## Compatibility contract

For the north star to remain a *deepening* rather than a *rewrite*, work done
under ADR 0102 (and any type-checker change before then) must preserve these
invariants:

1. **Subtyping is set inclusion.** Never introduce a subtyping rule that is not
   expressible as "`A and not B` is empty." The singleton-as-`Symbol` convention
   already obeys this; keep it that way.
2. **Operators normalise, and normalisation is centralised.** `union_of` /
   `intersect` / `difference` are the *only* places types are combined, so the
   decision procedure can later be swapped underneath them.
3. **Nominal types are basic elements.** `Known { class_name }` and `Meta` must
   read as opaque *basic types* in the lattice — never assume the algebra needs
   to look inside a class name beyond identity + declared inclusions.
4. **`Dynamic` is isolable.** Keep `Dynamic(reason)` behind the same combinators
   so it can be replaced by a structural `dynamic()` (a range kept at the root)
   without touching call sites. Do **not** let `Dynamic` leak special-case
   handling into unrelated match arms.
5. **Provenance survives operators.** `TypeProvenance` must thread through
   intersection/negation as it does through union, so error messages keep
   working when the engine deepens.

## Migration sketch (if/when we commit)

- **Nominal → basic types:** each class becomes a basic type; the hierarchy
  (`superclass_chain`) becomes a set of *declared inclusions* fed to the
  emptiness checker (`#foo <: Symbol <: Object` are just declared subset facts).
- **`Dynamic(reason)` → `dynamic(t)`:** the reason-carrying hole becomes a
  bounded range; sites that today branch on `Dynamic` to "skip checking" instead
  intersect with `dynamic()` and continue.
- **Methods → arrows:** method signatures become arrow types. Beamtalk has no
  clause heads, so intersection arrows type a *single* method whose result
  covaries with its argument type (the in-body `match:`/guard does the
  discrimination); the strong-arrow check gates dynamic-boundary inference.
- **Decision procedure:** introduce BDD-based emptiness under the existing
  `intersect`/`difference`/`union_of` API (contract invariant #2), swap the
  nominal shortcut for it incrementally, behind the same call sites.

## Open questions

- How do **structural protocols** (ADR 0068 Stage 2) relate to arrow
  intersections — are protocol conformance obligations expressible as arrow
  subtyping, or do they stay a separate structural check?
- Does the **metaclass tower** (`Meta`) embed cleanly as basic types, or does
  `C class` subtyping need bespoke inclusions?
- What is Beamtalk's **gradual guarantee** statement precisely, and which
  operations are "strong arrows" given DNU (`doesNotUnderstand:`) can make *any*
  selector succeed at runtime?
- Performance envelope of emptiness checking at REPL/LSP edit-time latency.

## References

- **Elixir gradual set-theoretic types:**
  [docs](https://hexdocs.pm/elixir/main/gradual-set-theoretic-types.html) ·
  [Strong arrows — elixir-lang.org](https://elixir-lang.org/blog/2023/09/20/strong-arrows-gradual-typing/)
- **Theory:** Giuseppe Castagna, Guillaume Duboc, José Valim — "The Design
  Principles of the Elixir Type System"; Castagna et al. semantic subtyping /
  CDuce line of work.
- **Beamtalk:** [ADR 0102](../ADR/0102-set-theoretic-type-operators.md) (the
  first step) · [ADR 0068](../ADR/0068-parametric-types-and-protocols.md)
  (unions, narrowing, the noted difference-types gap) ·
  [ADR 0025](../ADR/0025-gradual-typing-and-protocols.md) ·
  [ADR 0036](../ADR/0036-full-metaclass-tower.md) ·
  [type-system-design.md](type-system-design.md)
