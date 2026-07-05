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

## Nominal vs structural — and why it matters here

This whole document hinges on one distinction, so it is worth stating plainly.

- **Nominal typing:** a type's identity is its *name*, and subtyping is
  *declared*. `Counter` is `Counter` because it is named so; `SavingsAccount <:
  Account` because the class declaration says `Account subclass: SavingsAccount`.
  Two classes with identical shape are still different types.
- **Structural typing:** a type's identity is its *shape* (its set of
  messages/fields), and subtyping is "has at least the required members" —
  no declaration needed. Anything responding to `asString -> String` inhabits
  `< asString -> String >`, whatever its class.

**Beamtalk today leans nominal.** The evidence is in the representation:
`InferredType::Known` carries a **`class_name`**, and subtyping is a walk of the
*declared* hierarchy (`superclass_chain`). The class tower, the metaclass tower
(ADR 0036), and the singleton-as-`Symbol` convention are all name-and-declaration
based. The one structural exception is **protocols** (ADR 0068 Stage 2):
conformance to `Printable` is automatic and shape-based — a structural layer
grafted onto a nominal core.

**The tension:** set-theoretic types are **structural at heart** — a type *is* a
set of values and subtyping *is* inclusion computed on structure. So the nominal
core is the single biggest impedance mismatch on the road to the north star.
Adopting the full engine means embedding each nominal class as a **basic element**
in the lattice and feeding its hierarchy in as **declared inclusion facts**
(`#foo ⊆ Symbol ⊆ Object`) — i.e. *manufacturing* structure out of names, rather
than reading it off the values.

**Aside — could Beamtalk be structural instead?** Yes, and it would fit the
set-theoretic model *more* naturally. Message-passing OO is structural by
construction (dispatch is on the message, not a declared interface), so
Smalltalk's duck typing is already structural-in-spirit; the nominal core is the
pragmatic departure Beamtalk made for tooling. A structural Beamtalk would make
message-sets (protocols) the type layer and demote classes to
implementation-sharing — essentially **Strongtalk** (Bracha & Griswold, 1993,
already cited in ADR 0068), a static structural type system for Smalltalk. The
two things usually called "blockers" — (a) **`Self`/binary-method typing** and
(b) **`doesNotUnderstand:`** — are better described as *constraints with known,
mainstream solutions*; the next two subsections work each one out, and a third
sketches what would actually change if Beamtalk made the move. This
document does **not** propose going structural; it records the option because
"adopt full set-theoretic types" and "how nominal is Beamtalk" are the same
question viewed from two sides.

### `Self` types and binary methods

The awkward case is a **binary method** whose argument should be "the same type
as the receiver": `Comparable` wants `< other :: Self -> Boolean`, `Equatable`
wants `= other :: Self -> Boolean`, and the numeric tower wants `+ other`. This
is the classic *binary-method problem* (Bruce, Cardelli, Castagna, Leavens,
Pierce, "On Binary Methods", 1995): binary methods and ordinary **width
subtyping conflict**. If `ColorPoint <: Point` and `Point` has `= : Point ->
Bool`, then a meaningful `ColorPoint` needs `= : ColorPoint -> Bool` — but the
argument is *contravariant*, so `ColorPoint`'s method is **not** a subtype of
`Point`'s. You cannot have both "`ColorPoint` is-a `Point`" and "`=` compares me
to my own type." This is a theorem, not a Beamtalk gap.

The mainstream fixes, in order of how well they fit Beamtalk:

1. **`Self` type resolved at the call site — Beamtalk already has this.**
   `TypeAnnotation::SelfType` (bare `Self`) resolves to `Dynamic(Unknown)` at
   annotation time and to the *static receiver class* at each call site
   (`type_resolver.rs:61`, `:152`). So `< other :: Self -> Boolean` on an
   `Integer` receiver already means `Integer -> Boolean` there. This is Kim
   Bruce's **matching** relation done pragmatically at the call site rather than
   as a separate judgement — and it is the mechanism a structural Beamtalk uses
   for binary methods.
2. **F-bounded polymorphism for *use-site* generic code.** A method that is
   generic over comparables carries the recursive bound: `sort: items ::
   Collection(T) where T <: Comparable(T)` — "T is any type that compares against
   itself." Structurally, "X conforms to `Comparable`" just *is* the
   self-referential check "X responds to `< : X -> Boolean`". This is exactly
   Swift `T: Comparable`, Scala `T <: Ordered[T]`, Rust `T: Ord`, Java
   `<T extends Comparable<T>>`, Haskell `Ord a =>`. Nothing exotic.
3. **Set-theoretic intersection arrows do better than F-bounds for the tower.**
   The north-star engine can give `+` an intersection type
   `(Integer -> Integer) & (Float -> Float)` and refine the argument per branch
   via typecase — Castagna's set-theoretic treatment of covariance/contravariance
   handles binary methods more precisely than F-bounds alone. So going
   set-theoretic *helps* here rather than making it worse.

The price is real but bounded: **`Self`-using protocols (`Comparable`,
`Equatable`) do not get free width subtyping** — you interact with them through
`Self`/F-bounded polymorphism, not by upcasting a `ColorPoint` to a `Point` that
has a binary `=`. Every production structural/OO type system (Swift, Scala,
Rust) accepts exactly this trade, so it is well-trodden, not experimental.

### Working around `doesNotUnderstand:`

`doesNotUnderstand:` (DNU) looks like it destroys structural typing — an
overriding receiver responds to *every* message, so it would structurally
satisfy *every* type. That conclusion comes entirely from a **wrong definition
of conformance**, and dissolves once fixed. Removing DNU is not on the table (it
is the metaprogramming that makes this a Smalltalk); the work is to type
DNU-*overriding* objects honestly. Note first that *default* DNU (raise
`does_not_understand`) is already type-friendly — such an object responds to
exactly its declared methods. Only a DNU **override** is the wrinkle.

1. **Conform on the *published* interface, not the runtime-possible one.**
   Define "A conforms to B" as "A's statically published interface covers B", not
   "A could respond at runtime." Then a `method_missing`-style object is **not** a
   universal subtype — its static interface is what it declares/publishes, and the
   messages its DNU handler also catches are simply *outside the typed contract*.
   Beamtalk is already committed to this stance: **ADR 0100** treats an unresolved
   send as *not* proof of a bug precisely because DNU/`perform:`/reflection exist,
   and only speaks up under closed knowledge.
2. **Model a DNU object as an open row, not "everything".** Structural theory
   already has the encoding: OCaml's `< foo : T; .. >` row variable and
   TypeScript's `[msg: string]: …` index signature both mean "responds to *at
   least* these, and maybe more." The `..` **is** the typed presence of a DNU
   handler — sound, because you may only *rely* on the listed messages; the rest
   need a guard.
3. **Let a DNU class publish a forwarded protocol → a typed proxy.** A remote/lazy
   proxy declares "I dynamically handle everything `Account` handles" (hypothetical
   `forwards :: Account`), and the checker types it structurally as `Account` —
   fully checked. This is Objective-C's `methodSignatureForSelector:` lifted to
   compile time.
4. **Fall back to `dynamic()` when the surface is genuinely open.** Un-typeable
   metaprogramming (a builder fabricating arbitrary accessors) is typed
   `dynamic()` — Beamtalk's existing `proxy :: Dynamic` opt-out. Gradual typing
   exists *for exactly this*, and (§2) `dynamic()` composes without spreading.
   Beamtalk's current protocol design already ships the optimistic version of
   this as conformance tier 3 ("DNU-overriding classes conform to every
   protocol"); the structural refinement keeps it as the fallback and *adds*
   options 1–3 for when a tighter interface is knowable.
5. **Interact via `respondsTo:` narrowing — already in the language.**
   `x respondsTo: #foo ifTrue: [x foo]` turns "might not understand" into a
   *checked* branch, and it is already a narrowing rule
   (`narrowing/rules/responds_to.rs`). Row polymorphism + `respondsTo:` narrowing
   is the complete, sound way to talk to an open-row/dynamic object.

The one genuine casualty is the **strong-arrow check** (§3): you cannot prove an
arrow "provably errors out of domain" for a receiver that might override DNU to
*handle* the out-of-domain input instead of erroring. So strong-arrow *soundness
claims* must be gated on sealed / non-DNU receivers — a narrow, well-understood
restriction, not a system-wide blocker.

**Prior art for both:** Strongtalk kept DNU and used a dynamic type as the escape
(options 4–5); Ruby's Sorbet types `method_missing` via `T.untyped`/shims; Swift,
Scala, Rust, and Haskell all resolve binary methods with `Self`/F-bounded
constraints exactly as above.

### What would change if Beamtalk went structural

Perhaps surprisingly: **class definition *syntax* barely changes.** You would
still write `Account subclass: SavingsAccount`, `field:`/`state:` declarations,
and methods exactly as today. What changes is what a class name **means in type
position**, and nearly all of that lives in the type checker.

**The semantic pivot.** Today `deposit: (into :: Account)` means "an `Account`
or a *declared* subclass" (`Known { class_name }` + `superclass_chain` walk).
Structurally, the same annotation means "**anything exposing `Account`'s message
interface**" — a class name in type position becomes shorthand for the *shape*
its instances expose (TypeScript's reading of a class name). Crucially, that
shape is the **method set only**: `field:`/`state:` declarations are private —
stdlib-wide, fields are accessed via `self.field` internally and messages
externally — so fields never enter the public type. Encapsulation is what makes
class-as-shape well-defined here.

**`subclass:` sheds its typing job — carefully stated.** Today `subclass:` does
two jobs: implementation reuse *and* declaring a subtype. Structurally,
subtyping is *derived from shape*, so the declaration stops being the source of
truth. But this is **not** a licence to "inherit without conforming":
Smalltalk-style inheritance only adds/overrides methods, so a subclass's method
set is a superset of its superclass's, and inheritance still *produces*
structural subtyping in the ordinary case. What decoupling actually changes:

- the **converse** becomes possible — a class can be `Account`-shaped, and
  usable as one, *without* inheriting from `Account`;
- a **signature-incompatible override** (narrowing a parameter, changing a
  return) now honestly breaks the subtype relation instead of being silently
  blessed by the declaration — the binary-method situation (see *`Self` types
  and binary methods*) surfacing as a checked fact rather than a soundness hole.

**What concretely changes elsewhere:**

| Aspect | Nominal (today) | Structural |
|---|---|---|
| Type identity | `Known { class_name }` — the name | a message-set; a class name abbreviates one |
| Subtyping | declared `superclass_chain` walk | shape inclusion (methods ⊇, signatures compatible) |
| Annotations | class / protocol names | names **plus anonymous inline shapes**, e.g. `:: < withdraw: Money; balance -> Money >` |
| Fields | private, not part of the type | unchanged — only the message surface types |
| Narrowing idiom | `isKindOf:` / `class =` central | **`respondsTo:`** becomes primary (already a rule); `class =` narrows to a shape |
| Exhaustiveness | `sealed` nominal hierarchies (BT-1299) | shifts toward **atom/tagged unions** (ADR 0102's machinery); `sealed` survives as a nominal island |
| Errors | "expected `Account`, got `String`" | shape diff: "`String` is missing `withdraw:`" |
| Tooling | "find subtypes" = hierarchy walk | "find conformers" = method-set search; rename refactors lose declared links to follow |

Three second-order points, double-checked against the codebase:

1. **Beamtalk is already further along than "leaning".** Structural conformance
   is *implemented*, not just specified: `protocol.rs`
   (`check_protocol_conformance_in_module`, ADR 0068 Phase 2b) performs
   shape-based checking at call sites today. "Going structural" is therefore
   promoting an existing mechanism from protocols-only to class-names-in-type-
   position — an extension of shipped code, not new theory.
2. **`sealed` does not disappear.** The stdlib uses `sealed` pervasively
   (`sealed typed Object subclass: Subscription`, sealed `Announcement`
   subclasses, `Result`). Sealed hierarchies remain *nominal islands* —
   closed-world sets where BT-1299-style exhaustiveness and identity semantics
   stay exact. This lands on the hybrid every production system chose:
   TypeScript (structural + brands), Scala (nominal + refinements), Pony
   (nominal `trait` + structural `interface`).
3. **Structure is the BEAM-native fit.** Erlang data *is* structural — maps,
   tagged tuples, atoms carry no nominal identity. Class-as-shape composes
   directly with §4 (structural products) and the unified interop story
   (ADR 0101): an Erlang map that satisfies a shape *is* that type, no wrapper
   blessing required.

So the realistic "structural Beamtalk" is not a rewrite of class definitions —
it is: keep classes and `sealed` for implementation and closed-world cases, let
class names in type position denote shapes, lean on protocols and
`respondsTo:` narrowing as the primary vocabulary, and let atoms/unions carry
exhaustiveness. Which is, deliberately, the same destination the set-theoretic
sections describe from the value-set side.

### Recommendation: hybrid, structural by increments

Should Beamtalk *move* to structural typing? **No — not as a project-level
pivot. Yes — as a direction the existing roadmap already walks.** The
reasoning, so it doesn't get re-litigated from scratch:

**Against a wholesale move:**

1. **Tooling is the value proposition, and nominal is better at tooling's
   bread-and-butter.** Crisp errors ("expected `Account`, got `String`" beats a
   method-set diff), O(depth) subtyping at LSP latency, declared links for
   rename/find-subtypes. A pivot trades away strengths in exactly the dimension
   Beamtalk competes on ("types serve the developer",
   `type-system-design.md`).
2. **The nominal machinery is deep and recent** — metaclass tower (ADR 0036),
   metaclass-aware inference (ADR 0083), sealed exhaustiveness (BT-1299), the
   singleton convention. A pivot strands that investment and reopens settled
   decisions.
3. **DNU + ADR 0100 cap the payoff.** The open-world policy forces the checker
   to stay conservative regardless of conformance model; the soundness win
   structural typing promises in a closed language mostly evaporates in an
   open one.
4. **Most code is unannotated** (gradual typing), and where annotations
   concentrate — public API parameters — **protocols already provide structural
   typing today** (`check_protocol_conformance_in_module`, shipped). The
   marginal gain of also making class names structural is small relative to
   the semantic rupture.

**For continuing the structural lean** — message dispatch is structurally
honest, BEAM data is structural (ADR 0101), and the set-theoretic north star
delivers structural *semantics* incrementally with each step paying for
itself. "Move to structural" and "walk the north star" are the same road; a
separate pivot would just be the riskier way to travel it.

**Sequence:**

1. **Now:** ADR 0102 (atoms, unions, narrowing, advisory exhaustiveness).
2. **Cheap, next:** make protocols the *recommended* annotation vocabulary for
   public API parameters (docs/guidelines, possibly a lint) — more structural
   typing using only shipped machinery.
3. **Evidence-driven, later:** anonymous inline shapes
   (`:: < withdraw: Money >`) *if* users demonstrably hit "I don't want to
   name a protocol for one call site." Not before.
4. **Only with the north-star engine:** the real pivot — class-name-in-type-
   position denotes shape — because it needs set-inclusion subtyping anyway,
   and doing it earlier means doing it twice.
5. **Never:** removing nominal classes or `sealed` (the closed-world islands
   do real exhaustiveness work).

**Revisit triggers:** protocol annotations becoming the dominant annotation
form in real code; interop typing demanding map-shapes; or a commitment to the
north-star engine. Any of these makes step 4 timely. Absent them, the hybrid
is the resting point — the same one TypeScript, Scala, and Pony converged on
from both directions.

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
- **Nominal-core mismatch.** The big one, detailed in *Nominal vs structural*
  above: Beamtalk's names-and-hierarchy core must be embedded as basic types
  with declared inclusions before a structural, set-theoretic engine can decide
  subtyping, plus the metaclass story has to be reconciled.
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
- What is Beamtalk's **gradual guarantee** statement precisely? (The
  **`doesNotUnderstand:`** interaction — strong arrows gated on non-DNU
  receivers, conformance on published interfaces — is worked out in *Working
  around `doesNotUnderstand:`* above; what remains open is the formal guarantee
  statement, not the strategy.)
- Should `Self`-using protocols use call-site resolution (today's mechanism) or
  explicit F-bounded quantification (`T <: Comparable(T)`) for *use-site* generic
  code — or both? (See *`Self` types and binary methods*.)
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
