# ADR 0102: Set-Theoretic Type Operators (Intersection and Negation) for Uniform Narrowing

## Status
Proposed (2026-07-05)

## Context

### Problem statement

Beamtalk's type checker narrows types by pattern-matching a **fixed, growing
list of AST idioms**. Each recognised shape (`isNil`, `class = Foo`,
`isKindOf:`, `respondsTo:`, `x = #foo`, `isOk`/`isError`) is a separate
detector file under `type_checker/narrowing/rules/`, and the true/false branch
types are computed by bespoke, per-idiom logic. Adding a new form of narrowing
means adding a new file and a new entry in the `RULES` table
(`narrowing/rules/mod.rs`).

Underneath these idioms, the checker is already performing **set operations on
types by hand** — but only for the narrow cases each idiom needs:

- `union_without` (`inference.rs:2388`) is a **set-difference** that only knows
  how to subtract a single `#name` singleton from a *flat* union. It is the
  engine behind the `x = #foo` false-branch (`Integer | #infinity` minus
  `#infinity` ⇒ `Integer`).
- `type_admits_singleton` (`inference.rs:2425`) is a **membership / subtyping
  test** hand-written for the singleton-vs-`Symbol` case.
- `InferredType::union_of` (`types.rs:442`) is the **union** operator, with
  flattening and deduplication.

There is no general algebra tying these together. The true branch of a test is
computed one way, the false branch another, and each idiom re-derives both.
ADR 0068 explicitly flagged the missing piece:

> False-branch complement types (`ifFalse:` knowing "x is NOT Integer" —
> requires difference types)

as a known limitation. We have difference — but only for singletons removed
from a flat union.

### Current state

Singletons are represented as `InferredType::Known { class_name: "#foo", .. }`
— the `#` prefix *is* the singleton marker — and treated as subtypes of
`Symbol` (the "singleton-as-Symbol convention", `type_resolver.rs:73`,
`inference.rs:2035`). Unions are a first-class `InferredType::Union` variant.
There is **no** intersection type and **no** negation type. `InferredType` is
otherwise nominal: `Known`, `Union`, `Meta`, `Dynamic(reason)`, `Never`.

`Dynamic(reason)` is a *checking hole* — it means "skip checking", not a type
with structure.

### Constraints

- **Nominal core.** Beamtalk leans nominal — classes, the metaclass tower
  (ADR 0036), structural protocols bolted on (ADR 0068 Stage 2). Any algebra
  must embed nominal class types as basic elements, not replace them.
- **Gradual-typing goal.** Types serve the developer; annotations are optional
  (`docs/internal/type-system-design.md`). Nothing here may make untyped code
  produce errors.
- **BEAM-native.** Singletons *are* Erlang atoms; the algebra must stay
  faithful to runtime atom semantics.
- **Controlled regressions.** The seven existing narrowing rules (across six
  detector files) split into: those refactored with *identical* results
  (`singleton_eq`), those changed *deliberately* with updated tests
  (`class_eq`/`is_kind_of` true branch), and those left untouched (`is_result`).
  See §2 — this is not a uniform "refactor everything onto one core".

## Decision

Adopt the two missing **set-theoretic operators as first-class members of
`InferredType`** — intersection and negation/difference — and rebuild narrowing
on top of them as a **uniform algebra** rather than per-idiom branch logic.
Expose negation in surface type-annotation syntax, and use the algebra to drive
**exhaustiveness checking** for `case`/multi-branch dispatch over singleton
unions.

This is the middle of three possible commitments (see *Alternatives*): more
than an internal-only refactor, less than a full CDuce-style semantic-subtyping
engine.

### 1. Operators, mostly as normalising functions

The two operators are introduced primarily as **normalising functions** —
`intersect(A, B)` and `difference(A, B)` — that return *existing* `InferredType`
variants (`Known` / `Union` / `Never`) wherever the result is representable.
This is deliberate: `refine_singleton_narrowing` today already returns the bare
`matched` singleton, never a wrapped intersection, so most narrowing never needs
new storage. Subtyping remains **set inclusion**, consistent with how singletons
already relate to `Symbol` (`A <: B` iff `difference(A, B) == Never`).

Only **irreducible** results need new stored variants, and there are exactly two:

```rust
enum InferredType {
    Known { class_name, type_args, provenance },
    Union { members, provenance },        // t1 | t2 | ...
    // NEW — only when the result cannot collapse to the above:
    Intersection { members, provenance },  // irreducible conjunction, e.g.
                                           // class ∩ protocol (unifies ADR 0068's `&`)
    Negation { base, excluded, provenance }, // co-finite complement, e.g. Symbol \ #foo
    Meta { class_name, provenance },
    Dynamic(DynamicReason),                // unchanged: gradual hole
    Never,                                  // bottom / none()
}
```

`Intersection` is the representation for ADR 0068's `&` protocol composition
(`Collection(Object) & Comparable`, 0068 §Protocol Composition) generalised to
any type — a single stored form for both, rather than two mechanisms. (0068
specifies `&` at the syntax level; `TypeAnnotation` does not yet carry an
intersection variant, so this ADR adds it — see Implementation.) See §3 for the
syntax reconciliation.

Normalisation rules (`intersect` / `difference`; **not** a blind mirror of
`union_of` — the `Dynamic` handling is deliberately different, see below):

- `intersect(T, T)` ⇒ `T`; `intersect(T, Never)` ⇒ `Never`;
  `intersect(T, Object)` ⇒ `T`.
- `difference(Union[..], #foo)` drops the member — this is exactly today's
  `union_without`, which is deleted in favour of the general function.
- `difference(Symbol, #foo)` ⇒ `Negation{ base: Symbol, excluded: #foo }` —
  "any Symbol except `#foo`", the **co-finite atom set** Beamtalk cannot express
  today. Nominal-class difference beyond atoms (`difference(Object, Number)`) is
  *not* defined by this ADR — see §2 and Consequences.
- **`Dynamic` is asymmetric and must be stated explicitly**:
  `intersect(Dynamic, P) = P` (a positive test refines an unknown value to `P`),
  but `difference(Dynamic, P) = Dynamic` (we cannot subtract from the unknown).
  This mirrors today's behaviour — `is_nil` sets the true branch to
  `UndefinedObject` regardless of receiver type, while `non_nil_type`
  (`inference.rs:3306`) leaves `Dynamic` unchanged on the false branch. It is the
  **opposite** of `union_of`, where any `Dynamic` member absorbs the whole union;
  implementing "like `union_of`" here would silently stop narrowing the common
  case of unannotated (`Dynamic`) receivers. Regression test required.
- `Meta` and `Dynamic` are opaque to `Negation`: `difference` leaves them
  unchanged rather than fabricating a complement.

### 2. Narrowing expressed as intersect / difference — where it applies

The target shape for a test that a value has pattern-type `P` against current
type `T` is:

- **true branch:** `intersect(T, P)`
- **false branch:** `difference(T, P)`

**This is an honest generalisation of `singleton_eq`, not a free port of all
seven narrowing rules.** The rules divide into three groups, and the ADR must
not pretend otherwise:

1. **Already this shape — `singleton_eq`.** `refine_singleton_narrowing`
   (`inference.rs:2316`) hand-picks `matched` for one branch and `union_without`
   for the other; it becomes literally:
   ```rust
   info.true_type  = intersect(&current_ty, &matched);
   info.false_type = Some(difference(&current_ty, &matched));  // swapped if negated
   ```
   `type_admits_singleton` becomes `intersect(T, #foo) != Never`. **Pure win, no
   behaviour change.**

2. **Ported, but a genuine behaviour change — `class_eq`, `is_kind_of`.** Today
   these set `true_type = P` *unconditionally* and `false_type = None`
   (`class_eq.rs`: `true_type: Known(name), false_type: None`). Moving the true
   branch to `intersect(T, P)` is *new* logic: `x class = Bar` where `T` and
   `Bar` are unrelated sealed classes would now yield `Never` and should route
   through the same impossible-comparison hint the singleton path already has
   (`check_impossible_singleton_comparison`, `inference.rs:2354`). Computing the
   *false* branch requires **nominal-class difference** (`difference(T, Bar)`
   over the class hierarchy), which §1 explicitly does *not* define. So closing
   the class/`isKindOf:` false-branch gap is its own design step (nominal
   negation semantics: membership, display, hover), tracked separately — not
   assumed done by this ADR.

3. **Cannot be intersect/difference at all — `is_result` (`isOk`/`isError`).**
   `refine_result_narrowing` keeps the full, unchanged `Result(T, E)` on *both*
   branches: it narrows a runtime tag that has no `InferredType` representation.
   `difference(Result, Result)` would wrongly yield `Never`. This idiom stays a
   bespoke rule; the algebra does not touch it.

So the deliverable is: unify group 1 immediately, route group 2 through
`intersect` for the true branch (with the false branch gated on nominal-difference
design), and leave group 3 alone. The detectors in `narrowing/rules/` all stay —
they still recognise AST *shape* and produce `P`.

### 3. Surface syntax — reuse `&`, add a difference operator

**Intersection already has a spelling.** ADR 0068 §Protocol Composition ships
`&` in type annotations (`sort: items :: Collection(Object) & Comparable`), which
is semantically an intersection. This ADR therefore does **not** invent a second
intersection keyword — it *generalises `&`* from protocol-only conjunction to any
two types. Only **difference/complement** is new surface syntax, written `\`
("T without U"):

```beamtalk
// "any symbol except the reserved ones"
tag :: Symbol \ (#reserved | #internal)

// residual after handling the known cases
handleRest: dir :: (#north | #south | #east | #west) \ #north => ...

// intersection is the existing 0068 operator, now general
thing :: Printable & Comparable
```

Precedence, lowest-binding to highest (all parsed only inside a type-annotation
context, so none collides with a binary message selector — `<` stays a message,
per ADR 0068):

| Operator | Meaning | Binding |
|---|---|---|
| `\|` | union | lowest |
| `&` | intersection | middle |
| `\` | difference | middle (left-assoc, same tier as `&`) |
| _(atomic type / `(...)`)_ | grouping | highest |

`Integer \| Symbol \ #foo` therefore parses as `Integer \| (Symbol \ #foo)`;
mixed `&`/`\` at the same tier require parentheses. Using operator symbols (not
`and`/`not` identifiers) also sidesteps ADR 0068's rule that an unrecognised
*identifier* in type position is an implicit method-local type parameter — a
type param spelled `and` would otherwise silently shadow the operator.

### 4. Exhaustiveness

Beamtalk has no Erlang-style multi-clause method heads — a selector maps to one
method body, and case analysis happens *inside* it via the `match:` keyword
message. `match:` **already performs exhaustiveness checking** (BT-1299) for
sealed types with constructor patterns. This ADR *extends that same check to
singleton-union scrutinees*, with the set-theoretic residual as its engine:
because difference is now total, a `match:` over a singleton union can compute
its **residual type** (`difference(scrutinee_type, covered patterns)`). When the
residual is `Never` the `match:` is exhaustive; when it is non-`Never` the
checker warns with the uncovered members.

```beamtalk
compass :: #north | #south | #east | #west := readHeading

result := compass match: [
  #north -> "up";
  #south -> "down";
  #east  -> "right"
]
// ⚠️ non-exhaustive: `#west` is not handled  (residual type: `#west`)
//    — either add a `#west ->` arm or a `_ ->` wildcard
```

**This is a type-based check, distinct from existing BT-1299 — and deliberately
weaker.** Today's `match:` exhaustiveness is **pattern-based**: it keys on the
constructor patterns in the arms and emits a hard `Diagnostic::error()`,
*precisely because* Beamtalk "is dynamically typed — there is no resolved
scrutinee type available at compile time." This ADR proposes trusting the
*inferred* scrutinee type instead, which is strictly less conservative:
inference can be wrong, and — under gradual typing — annotations are **not
runtime-enforced**, so a "provably exhaustive" static claim cannot promise the
`match:` won't crash at runtime (e.g. an FFI call typed `#north | #south` that
actually returns `#west`). Therefore:

- The singleton-union residual check is a **`Warning`/`Lint`, never a
  build-breaking `Error`** — it is advisory, not a soundness guarantee. The
  existing pattern-based BT-1299 error on sealed *constructor* patterns is
  unchanged.
- **Severity is governed by ADR 0100** (Open-World Diagnostic Policy), graded by
  how complete the checker's knowledge is: emitted only when the scrutinee's
  type is a *known-closed* singleton union — silent under `Dynamic`, an open
  `Symbol`, `perform:`, or a possible `doesNotUnderstand:` receiver. The same
  rule already governs `check_impossible_singleton_comparison`.
- Because it is additive and non-`Error`, it does **not** regress the "no meaning
  changes" migration claim: previously-clean code that is now *inferred* as a
  singleton-union scrutinee gets an advisory warning, not a new build failure.

### REPL session

```
bt> x := someUnionReturning   "x :: Integer | #infinity"
bt> (x = #infinity) ifTrue: [x] ifFalse: [x + 1]
                                          ^^^^^
   in the false branch, x :: Integer  (#infinity removed) → x + 1 type-checks
bt> :type (Symbol \ #foo)
   Symbol \ #foo   "co-finite: every atom except #foo"
```

### Error example

```beamtalk
d :: #north | #south := heading
d = #west
// ⚠️ comparison can never be true: `#west` is not a value of `#north | #south`
//    (this diagnostic already exists via check_impossible_singleton_comparison;
//     it is now a direct consequence of `intersect(T, #west) == Never`, not a
//     special-cased check)
```

## Prior Art

| Language / system | Approach | What we take / leave |
|---|---|---|
| **Elixir (1.17+)** — gradual set-theoretic types (Castagna, Duboc, Valim) | Types *are* sets of values; closed under `or` / `and` / `not`. Atoms are singleton types (`:ok` is both value and type); `boolean() = true or false`; `nil`, `true`, `false` are atoms. `atom() and not (:foo or :bar)` expresses co-finite atom sets. `dynamic()` is a *range* of types kept "at the root", enabling gradual typing that still tracks structure. | **Adopt:** the three-operator algebra and singleton-as-set model — it is exactly what our `#foo`/`Symbol`/`union_without` code approximates. **Adapt:** we reuse ADR 0068's `&` for intersection and add `\` for difference as surface syntax (not Elixir's `and`/`not` keywords), with internal `intersect`/`difference` operators, without adopting the full semantic-subtyping decision procedure. **Leave (for now):** replacing `Dynamic(reason)` with a structural `dynamic()`; BDD emptiness checking. |
| **CDuce / semantic subtyping** (the theory underneath Elixir) | Subtyping = set inclusion decided via emptiness of a Boolean combination of type constructors, implemented with BDDs. | The north-star. Rejected as the *implementation* today (XL, nominal-core mismatch); adopted as the *mental model* for our operators so a future upgrade is a deepening, not a rewrite. |
| **TypeScript** | Discriminated unions + control-flow narrowing; literal (singleton) types; `Exclude<T, U>` at the *type-operator* level but **no first-class negation type** in values. | We go slightly further than TS values by making negation a real `InferredType`, which is what makes uniform false-branch narrowing and exhaustiveness fall out. |
| **Gleam** (BEAM, Rust-implemented, our closest typed-BEAM peer) | Sound nominal HM; **no** union/intersection/negation, exhaustiveness via nominal custom types. | Confirms exhaustiveness is table-stakes on BEAM, but Gleam gets it from closed nominal sum types; we get it from the atom-union algebra, which fits Beamtalk's open, atom-rich style better. |
| **Dialyzer** (success typing) | Post-compilation, optimistic; has union/negation internally but no IDE-time narrowing. | Reaffirms why we compute this at edit time, not after codegen (`type-system-design.md`). |
| **Smalltalk (Pharo/Squeak)** | No static types; `#foo` is just a Symbol, no singleton *type*. | No prior art to preserve — this is purely additive tooling that does not touch message-passing semantics. |

## User Impact

- **Newcomer (from Python/JS/TS):** `Symbol \ #foo` reads naturally to
  anyone who has met TypeScript's `Exclude`/discriminated unions. The main new
  payoff they *feel* is the non-exhaustive-`match:` warning catching a missed
  atom — a bug class that is otherwise a runtime error.
- **Smalltalk developer:** Zero change to message-passing or runtime semantics.
  `#foo` is still a Symbol at runtime; this is edit-time tooling only. Purists
  who never annotate see no new obligations (gradual guarantee preserved).
- **Erlang/BEAM developer:** The algebra mirrors how they already think about
  atoms (`ok | error | timeout`, "any atom but these"). Exhaustiveness over
  atom unions matches the safety they'd want from `case`.
- **Operator:** No runtime, codegen, or hot-reload impact — purely a
  compile/edit-time analysis. Generated BEAM is unchanged.
- **Tooling developer:** *Simpler*, not harder. One narrowing core (intersect /
  difference) instead of N per-idiom branch computations; hover can render
  `Symbol \ #foo`; new idioms need only produce a pattern-type `P`.

## Steelman Analysis

### Chosen: Internal operators + surface syntax + exhaustiveness
- 🧑‍💻 **Newcomer:** "The `match:` warning literally tells me which atom I
  forgot — that's the feature I didn't know I wanted."
- 🎩 **Smalltalk purist:** "It's opt-in annotation sugar; my un-annotated code
  is untouched and messages still dispatch dynamically."
- ⚙️ **BEAM veteran:** "Co-finite atom sets (`Symbol \ …`) are how I model
  Erlang tagged returns; finally expressible."
- 🏭 **Operator:** "Compile-time only, generated code identical — nothing new to
  observe or debug in prod."
- 🎨 **Language designer:** "Two normalising operators unify `singleton_eq`,
  give `class`/`isKindOf:` a principled true branch, express co-finite atom
  sets, and enable advisory `match:` exhaustiveness — one small algebra, several
  payoffs."

### Rejected A: Internal algebra only (no surface syntax, no exhaustiveness)
- 🎨 **Language designer:** "Cleanest possible diff — refactor the hand-rolled
  set ops onto one core, ship nothing user-facing, zero syntax risk."
- ⚙️ **BEAM veteran:** "Lowest chance of destabilising the type checker."
- **Why not chosen:** leaves the two most valuable user outcomes — writable
  complements and exhaustiveness — on the table, when they are *cheap once the
  operators exist*. Kept as the recommended **first implementation phase**.

### Rejected B: Full semantic subtyping (CDuce engine, structural `dynamic()`)
- 🎨 **Language designer:** "Provably sound, one decision procedure for
  everything, matches Elixir exactly."
- 🧑‍💻 **Newcomer:** "The type system would 'just know' every relationship."
- **Why not chosen:** XL effort; a BDD emptiness engine and a structural
  `dynamic()` are a rewrite of the nominal core (`Known` + hierarchy walk) for
  benefit Beamtalk cannot yet spend. Documented in full as the **north star**
  this ADR is deliberately kept compatible with —
  [`docs/internal/set-theoretic-types-north-star.md`](../internal/set-theoretic-types-north-star.md),
  including the compatibility contract these operators must uphold.

### Tension points
- Designers split between A (minimal, safe) and B (complete, sound); the chosen
  option is the pragmatic midpoint and sequences A as phase 1 → chosen as
  phase 2.
- Purists want *nothing* new; newcomers/BEAM devs want the expressiveness. The
  gradual guarantee (untyped code unaffected) resolves this: the new power is
  entirely opt-in.

## Alternatives Considered

### Status quo — keep adding per-idiom rules
Every new narrowing shape stays a new file plus bespoke true/false logic, and
false-branch complements remain impossible beyond the singleton case (ADR 0068's
noted gap persists). Rejected: the idiom list grows unboundedly and duplicates
set logic the codebase already contains three times.

### Internal algebra only
Add `Intersection`/`Negation` internally, refactor `union_without` /
`type_admits_singleton` / branch computation onto them, but expose no syntax and
add no exhaustiveness. Rejected *as the terminal state* (adopted as phase 1):
the operators are the expensive part; withholding the syntax + exhaustiveness
that ride on them for free wastes the investment.

### Full semantic subtyping engine
Replace nominal comparison with BDD-based emptiness checking and a structural
`dynamic()`. Rejected as premature (see Steelman B); revisit if/when Beamtalk
needs full arrow/intersection-function typing. Documented as the destination in
[`docs/internal/set-theoretic-types-north-star.md`](../internal/set-theoretic-types-north-star.md).

## Consequences

### Positive
- Shared `intersect` / `difference` primitives replace `singleton_eq`'s
  hand-rolled branch logic (`union_without`, `type_admits_singleton`) with one
  normalising core, and give `class_eq`/`is_kind_of` a principled true branch.
- **Atom** complements become expressible — co-finite sets `Symbol \ #foo`,
  closing ADR 0068's difference-types gap *for the singleton case*.
- Advisory exhaustiveness over singleton-union `match:` scrutinees — a real
  bug-catcher — falls out of atom difference.
- The `check_impossible_singleton_comparison` diagnostic becomes a consequence
  of `intersect(T, s) == Never` rather than a special case.

### Negative
- `InferredType` grows **two** variants (`Intersection`, `Negation`); every
  exhaustive `match` on it (equality, display, provenance, hover) must handle
  them — though `intersect`/`difference` are normalising functions, so most
  results stay in existing variants (see §1).
- **`TypeAnnotation` (`ast/expression.rs`) also grows variants** for `&`/`\`
  (the AST enum is already ad-hoc — `FalseOr`, `ClassOf`, `SelfType`); every
  exhaustive match over it and `type_resolver.rs` must be updated. This cost was
  easy to miss and is real.
- **Nominal-class difference is left undefined** (`difference(Object, Number)`).
  Closing the `class_eq`/`is_kind_of` *false* branch (§2 group 2) needs its
  membership/display/hover semantics designed — deferred, not delivered here.
- Normalisation (dedup, absorption, `Dynamic` asymmetry, `&`/`\`/`|`
  interaction) is fiddly and needs property tests. Termination must be argued
  (finite atom/class basis; no recursive blow-up).
- Surface `\` adds parser surface and a new precedence tier.

### Neutral
- No runtime, codegen, or hot-reload change — edit/compile-time only.
- The new exhaustiveness diagnostic is advisory (`Warning`/`Lint`), *not* a
  soundness guarantee — gradual annotations aren't runtime-enforced (§4).
- `Dynamic(reason)` is untouched and stays a checking hole; this ADR does **not**
  make it structural (that is the deferred north-star work), but keeps it
  isolable behind the operators per the north-star compatibility contract.

## Implementation

**Phase 1 — internal operators + `singleton_eq` unification (Alternative A core):**
1. Add `intersect` / `difference` normalising functions and the `Intersection` /
   `Negation` variants to `InferredType` (`types.rs`). Encode the `Dynamic`
   asymmetry (§1) explicitly with a regression test; do **not** mirror
   `union_of`'s Dynamic-absorbs rule.
2. Update all exhaustive matches: `PartialEq`, `display_for_diagnostic`,
   provenance accessors, `as_known`, hover rendering.
3. Rewrite `refine_singleton_narrowing` (group 1), delete `union_without`,
   redefine `type_admits_singleton` as `intersect(T, s) != Never`. **No
   behaviour change — pin with existing narrowing/union tests.**
4. Route `class_eq` / `is_kind_of` true branch through `intersect(T, P)` (group
   2, true branch only), wiring the impossible-comparison hint. Leave their
   false branch and `is_result` (group 3) untouched.

**Phase 1b — nominal-class difference (prerequisite for the class false-branch):**
5. Define `difference` over the class hierarchy (membership, display, hover for
   `Negation` of a nominal base). Only then close the `class_eq`/`is_kind_of`
   *false* branch. Sequenced separately because it is new semantics, not a port.

**Phase 2 — surface syntax + advisory exhaustiveness:**
6. Generalise ADR 0068's `&` to any-type intersection and add `\` (difference)
   to the annotation grammar — **new `TypeAnnotation` variants**
   (`ast/expression.rs`) plus every exhaustive match and `type_resolver.rs`.
   Add the precedence tier (§3).
7. Add a **type-based, advisory** singleton-union exhaustiveness check to
   `match:`: compute `difference(scrutinee, covered)`; if non-`Never`, emit a
   `Warning`/`Lint` (never `Error`) gated by ADR 0100 completeness. Distinct from
   BT-1299's pattern-based error, which is unchanged.

**Affected components:** type checker (`semantic_analysis/type_checker/*`),
annotation parser + `TypeAnnotation` AST, `match:` exhaustiveness validator,
hover/diagnostic rendering (`queries/hover_provider.rs`). **Not** affected:
codegen, runtime, REPL output values.

## Migration Path

Backward compatible. `singleton_eq` narrowing (Phase 1 group 1) produces
identical *results*, now via the shared algebra (pinned by existing narrowing/
union tests). `class_eq`/`is_kind_of` true-branch narrowing (group 2) is a
*deliberate* refinement — it can newly report an impossible comparison where the
old code silently kept `P`; this is the intended improvement, routed through the
existing hint machinery, and any affected test is updated in the same change.
`&`/`\` syntax and the exhaustiveness diagnostic are additive; the latter is
advisory (`Warning`/`Lint`), so no previously-clean build newly fails — even when
a scrutinee is now *inferred* as a singleton union. No existing `.bt` source
changes meaning.

## References
- Related issues: BT-XXX (to be filed via `/plan-adr`); builds on BT-2617,
  BT-2624, BT-2631 (singleton narrowing / impossible-comparison diagnostics) and
  BT-1299 (`match:` exhaustiveness for sealed constructor patterns — extended
  here to singleton-union scrutinees)
- Related ADRs: ADR 0068 (Parametric Types and Protocols — unions, narrowing,
  and the noted difference-types gap), ADR 0100 (Open-World Diagnostic Policy —
  governs the severity of the exhaustiveness and impossible-comparison
  diagnostics this ADR relies on), ADR 0025 (Gradual Typing and Protocols),
  ADR 0053 (`::` annotation syntax), ADR 0036 (Metaclass tower), ADR 0083
  (Metaclass-Aware Type Inference — the `Meta` variant these operators must
  carry through)
- Documentation: `docs/internal/set-theoretic-types-north-star.md` (the full
  vision + compatibility contract this ADR upholds);
  `docs/internal/type-system-design.md` (Union Types and
  Narrowing); `crates/beamtalk-core/src/semantic_analysis/type_checker/`
  (`types.rs` `union_of`, `inference.rs` `union_without` /
  `type_admits_singleton` / `refine_singleton_narrowing`, `narrowing/rules/`)
- External: [Gradual set-theoretic types — Elixir](https://hexdocs.pm/elixir/main/gradual-set-theoretic-types.html);
  "The Design Principles of the Elixir Type System" (Castagna, Duboc, Valim);
  [Strong arrows — elixir-lang.org](https://elixir-lang.org/blog/2023/09/20/strong-arrows-gradual-typing/)
