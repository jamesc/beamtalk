# ADR 0102: Set-Theoretic Type Operators (Intersection and Negation) for Narrowing and Atom Exhaustiveness

## Status
Implemented (Phases 1, 2, 4, 5, 2026-07-06; Phase 1b, BT-2744, nominal-class
difference + the `class`/`isKindOf:` false branch, 2026-07-10 — design
specified and implemented, see §5).

## Implementation Tracking

**Epic:** [BT-2738](https://linear.app/beamtalk/issue/BT-2738)
**Issues:**

| Phase | Issue | Title | Size | Blocked by |
|---|---|---|---|---|
| 1 | [BT-2739](https://linear.app/beamtalk/issue/BT-2739) | intersect/difference operators + `Negation` variant | M | – |
| 2 | [BT-2740](https://linear.app/beamtalk/issue/BT-2740) | Unify `singleton_eq`/`is_nil` narrowing; delete `union_without` | M | BT-2739 |
| 2 | [BT-2741](https://linear.app/beamtalk/issue/BT-2741) | `class_eq`/`is_kind_of` true branch + `Never`-receiver policy | S | BT-2739 |
| 1b | [BT-2744](https://linear.app/beamtalk/issue/BT-2744) | Nominal-class difference + class false branch | M | BT-2741 |
| 4 | [BT-2742](https://linear.app/beamtalk/issue/BT-2742) | `\` difference annotation syntax | M | BT-2739 |
| 4 | [BT-2743](https://linear.app/beamtalk/issue/BT-2743) | `&` intersection syntax + stored `Intersection` variant | M | BT-2739 |
| 5 | [BT-2745](https://linear.app/beamtalk/issue/BT-2745) | Advisory singleton-union `match:` exhaustiveness | M | BT-2740 |
| 6 | [BT-2746](https://linear.app/beamtalk/issue/BT-2746) | E2E + docs + status flip | S | BT-2742, BT-2743, BT-2745 |

**Status:** Implemented — Phases 1, 2, 4, 5, 6, and 1b (BT-2744) have all
landed. Phase 1b closes the `class_eq`/`is_kind_of` false branch via
nominal-class difference (§5); it was sequenced separately because it is new
semantics, not a port, and was not required for `\`/`&` surface syntax or the
advisory `match:` exhaustiveness check, which only need atom-level
(singleton) difference.

## Context

### Problem statement

Beamtalk's type checker narrows types by pattern-matching a **fixed, growing
list of AST idioms**. Each recognised shape (`isNil`, `class =:= Foo`,
`isKindOf:`, `respondsTo:`, `x =:= #foo`, `isOk`/`isError`) is a separate
detector file under `type_checker/narrowing/rules/`, and the true/false branch
types are computed by bespoke, per-idiom logic. Adding a new form of narrowing
means adding a new file and a new entry in the `RULES` table
(`narrowing/rules/mod.rs`).

Underneath these idioms, the checker is already performing **set operations on
types by hand** — but only for the narrow cases each idiom needs:

- `union_without` (`inference.rs:2388`) is a **set-difference** that only knows
  how to subtract a single `#name` singleton from a *flat* union. It is the
  engine behind the `x =:= #foo` false-branch (`Integer | #infinity` minus
  `#infinity` ⇒ `Integer`).
- `non_nil_type` (`inference.rs:3306`) is a **second, independent set-difference**
  — remove `UndefinedObject` from a union — whose own doc comment calls
  `union_without` its "singleton counterpart". Two hand-rolled differences that
  don't share code.
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
  detector files, `narrowing/rules/mod.rs:49-57`) split into: those refactored
  with near-identical results (`singleton_eq`, `is_nil`), those changed
  *deliberately* with updated tests (`class_eq`/`is_kind_of` true branch), and
  those left untouched (`is_result`, `responds_to`). See §2 — this is not a
  uniform "refactor everything onto one core".

## Decision

Adopt the two missing **set-theoretic operators as first-class members of
`InferredType`** — intersection and negation/difference — and express narrowing
through a shared `intersect`/`difference` core **where it applies** (unifying
`singleton_eq` and giving `class`/`isKindOf:` a principled true branch; §2 is
explicit that this is *not* a uniform rewrite of all seven rules). Expose
difference in surface type-annotation syntax, and use the algebra to drive
advisory **exhaustiveness checking** for `match:` over singleton unions.

This is the middle of three possible commitments (see *Alternatives*): more
than an internal-only refactor, less than a full CDuce-style semantic-subtyping
engine.

### 1. Operators, mostly as normalising functions

The two operators are introduced primarily as **normalising functions** —
`intersect(A, B)` and `difference(A, B)` — that return *existing* `InferredType`
variants (`Known` / `Union` / `Never`) wherever the result is representable.
This is deliberate: `refine_singleton_narrowing` today already returns the bare
`matched` singleton, never a wrapped intersection, so most narrowing never needs
new storage. Subtyping remains **set inclusion** as the *mental model*, but the
equation `A <: B` iff `difference(A, B) == Never` holds only on the **atom
fragment** this ADR defines (singletons, `Symbol`, unions of them) — it is the
north-star invariant (contract #1), not a claim about these partial operators.
Nominal subtyping (`Integer <: Number`) remains the hierarchy walk, and
`difference(Dynamic, P) = Dynamic` deliberately says nothing about gradual
subtyping.

Only **irreducible** results need new stored variants — and in Phase 1 there is
exactly **one**:

```rust
enum InferredType {
    Known { class_name, type_args, provenance },
    Union { members, provenance },        // t1 | t2 | ...
    // NEW (Phase 1) — only when the result cannot collapse to the above:
    // `excluded` is an InferredType (a singleton or a normalised union of
    // singletons), NOT a raw atom name — `Symbol \ (#a | #b)` must be
    // representable. See the union-excluded and flattening rules below.
    Negation { base, excluded, provenance }, // co-finite complement, e.g. Symbol \ #foo
    // NEW (Phase 2, with `&` syntax) — see below:
    Intersection { members, provenance },  // irreducible conjunction: class ∩ protocol
    Meta { class_name, provenance },
    Dynamic(DynamicReason),                // unchanged: gradual hole
    Never,                                  // bottom / none()
}
```

**The stored `Intersection` variant is deferred to Phase 2**, by the same
precedent that defers nominal-class difference: under single inheritance,
class ∩ class always *reduces* (to the subclass, or to `Never` for unrelated
sealed classes), so `intersect()` in Phase 1 is a pure function over existing
variants. The only *irreducible* intersection is **class ∩ protocol**, and its
only producers are ADR 0068's `&` protocol composition — which 0068 *specifies*
(§Protocol Composition, `Collection(Object) & Comparable`) but which is **not
yet implemented** (`TypeAnnotation` has no intersection variant) — and a future
`responds_to`-narrowing refinement this ADR does not touch (§2). Storing
`Intersection` before either producer exists would tax every exhaustive match
for a variant nothing constructs. It lands in Phase 2 alongside the `&` surface
syntax; see §3 for the reconciliation.

Normalisation rules (`intersect` / `difference`; **not** a blind mirror of
`union_of` — the `Dynamic` handling is deliberately different, see below):

- **Rule priority: the `Dynamic` rules are checked first**; the general rules
  below apply only to non-`Dynamic` operands. This matters concretely for
  `intersect(Dynamic, Object)`: the Dynamic rule gives `Object` (a positive
  `x class =:= Object` test *refines* an unannotated variable), while the
  general identity rule would wrongly give `Dynamic` (no narrowing). An
  implementation that checks `intersect(T, Object) = T` first gets the wrong
  answer silently.
- `intersect(T, T)` ⇒ `T`; `intersect(T, Never)` ⇒ `Never`;
  `intersect(T, Object)` ⇒ `T` (non-`Dynamic` `T` — see priority above).
- **LHS-union distribution** (load-bearing for the `singleton_eq` port — §2
  group 1's "no behaviour change" claim depends on it):
  `intersect(Union[T1 .. Tn], P) = union_of(intersect(T1, P) .. intersect(Tn, P))`
  — so `intersect(Integer | #infinity, #infinity)` normalises to the bare
  `#infinity`, matching today's `matched` lookup, never a stored wrapper.
- Boundary rules: `difference(Never, P) = Never`; `difference(T, Never) = T`.
- `difference(Union[..], #foo)` drops the member — this is exactly today's
  `union_without`, which is deleted in favour of the general function.
- `difference(Symbol, #foo)` ⇒ `Negation{ base: Symbol, excluded: #foo }` —
  "any Symbol except `#foo`", the **co-finite atom set** Beamtalk cannot express
  today. `excluded` is an `InferredType`:
  `difference(Symbol, #a | #b)` ⇒ `Negation{ Symbol, #a | #b }` (union of
  singletons, kept in `union_of` normal form).
- **Same-base flattening** — nested negation must not escape normal form:
  `difference(Negation{ Symbol, E }, #bar)` ⇒
  `Negation{ Symbol, union_of(E, #bar) }`. Termination follows: `excluded`
  only grows within the finite set of singletons appearing in the program, so
  no chain of `difference` calls recurses or grows unboundedly.
- **Intersect through a complement** — required the moment narrowing *chains*
  (the false branch of `x =:= #foo` leaves `x :: Symbol \ #foo`; a later
  `x =:= #bar` in the same method computes
  `intersect(Negation{Symbol, #foo}, #bar)`). The general law:
  `intersect(Negation{B, E}, P) = difference(intersect(B, P), E)` —
  set-theoretically `(B \ E) ∩ P = (B ∩ P) \ E` — which reduces via the rules
  already given. For a singleton `s`: `s ∉ E` ⇒ `intersect(B, s)` (so
  `#bar`), `s ∈ E` ⇒ `Never`. Without this rule a `Negation` LHS hits an
  unhandled arm and every conservative fallback (panic, `Dynamic`, keep the
  `Negation`) is a wrong narrowing outcome.
- **Negation on the RHS (symmetric complement)**:
  `intersect(A, Negation{B, E}) = difference(intersect(A, B), E)` —
  set-theoretically `A ∩ (B \ E) = (A ∩ B) \ E`. Dormant in Phase 1 (narrowing
  pattern types are always `Known`/`Union`) but **fires in Phase 2**, when the
  `\` surface syntax puts a `Negation` in pattern position — a `match` on the
  first operand's variant would otherwise miss `(Known, Negation)` and fall
  through, with the same wrong-outcome consequences as the LHS case.
  Implementations may instead canonicalise (swap the `Negation` to the LHS and
  apply the single rule — intersection is commutative), provided they do so
  explicitly rather than by fall-through.
- **Nominal base case** (required by the complement rule above *and* by
  LHS-union distribution — `intersect(Integer | #infinity, #infinity)` only
  reduces because `intersect(Integer, #infinity) = Never`):
  `intersect(A, B) = B` when `B <: A` in the nominal hierarchy (e.g.
  `intersect(Symbol, #foo) = #foo`), symmetrically `= A` when `A <: B`; and
  `intersect(A, B) = Never` when `A` and `B` are hierarchy-unrelated (under
  single inheritance an instance's class would have to sit below both, which
  forces a chain relation — so unrelated classes are disjoint; e.g.
  `intersect(Integer, #foo) = Never`). This follows from set inclusion;
  stating it prevents implementations from silently falling through to a
  conservative default on the most common call of all.
  Nominal-class difference beyond atoms (`difference(Object, Number)`) is
  *not* defined by this ADR — see §2 and Consequences.
- **`Dynamic` is asymmetric and must be stated explicitly**:
  `intersect(Dynamic, P) = P` **and symmetrically `intersect(P, Dynamic) = P`**
  (a positive test refines an unknown value to `P`; `Dynamic` on *either* side
  acts as the identity for intersect — a match that handles only
  `(Dynamic, _)` and falls through on `(Known, Dynamic)` silently loses
  narrowing), but `difference(Dynamic, P) = Dynamic` (we cannot subtract from
  the unknown).
  This mirrors today's behaviour — `is_nil` sets the true branch to
  `UndefinedObject` regardless of receiver type, while `non_nil_type`
  (`inference.rs:3306`) leaves `Dynamic` unchanged on the false branch. It is the
  **opposite** of `union_of`, where any `Dynamic` member absorbs the whole union;
  implementing "like `union_of`" here would silently stop narrowing the common
  case of unannotated (`Dynamic`) receivers. Regression test required.
- `Meta` and `Dynamic` are opaque to `Negation`: `difference` leaves them
  unchanged rather than fabricating a complement. Symmetrically,
  `difference(T, Dynamic) = T` — we cannot prove the unknown removed anything.
- **Union on the right-hand side folds member-wise**:
  `difference(T, A | B) = difference(difference(T, A), B)` and
  `intersect(T, A | B) = union_of(intersect(T, A), intersect(T, B))`. The fold
  is required by both §4's residual computation
  (`difference(scrutinee, covered)`) and this ADR's own syntax example
  `Symbol \ (#reserved | #internal)`.
- **Generics (`type_args`) compare exactly, or not at all.** A member is
  subtracted / matched only under *exact equality including type args*; the same
  class name with different (or unknown) args is conservatively left alone —
  `difference(List(Integer) | List(Symbol), List(Integer))` removes exactly one
  member, and `difference(List(Integer) | List(Symbol), List)` is a no-op.
  Today's `union_without` compares by class *name only* (`as_known()`,
  `inference.rs:2400`) — safe solely because singletons carry no args; a general
  `difference` inheriting name-only comparison would wrongly subtract both
  `List` members. Property tests required.
- **`union_of` learns the new variant and its full absorption-law set**
  (`union_of` at `types.rs:442` matches exactly the current five variants with
  no wildcard, so this is an algebra decision, not a match-arm chore):
  - **Partial absorption** — adding back an exclusion removes it from
    `excluded`: `(Symbol \ (#foo | #bar)) | #foo` ⇒ `Symbol \ #bar`; an
    emptied `excluded` yields the bare base.
  - **Full restoration** — `(Symbol \ #foo) | #foo` ⇒ `Symbol` — is the
    *degenerate case* of partial absorption (`excluded` empties), **not a
    separate code path**: one rule implements both.
  - **Complement union (same base)** —
    `Negation{B, E1} | Negation{B, E2}` ⇒ `Negation{B, intersect(E1, E2)}`
    (set-theoretically `(B \ E1) ∪ (B \ E2) = B \ (E1 ∩ E2)`: "any Symbol
    except {#a,#b}" or "any Symbol except {#b,#c}" = "any Symbol except #b").
  - `Negation` equality needs a canonical ordering of `excluded` so
    order-independent union dedup keeps working — **specified: members sorted
    ascending by `class_name`**, so the form is deterministic across
    serialisation round-trips and independent implementations. Without the
    two laws above, logically-equal `Negation` members inside a `Union`
    escape `PartialEq` dedup and unions grow across repeated
    narrowing/widening passes.

### 2. Narrowing expressed as intersect / difference — where it applies

The target shape for a test that a value has pattern-type `P` against current
type `T` is:

- **true branch:** `intersect(T, P)`
- **false branch:** `difference(T, P)`

**This is an honest generalisation of `singleton_eq`, not a free port of all
seven narrowing rules** (`narrowing/rules/mod.rs` lists seven: `is_nil`, two
`is_result` rules, `responds_to`, `is_kind_of`, `class_eq`, `singleton_eq`).
They divide into three groups, and the ADR must not pretend otherwise:

1. **Already (nearly) this shape — `singleton_eq`, `is_nil`.**
   `refine_singleton_narrowing` (`inference.rs:2316`) hand-picks `matched` for
   one branch and `union_without` for the other; it becomes literally:
   ```rust
   info.true_type  = intersect(&current_ty, &matched);
   info.false_type = Some(difference(&current_ty, &matched));  // swapped if negated
   ```
   `type_admits_singleton` becomes `intersect(T, #foo) != Never`. One
   deliberate corner changes: today the true branch is `matched`
   *unconditionally* (`inference.rs:2326`), so `x :: Integer; x =:= #foo` types
   the true branch `#foo`; under `intersect` it becomes `Never`. The
   impossible-comparison diagnostic **already fires** for exactly this case
   (`check_impossible_singleton_comparison`), so only the (unreachable)
   branch's *type* changes — pin with a new test.
   `is_nil` is the same shape with `P = UndefinedObject` — its difference
   engine, `non_nil_type` (`inference.rs:3306`), is the second hand-rolled
   subtraction this ADR unifies. One documented divergence: `non_nil_type`
   turns a nil-*only* union into `Dynamic`, not `Never` (test
   `non_nil_type_all_nil_becomes_dynamic`), a deliberate open-world softening.
   The port **keeps** that behaviour, and the mechanism is **committed** (not
   an implementation choice): `difference` itself stays **pure** —
   `difference(Union[Nil], UndefinedObject)` is `Never`, because §4's
   exhaustiveness residual *requires* all-members-removed to mean `Never` —
   and the **`is_nil` narrowing call-site maps `Never` → `Dynamic`** as its
   own open-world policy. Softening is a narrowing decision, not algebra; a
   `difference`-internal special case would corrupt residuals, and a private
   `is_nil` wrapper would keep a second hand-rolled subtraction alive,
   defeating the unification. Porting blindly "for uniformity" would be a
   silent behaviour change.

2. **Ported, but a genuine behaviour change — `class_eq`, `is_kind_of`.** Today
   these set `true_type = P` *unconditionally* and `false_type = None`
   (`class_eq.rs`: `true_type: Known(name), false_type: None`). Moving the true
   branch to `intersect(T, P)` is *new* logic: `x class =:= Bar` where `T` and
   `Bar` are unrelated sealed classes would now yield `Never` and should route
   through the same impossible-comparison hint the singleton path already has
   (`check_impossible_singleton_comparison`, `inference.rs:2354`). One
   refinement, learned in implementation (BT-2741): the class hint is
   **provenance-gated** — it fires only when the receiver's type was
   *inferred* from value flow, and stays silent when it came from a declared
   annotation. Under gradual typing an annotation is an unverified promise,
   and `isKindOf:` is precisely how code verifies it at runtime, so a
   defensive guard against a declared type (stdlib `SystemNavigation
   referencesTo:`'s `(aClass isKindOf: Symbol)` check) is legitimate and must
   not warn; the true-branch narrowing to `Never` is *not* gated (sends there
   are silent per the `Never`-receiver policy, so it stays harmless). The
   singleton hint's behaviour (fires on declared types too, pinned by
   BT-2740) is unchanged precedent. Computing the
   *false* branch requires **nominal-class difference** (`difference(T, Bar)`
   over the class hierarchy), which §1 explicitly does *not* define. So closing
   the class/`isKindOf:` false-branch gap is its own design step (nominal
   negation semantics: membership, display, hover), tracked separately — not
   assumed done by this ADR.

3. **Not expressible as intersect/difference — `is_result`, `responds_to`.**
   `refine_result_narrowing` keeps the full, unchanged `Result(T, E)` on *both*
   branches: it narrows a runtime tag that has no `InferredType` representation
   (`difference(Result, Result)` would wrongly yield `Never`). `responds_to`
   narrows to *protocol conformance*, which today has its own representation
   path; it is untouched by this ADR, but is the designated **first consumer of
   the stored `Intersection` variant** in Phase 2 (`T & Protocol` is exactly
   what a `respondsTo:` true branch means) — noted so the Phase 2 design has a
   real producer, not just syntax.

So the deliverable is: unify group 1 immediately (with the two documented
corners), route group 2 through `intersect` for the true branch (false branch
gated on nominal-difference design), and leave group 3 alone. The detectors in
`narrowing/rules/` all stay — they still recognise AST *shape* and produce `P`.

### 3. Surface syntax — reuse `&`, add a difference operator

**Intersection already has a spelling — on paper.** ADR 0068 §Protocol
Composition *specifies* `&` in type annotations
(`sort: items :: Collection(Object) & Comparable`) — semantically an
intersection — but it is **not yet implemented** (no `TypeAnnotation` variant,
no `&` handling in type position). This ADR therefore does **not** invent a
second intersection keyword — implementing 0068's `&` and generalising it
beyond protocol conjunction is Phase 2 work here, where class ∩ class reduces
via the hierarchy (subclass, or `Never` for unrelated sealed classes — so
`Integer & String` *is* defined: it is `Never`) and class ∩ protocol is the
irreducible stored case. Only **difference/complement** is new surface syntax,
written `\` ("T without U"):

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

`Integer \| Symbol \ #foo` therefore parses as `Integer \| (Symbol \ #foo)`.
Within one operator the tier is left-associative (`A \ B \ C` = `(A \ B) \ C`).
**Mixing `&` and `\` without explicit grouping is a deliberate parse error** —
not left-associative resolution: `A & B \ C` is rejected with "parenthesise to
disambiguate". This is a grammar rule (the "incompatible infix operators"
pattern, as OCaml and Rust use for ambiguous operator mixes), chosen because
`(A & B) \ C` and `A & (B \ C)` differ and neither reading is obviously
dominant. Using operator symbols (not
`and`/`not` identifiers) also sidesteps ADR 0068's rule that an unrecognised
*identifier* in type position is an implicit method-local type parameter — a
type param spelled `and` would otherwise silently shadow the operator.

**Lexing is not symmetric with `|`, and the ADR must not pretend it is.** `|`
works in type position because it is a **dedicated token**
(`TokenKind::Pipe`, `token.rs:122`), deliberately split from `BinarySelector`.
`\` and `&` currently lex as ordinary `BinarySelector`s (`lexer.rs:436`), and
both are *live expression operators* — `&`/`|` are boolean ops and `\\`
(double backslash) is the Smalltalk **modulo** selector (see
`is_pure_binary_op`, `lint_validators.rs`). So the annotation parser must
either special-case `BinarySelector("\\")` / `BinarySelector("&")` in type
position, or promote them to dedicated tokens (which touches expression
lexing). Additionally, because the lexer greedily consumes selector characters,
the near-certain typo `Symbol \\ #foo` lexes as the *modulo* selector — Phase 2
must ship a targeted parse error for `\\` in type position ("did you mean
`\`?").

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
- **Known discoverability cliff:** the warning silently vanishes when inference
  widens the scrutinee (one `Dynamic`-returning arm, a reassignment) — the user
  cannot distinguish "exhaustive" from "checker gave up". An opt-in assertion
  (a marker on `match:` requesting exhaustiveness, analogous to TypeScript's
  `satisfies never` idiom, which would then diagnose "cannot verify: scrutinee
  is Dynamic") is the natural follow-up; deliberately **out of scope** here and
  left to a future issue so this ADR stays advisory-only.

  > **Amendment (2026-07-07, BT-2763):** implemented as `matchExhaustive:`, a
  > keyword variant of `match:` that asserts exhaustiveness at `Error`
  > severity and fails loudly (`Error`, naming the inferred scrutinee type)
  > when it cannot be verified. This advisory check (`match:`) is completely
  > unchanged by that follow-up. See
  > [ADR 0106](0106-opt-in-match-exhaustiveness-assertion.md).

### 5. Phase 1b design note — nominal-class difference (2026-07-10, BT-2744)

Answers the four open questions this ADR deferred in §1/§2 (Consequences:
"nominal-class difference is left undefined") and in BT-2744's `needs-spec`.
Design only — implementation is tracked by BT-2744.

**Difference over the class hierarchy.** Layered onto the existing nominal
base case (§1), using nominal subtyping `<:` (the `superclass_chain` walk):

- `difference(A, B)` where `B` is a strict nominal subclass of `A` (`B <: A`,
  `B != A`) ⇒ `Negation{ base: A, excluded: B }` — the class-hierarchy
  analogue of `difference(Symbol, #foo) = Negation{Symbol, #foo}`.
- `difference(A, B)` where `A <: B` (including `A == B`) ⇒ `Never` — every
  instance of `A` is also an instance of `B`, nothing survives.
- `difference(A, B)` where `A` and `B` are hierarchy-unrelated (the existing
  disjoint case) ⇒ `A` unchanged — nothing to remove.

This closes the `is_kind_of` false branch (§2 group 2): `false_type =
difference(T, Bar)`. **`class_eq`'s false branch is deliberately excluded**
(refinement found in implementation, see Amendment below) — `x class =:= C`
tests *exact* runtime-class identity, and its negation ("not exactly `C`")
does not exclude `C`'s subclasses the way `Negation`'s subtree-exclusion
semantics (Q1) require; only `isKindOf:`'s negation ("not `C` and not any
subclass of `C`") matches.

**Q1 — Membership.** `Negation{ base: A, excluded: B }` admits exactly the
classes `C` such that `C <: A` and `C` is not `<: B`. Membership is decided
per-candidate-class via the hierarchy walk, never by enumerating `A`'s
subclasses — open-world-safe by construction: a class introduced later by
hot reload is admitted correctly the first time it's checked, the same
posture as today's `isKindOf:` check.

**Q2 — Display.** `<base> \ <excluded>`, e.g. `Object \ Number` — identical
convention to the singleton case (`Symbol \ #foo`); no new rendering logic,
`display_for_diagnostic`/hover already render `Negation{base, excluded}`
generically once `excluded` can hold a class name.

**Q3 — Method lookup / conformance.** Resolves through `base`. Every class
admitted by `Negation{base, excluded}` is, by construction, a subclass of
`base`, so the receiver's statically-known protocol is `base`'s — exclusion
narrows *which* concrete classes can appear, never adds capability beyond
what `base` declares. A `Negation{base, excluded}`-typed receiver is checked
against `base`'s method surface, identically to a bare `base`-typed
receiver. Mirrors the singleton-through-Symbol convention (a singleton
checks against `Symbol`'s surface; a nominal negation checks against
`base`'s).

**Q4 — Interaction with ADR 0100.** Because lookup and conformance resolve
through `base` (Q3), the open-world receiver-knowledge classification
(`Dynamic`/`Open`/`ClosedComplete`, ADR 0100 Rule 1) for a
`Negation{base, excluded}` receiver is inherited directly from classifying
`base` — no separate classification rule needed. A DNU hint/warning derived
from a nominal complement is exactly as safe as one derived from `base`
directly, and is silenced under the same conditions (`base` is `Dynamic`,
has a DNU override, carries unloaded cross-package extensions, etc.).

**Scope note:** `excluded` for the nominal case is a single class (matching
`class_eq`/`is_kind_of`'s one-class comparisons) — generalising `excluded`
to a union of classes is not required to close the group 2 false branch and
is left for a future issue if a producer needs it.

> **Amendment (2026-07-10, BT-2744):** implemented. `difference` now takes
> an `Option<&ClassHierarchy>` (mirroring `intersect`) and applies the three
> rules above when a hierarchy is supplied and neither operand is a
> singleton; `refine_class_narrowing` (`inference.rs`) now sets
> `false_type = difference(current, C, Some(hierarchy))` for `isKindOf:`
> only, closing the group 2 false branch for that idiom. Method lookup /
> conformance (Q3/Q4) is implemented by resolving a `Negation`-typed receiver
> through its `base` at the top of `infer_message_send_with_receiver_ty` (and
> the `Cascade` dispatch path) before the rest of the existing `Known`-receiver
> dispatch runs — a deliberate simplification that also widens a
> `Self`-returning method's result back to `base` rather than re-wrapping the
> exclusion (the ADR specifies lookup/conformance parity with `base`, not
> flow-preservation of the exclusion through `Self`).
>
> **Correction (2026-07-10, BT-2744, found in adversarial review):** the
> original design note above did not distinguish `class_eq` from
> `is_kind_of` and specified `false_type = difference(T, Bar)` for *both*.
> That is unsound for `class_eq`: `x class =:= C` false means "`x`'s class is
> not exactly `C`", which does **not** rule out `x` being an instance of a
> *subclass* of `C` — but `Negation{base, excluded}`'s membership rule (Q1)
> always excludes `excluded`'s entire subtree. Concretely, `Character` is a
> sealed `Integer` subclass; for `x :: Number` holding a `Character`, `x
> class =:= Integer` is false (its class is `Character`) while `x isKindOf:
> Integer` is true. Narrowing `class =:=`'s false branch to `Number \
> Integer` would have wrongly excluded that live possibility and produced a
> false "comparison can never be true" hint on a subsequent, satisfiable
> `isKindOf: Integer` test. **Fixed:** `class_eq`'s false branch is left
> unnarrowed (`None`), exactly as it was before this issue — only its
> (already-shipped, BT-2741) true branch is affected by set-theoretic
> narrowing. `NarrowingInfo` gained a `class_test_is_kind_of: bool` field so
> `refine_class_narrowing` can distinguish the two idioms, which previously
> shared the untyped `class_test: Option<EcoString>` field with no way to
> tell them apart.

### REPL session

```
bt> x := someUnionReturning   "x :: Integer | #infinity"
bt> (x =:= #infinity) ifTrue: [x] ifFalse: [x + 1]
                                          ^^^^^
   in the false branch, x :: Integer  (#infinity removed) → x + 1 type-checks
```

Co-finite types surface through **LSP hover** (already in scope — hover
rendering is listed under affected components), which displays
`Symbol \ #foo` for a variable so narrowed. No new REPL command is added —
a `:type`-style command would be its own surface-parity decision
(`docs/development/surface-parity.md`) and is out of scope here.

### Error example

```beamtalk
d :: #north | #south := heading
d =:= #west
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
| **TypeScript** | Discriminated unions + control-flow narrowing; literal (singleton) types; `Exclude<T, U>` at the *type-operator* level but **no first-class negation type** in values. | We go slightly further than TS values by making negation a real `InferredType`, which is what makes both *atom* and *nominal-class* false-branch narrowing and exhaustiveness fall out (nominal-class difference: §5, BT-2744). |
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
set logic the codebase already contains four times (`union_of`, `union_without`,
`non_nil_type`, `type_admits_singleton`).

### Functions only — no stored `Negation` either
The true minimum between status quo and Alternative A: ship `intersect` /
`difference` as normalising functions, and when a difference is co-finite
(irreducible), fall back to today's behaviour (`false_type = None` — no
narrowing). Zero new `InferredType` variants, zero new match arms. Rejected
because the fallback silently discards exactly the new expressiveness: the
`Symbol`-typed false branch of `x =:= #foo` stays un-narrowed, hover cannot
display "everything except `#foo`", and Phase 2's `\` syntax and residual-based
exhaustiveness both *need* a representable co-finite result. Priced honestly:
this saves one variant's worth of match arms, at the cost of the feature's
point.

### Internal algebra only
Add `Intersection`/`Negation` internally, refactor `union_without` /
`type_admits_singleton` / branch computation onto them, but expose no syntax and
add no exhaustiveness. Rejected *as the terminal state* (adopted as phase 1):
the operators are the hard part, and the incremental cost of the surface syntax
and exhaustiveness on top is small (chiefly `TypeAnnotation` variants and a
residual check — see Implementation), so stopping short leaves most of the user
value unrealised.

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
- `InferredType` grows one variant in Phase 1 (`Negation`) and a second in
  Phase 2 (`Intersection`); every exhaustive `match` on it (equality, display,
  provenance, hover, **and `union_of` itself** — which has no wildcard arm and
  needs the `(Symbol \ #foo) | #foo ⇒ Symbol` absorption law) must handle them —
  though `intersect`/`difference` are normalising functions, so most results
  stay in existing variants (see §1).
- **Live `Never` branches are new.** Group 2 true branches (and group 1's
  impossible-comparison corner) type reachable code regions as `Never` for
  the first time. **Implemented (BT-2741): sends on a `Never`-typed receiver
  are silent** (silent-as-unreachable, per ADR 0100's conservatism — the
  branch is already flagged by the impossible-comparison hint at its source,
  so a second diagnostic per send inside it would be noise). This falls out
  of validation's existing structure (`check_instance_selector` is only
  reached for a `Known` receiver, so a `Never` receiver already produced no
  diagnostics before this ADR) and is now pinned by a regression test. A
  future unreachable-code hint remains a possible revision if implementation
  experience argues for it, but is not proposed here.
- **`TypeAnnotation` (`ast/expression.rs`) also grows variants** for `&`/`\`
  (the AST enum is already ad-hoc — `FalseOr`, `ClassOf`, `SelfType`); every
  exhaustive match over it and `type_resolver.rs` must be updated. This cost was
  easy to miss and is real.
- **Nominal-class difference** (`difference(Object, Number)`) was left
  undefined by Phase 1 — its membership/display/hover semantics needed their
  own design (§5), delivered by BT-2744, which closes the `class_eq`/
  `is_kind_of` *false* branch (§2 group 2).
- Normalisation (dedup, absorption, `Dynamic` asymmetry, `&`/`\`/`|`
  interaction) is fiddly and needs property tests. Termination must be argued
  (finite atom/class basis; no recursive blow-up).
- Surface `\`/`&` are harder to parse than `|` was: `|` is a dedicated token
  while `\`/`&` lex as live `BinarySelector`s (`\\` is modulo), requiring
  type-position special-casing or token promotion, plus the `\\`-typo parse
  error (see §3).

### Neutral
- No runtime, codegen, or hot-reload change — edit/compile-time only.
- The new exhaustiveness diagnostic is advisory (`Warning`/`Lint`), *not* a
  soundness guarantee — gradual annotations aren't runtime-enforced (§4).
- `Dynamic(reason)` is untouched and stays a checking hole; this ADR does **not**
  make it structural (that is the deferred north-star work), but keeps it
  isolable behind the operators per the north-star compatibility contract.

## Implementation

**Phase 1 — internal operators + group-1 unification (Alternative A core) — ~M:**
1. Add `intersect` / `difference` normalising functions and the **`Negation`
   variant only** to `InferredType` (`types.rs`) — the stored `Intersection`
   is Phase 2 (§1). Encode the `Dynamic` asymmetry, the RHS-union fold, the
   exact-`type_args` rule, and the `union_of` absorption law (§1), each with a
   regression/property test; do **not** mirror `union_of`'s Dynamic-absorbs rule.
2. Update all exhaustive matches: `PartialEq` (canonical `excluded` ordering),
   `display_for_diagnostic`, provenance accessors, `as_known`, hover rendering,
   **`union_of`**.
3. Rewrite `refine_singleton_narrowing` and `is_nil`'s branch computation
   (group 1), delete `union_without`, redefine `type_admits_singleton` as
   `intersect(T, s) != Never`. Behaviour pinned by existing narrowing/union
   tests **plus two new pins** for the documented corners: the impossible
   singleton test's true branch becoming `Never`, and `is_nil`'s
   nil-only-union → `Dynamic` softening being preserved.
4. Route `class_eq` / `is_kind_of` true branch through `intersect(T, P)` (group
   2, true branch only), wiring the impossible-comparison hint, **and decide the
   `Never`-receiver send policy** (silent-as-unreachable vs. hint). Leave their
   false branch, `is_result`, and `responds_to` (group 3) untouched.

**Phase 1b — nominal-class difference (prerequisite for the class false-branch) — ~M (new semantics, design-heavy) — done (BT-2744, 2026-07-10):**
5. Define `difference` over the class hierarchy (membership, display, hover for
   `Negation` of a nominal base). Only then close the `class_eq`/`is_kind_of`
   *false* branch. Sequenced separately because it is new semantics, not a port.
   Implemented per §5's design note.

**Phase 2 — surface syntax + advisory exhaustiveness — ~M (parser + validator):**
6. Implement ADR 0068's `&` (spec-only today) generalised to any-type
   intersection, add the stored `Intersection` variant (§1), and add `\`
   (difference) to the annotation grammar — **new `TypeAnnotation` variants**
   (`ast/expression.rs`) plus every exhaustive match and `type_resolver.rs`.
   Handle the token asymmetry (§3): special-case `BinarySelector("&")`/`("\\")`
   in type position or promote to dedicated tokens; ship the `\\`-typo parse
   error. Add the precedence tier (§3).
7. Add a **type-based, advisory** singleton-union exhaustiveness check to
   `match:`: compute `difference(scrutinee, covered)`; if non-`Never`, emit a
   `Warning`/`Lint` (never `Error`) gated by ADR 0100 completeness. Distinct from
   BT-1299's pattern-based error, which is unchanged.

**Affected components:** type checker (`semantic_analysis/type_checker/*`),
annotation parser + `TypeAnnotation` AST, `match:` exhaustiveness validator,
hover/diagnostic rendering (`queries/hover_provider.rs`). **Not** affected:
codegen, runtime, REPL output values.

## Migration Path

Backward compatible. `singleton_eq`/`is_nil` narrowing (Phase 1 group 1)
produces identical *results* except two documented corners — the impossible
singleton test's (already-diagnosed, unreachable) true branch becomes `Never`,
and `is_nil`'s nil-only-union → `Dynamic` softening is deliberately preserved —
both pinned by new tests alongside the existing narrowing/union suites. `class_eq`/`is_kind_of` true-branch narrowing (group 2) is a
*deliberate* refinement — it can newly report an impossible comparison where the
old code silently kept `P`; this is the intended improvement, routed through the
existing hint machinery, and any affected test is updated in the same change.
`&`/`\` syntax and the exhaustiveness diagnostic are additive; the latter is
advisory (`Warning`/`Lint`), so no previously-clean build newly fails — even when
a scrutinee is now *inferred* as a singleton union. No existing `.bt` source
changes meaning.

## References
- Related issues: Epic BT-2738 (implementation — see Implementation Tracking
  above for BT-2739…BT-2746); builds on BT-2617,
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
