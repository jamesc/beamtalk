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
- **No regressions.** The seven existing idioms and their tests must keep
  passing; this is a refactor of their *foundation*, plus new expressiveness on
  top.

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

### 1. Type representation

Extend `InferredType` with two operators. Subtyping remains **set inclusion**,
consistent with how singletons already relate to `Symbol`.

```rust
enum InferredType {
    Known { class_name, type_args, provenance },
    Union { members, provenance },        // t1 or t2 or ...
    Intersection { members, provenance },  // NEW: t1 and t2 and ...
    Negation { base, excluded, provenance }, // NEW: base and not excluded
    Meta { class_name, provenance },
    Dynamic(DynamicReason),                // unchanged: gradual hole
    Never,                                  // bottom / none()
}
```

Normalisation rules (mirroring `union_of`'s flatten/dedup discipline):

- `T and T` ⇒ `T`; `T and Never` ⇒ `Never`; `T and Object` ⇒ `T`.
- `Symbol and not #foo` is the canonical singleton complement — "any Symbol
  except `#foo`" — the **co-finite atom set** Beamtalk cannot express today.
- Difference is derived: `A \ B ≡ A and not B`. `union_without` becomes the
  special case `Union[..] and not #name` and is deleted.

### 2. Narrowing becomes two operations

Every narrowing idiom collapses to the same two-line core. Given a test that a
value has pattern-type `P` and a current type `T`:

- **true branch:** `T and P` (intersection)
- **false branch:** `T and not P` (negation/difference)

`singleton_eq`'s `refine_singleton_narrowing` (`inference.rs:2316`) — which today
hand-picks `matched` for one branch and `union_without` for the other — becomes:

```rust
info.true_type  = intersect(&current_ty, &matched);
info.false_type = Some(difference(&current_ty, &matched));
// (swapped when the test is negated)
```

The idiom detectors in `narrowing/rules/` stay — they still recognise AST
*shape* and produce the pattern-type `P` — but they no longer each re-implement
branch computation. `type_admits_singleton` becomes `intersect(T, #foo) !=
Never`.

### 3. Surface syntax

Negation becomes writable in type annotations (`::`), composing with the
existing `|` union syntax:

```beamtalk
// "any symbol except the reserved ones"
tag :: Symbol and not (#reserved | #internal)

// residual after handling the known cases
handleRest: dir :: (#north | #south | #east | #west) and not #north => ...
```

`and` / `not` are parsed only inside a type-annotation context, so they do not
collide with any binary message selector (`<` stays a message, per ADR 0068's
rationale for parenthesis generics).

### 4. Exhaustiveness

Because difference is now total, a multi-branch dispatch over a singleton union
can compute its **residual type**. When the residual is `Never`, the dispatch is
exhaustive; when it is non-`Never`, the checker warns with the uncovered
members.

```beamtalk
compass :: #north | #south | #east | #west := readHeading

result := compass caseOf: {
  [#north] -> "up".
  [#south] -> "down".
  [#east]  -> "right"
}
// ⚠️ non-exhaustive: `#west` is not handled
//    (residual type: `#west`)
```

### REPL session

```
bt> x := someUnionReturning   "x :: Integer | #infinity"
bt> (x = #infinity) ifTrue: [x] ifFalse: [x + 1]
                                          ^^^^^
   in the false branch, x :: Integer  (#infinity removed) → x + 1 type-checks
bt> :type (Symbol and not #foo)
   Symbol and not #foo   "co-finite: every atom except #foo"
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
| **Elixir (1.17+)** — gradual set-theoretic types (Castagna, Duboc, Valim) | Types *are* sets of values; closed under `or` / `and` / `not`. Atoms are singleton types (`:ok` is both value and type); `boolean() = true or false`; `nil`, `true`, `false` are atoms. `atom() and not (:foo or :bar)` expresses co-finite atom sets. `dynamic()` is a *range* of types kept "at the root", enabling gradual typing that still tracks structure. | **Adopt:** the three-operator algebra and singleton-as-set model — it is exactly what our `#foo`/`Symbol`/`union_without` code approximates. **Adapt:** we add `and`/`not` as surface syntax and internal operators without adopting the full semantic-subtyping decision procedure. **Leave (for now):** replacing `Dynamic(reason)` with a structural `dynamic()`; BDD emptiness checking. |
| **CDuce / semantic subtyping** (the theory underneath Elixir) | Subtyping = set inclusion decided via emptiness of a Boolean combination of type constructors, implemented with BDDs. | The north-star. Rejected as the *implementation* today (XL, nominal-core mismatch); adopted as the *mental model* for our operators so a future upgrade is a deepening, not a rewrite. |
| **TypeScript** | Discriminated unions + control-flow narrowing; literal (singleton) types; `Exclude<T, U>` at the *type-operator* level but **no first-class negation type** in values. | We go slightly further than TS values by making negation a real `InferredType`, which is what makes uniform false-branch narrowing and exhaustiveness fall out. |
| **Gleam** (BEAM, Rust-implemented, our closest typed-BEAM peer) | Sound nominal HM; **no** union/intersection/negation, exhaustiveness via nominal custom types. | Confirms exhaustiveness is table-stakes on BEAM, but Gleam gets it from closed nominal sum types; we get it from the atom-union algebra, which fits Beamtalk's open, atom-rich style better. |
| **Dialyzer** (success typing) | Post-compilation, optimistic; has union/negation internally but no IDE-time narrowing. | Reaffirms why we compute this at edit time, not after codegen (`type-system-design.md`). |
| **Smalltalk (Pharo/Squeak)** | No static types; `#foo` is just a Symbol, no singleton *type*. | No prior art to preserve — this is purely additive tooling that does not touch message-passing semantics. |

## User Impact

- **Newcomer (from Python/JS/TS):** `Symbol and not #foo` reads naturally to
  anyone who has met TypeScript's `Exclude`/discriminated unions. The main new
  payoff they *feel* is the non-exhaustive-`caseOf:` warning catching a missed
  atom — a bug class that is otherwise a runtime `does_not_understand`.
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
  `Symbol and not #foo`; new idioms need only produce a pattern-type `P`.

## Steelman Analysis

### Chosen: Internal operators + surface syntax + exhaustiveness
- 🧑‍💻 **Newcomer:** "The `caseOf:` warning literally tells me which atom I
  forgot — that's the feature I didn't know I wanted."
- 🎩 **Smalltalk purist:** "It's opt-in annotation sugar; my un-annotated code
  is untouched and messages still dispatch dynamically."
- ⚙️ **BEAM veteran:** "Co-finite atom sets (`Symbol and not …`) are how I model
  Erlang tagged returns; finally expressible."
- 🏭 **Operator:** "Compile-time only, generated code identical — nothing new to
  observe or debug in prod."
- 🎨 **Language designer:** "Two operators subsume seven idioms' branch logic
  *and* give exhaustiveness for free. Maximum expressiveness-per-concept."

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
- One narrowing core (`intersect` for true, `difference` for false) replaces
  per-idiom branch logic; `union_without` and `type_admits_singleton` collapse
  into it.
- False-branch complements work for **all** types, closing ADR 0068's gap.
- Co-finite atom sets (`Symbol and not #foo`) become expressible.
- Exhaustiveness checking over singleton unions — a real bug-catcher — falls out
  of total difference.
- The `check_impossible_singleton_comparison` diagnostic becomes a consequence
  of `intersect(T, s) == Never` rather than a special case.

### Negative
- `InferredType` grows two variants; every exhaustive `match` on it (equality,
  display, provenance, codegen-facing conversions) must handle them.
- Normalisation (dedup, absorption, `and`/`or` interaction) is fiddly to get
  right and needs thorough property tests.
- Surface `and`/`not` adds parser surface in the annotation grammar.

### Neutral
- No runtime, codegen, or hot-reload change — edit/compile-time only.
- `Dynamic(reason)` is untouched; this ADR does **not** make it structural
  (that is the deferred phase-B/north-star work).

## Implementation

**Phase 1 — internal operators (the *Alternative A* core):**
1. Add `Intersection` / `Negation` to `InferredType` (`types.rs`) with
   normalisation helpers `intersect` / `difference`, mirroring `union_of`.
2. Update all exhaustive matches: `PartialEq`, `display_for_diagnostic`,
   provenance accessors, `as_known`, hover rendering.
3. Rewrite `refine_singleton_narrowing`, delete `union_without`, redefine
   `type_admits_singleton` as `intersect(T, s) != Never`.
4. Keep the seven idiom detectors; strip their bespoke branch logic to emit a
   pattern-type `P` consumed by the shared core.

**Phase 2 — surface + exhaustiveness (this ADR's added scope):**
5. Parse `and` / `not` inside type annotations (`source_analysis/parser`,
   `TypeAnnotation`), resolve in `type_resolver.rs`.
6. Compute residual types for `caseOf:` / multi-branch singleton dispatch and
   emit non-exhaustiveness diagnostics.

**Affected components:** type checker (`semantic_analysis/type_checker/*`),
annotation parser + `TypeAnnotation` AST, hover/diagnostic rendering
(`queries/hover_provider.rs`). **Not** affected: codegen, runtime, REPL output
values.

## Migration Path

Backward compatible. Existing annotations, inference, and the seven narrowing
idioms behave identically — their branch types are now produced by the shared
algebra but the *results* are unchanged (covered by the existing narrowing and
union tests). `and`/`not` annotation syntax and exhaustiveness warnings are
purely additive; no existing `.bt` source changes meaning.

## References
- Related issues: BT-XXX (to be filed via `/plan-adr`); builds on BT-2617,
  BT-2624, BT-2631 (singleton narrowing / impossible-comparison diagnostics)
- Related ADRs: ADR 0068 (Parametric Types and Protocols — unions, narrowing,
  and the noted difference-types gap), ADR 0025 (Gradual Typing and Protocols),
  ADR 0053 (`::` annotation syntax), ADR 0036 (Metaclass tower)
- Documentation: `docs/internal/set-theoretic-types-north-star.md` (the full
  vision + compatibility contract this ADR upholds);
  `docs/internal/type-system-design.md` (Union Types and
  Narrowing); `crates/beamtalk-core/src/semantic_analysis/type_checker/`
  (`types.rs` `union_of`, `inference.rs` `union_without` /
  `type_admits_singleton` / `refine_singleton_narrowing`, `narrowing/rules/`)
- External: [Gradual set-theoretic types — Elixir](https://hexdocs.pm/elixir/main/gradual-set-theoretic-types.html);
  "The Design Principles of the Elixir Type System" (Castagna, Duboc, Valim);
  [Strong arrows — elixir-lang.org](https://elixir-lang.org/blog/2023/09/20/strong-arrows-gradual-typing/)
