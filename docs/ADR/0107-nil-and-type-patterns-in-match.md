# ADR 0107: Nil and Type Patterns in `match:`

## Status
Proposed

## Context

### Problem statement

Beamtalk code that needs to branch on both the nilness and the runtime
class/shape of a single value has no construct for it other than sequential
guard clauses. Two real examples from a consuming application
(`beamtalk-symphony`):

```beamtalk
workspaceRoot -> String =>
  defaultRoot := File tempDirectory ++ "/symphony_workspaces"
  raw := self nested: "workspace" key: "root" default: nil
  raw isNil ifTrue: [^defaultRoot]
  (raw isKindOf: String)
    ifTrue: [
      (raw startsWith: "$")
        ifTrue: [
          resolved := self resolveEnv: raw
          resolved isNil ifFalse: [^self expandHome: resolved]
          ^raw
        ]
      ^self expandHome: raw
    ]
  defaultRoot
```

```beamtalk
renderForBody: body :: String items: coll :: Printable | Nil itemName: itemName :: String vars: vars :: Dictionary -> String =>
  coll isNil ifTrue: [^""]
  (coll isKindOf: List) ifFalse: [^""]
  items :: List(Printable) := coll
  items inject: "" into: [:acc :item | ...]
```

Both are guard-clause chains with early returns — functionally correct, but
every case the value can take (nil, matching-shape, non-matching-shape) is
implicit in a sequence of `ifTrue:`/`ifFalse:` branches rather than visible as
a flat, exhaustively-checkable list.

### Current state

- `match:` (`Expression::Match`, `crates/beamtalk-core/src/ast/expression.rs`)
  already supports `Wildcard`, `Literal`, `Variable`, `Tuple`, `Array`,
  `List`, `Binary`, `Map`, and `Constructor` patterns
  (`crates/beamtalk-core/src/ast/pattern.rs`). Parsing goes through
  `parse_pattern`
  (`crates/beamtalk-core/src/source_analysis/parser/expressions.rs:1388`).
- `Constructor` is documented as matching "a sealed Value type," but the
  codegen is hardcoded to exactly one class:
  `sealed_constructor_fields` (`crates/beamtalk-core/src/codegen/core_erlang/expressions.rs:3038`)
  only has arms for `("Result", "ok:")` / `("Result", "error:")`. Any other
  class — including a user-defined `sealed Value subclass:` — fails with
  "is not a known sealed type." The doc comment on the field-layout helper is
  explicit about this being **Phase 1 scope**: "Only stdlib sealed types are
  supported. User-defined sealed types require the Phase 2 `[pattern: ...]`
  annotation (tracked separately)." That Phase 2 is a different, narrower
  feature (destructuring a sealed *variant's* fields) than what this ADR
  proposes (testing the runtime class/nilness of an arbitrary value); see
  Prior Art / Alternatives for how the two relate.
- There is no `Nil` pattern (`Literal` covers Integer/Float/String/Symbol/
  Character/List — no nil), and no class/type-test pattern of any kind.
- Type narrowing today is a fixed set of separate, per-idiom rules under
  `crates/beamtalk-core/src/semantic_analysis/type_checker/narrowing/rules/`
  (`is_nil.rs`, `is_kind_of.rs`, `class_eq.rs`, `is_result.rs`,
  `responds_to.rs`, `singleton_eq.rs`) — each idiom (`isNil ifTrue:/ifFalse:`,
  `isKindOf: ifTrue:/ifFalse:`, `class = ifTrue:/ifFalse:`, ...) is narrowed
  independently; there is no single combined "narrow by nilness and class at
  once" construct. ADR 0102 established the set-theoretic (`&`, `\`) narrowing
  algebra this ADR's arm-binding narrowing reuses.
- `match:` exhaustiveness is currently two independent, narrow checks: BT-1299
  (`validators::match_validators::check_match_exhaustiveness`, sealed
  constructor coverage — today only `Result`) and BT-2745/ADR 0102 §4
  (closed `#symbol`-singleton unions, advisory `Warning`), escalatable via
  `matchExhaustive:` (ADR 0106, `Error`-severity opt-in). Neither reaches a
  `String | Nil`-shaped union.
- **This is anticipated, not new scope.** ADR 0060 §9 ("Forward
  Compatibility: Match Expression Integration") already states: *"If `match:`
  gains class/structural patterns (planned separately), Result becomes
  directly destructurable,"* with an example comment `// Future: class
  pattern arms in match:`, and "No changes to ADR 0060's design are needed to
  support this." This ADR is that "planned separately" work.

### Constraints

- **Non-goal, per `docs/beamtalk-principles.md`:** "Mandatory static typing —
  Dynamic by default... Type mismatches are warnings, not errors —
  interactive workflows are never blocked." Any new exhaustiveness checking
  this feature enables must stay advisory-by-default, escalating to `Error`
  only through the existing explicit `matchExhaustive:` opt-in (ADR 0106),
  never on its own.
- **Principle #6 ("Messages All The Way Down"):** "No special syntax for
  'primitives' — even `+` is a message." `match:` is itself a keyword
  message; new pattern grammar must read as an extension of that message's
  arm syntax, not a `case`/`switch`-style special statement.
- **Type erasure (ADR 0068, `docs/beamtalk-language-features.md`):** "All
  generic type information is erased at runtime (zero cost)." A type pattern
  can test a value's runtime class but cannot verify a generic parameter
  (`items :: List(String)` can confirm "this is a `List`" at runtime, not
  "every element is a `String`" — the element-type portion of the annotation
  is a static-only declaration, same as any other generic annotation).
- **`docs/beamtalk-syntax-rationale.md`'s "door left ajar":** the rejection of
  Erlang-style multi-clause method heads explicitly reserves *future*
  pattern-matching surface expansion for something "to be earned with
  evidence, not built speculatively." Two concrete, independent call sites
  (above) plus ADR 0060's pre-existing forward-compatibility note are that
  evidence; this ADR does not open the door further than what's motivated.
- **Layering (`docs/development/architecture-principles.md`):** new `Pattern`
  variants live in the parser/AST layer and are consumed downstream by
  semantic analysis (narrowing, exhaustiveness) and codegen — `codegen` may
  depend on `parse`'s AST types, never the reverse.

## Decision

Extend `match:` with two new pattern kinds, split into two phases so the
low-risk, immediately-useful part ships without waiting on the harder part.

### Phase A (this ADR): `nil` pattern + concrete-type patterns

```beamtalk
raw match: [
  nil -> defaultRoot;
  path :: String when: [path startsWith: "$"] -> (
    resolved := self resolveEnv: path
    resolved ifNil: [path] ifNotNil: [self expandHome: resolved]
  );
  path :: String -> self expandHome: path;
  _ -> defaultRoot
]
```

```beamtalk
coll match: [
  nil -> "";
  items :: List(Printable) -> (items inject: "" into: [:acc :item | ...]);
  _ -> ""
]
```

- **`nil` pattern:** matches the nil literal. `nil` is a reserved identifier
  today, not a `Literal` variant; this adds `Pattern::Nil(Span)` alongside the
  existing literal patterns.
- **Type pattern (`binding :: ClassName`):** matches if the scrutinee's
  runtime class is `ClassName`, binding the value (narrowed to `ClassName`
  for the arm body) to `binding`. Reuses the `::` token already established
  by ADR 0053 for parameter/state type annotations — this is not new
  punctuation, it is the same annotation syntax in a new grammatical
  position.
- **Phase A scope is deliberately concrete/leaf types only**: stdlib
  primitives (`String`, `Integer`, `Float`, `List`, `Dictionary`, `Boolean`,
  `Symbol`) and exact tagged `Value`/sealed classes. `binding :: SomeClass`
  where `SomeClass` has subclasses is a **compile error in Phase A**
  ("`SomeClass` has subclasses; type patterns are not yet supported for
  non-leaf classes — see ADR 0107 Phase B"), not silently-wrong behaviour.
  This keeps Phase A's runtime semantics identical to an exact class-tag
  check — never a mystery about whether subclass instances match.

### Phase B (explicitly out of scope here): subclass-polymorphic matching

`binding :: Shape` where `Shape` has subclasses `Circle`/`Square` needs
either compile-time subclass enumeration (using the class-hierarchy data
already available in `semantic_analysis`) or a wrapped runtime dispatch call
— genuinely new codegen/design work, not reuse of an existing mechanism (see
Prior Art and Implementation). Deferred to a follow-up ADR once Phase A has
shipped and there is evidence the leaf-type-only restriction is actually
limiting real code. Concretely, Phase A does not preclude Phase B: the AST
shape (`Pattern::Type { binding, class, .. }`) and syntax are identical;
Phase B only widens which class names are legal in that position.

### The `::` dual-semantics tension

This ADR's `binding :: ClassName` pattern performs a **real runtime test** —
it is how the arm is selected. This is a different runtime guarantee than
`::` in a cast position (`x :: T := y`), which performs **no runtime check at
all** — confirmed empirically this session: a field declared `String | Nil`
concatenated via `++` with no guard compiled clean (zero diagnostics) and
crashed at runtime with an opaque `invalid argument` badarg, because the
`::`-annotated binding never inserted a runtime tag check anywhere in
codegen (`spec_codegen.rs` only emits Erlang `-spec` attributes, tooling
metadata, never enforced at runtime). Reusing `::` for both an unchecked
static assertion (casts) and a checked runtime dispatch (patterns) is a
readability win — same token, "this value has this type" in both places —
but it does mean the *runtime guarantee* `::` carries depends on which
grammatical position it appears in. This is called out honestly in
Consequences rather than hidden.

### Error examples

```beamtalk
" Unknown class name — reuses the existing unresolved-class diagnostic (ADR 0100) "
raw match: [nil -> ""; x :: Sting -> x; _ -> ""]
" ⛔ Error: unknown class `Sting` (did you mean `String`?) "

" Phase A: non-leaf class in a type pattern "
shape match: [s :: Shape -> s area; _ -> 0]
" ⛔ Error: `Shape` has subclasses; type patterns are not yet supported for
"    non-leaf classes (ADR 0107 Phase B). Match on the concrete subclasses
"    instead, or use `isKindOf:` guard clauses. "

" matchExhaustive: on a closed Known|Nil union, one arm missing (Error, ADR 0106) "
raw :: String | Nil
raw matchExhaustive: [s :: String -> s size]
" ⛔ Error: non-exhaustive matchExhaustive: `Nil` is not handled
"    (residual type: `Nil`) "

" plain match:, same gap — advisory only (ADR 0102 §4 policy, unchanged) "
raw match: [s :: String -> s size]
" ⚠ Warning: non-exhaustive match: `Nil` is not handled
"    (residual type: `Nil`) "
```

### REPL example

```
> nil match: [nil -> 0; s :: String -> s size; _ -> -1]
0
> "hi" match: [nil -> 0; s :: String -> s size; _ -> -1]
2
> 42 match: [nil -> 0; s :: String -> s size; _ -> -1]
-1
```

## Prior Art

| Language | Approach | What we take / leave |
|---|---|---|
| **Smalltalk (Pharo/Squeak/Newspeak)** | No first-class class-pattern matching; `caseOf:otherwise:` exists but is treated as anti-pattern. The idiom is `isKindOf:` guard chains — exactly Beamtalk today — or virtual dispatch (add a method). Message-passing purity historically argues dispatch should be polymorphic lookup, not a match. | **Take the caution, not the conclusion.** The purist objection assumes you own the class to add a method to. Beamtalk's motivating cases are boundary values (parsed YAML/JSON, external API responses) where you cannot add a method to `String`/`Nil`/`Dictionary` for app-specific dispatch — the objection doesn't apply there. Scoping Phase A to exactly that boundary use (not general polymorphic dispatch) is the compromise. |
| **Gleam** | `case` matches variants of closed custom (sum) types, not an untyped value's class directly. Crossing a truly-dynamic boundary goes through `gleam/dynamic/decode` — decode/validate into a typed value first, then match exhaustively. | **Leave the "wrap first" purity** (that's `Constructor` pattern territory, already how `Result` works) but **take the boundary framing**: a type pattern here is the deliberately non-exhaustive-by-default escape hatch for values that haven't been wrapped yet, not a replacement for wrapping. |
| **Elixir/Erlang** (compile target) | `case x do b when is_binary(b) -> ...; nil -> ...; n when is_integer(n) -> ... end`. Type tests are guard-safe BIFs. Idiomatic, this is the direct lowering target. No exhaustiveness over type coverage (only over unmatched literals). | **Adopt directly as the codegen strategy** — Phase A's leaf-type patterns lower to exactly this shape. Confirms there is no "free" exhaustiveness to inherit from the runtime; Beamtalk's exhaustiveness (where it applies) has to come from the type checker's own closed-union tracking, same as BT-2745 already does for symbol unions. |
| **Swift / Kotlin / Rust** | Swift: `case let s as String:` — explicit named binding. Kotlin: `is String -> x.length` — implicit smart-cast (flow narrowing, no new name); known to break on mutable/cross-module properties. Rust: matches enum variants, not runtime classes; exhaustive only because enums are closed. | **Take Swift's explicit-binding shape** (`path :: String`, a real new name) **over Kotlin's smart-cast** — a named binding sidesteps the mutability/stability traps smart-casting has in Kotlin, at the cost of one more identifier per arm. **Confirms the cross-cutting lesson**: exhaustiveness only comes from closed unions; an open class hierarchy (Phase B) must require a wildcard, never claim completeness. |

## User Impact

- **Newcomer:** `path :: String` reads exactly like the type annotations
  already visible in every typed method signature and field declaration
  (`field: x :: String`) — no new syntax to learn, only a new position for
  syntax already seen. The Phase A "has subclasses" error is a discoverable
  teaching moment, not a silent wrong-behaviour trap.
- **Smalltalk developer:** the historical `isKindOf:`-chain-is-an-antipattern
  concern is real and addressed by scope, not dismissed: Phase A targets
  boundary/dynamic data, not a general alternative to polymorphic dispatch on
  classes you own. `match:` is already a keyword message, so this is grammar
  extension of an existing message, not new special-form syntax.
- **Erlang/BEAM developer:** Phase A patterns lower to the exact idiom already
  used in hand-written Elixir/Erlang (`case x do b when is_binary(b) -> ...
  end`) — nothing about the generated Core Erlang is surprising or novel to
  someone reading it in `observer`/`recon`.
- **Production operator:** bounded new runtime surface — a guard-safe BIF
  test or one map-key equality check per arm, the same class of dispatch
  `isKindOf:` already performs today, not a new kind of runtime behaviour.
  Phase B (subclass enumeration or dispatch) is deferred specifically because
  it is *not* this bounded, and this session's own BT-2844 investigation
  (narrow-Known-only-check bugs in the type checker's argument validation)
  is fresh evidence that shipping the more general, less-tested version first
  is a real risk, not a hypothetical one.
- **Tooling/LSP author:** a real `Pattern` variant (vs. an opaque `when:`
  guard) means completion, hover, and narrowing can reason about it
  structurally, the same way they already do for `Constructor` patterns.

## Steelman Analysis

### Chosen: `binding :: ClassName` type pattern + `nil` pattern, Phase A/B split
- 🧑‍💻 **Newcomer:** "`x :: String` is the exact syntax I already see in every
  typed method — I'd guess it cold, no docs needed."
- 🎨 **Language designer:** "A real Pattern AST variant means exhaustiveness,
  narrowing, and LSP completion all get to reason about this structurally,
  same as `Constructor` patterns already do — and scoping to leaf types first
  is exactly the 'earned with evidence, not built speculatively' discipline
  the syntax-rationale doc already asks for."
- ⚙️ **BEAM veteran:** "Lowers to the idiomatic Elixir shape 1:1 — nothing new
  to reason about in generated code."

### Rejected B: widen `when:` guards to allow arbitrary message sends (`isKindOf:`, `isNil`), no new Pattern variant
```beamtalk
raw match: [
  x when: [x isNil] -> defaultRoot;
  x when: [x isKindOf: String] -> self expandHome: x;
  _ -> defaultRoot
]
```
- 🎩 **Smalltalk purist:** "This is the *most* Smalltalk-pure option — not
  even new syntax, just `isKindOf:`/`isNil` sent as ordinary messages inside
  an existing guard block. Message-passing all the way down, zero new AST
  surface."
- **Why not chosen:** cheapest to build (only widening
  `generate_guard_expression` to emit a nested-`case` instead of requiring a
  native BEAM guard), but guards are opaque booleans to both the narrowing
  system and the exhaustiveness checker — `x` never gets a narrowed static
  type inside the arm without a separate manual cast, and this forecloses
  ever getting real exhaustiveness for this shape. It solves the syntax
  problem but not the two things (narrowing, exhaustiveness) that make a
  first-class pattern worth having over what guard clauses already do today.

### Rejected C: no new `match:` syntax — add exhaustiveness *lint tooling* for the existing guard-clause idiom
- 🏭 **Production operator:** "Zero runtime change, by definition the safest
  option — a lint that flags an incomplete `isNil`/`isKindOf:` chain over a
  known closed `Union` gets the safety benefit without touching codegen at
  all."
- ⚙️ **BEAM veteran:** "This is the Gleam-blessed answer: wrap boundary data
  into a proper closed type once, get exhaustiveness for free, rather than
  inventing syntax to match on an untyped value's class directly."
- **Why not chosen:** doesn't address the actual complaint that started this
  (verbose, deeply-nested guard clauses) — it makes the existing idiom safer
  without making it shorter or flatter. Real value as a *complement*, not
  a substitute; not pursued as an alternative *to* Phase A, but worth
  revisiting as an addition regardless of this ADR's outcome.

### Rejected D: full `isKindOf:`-semantics class pattern (subclass-polymorphic), single phase, no split
- 🎩 **Smalltalk purist:** "If we're adding a type pattern at all, it should
  have full `isKindOf:` semantics or none — a version that only works for
  leaf classes is a worse violation of 'do one thing well' than not having
  the feature."
- **Why not chosen:** the "full generality" case is real, but three of five
  cohorts pushed back hard once the actual cost was named. `isKindOf:`
  compiles to ordinary recursive message dispatch today (`self class
  includesBehaviour: aClass`, not a guard-safe BIF) — supporting it in a
  pattern needs either compile-time subclass enumeration or a wrapped
  runtime dispatch call, genuinely new infrastructure, not reuse of the
  Array-pattern/Result-constructor-pattern codegen Phase A reuses. Shipping
  that untested generality in v1 is precisely the "narrow check quietly
  wrong on an untested shape" risk this session's BT-2844 investigation
  spent real effort finding and fixing elsewhere in the type checker —
  reintroducing the same risk class on day one, in a brand-new subsystem,
  is not justified when Phase A already covers both motivating call sites.

### Tension points
Smalltalk purists are the one cohort not fully behind the chosen option —
genuinely split between B (zero new syntax, purest message-passing) and C
(truest to the historical anti-`isKindOf:`-dispatch tradition); neither loves
a new `Pattern` variant as much as the newcomer/language-designer/BEAM-veteran
cohorts do. Operators and BEAM veterans align firmly against D for the same
reason: the bounded, already-proven-safe runtime surface of Phase A is a
better trade than full generality shipped untested. No cohort's strongest
argument for D beats their argument for the chosen option — it is the
consensus-weakest alternative, specifically because of fresh, concrete
evidence (this session's own bug hunt) about what "ship the general,
untested version first" costs in this exact subsystem.

## Alternatives Considered

See Steelman Analysis above — guard-widening (B), lint-only (C), and
single-phase full generality (D) were all considered and rejected in favour
of the type-pattern-with-Phase-A/B-split, for the reasons given there.

## Consequences

### Positive
- Directly replaces both motivating call sites (`workspaceRoot`,
  `renderForBody:`) with a flat, single-expression arm list instead of nested
  guard clauses.
- Fulfils ADR 0060 §9's forward-compatibility note — no changes needed to
  ADR 0060's `Result` design; `Result ok:`/`Result error:` constructor
  patterns compose unchanged alongside the new patterns in the same `match:`.
- Reuses existing codegen paths verbatim for Phase A: the Array pattern's
  nested-`case`-with-guard-BIF-test shape (primitives) and the Result
  constructor pattern's tagged-map-key check (exact `Value`/sealed classes)
  — no new runtime mechanism, only generalizing two mechanisms that already
  exist.
- Composes with `matchExhaustive:` (ADR 0106) for free: a closed
  `Known | Nil` union becomes a case the existing advisory/asserted
  machinery can reach, which it cannot today.
- Phase A/B split means the higher-risk, higher-effort part (subclass
  matching) is explicitly deferred rather than silently under-scoped —
  matches this session's own recent lesson (BT-2844) about the cost of
  shipping under-tested generality.

### Negative
- `::` now carries two different runtime guarantees depending on
  grammatical position (checked in a pattern, unchecked in a cast) — a real
  wart, called out explicitly above and in documentation rather than papered
  over.
- Two ways to express the same nil/type dispatch (guard-clause chains vs.
  `match:` patterns) is one more idiom to teach; existing guard-clause code
  is not required to migrate (purely additive), so both will coexist in the
  codebase indefinitely.
- Phase A's "has subclasses → compile error" restriction is a real limitation
  a developer can hit in practice (e.g. wanting to match on an open class
  hierarchy) before Phase B exists to lift it.

### Neutral
- No change to any existing `match:` behaviour — purely additive new pattern
  kinds; every existing pattern (`Literal`, `Constructor`, etc.) is
  unaffected.
- Phase B is deliberately left undesigned here beyond noting the two
  implementation strategies (compile-time enumeration vs. runtime dispatch)
  — a follow-up ADR, not this one, picks between them once there's evidence
  Phase A's leaf-type restriction is actually limiting real code.

## Implementation

- **AST:** `crates/beamtalk-core/src/ast/pattern.rs` — add
  `Pattern::Nil(Span)` and `Pattern::Type { binding: Identifier, class:
  Identifier, span: Span }`.
- **Parser:** `parse_pattern`
  (`crates/beamtalk-core/src/source_analysis/parser/expressions.rs:1388`) —
  recognise the reserved `nil` identifier as `Pattern::Nil`, and
  `identifier :: ClassName` as `Pattern::Type` (reusing the existing `::`
  tokenization from ADR 0053's parameter-annotation parsing).
- **Semantic analysis (bindings/scope):**
  `crates/beamtalk-core/src/semantic_analysis/pattern_bindings.rs` and
  `analyser.rs`/`block_facts.rs` (the same files that already handle
  `Constructor`'s keyword bindings) — extend to define `Pattern::Type`'s
  `binding` in the arm's scope, narrowed to `class`.
- **Semantic analysis (narrowing):**
  `crates/beamtalk-core/src/semantic_analysis/type_checker/narrowing/rules/`
  — a new rule (or extension of `is_kind_of.rs`'s existing algebra) applies
  the same `T \ ClassName` difference-type narrowing ADR 0102 already defines
  for `isKindOf:` guard clauses, so code inside a `path :: String` arm sees
  `path` statically as `String`, and subsequent arms see the scrutinee
  narrowed by `\ String` (feeding exhaustiveness residual computation, next
  bullet).
- **Semantic analysis (exhaustiveness):**
  `crates/beamtalk-core/src/semantic_analysis/validators/match_validators.rs`
  and the BT-2745 residual/coverage machinery in
  `type_checker/inference.rs` (`check_singleton_match_exhaustiveness` /
  ADR 0106's `check_asserted_match_exhaustiveness`) — extend residual
  computation to cover `Nil`/`Type`-pattern coverage over closed
  `Known | Nil` (and small closed unions of concrete leaf classes), reusing
  the existing advisory-`Warning`-vs-asserted-`Error` split verbatim; no new
  severity policy needed.
- **Codegen:**
  `crates/beamtalk-core/src/codegen/core_erlang/expressions.rs` — `Nil`
  pattern reuses the existing atom-literal codegen path
  (`Document::Str("'nil'")`). `Type` pattern gets a new
  `generate_type_pattern` dispatching on class name: stdlib primitives reuse
  `generate_array_match_arm`'s nested-`case`-wrapping-a-guard-BIF-test shape
  (~line 2407) generalized to a class-name → BIF table (`is_binary`,
  `is_integer`, `is_float`, `is_list`, `is_boolean`, ...); exact tagged
  `Value`/sealed classes reuse `generate_constructor_pattern`'s
  `'$beamtalk_class'` map-key check (~line 2761), generalized from
  hardcoded `Result` to the pattern's `class` field. Non-leaf classes in
  Phase A are rejected at this stage with the "has subclasses" compile error
  (using the class-hierarchy data already available to semantic analysis to
  detect this before codegen is reached).
- **Tests:** parser round-trip (`parse_pattern`, unparse), narrowing tests
  (new rule / `is_kind_of.rs` extension), exhaustiveness tests (advisory +
  asserted, mirroring `asserted_match_exhaustiveness.rs`'s structure from
  ADR 0106), codegen tests for both the BIF-test and map-tag-check paths,
  plus BUnit runtime tests in `stdlib/test/`.
- **Docs:** `docs/beamtalk-language-features.md` §Pattern Matching — add
  `Nil` and `Type` to the supported-patterns table.
- **LSP:** completion for `nil` as a pattern keyword and `::` as a pattern
  position (`crates/beamtalk-core/src/queries/completion_provider.rs`), for
  surface parity with existing pattern completions.

## References
- Related issues: none yet — this ADR precedes any implementation ticket;
  `/plan-adr` creates the implementation Epic and issues once this is
  Accepted.
- Related ADRs: ADR 0053 (`::` type annotation syntax — the token this reuses
  in a new position), ADR 0060 §9 (the forward-compatibility note this ADR
  fulfils), ADR 0068 (parametric types / runtime type erasure — bounds what a
  generic type-argument in a pattern can and can't verify), ADR 0100
  (Open-World Diagnostic Policy — governs severity for the unknown-class and
  non-leaf-class compile errors), ADR 0102 (set-theoretic narrowing algebra
  this ADR's arm-binding narrowing reuses; §4's advisory exhaustiveness this
  extends), ADR 0106 (`matchExhaustive:` — the assertion mechanism this ADR
  composes with directly, unmodified)
- Documentation: `docs/beamtalk-language-features.md` §Pattern Matching,
  §Control Flow Narrowing; `docs/beamtalk-syntax-rationale.md` §Rejected
  Alternatives (the "door left ajar" note this ADR walks through);
  `docs/beamtalk-principles.md` (Messages All The Way Down; Non-Goals —
  mandatory static typing)
