# ADR 0148: Checked Narrowing Casts and the Trusted-Type Comparison Hole

## Status
Proposed (2026-11-14). Draft for owner review; not accepted. Tracking: BT-4772.

### Scope amendment (2026-11-14): part 1 is being done as BT-4823

The owner fast-tracked part 1 as a separate bug, **BT-4823** ("Typed comparison
fast path answers wrong when the declared type is violated"), implemented ahead
of this ADR. Consequences for this document:

- **Part 1 is no longer a decision of this ADR.** It is kept below as the
  rationale and the record of what BT-4823 must do. The decision left open here
  is **part 2 (explicit checked cast) and part 3 (no implicit guard on `::`
  locals)**.
- BT-4823's scope is narrower than part 1's wording: **receiver side only**.
  The comparison *argument* stays unchecked by decision (BT-3928, 2026-10-19):
  `5 < nil` still answers `true`, with the static warning and the documented
  term-order fallback. Part 1 never proposed otherwise; this note makes it
  explicit.
- BT-4823 must be benchmarked, and adds the row this ADR called 11e. The ~1 ns
  figures below are estimates until that PR reports a measurement; **replace
  them with its numbers** when it merges.
- The evidence test `stdlib/test/narrowing_cast_wrong_value_test.bt` pins
  today's wrong answers. It **will need flipping** when BT-4823 merges: the
  `...AnswersSilently` tests for `<`, `>` and the field case (and
  `testSameWrongAnswerWithoutAnyCast`) must expect `does_not_understand`.
  BT-4823's description tells its author to flag this so this branch can be
  rebased; whoever merges first updates the test. The cast-local and
  arithmetic tests do not change.

## Context

### Problem statement

A narrowing local annotation is a cast the type checker cannot prove:

```beamtalk
n :: Integer := opts at: #max     // RHS is Object or Dynamic: accepted silently (BT-2015)
```

Nothing checks it at run time. BT-4719 made these casts visible (their own
`type-coverage` category and an opt-in `narrowing-cast` lint). This ADR decides
whether to make them **checked**, and what the evidence says about how urgent
that is.

The issue raised one open fact that decides urgency: codegen trusts declared
types (row 11b of `just bench-vs-elixir`: `a + 1` with `a :: Integer` is a bare
`erlang:'+'`), so can a wrong cast give a **silently wrong answer** rather than
a crash?

### Finding: yes, but not where the issue looked

**A wrong value can produce a silently wrong answer today.** The test
`stdlib/test/narrowing_cast_wrong_value_test.bt` (with fixtures
`narrowing_cast_probe.bt` and `narrowing_cast_limit.bt`) feeds non-Integer
values through a narrowing cast and records what happens:

| Where the wrong value is used | `nil < 10` | `nil > 10` | `nil + 1` |
|---|---|---|---|
| The cast **local** (`n :: Integer := opts at: #v`, then `n > 10`) | `does_not_understand` | `does_not_understand` | `does_not_understand` |
| A typed **parameter** it is passed to (`paramGreater: n :: Integer`) | **`false`** | **`true`** | `type_error` (raw `badarith`) |
| A typed **field** it is stored in (`field: limit :: Integer`) | – | **`true`** | – |

`#foo` and `true` behave like `nil` in the parameter and field rows. A real
`Integer` or `Float` answers correctly everywhere.

Three facts follow from the table:

1. **The silent answer comes from the comparison fast path, not from the
   annotated local.** Codegen trusts the declared type of a *parameter* or
   *field* (`trusted_param_types`, `field_types.rs`) and emits a bare
   `erlang:'<'` / `'>'` for it. Erlang's comparison is a total term order that
   never raises, so `nil > 10` is `true` (atoms sort above numbers). An
   annotated *local* is not trusted by the operator path, so the same value
   fails loudly there. Arithmetic fails loudly everywhere (`badarith`), only
   with the wrong error kind.
2. **A guard at narrowing casts would not close it.** The last test in the
   file passes `d at: #v` straight to the typed parameter with no annotation
   anywhere, and gets the same wrong answer. A wrong value reaches a typed
   parameter or field through a cast, a `Dynamic` argument, an untyped caller
   or an FFI result. The cast is one route of several.
3. **It contradicts a rule already on the books.** ADR 0025's amendment
   (BT-4118) says types "may choose code, never results" and that a lying
   annotation "simply misses the guard and gets the dynamic answer". BT-3924
   already decided that a silent term-order answer is unacceptable for
   untyped receivers and `Nil` unions (ADR 0116, last paragraph of the
   field-types section). The typed `Integer`/`String` parameter and field
   are the one remaining exception.

BT-1944's `typed_param_erasure_test.bt` states the intent ("adding `:: Integer
| Nil` should NOT change runtime behavior"). For comparisons it does change
it: `paramLess: nil` answers `false` while the same expression on an untyped
receiver raises.

### Current state

- `asType:` already exists as the explicit, **unchecked** cast. It is erased
  in codegen (`dispatch_codegen/mod.rs`) and ADR 0025 documents it as the
  escape hatch. ADR 0025 also records that `as:` was considered and rejected
  because it conflicts with ADR 0004's workspace registration API.
- Typed **sends** are already safe: a static class selects a direct call
  behind a tag or representation guard, with a byte-identical dynamic
  fallback (ADR 0025 Amendment §1). Only the operator fast paths (and the FFI
  pass-through proof, which has its own runtime guard) trust a declared type
  without a guard.
- `routing::type_test(class)` returns the representation test of a built-in
  class (`is_integer`, the `String` tag test, the `Symbol` test, …) and is what
  a `binding :: C` match arm and `isKindOf: <built-in>` already compile to
  inline. It returns `None` for a user class, which needs the hierarchy
  (`beamtalk_behaviour_intrinsics:valueIsKindOfClass`, or the name-only path
  BT-4769 is making cheaper).
- ADR 0147 (map-shape types, Implemented) removes the option-map casts that
  motivated much of BT-4719.

### Constraints

- Types are erased (ADR 0025); only guard-protected code selection is allowed.
  A checked cast is the first annotation whose mismatch *raises*, so it needs
  an explicit rule rather than slipping in.
- Generic arguments, protocols, shapes and type variables cannot be fully
  checked at run time. A guard can only test the outer representation.
- "No image mechanics" (`docs/beamtalk-principles.md` §4): behaviour must not
  differ between REPL, `beamtalk run`, `beamtalk test` and releases.
- No Beamtalk syntax is invented here. A checked form is a plain keyword
  message, as `asType:` is.

## Decision

Two parts, in this order. Part 1 is independent of part 2 and is the actual
fix for the silent wrong answer. **Part 1 is implemented as BT-4823 (see the
scope amendment above); the open decision is part 2.**

### 1. Make the comparison fast path obey "never results" (BT-4823)

Drop the bare-BIF path for `<`, `>`, `<=`, `>=` on a typed parameter or field.
Every comparison whose receiver is not a numeric/string literal (or `self` in
a numeric class) takes the existing inline `OperatorGuard::Comparison` test
(`is_number or is_binary`). `nil > 10` then raises `does_not_understand` for a
typed parameter exactly as it does for an untyped one. Only the *receiver* is
tested; the argument stays unchecked (BT-3928), so `5 < nil` is still `true`.
Only declared types are guarded: a receiver whose kind the compiler proves (a
literal, integer arithmetic) keeps the bare BIF. Annotated *locals* already take
the guard today (the evidence table), so the sites that change are typed
parameters and typed fields.

Arithmetic keeps its bare path: its BIF raises, so a lying annotation fails
loudly, and row 11b keeps its zero-guard fast path. It still reports
`type_error` where an untyped receiver would dispatch to a user-defined `+`;
that remaining gap is listed under Open Questions.

### 2. An explicit checked cast; no implicit guard on `::` locals

Add a checked sibling of `asType:`, spelled here `checkedAs:` (name to be
settled; `as:` is ruled out by ADR 0025):

```beamtalk
n := opts at: #max checkedAs: Integer      // answers Integer, raises on a mismatch
n :: Integer := opts at: #max              // unchanged: unchecked, still lintable
n := (opts at: #max) asType: Integer       // unchanged: unchecked, erased
```

`asType:` stays the unchecked escape hatch (question 2). A narrowing `::` local
stays unchecked, which keeps ADR 0025's erasure rule intact for annotations.
The `narrowing-cast` lint message gains a hint pointing at `checkedAs:`.

Semantics of `checkedAs: C`:

- **Built-in class** (`Integer`, `String`, `Symbol`, …): the inline
  `routing::type_test(C)` test, the one `match:` arms use.
- **Any other class (user class, actor, supervisor):** exactly the test a
  `c :: C` match arm compiles to, from the one renderer in
  `patterns::type_tests`, which already handles tagged maps, actor
  `{beamtalk_object, ...}` tuples and supervisor tuples, with the hierarchy
  check as fallback so a subclass instance passes. Not "the typed-send tag
  check", which only matches tagged maps. (Behaviour for a dead pid or a class
  removed/redefined after compile is an open question below.)
- **`String` vs `Binary`:** a `String` is the tagged tuple and a bare binary is
  `Binary`. Text from stock Erlang modules and decoded JSON is a raw binary, so
  `checkedAs: String` would raise on exactly the FFI boundaries this form
  targets; see Open Questions.
- **Union:** the test of any member. **Alias:** expands first (ADR 0108).
- **Parametric, shape, protocol, type variable:** only the outer class is
  tested (`List`, `Dictionary`). A protocol or type variable is a compile
  error ("cannot be checked at run time"), not a silent pass. A shape is
  proposed to behave as its outer class and answer that class, **not**
  `Known(shape)`: ADR 0147 says shapes are not a way to validate external
  data, and an `is_map` test also admits tagged Value objects. (Open question
  whether a shape argument should instead be a compile error.)
- **Form:** `asType:` is a codegen special form with no runtime method
  (`dispatch_codegen/mod.rs`), so `checkedAs:` cannot be "a plain message"
  with an argument evaluated at run time: `Integer | Nil` and `List(Integer)`
  are type syntax, not expressions. It is the same kind of special form,
  requiring a literal class/type argument (anything else is a compile error);
  `perform: #checkedAs:` is not supported. Whether to also add a real
  `Object>>checkedAs:` for dynamic class arguments is an open question, and
  ADR 0025's "runtime dispatch is the contract" favours it.
- **On mismatch:** `#beamtalk_error{kind = type_error}` raised at the cast
  expression. The record has no position field, so the expected class, actual
  class and source position go in `details` (new keys to be named at
  implementation). Note the kinds now in play: part 1 raises
  `does_not_understand`, raw arithmetic `type_error` (from `badarith`),
  and `pattern_mismatch` exists for failed match arms; the implementation
  must pick one cast-failure kind deliberately.

```
> (#{#max => "ten"} at: #max) checkedAs: Integer
=> type_error: expected Integer, got String (at the checkedAs: on line 1)
```

(Proposed output; nothing here is implemented.)

### 3. Not now: an implicit guard on narrowing `::` locals

Recorded as the alternative the issue proposed, with the conditions that would
reopen it (see Recommendation changes).

## Answers to the five questions

**1. Where the guard applies.** The explicit form applies wherever it is
written, in any class, typed or not; it is opt-in per site, so there is no
class-level switch and no REPL/build divergence. For the implicit variant (not
adopted), the answer would be `typed` classes only, since `typed` already means
"opts into thorough checking" and untyped exploratory code stays erased.

**2. Unchecked escape hatch.** `asType:` already is one and needs no new
spelling. Under an implicit guard it would be the way to opt a hot site out.

**3. Migration.** Part 2 has none: no existing program changes meaning. Part 1
changes only programs where a comparison receiver on a typed parameter or
field holds a non-number/string, which today answer by atom/number term order;
those are bugs. Measured narrowing-cast counts (command below):

| Tree | Narrowing casts | Notes |
|---|---|---|
| `stdlib/src` | 3 | all `:: String from Dynamic`, `system_navigation.bt` lines 907, 2241, 2489 |
| `examples/` | 0 | |
| `stdlib/test` | 41 (+9 added by this ADR's fixture) | `:: SomeActor from Object`: spawn results and supervisor child lookups; no built-in-typed casts |
| `tests/` | 1 | `:: E2EInitChildA from Object` |
| `benchmarks/` | 1 | vs-elixir row 13b (`s :: String := Erlang erlang integer_to_binary: a`) |
| mint, buccaneer, catalina | **not measured** | repos are not reachable from this session |

Run in each satellite repo root, then add the totals to this table:

```bash
beamtalk type-coverage src --format json | python3 -c 'import sys,json; j=json.load(sys.stdin); print(j["narrowing_casts"])'
beamtalk type-coverage src --detail | sed -n '/^Narrowing casts:/,/^$/p'
```

For scale only: ADR 0147's survey counted literal-key `at:` reads (buccaneer 89,
beamtalk-http 11, catalina 16). That is a proxy for dictionary casts, not a
count of them, and BT-4736 has since moved the option aliases to shapes.

**4. Map-shape types.** ADR 0147 deletes the typed option-map reads, which were
the largest cast family in the libraries, so a checked form would have had to
fire there mostly for casts that no longer exist. What remains is where
untyped data enters: decoded JSON/YAML (string keys, outside ADR 0147), FFI
results, spawn results and the one shape assertion at a config boundary. Those
are few and deliberate, which suits an explicit per-site form better than a
blanket guard. A shape cast checks only "is a map" in v1; key and value
checks would cost O(size) and are left to a validator.

**5. Measured cost.** See below. The short version: a built-in-class guard is
about one `is_*` test; a user-class guard costs far more until BT-4769 lands;
and part 1 costs about the same as the untyped guard.

## Measured cost

Measured here (the committed `benchmarks/vs-elixir/README.md` figures):

| Row | What | ns/op | Reading |
|---|---|---|---|
| 11b | `a + 1`, `a :: Integer` (bare `erlang:'+'`) | 0.23 | the only row whose speed depends on a trusted declared type |
| 11c | `a + 1`, `a` untyped (`is_number` guard + BIF) | 1.31 | guard delta is **~1.1 ns**, 0 reductions |
| 12b | `p isKindOf: VbPoint` (user class, hierarchy path) | 6814 | what a user-class guard costs today if routed through `isKindOf:` |

Re-running rows 11a-c, 12b and 14a in this cloud container was too noisy to
use (11c came out at 8.0 ns against 1.3 committed); I rely on the committed
figures and did not change them.

Rows that depend on a trusted declared type: **only 11b** (and the guarded
typed-send rows 1a and 14a, which already stand behind a guard and are
unaffected). Row 13b is the only vs-elixir row containing a narrowing cast, so
it is the only row an implicit `::` guard would slow. No row measures a
comparison on a typed parameter, so part 1 has no row today; BT-4823 adds **11e**
(`a < 1`, `a :: Integer`). BT-3928 rejected an *argument* check partly because
the stdlib sort and `max:` hot paths compare typed values with no benchmark to
measure the cost; BT-4823 adds a *receiver* branch on those same paths, so its
before/after numbers for them, not the 11b/11c delta, are the figure to trust.

Estimates, not measurements:

- **Part 1:** ~1 ns per comparison on a typed parameter or field, from the
  11b/11c delta. BT-3925 found the guarded untyped Actor counter loop no
  slower than the bare one (`self_send_bench`, `SsbMain counters`).
- **`checkedAs:` on a built-in class:** ~1 ns, one `is_*` test, the same code
  as `routing::type_test`. Only `String` (tag tuple test) and `Symbol`
  (atom, not `true`/`false`/`nil`) are multi-instruction.
- **`checkedAs:` on a user class:** unmeasured. Through `isKindOf:` it would
  cost as much as row 12b (hundreds of ns to µs). An exact-tag test on the hit
  path should be a few ns, but a subclass instance falls to the hierarchy
  path. Measure once BT-4769 lands.
- **A library bench** (the second half of question 5) was **not run**: no
  satellite library is reachable here.

## Prior Art

- **Dart, Kotlin, Swift (`as` / `as!`):** checked downcasts that throw. This
  is the model for `checkedAs:`. They are explicit per site.
- **TypeScript `as`, Python `typing.cast`:** erased and unchecked, like
  `asType:`. Runtime validation lives in separate libraries (zod, typeguard).
- **Gleam:** no casts; untyped data goes through decoders. The nearest thing
  to ADR 0147's boundary validator.
- **Erlang/Elixir:** specs are not enforced; function-head guards are. In OTP
  a wrong value reaching `when is_integer(N)` fails the clause; trusting a
  declared type with no guard is *less* safe than that, which part 1 corrects.
- **Racket (contracts):** checked at module boundaries, with real cost for
  higher-order values. The warning against checking generic arguments.
- **Pharo/Squeak:** untyped; `assert:` is the idiom. `checkedAs:` is the typed
  analogue.

## User Impact

- **Newcomer:** annotations behave as annotations: they never change an
  answer when the typed value is the receiver (part 1 restores that); the
  argument position keeps the documented term-order fallback (BT-3928). The wrong-cast error, when asked for, names
  the types and the line.
- **Smalltalk developer:** `x checkedAs: Integer` reads like `assert:`-plus-
  cast; the message form needs no new syntax.
- **Erlang/Elixir developer:** `nil > 10` no longer answers `true` for a typed
  parameter, matching how Dialyzer would treat it as a bug, not a feature.
- **Operator:** a wrong value now fails at the boundary that admitted it, not
  several calls later. Cost is ~1 ns per typed comparison.
- **Tooling developer:** the lint hint can offer a quick fix; the checker
  already returns `Known(C)` for `asType:` and does the same for `checkedAs:`.

## Steelman Analysis

| Option | Strongest case |
|---|---|
| **Lint only** | *Language designer:* erasure is the contract; a guard that raises makes annotations load-bearing and splits "typed" from "untyped" semantics. *Operator:* zero cost, zero migration risk. |
| **Guard every narrowing local** | *Newcomer:* "if I wrote `:: Integer` and it is not one, tell me now." *Operator:* the cast line is where the data entered; every later error is misleading. *Tooling:* one rule, no new selector. |
| **Explicit `checkedAs:`** | *Smalltalk purist:* it is a message, like `asType:`. *Erlang veteran:* mirrors a function-head guard written where you want it. *Operator:* no behaviour change to ship. |
| **Fix the comparison hole only (part 1)** | *Language designer:* it removes the one place where the contract is actually broken, for every route, at ~1 ns. |

Tension: newcomers and operators want the implicit guard; the language
designer and the satellite maintainers (unmeasured migration cost) want
erasure kept. The no-cast route in the finding is why the explicit form does
not give up much: the implicit guard would close only one of several routes
into the same hole.

## Alternatives Considered

### Lint only (BT-4719 as shipped)
Cheapest, no semantic change. Rejected as the whole answer because it leaves
the comparison hole open: a lint cannot see the `Dynamic`-argument route.
Part 1 closes the hole for every route, so lint-only remains acceptable for the
cast itself.

### Guard every narrowing site, in all classes
The issue's proposal. Rejected for now: it closes only one route, makes
annotations load-bearing in untyped code and the REPL, forces a migration that
is unmeasured for three libraries, and a user-class guard is expensive today.
It becomes attractive again under the conditions below.

### Guard narrowing sites in `typed` classes only, warn first
The staged variant: a lint level `narrowing-cast = "check"`, off by default,
per package in `beamtalk.toml`. Keeps untyped code erased. Rejected for now
because it makes generated code depend on a manifest setting, which sits badly
with the no-image-mechanics rule for the REPL (no package) and because the
explicit form delivers the same diagnostics site by site.

### Explicit checked form (`checkedAs:`)
Adopted as part 2.

### Checked narrowing with existing `match:` (not in the draft)
`x match: [n :: Integer -> n; _ -> self error: ...]` already narrows with a
run-time check and needs no new selector. Weakness: verbose, and the `_` arm
is easy to omit or fill with a silent default. A stdlib helper built on
`match:` would give `checkedAs:`'s call-site brevity without a new special
form. Added at review; the owner should weigh it against part 2.

### Ship part 1 only, defer part 2 (not in the draft)
With BT-4823 landing, the measured cast population (3 stdlib `String` casts,
0 in examples, actor casts in tests) shows no built-in-typed cast that needs a
checked form. Deferring part 2 until the satellite counts exist costs nothing
now and avoids adding a selector with unspecified actor/`String` semantics.

## Recommendation, and what would change it

Adopt part 1 first; it is the real fix and does not depend on any decision
about casts. Adopt part 2 as the cast story, and keep the lint. Defer the
implicit guard.

I would move to a `typed`-class implicit guard (the staged variant) if:

- the satellite counts come out small (a few dozen per library) *and* running
  them with a prototype guard shows real wrong casts already hiding there, or
- part 1 is rejected and typed parameters stay trusted, since then a wrong
  value has fewer chances to be caught, or
- user-class guard cost falls to a few ns after BT-4769, removing the main
  performance objection.

I would drop part 2 and keep lint only if part 1 lands and the satellite
counts show casts concentrated in code that already validates its input.

## Consequences

### Positive
- The silent wrong answer is closed for every route *into the receiver*, not
  only casts. The argument side stays open by decision (BT-3928): `10 < n`
  with `n :: Integer` holding `nil` is still a bare `erlang:'<'` and answers
  `true`, and a literal receiver is never guarded. The evidence test does not
  cover this reversed form; BT-4823 should pin it as intended behaviour.
- ADR 0025's "never results" rule becomes true for comparisons.
- Explicit checked casts exist for the boundaries that need them, at no
  migration cost.

### Negative
- Typed-parameter comparisons gain ~1 ns (estimate).
- Arithmetic on a lying typed parameter still reports `type_error` and cannot
  reach a user-defined `+`.
- The comparison guard's `is_binary` admits a bare `Binary`, not a tagged
  `String` (`{beamtalk_string, Bin}`, `routing` `StringTag`). A `String`
  receiver therefore already dispatches (term-compares, one hop slower, per the
  comment in `operators.rs`), and `"abc" > 10` still answers by term order, in
  untyped code as well. Part 1 does not change that.
- A `:: String` parameter or field is in `COMPARABLE_TYPES` and is bare today,
  so under part 1 it moves to the guarded dispatch path: more than the ~1 ns
  an `Integer` receiver pays. BT-4823's benchmark must include a `String`
  comparison row (reviewer finding; not in the original estimate).
- Two unchecked-cast spellings remain (`::` local, `asType:`) beside one
  checked one.

### Neutral
- Row 11b is unchanged; BT-4823 adds an 11e row that records the part 1 cost.

## Open Questions

- Selector name: `checkedAs:` vs `assertType:` vs another.
- Should arithmetic on trusted parameters also take the guard, to make
  "never results" literally true (gives up row 11b's 0.23 ns)? This ADR keeps
  the bare path because its failure is loud.
- What does `checkedAs:` test for an **actor** class? `routing::type_test`
  returns `None` for actors and user classes, and the measured casts in
  `stdlib/test` and `tests/` are almost all `:: SomeActor from Object`. The
  semantics above only specify built-in and tagged user classes. This must be
  settled before part 2 is accepted.
- Does part 2 have demonstrated demand? The measured casts are 3 `String`
  casts in stdlib, 0 in examples and actor casts in tests; no built-in-typed
  cast needs a checked form today. The owner may prefer to accept BT-4823,
  keep the lint, and defer part 2 until the satellite counts exist.
- Should BT-4823 also cover `=`/`~=`, `max:`/`min:` and `between:and:` on a
  typed receiver, if they can answer wrongly? BT-4823 asks for this to be
  checked; record the finding here once known.
- `String` vs raw binary: a reviewer reading `erlang_proxy` found only
  charlists are converted to `String`; other FFI/decoded text stays a raw
  `Binary`. So the `String` tag test does **not** match FFI results before
  coercion, and row 13b's `s :: String := Erlang erlang integer_to_binary: a`
  is itself a wrong cast. Does `checkedAs: String` accept a `Binary`, and does
  the implicit-guard question change? (Needs the owner; not resolved here.)
- `checkedAs:` on a dead pid, or a class removed/redefined after compile
  (ADR 0025 Amendment §2 caveats): raise, or fall back to the name-only test?
- Tooling the draft omits: `type-coverage` must count `checkedAs:` as checked
  rather than narrowing, the `narrowing-cast` lint must skip it, and the LSP
  quick fix needs an owner. Add to Implementation once part 2 is accepted.

## Implementation

Part 1 is BT-4823 (in progress, separate PR). Part 2 is not started, and no implementation issues are filed until this ADR is
accepted.

Part 1 (BT-4823, not this ADR's work) touches `receiver_is_statically_comparable` / `operator_guard`
(`core_erlang/operators.rs`) and the comparison half of `field_types.rs`
(`BareOperator::Comparison`, `trusted_param_types` for comparisons); it flips
the pinned assertions in `narrowing_cast_wrong_value_test.bt`. Part 2 touches
the type checker (`inference/send/receiver.rs`, next to `asType:`), the
dispatch lowering (`dispatch_codegen/mod.rs`), the shared `routing::type_test`
renderer (one rendering, no copy), the `narrowing-cast` lint hint, and
`docs/beamtalk-language-features.md` § Local Variable Type Annotations.

## Migration Path

Part 1 (BT-4823): one release note; no source change expected. Part 2: none.

## References
- Related issues: BT-4772 (this ADR), BT-4719 (visibility), BT-2015 (accepting
  narrowing silently), BT-4796, BT-4788, BT-4769 (`isKindOf:` fast paths),
  BT-4736 (option aliases to shapes), BT-4823 (part 1 fix), BT-3928
  (comparison argument stays unchecked), BT-3921 / BT-3924 / BT-3925 (trusted
  operator types), BT-1944 (erasure test)
- Related ADRs: ADR 0025 (gradual typing; `asType:`; Amendment BT-4118),
  ADR 0108 (aliases), ADR 0116 (operator guards), ADR 0147 (map-shape types),
  ADR 0004 (why `as:` is taken)
- Test: `stdlib/test/narrowing_cast_wrong_value_test.bt`
- Benchmarks: `benchmarks/vs-elixir/README.md` rows 11b, 11c, 12b, 13b
