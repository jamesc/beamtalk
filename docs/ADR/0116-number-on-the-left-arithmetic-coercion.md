# ADR 0116: Double-Dispatch Coercion for Number-on-the-Left Arithmetic

## Status
Proposed (2026-08-25)

## Context

ADR-less Phase 1 of the "operators as messages" epic (BT-2708) — implemented in
BT-2709 and BT-2710 — made `+ - * /` and `< > <= >=` dispatchable messages so a
user value-type can overload them as the *receiver*: `aMoney + bMoney` works
because `Money` defines `+`. The codegen shape (`generate_binary_op` in
`crates/beamtalk-core/src/codegen/core_erlang/operators.rs`) is:

- **Statically known numeric receiver** (a numeric literal, `self` inside
  `Integer`/`Float`, or an identifier bound to a `:: Integer/Float/Number`
  parameter) → bare `call 'erlang':'+'(...)`. Zero cost, no runtime check —
  this is the fast path that covers essentially all stdlib hot-loop
  arithmetic.
- **Everything else** → a runtime `is_number` guard
  (`OperatorGuard::Arithmetic` / `guarded_op_doc`): `is_number(Left)` true →
  bare BIF, false → `beamtalk_message_dispatch:send(Left, Op, [Right])`. This
  is what makes `aMoney + b` dispatch to `Money>>+`.

This is **receiver-dispatch only**, by design (BT-2709's explicit scope cut).
It has no answer for `5 + aVector` — a number on the **left**, a
non-numeric value-type on the right. Concretely:

```beamtalk
5 + aVector    "aVector is a Vector, not a Number"
```

Here the left operand is a numeric literal, so
`receiver_is_statically_numeric` is `true` and codegen emits the bare
`erlang:'+'(5, X)` with **no guard at all**. At runtime this crashes with a
raw `badarith` — not even a `does_not_understand`, since the dispatch layer
is never reached. The same gap exists for any receiver that is guarded but
turns out numeric at runtime (`guarded_op_doc`'s `is_number` branch also
calls the bare BIF): whenever the *left* operand is a number, the *right*
operand is assumed to be one too, and nothing catches the case where it
isn't.

### Constraints carried over from Phase 1

1. **The numeric fast path must stay zero-cost.** BT-2709 measured
   guard-vs-bare overhead specifically to protect this; any fix here must not
   turn `total + delta` (right operand a plain numeric variable) into an
   always-guarded operation.
2. **Static typing must stay informed.** BT-2709 deliberately avoided
   `perform:`-based dynamic dispatch so the type checker keeps a known return
   type at each call site (covariant-return refinement, ADR 0068). Any
   number-on-the-left mechanism must preserve that — a reflected hook needs a
   declared, checkable signature, not a string-keyed `perform:`.
3. **No scalar tower exists on BEAM.** Beamtalk has no `Fraction`,
   `ScaledDecimal`, or `Rational` class — `erlang:'+'` already fuses
   int↔float natively. Classic Smalltalk "generality" (rank types on a total
   order, coerce the lower-ranked operand, retry) was built to solve exactly
   that mixing problem, which BEAM has already solved at the BIF level.
4. **Comparison operators are out of scope for this ADR.** `< > <= >=`
   share the same operator-guard machinery (BT-2710) but fail differently:
   Erlang's `<` never raises, so `5 < aVector` today silently term-orders
   instead of crashing — a worse failure mode (silent wrong answer vs. a
   crash) tracked as a separate follow-up, not designed here.

## Decision

Beamtalk adopts **double dispatch via per-operator reflected methods** — not
a generality/coercion tower, and not a `perform:`-based reflective retry.

### Reflected method protocol

A value-type that wants `aNumber <op> self` to work declares one reflected
method per arithmetic operator it supports, using the `<verb>FromNumber:`
naming convention:

```beamtalk
Value subclass: Vector
  field: components :: Array = #()

  "receiver-dispatch: aVector + 5"
  + other :: Number -> Vector => self collect: [:c | c + other]

  "number-on-the-left: 5 + aVector"
  plusFromNumber:  n :: Number -> Vector => self collect: [:c | n + c]
  timesFromNumber: n :: Number -> Vector => self collect: [:c | n * c]
```

Each reflected method is an ordinary, fully-typed method — no `perform:`, no
dynamic selector construction. There is exactly **one** reflected method per
operator (`plusFromNumber:` / `minusFromNumber:` / `timesFromNumber:` /
`divFromNumber:`), not the `adaptToInteger:` / `adaptToFloat:` /
`adaptToFraction:` fan-out Pharo needs — because BEAM fuses int/float into a
single effective `Number` category on the left, one hook per operator
suffices.

### Dispatch mechanism (codegen)

The two call sites in `operators.rs` that currently emit a bare
`erlang:'<op>'(...)` whenever the *left* operand is (statically or
dynamically) known to be a number — the always-bare fast path, and the
`is_number`-true branch of the existing Phase 1 guard — both wrap that BIF
call in a `try`/`catch` on `badarith`:

```erlang
try
    call 'erlang':'+'(Left, Right)
catch
    <'error', 'badarith', _Stack> ->
        call 'beamtalk_message_dispatch':'send'(Right, 'plusFromNumber:', [Left])
end
```

`number + number` never raises `badarith`, so the `try` costs nothing on the
happy path and the `catch` only ever fires when `Right` isn't a number — at
which point it becomes a normal message send to `Right`, resolved through
the existing dispatch layer. If `Right`'s class has no `plusFromNumber:`, the
dispatch layer's ordinary `does_not_understand` handling applies — no new
error path to build.

This is deliberately the one place BT-2709 avoided (catch-on-failure instead
of a guard) — correct here specifically because the happy path (number +
number) can never raise, unlike the general receiver-dispatch case where a
non-numeric receiver is the *common* case a guard has to cover cheaply.

### REPL example

```
> 5 + aVector
Vector(6, 7, 8)
> 5 + "not a vector"
does_not_understand: 'plusFromNumber:' not understood by String
> aVector + 5
Vector(6, 7, 8)
```

### What this ADR does not do

- **No generality/coercion tower.** No `generality` ranking, no
  `adaptTo:andSend:`. If a future closed numeric family (`Rational`,
  `BigDecimal`, `Complex`) needs tower-style promotion, it implements the
  same `plusFromNumber:`-style hooks itself — coercion becomes a private
  detail of that hook's body, not a language mechanism.
- **No comparison-operator reflection.** `5 < aVector` is unchanged by this
  ADR; its silent-wrong-order behavior is tracked separately.
- **No central protocol declaration.** Reflected methods are opt-in per
  type, discovered the same way any other message is — via
  `does_not_understand` when absent. `Number.bt` is not modified.

## Prior Art

| Language | Mechanism | Verdict |
|---|---|---|
| **Pharo/Squeak Smalltalk** | `generality`-ranked `retryRelationalOp:coercing:` / `aNumber adaptToInteger: self andSend: #+`, dispatched via `perform:` | Rejected wholesale — the generality ladder assumes a total-ordered scalar tower we don't have, and non-tower types (`Vector`, `Money`) have no meaningful rank. The `perform:` retry is opaque to Beamtalk's type checker, which Phase 1 was specifically built to avoid. |
| **Python** | `__radd__`/`__rmul__` reflected dunder methods, tried automatically when the left operand's `__add__` returns `NotImplemented` | Adopted as the closest model — one statically-dispatchable method per operator, owned by the type that wants to participate, with no shared coercion ladder. Beamtalk's difference: the trigger is a caught `badarith` from the numeric BIF rather than a sentinel return value, since the numeric fast path never returns a sentinel — it's a bare BIF call. |
| **Gleam** | No operator overloading at all; `+`/`+.` are fixed to `Int`/`Float`, mixed-type or custom-type arithmetic requires named functions | Rejects the whole problem by design. Not viable for Beamtalk, which already committed to overloadable operators in BT-2709/2710 for value-type ergonomics (`Money + Money`, `Vector + Vector`). |
| **Elixir/Erlang** | No user-defined operator overloading; `+` is fixed to numbers, structs use named functions (`Vector.add/2`) or protocols (`Numeric` behaviour, via libraries) | Confirms BEAM itself has no operator-dispatch story to inherit — Beamtalk's message-based operators are already a deliberate departure the epic (BT-2708) took on. |
| **Newspeak** | No numeric coercion protocol distinct from ordinary message dispatch — arithmetic is just messages, and mixed-type numeric towers are avoided by keeping the numeric class hierarchy shallow | Reinforces "keep it as ordinary dispatch" — supports treating `plusFromNumber:` as just another message rather than inventing new machinery. |

## User Impact

- **Newcomer:** `5 + aVector` failing today with a raw `badarith` is a
  confusing first BEAM-interop surprise — nothing in the Beamtalk syntax
  suggests Erlang is involved. After this change, the failure mode for an
  unsupported type becomes an ordinary `does_not_understand`, consistent
  with every other message send in the language, and the success case
  (`Vector` implementing the hook) "just works" without the newcomer needing
  to know left-vs-right dispatch exists.
- **Smalltalk developer:** Recognizes the shape (`aVector + 5` and
  `5 + aVector` both dispatching to `Vector`) even though the underlying
  mechanism (`plusFromNumber:` reflected methods vs. `generality`/
  `adaptTo:andSend:`) is different from what they know. The departure is
  documented here with its rationale (no scalar tower on BEAM) rather than
  left as an unexplained gap from Pharo.
- **Erlang/Elixir developer:** The generated code is exactly the
  `try`/`catch` idiom they'd write by hand for "try the fast numeric op,
  fall back to a dispatch on failure" — no magic, inspectable in the
  compiled `.core`/`.beam` output.
- **Production operator:** The `try`/`catch` is scoped tightly around the
  single BIF call, so a crash report distinguishing "genuine badarith bug"
  from "number-on-the-left dispatch" stays legible in stack traces —
  the catch only ever re-raises as `does_not_understand`, never masks an
  unrelated error.
- **Tooling developer:** `plusFromNumber:` is an ordinary typed method, so
  LSP completion, hover types, and static DNU checking all work on it with
  zero special-casing — the same infrastructure that already handles
  `+`/`-`/`*`/`/` covers the reflected names.

## Steelman Analysis

### Alternative: Generality/coercion tower
- 🧑‍💻 **Newcomer:** "If I already know Smalltalk's numeric tower, this is
  one fewer thing to relearn — same mental model as Pharo."
- 🎩 **Smalltalk purist:** "This *is* Smalltalk's actual answer to this
  problem, refined over decades — reinventing it risks missing edge cases
  the tower already solved (e.g. exact vs. inexact arithmetic ordering)."
- ⚙️ **BEAM veteran:** "None — a generality rank has no BEAM analog and
  doesn't map to any existing OTP idiom."
- 🏭 **Operator:** "A single well-tested coercion path might be easier to
  reason about in production than N independent per-type hooks."
- 🎨 **Language designer:** "It's elegant *if* the domain is genuinely a
  scalar tower — but Beamtalk's actual target types (`Vector`, `Money`,
  `Matrix`) aren't scalars, so the elegance doesn't transfer."

### Alternative: `perform:`-based reflective retry (`adaptTo:andSend:`)
- 🧑‍💻 **Newcomer:** "Fewer method names to learn — one reflective hook
  instead of one method per operator."
- 🎩 **Smalltalk purist:** "This is the literal, faithful Smalltalk
  mechanism — closest to what they already know from Pharo."
- ⚙️ **BEAM veteran:** "None — dynamic `perform:` dispatch has no
  particular BEAM-native benefit over a typed method."
- 🏭 **Operator:** "None — opaque dynamic dispatch is *harder* to trace in
  production, not easier."
- 🎨 **Language designer:** "Less boilerplate per type is a real
  maintainability win worth weighing against the type-checker cost."

### Tension points
- The Smalltalk-purist case for both rejected alternatives is genuinely the
  strongest of any cohort — both are more faithful to Pharo/Squeak than the
  chosen design. Beamtalk's departure is justified by constraint 2 (static
  typing must stay informed), which is specific to this codebase's gradual
  typing design (ADR 0025/0068), not a claim that generality or `perform:`
  are bad ideas in general.
- Language designers and Smalltalk purists would pick the generality tower
  or `perform:`-retry for their elegance/faithfulness; BEAM veterans and
  operators are indifferent-to-negative on both, and side with the chosen
  design once static-typing preservation is on the table.

## Alternatives Considered

### Generality/coercion tower (`generality`-ranked retry)
Rank every numeric-participating type on a total order and retry the
operator after coercing the lower-ranked operand to the higher-ranked type's
representation, à la Pharo `Number>>retryRelationalOp:coercing:`. Rejected:
the tower this exists to order (`SmallInteger < Fraction < Float < …`) barely
exists in Beamtalk (BEAM already fuses int/float at the BIF level, and there
is no `Fraction`/`ScaledDecimal`/`Rational`), and the types this ADR actually
needs to support (`Vector`, `Money`, `Matrix`) have no meaningful rank
relative to `Number` — `5 + aVector` isn't "promote 5 to a vector," it's
"broadcast 5 across the vector," which a coerce-to-common-type model can't
express.

### `perform:`-based reflected dispatch (`adaptToInteger:andSend:`)
Re-invoke the operator dynamically via `anObject adaptTo: self andSend: #+`,
internally using `perform: aSymbol with: anArgument`. Rejected: this is
opaque to the type checker — Phase 1's entire design (BT-2709) exists to
keep a static fast path and covariant-return inference (ADR 0068) working
for arithmetic, and a `perform:`-based retry throws that guarantee away for
exactly the call sites this ADR touches.

### Static guard on the right operand's type
Instead of catching `badarith`, emit a proactive runtime check on the right
operand (e.g. `is_number(Right)`) alongside the existing left-operand guard,
so both operands are checked before ever calling the bare BIF. Rejected: the
common case is `total + delta` — a numeric literal or known-numeric
receiver on the left, an *arbitrary, usually also-numeric* variable on the
right. Guarding the right operand unconditionally would regress that
majority case from a zero-cost bare BIF to an always-checked one, which is
exactly the cost Phase 1 was built to avoid. The catch-on-`badarith`
approach only pays a cost when the fallback actually fires.

### Extend reflection to comparison operators now
Apply the same `<verb>FromNumber:` pattern to `< > <= >=` in this ADR,
since they share the guard machinery. Deferred, not rejected: comparison's
failure mode (silent wrong term-ordering, since Erlang `<` never raises) has
no `badarith`-style signal to catch, so it needs its own mechanism (likely a
proactive `is_object`-style check on the right operand, mirroring BT-2710's
existing left-operand guard) — different enough to warrant its own
follow-up rather than folding it into this arithmetic-specific design.

## Consequences

### Positive
- `5 + aMoney`, `5 * aVector` and similar number-on-the-left expressions
  resolve to the value type's own arithmetic instead of crashing.
- The numeric happy path (`number <op> number`) stays a bare BIF call
  wrapped in a `try` that never actually unwinds — no guard, no regression
  to Phase 1's zero-cost invariant, pending the benchmark called out below.
- Reflected methods are ordinary typed methods: return types are known
  statically, so covariant-return inference (ADR 0068) and compile-time DNU
  checking work on them for free.
- No new language-level concept (no `generality`, no reflective `perform:`)
  — the mechanism is "a message send from inside a catch handler," which
  the runtime already fully supports.
- A type author who doesn't need number-on-the-left arithmetic pays zero
  cost and needs zero code — the hooks are opt-in.

### Negative
- Per-type boilerplate: a value type wanting full bidirectional arithmetic
  writes up to 8 small methods (`+ - * /` and their `…FromNumber:`
  counterparts) instead of 4. Acceptable per the ticket's own framing — a
  future `deriving`/macro mechanism (BT-2714, synthesized-method deriver
  framework) is the right place to reduce this, not a reason to weaken the
  type-checker guarantee now.
- `try`/`catch` around a BIF call has *some* nonzero runtime cost model
  even when it never triggers (frame setup for the catch), separate from
  the `is_number` guard's cost model BT-2709 already measured — this ADR's
  zero-cost claim needs its own benchmark confirmation before Implemented
  status (see Implementation).
- Asymmetric operators (`5 * aMatrix` vs. `aMatrix * 5` meaning different
  things mathematically) place the burden on each type's author to document
  the semantic difference between its `*` and its `timesFromNumber:` —
  the language enforces nothing here beyond "both exist and are callable."

### Neutral
- Comparison operators (`< > <= >=`) are explicitly out of scope; the
  existing silent-wrong-order behavior for `5 < aVector` is unchanged by
  this ADR and remains tracked as a separate follow-up.
- No generality/coercion hook is exposed for a hypothetical future numeric
  tower type — if one is ever added, it implements `plusFromNumber:` etc.
  like any other type, and any tower-specific promotion logic lives inside
  those method bodies.

## Implementation

Affected components: codegen only (no parser, type-checker, or runtime
protocol changes).

- `crates/beamtalk-core/src/codegen/core_erlang/operators.rs`:
  - Wrap the always-bare arithmetic path (the `else` branch in
    `generate_binary_op` when `receiver_is_statically_numeric(left)` is
    true) in the `try`/`catch` on `badarith`, re-dispatching to
    `<verb>FromNumber:` on the right operand.
  - Wrap the `is_number`-true branch of `guarded_op_doc`'s
    `OperatorGuard::Arithmetic` case the same way, so a *dynamically*
    numeric left operand gets the same fallback as a *statically* numeric
    one.
  - New helper mapping `+ - * /` → `plusFromNumber:` / `minusFromNumber:` /
    `timesFromNumber:` / `divFromNumber:`, built with `Document`/`docvec!`
    typed leaves (CLAUDE.md, ADR 0089) — no `format!()`.
- Codegen regression tests (mirroring the existing
  `tests/expressions.rs` guard tests from BT-2709/2710): assert the
  `try`/`catch` shape is emitted for both call sites, and that a plain
  `number <op> number` expression's generated Core Erlang is unaffected
  outside the added `try`/`catch` wrapper.
- `stdlib/test/*.bt` (BUnit): a `Vector`-style value type implementing
  `plusFromNumber:`/`timesFromNumber:`, asserting `5 + aVector` dispatches
  correctly; a type with no reflected hook, asserting `5 + aThing` raises
  `does_not_understand` (not a raw `badarith`).
- Benchmark: extend the BT-2709 guard-vs-bare harness
  (`runtime/perf/bench_collect_selfhost.escript`,
  `docs/development/benchmarks.md`) with a `try`/`catch`-vs-bare-BIF
  measurement on the numeric happy path, to confirm the zero-cost claim
  before this ADR moves to Implemented.

No `Number.bt` changes — reflected methods are declared only on the types
that opt in, not as an abstract protocol entry, since there is no receiver
to declare `subclassResponsibility` against (the hook is looked up
dynamically on whatever the right operand turns out to be).

## Migration Path

Not applicable — this only changes behavior for expressions that currently
crash with a raw `badarith` (number-on-the-left arithmetic against a
non-numeric right operand). No existing passing code changes behavior.

## References
- Related issues: BT-2712 (this ADR), BT-2708 (epic), BT-2709 (Phase 1,
  receiver-dispatch arithmetic), BT-2710 (Phase 2, comparison operators —
  informs the deferred comparison follow-up), BT-2714 (synthesized-method
  deriver framework — future boilerplate-reduction path)
- Related ADRs: ADR 0068 (parametric types and covariant returns), ADR 0025
  (gradual typing and protocols), ADR 0042 (immutable value objects), ADR
  0089 (typed document leaves), ADR 0002 (Erlang comparison operators —
  equality operators' non-dispatch precedent)
- Documentation: `docs/beamtalk-language-features.md` § Comparison /
  Additive operators; `docs/development/benchmarks.md`
