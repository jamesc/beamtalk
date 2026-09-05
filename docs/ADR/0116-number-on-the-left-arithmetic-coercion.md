# ADR 0116: Double-Dispatch Coercion for Number-on-the-Left Arithmetic

## Status
Accepted (2026-08-25)

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

  "receiver-dispatch: aVector + 5, aVector - 5"
  + other :: Number -> Vector => self collect: [:c | c + other]
  - other :: Number -> Vector => self collect: [:c | c - other]

  "number-on-the-left: 5 + aVector, 5 - aVector"
  plusFromNumber:  n :: Number -> Vector => self collect: [:c | n + c]
  timesFromNumber: n :: Number -> Vector => self collect: [:c | n * c]
  minusFromNumber: n :: Number -> Vector => self collect: [:c | n - c]
```

Operand order matters for non-commutative operators and is easy to get
backwards: `minusFromNumber:`'s receiver (`self`) is the value that was on
the **right** of the original expression, and its parameter (`n`) is the
value that was on the **left** — `5 - aVector` sends `aVector
minusFromNumber: 5`, computing `n - self` (`5` minus each component), not
`self - n`. `aVector - 5` (receiver-dispatch `-`, unchanged from BT-2709)
computes the opposite: each component minus `5`. The two methods above are
not one implementation reused for both directions — the subtraction is
genuinely reversed between them, same as `divFromNumber:`.

Each reflected method is an ordinary, fully-typed method — no `perform:`, no
dynamic selector construction. There is exactly **one** reflected method per
operator (`plusFromNumber:` / `minusFromNumber:` / `timesFromNumber:` /
`divFromNumber:`), not the `adaptToInteger:` / `adaptToFloat:` /
`adaptToFraction:` fan-out Pharo needs — because BEAM fuses int/float into a
single effective `Number` category on the left, one hook per operator
suffices. (`divFromNumber:` reflects `/`, Beamtalk's single division
operator — distinct from the separately-named `div:` keyword message for
integer division on `Integer`, so the shared "div" substring doesn't
collide as a selector, just as a reading hazard worth a beat of attention.)

#### When an operator doesn't apply: implement it anyway, to reject with intent

Not every reflected operator makes sense for every type — `5 / aVector`
("a number divided by a vector") rarely has an obvious meaning the way
`5 * aVector` (scale) or `5 + aVector` (broadcast-add) do, and some types
are point-like rather than vector-space-like (a `Temperature` sensibly
supports `5 + aTemperature`, an offset, but not `5 * aTemperature`, which
has no physical meaning). For those, the recommended pattern is to
implement the method anyway, with a body that rejects explicitly:

```beamtalk
divFromNumber: n :: Number -> Vector =>
  self error: "Cannot divide a number by a Vector — did you mean (aVector / n)?"
```

This is better than simply omitting the method. Omitting it leaves
`does_not_understand` to fire on the *synthesized* selector
(`'divFromNumber:'`) — a name the caller never typed (they wrote `5 /
aVector`) — which is harder to connect back to their own code than an
ordinary DNU is (see the dispatch-mechanism hint below, which narrows but
doesn't eliminate this gap for methods left unimplemented). A deliberate
`self error:` names the actual problem and, where relevant, points at the
supported alternative. Practically, this means a type author who commits
to number-on-the-left arithmetic at all will typically implement all four
methods — some as real arithmetic, the rest as intentional rejections —
not a variable subset; see § Consequences, Negative for what this means
for the earlier boilerplate estimate.

### Dispatch mechanism (codegen)

**Trigger condition, refined.** `receiver_is_statically_numeric` (BT-2709)
already exists and takes an arbitrary `Expression`, not just the receiver —
so the same check applies to the *right* operand for free. The mechanism
below only engages when the left operand is (statically or dynamically)
numeric **and** `receiver_is_statically_numeric(right)` is `false` — i.e.
the right operand's type genuinely isn't known at compile time. When the
right operand *is* statically numeric too (a literal, a `:: Number`-family
param, `self` in `Integer`/`Float`, or a numeric/untyped field — the exact
same rule already applied to the left operand), codegen skips this
mechanism entirely and emits the bare BIF, identical to today. Concretely:
`total + delta` where `delta :: Integer` — the common case this ADR must
not tax — never enters a `try` at all; only a right operand whose type is
genuinely unknown (`5 + aVector`) does. This also converts the "does the
`try` cost anything on the happy path" question from "prove it's
negligible everywhere" to "prove it's negligible on the strictly smaller
set of already-dynamically-dispatched call sites" — the de-risking spike
(§ Implementation) measured exactly that narrower question and found the
cost comparable to the already-accepted `is_number` guard's on those same
call sites.

For that remaining case, the call site wraps the BIF call in a `try`/
`catch` on `badarith`, following the same catch-clause shape already used
throughout the codebase (`control_flow/exception_handling.rs`'s
`on_do_catch_preamble`: bind the raw type/error/stack as variables, `case`-
match on them with an explicit fallback arm, re-raise via
`primop 'raw_raise'`, never `erlang:raise/3` — which expects a pre-built
stacktrace term, not the raw internal trace a catch clause binds). Both
operands are `let`-bound before the `try` (mirroring `guarded_op_doc`'s own
`left_var`/`right_var` binding), so the catch handler can reference `Right`
without re-evaluating it — referencing the inlined expression twice, the
way the existing bare fast path embeds `left_code`/`right_code` directly,
would double-evaluate a non-trivial right operand:

```erlang
let BinLeft = <left operand> in
let BinRight = <right operand> in
try
    call 'erlang':'+'(BinLeft, BinRight)
of <TryResult> -> TryResult
catch <Type, Error, Stack> ->
    case {Type, Error} of
        <{'error', 'badarith'}> when 'true' ->
            case call 'erlang':'is_number'(BinRight) of
                <'true'> when 'true' ->
                    %% BinRight IS a number — badarith wasn't a coercion
                    %% miss (e.g. `5 / 0`, or float overflow). Re-raise
                    %% unchanged so the existing badarith classification
                    %% (ADR 0028/BT-2704) still handles it.
                    primop 'raw_raise'(Type, Error, Stack)
                <'false'> when 'true' ->
                    call 'beamtalk_message_dispatch':'send_number_coercion'(
                        BinRight, 'plusFromNumber:', [BinLeft], '+')
                %% is_number/1 is boolean-exhaustive — `erlc`'s own BIF
                %% return-type inference proves this arm unreachable (it
                %% warns "clause cannot match" if present, confirmed by the
                %% spike below) — but `guarded_op_doc`'s existing
                %% `case_clause_fallback` convention adds it anyway,
                %% defensively, rather than depending on that inference
                %% holding across OTP releases. `error({case_clause, _})`,
                %% not `raw_raise` — this is an internal-invariant guard, not
                %% a real exception to propagate.
                <NoMatch> when 'true' ->
                    call 'erlang':'error'({'case_clause', NoMatch})
            end
        %% Not badarith at all (shouldn't occur for this specific BIF call,
        %% but the case must be exhaustive). Binds a fresh throwaway
        %% variable, not `Type`/`Error` again — those are already bound by
        %% the enclosing `catch` clause and stay in scope for `raw_raise`
        %% below without needing to be re-destructured here, mirroring
        %% `on_do_catch_preamble`'s own catch-all arm.
        <OtherPair> when 'true' ->
            primop 'raw_raise'(Type, Error, Stack)
    end
end
```

**Spike-verified** (see Implementation § De-risking spike results): the
`try`/`catch`/`is_number`/`raw_raise` shape — including the mandatory
`try ... of ... catch ... end` form (Core Erlang, unlike Erlang source,
requires the `of` clause explicitly; its absence is a syntax error) and the
mandatory `when` guard on every `case` clause — was hand-compiled with
`erlc` and confirmed to core_lint cleanly, then functionally exercised
end-to-end for all three branches (happy-path add, non-numeric-right
dispatch, and numeric-right re-raise via float overflow). That spike used a
bare `send/3` stub, predating the `send_number_coercion/4` wrapper added
below — the outer `try`/`catch` shape shown above is unchanged by that
addition (still spike-verified), but `send_number_coercion/4` itself is a
new, not-yet-compiled function and should go through the same hand-compile-
and-verify treatment before implementation, not be assumed correct by
extension.

`+ - * /` can each raise `badarith` between two genuine numbers, not only
when the right operand isn't a number: `/` on a zero divisor, and —
empirically verified against the BEAM runtime, since Erlang floats have no
IEEE infinity/NaN representation — `+ - *` on float overflow
(`1.0e308 + 1.0e308` raises `badarith` rather than returning an infinite
float). Naively catching every `badarith` and dispatching unconditionally
would misroute `5 / 0` (divisor `0` *is* a number) to
`beamtalk_message_dispatch:send(0, 'divFromNumber:', [5])`, which fails as
`does_not_understand`: `Integer does not understand 'divFromNumber:'` — a
regression of the existing, documented `badarith` → `TypeError` "bad
arithmetic operation" classification (`beamtalk_exception_handler:wrap_raw/2`,
ADR 0028), which correctly explains it as an arithmetic error today. The
`is_number(BinRight)` check inside the catch handler distinguishes the two
`badarith` causes generically, independent of which specific numeric failure
triggered it: a non-numeric `BinRight` is a genuine coercion miss and
dispatches to the reflected method; a numeric `BinRight` means the failure
is a real numeric error unrelated to coercion (division by zero or float
overflow), and `primop 'raw_raise'` re-raises it with its original class,
reason, and stacktrace — identical to today's uncaught propagation — so it
reaches the same classification layer unchanged regardless of which of the
two numeric causes produced it. The `catch` block itself only ever runs on
an actual `badarith`, so its cost is paid exclusively on the
already-failing path; the `try` wrapper's cost *on the non-failing path,
restricted to call sites where the right operand's type is genuinely
unknown* was measured by the de-risking spike (§ Implementation) and found
comparable to the already-accepted `is_number` guard's cost on those same
call sites — not the zero-cost claim an unqualified reading of "happy path"
might suggest, but the correct comparison given the compile-time skip
already removes the true zero-cost call sites from this mechanism's reach
entirely.

This is deliberately the one place BT-2709 avoided (catch-on-failure instead
of a guard) — correct here specifically because the happy path (number +
number) can never raise, unlike the general receiver-dispatch case where a
non-numeric receiver is the *common* case a guard has to cover cheaply, and
because the refined trigger condition above already removes the
statically-known-numeric-right-operand cases a guard would otherwise have
to cover for free.

Compiler-generated exception handling on Beamtalk's own state-threading
constructs (loops, `on:do:`/`ensure:`, NLR boundaries, class-var
shadow-writes) is required to lower through `ThreadedIr` and its `verify()`
(CLAUDE.md, ADR 0111). This `try`/`catch` doesn't: it threads no state
across the boundary — no class variable, no loop accumulator, no NLR relay
— and lives entirely inside a single expression's evaluation, the same
"general expression codegen stays AST-directed" category the existing
Phase 1 `is_number` guard (`guarded_op_doc`, a plain `case` `Document`) is
already in. It is built directly as a `Document` from the AST and generator
state, with no `ThreadedIr` involvement, consistent with that existing
guard and outside `ThreadedIr`'s scope.

#### Hinting the DNU when the reflected method is missing

A plain `beamtalk_message_dispatch:send(BinRight, 'plusFromNumber:',
[BinLeft])` that finds no `plusFromNumber:` on `BinRight`'s class produces
an ordinary `does_not_understand`: `"<Class> does not understand
'plusFromNumber:'"`. That's a real gap for a selector the caller never
typed — unlike a normal DNU (`foo bar` → `"Foo does not understand 'bar'"`,
directly traceable to the caller's own source), `plusFromNumber:` is
synthesized by this mechanism; someone who wrote `5 + aVector` has no
reason to already know that name.

Rather than teach the general DNU-formatting path (`beamtalk_error.erl`'s
`generate_message/3` / `maybe_enrich_dnu_hint/1`) about this one synthetic
selector family — those stay generic, by design, and have no way to
recover "this was tried because of the `+` on line N" once the failure
reaches them — the fix lives at the one place that still has that context:
the coercion-dispatch call site itself. A new runtime helper,
`beamtalk_message_dispatch:send_number_coercion/4` (`Right, Selector,
Args, OrigOp`), wraps the existing `send/3`:

```erlang
send_number_coercion(Right, Selector, Args, OrigOp) ->
    RightClass = beamtalk_primitive:class_of(Right),
    try
        send(Right, Selector, Args)
    catch
        error:#{'$beamtalk_class' := _,
                error := #beamtalk_error{kind = does_not_understand,
                                          class = RightClass,
                                          selector = Selector} = Error}:Stack ->
            %% RightClass and Selector are already bound above/as this
            %% function's argument — the catch pattern re-uses them, so this
            %% only matches a does_not_understand for *this exact* class and
            %% selector, not any DNU that happens to propagate through.
            Hint = iolist_to_binary(io_lib:format(
                "~s has no '~s' — implement it to support 'number ~s ~s' arithmetic",
                [RightClass, Selector, OrigOp, RightClass])),
            HintedError = beamtalk_error:with_hint(Error, Hint),
            erlang:raise(error, beamtalk_exception_handler:wrap(HintedError), Stack)
    end.
```

This mirrors the real shape a DNU actually propagates as:
`beamtalk_error:raise/1` (the path every production DNU already goes
through — e.g. `raise_class_dnu`/`raise_class_self_dnu` in
`beamtalk_class_dispatch.erl`) wraps the `#beamtalk_error{}` via
`beamtalk_exception_handler:wrap/1` into a `#{'$beamtalk_class' := ...,
error := #beamtalk_error{...}}` tagged map *before* calling `erlang:error/1`
— so a `catch` here has to match that wrapped shape, not the bare record,
and the re-raise has to re-wrap the hinted error the same way to stay
consistent with every other exception in the system.

Why match on both fields, not just the selector: if `plusFromNumber:`
*does* exist on `Right`'s class but its body calls something else that
itself DNUs (an unrelated bug inside the user's own method), that inner
failure has a different selector and must propagate unchanged — not get
relabeled as "coercion hook missing" when the hook was actually present
and something else broke. Same discipline § Dispatch mechanism already
applies to `badarith`: narrow matching on the exact failure this call site
can actually produce, not a broad catch-and-relabel of anything that
happens to propagate through.

`kind` stays `does_not_understand` (`RuntimeError`) — this only adds a
`hint`, the same field `format/1` already renders for every DNU
(§ REPL example below). No new exception class, no change to how `on:
RuntimeError do:` or `on: Error do:` catch it.

### REPL example

Using the project's `.btscript` `// =>` assertion convention (the verified
substring the REPL/eval error message actually contains —
`beamtalk_error:generate_message/3` for the DNU case,
`beamtalk_exception_handler:wrap_raw/2` for the `badarith` case; see
`tests/repl-protocol/cases/errors.btscript`):

```beamtalk
5 + aVector
// => Vector(6, 7, 8)

5 + "not a vector"
// => ERROR: String does not understand 'plusFromNumber:'
// => ERROR: String has no 'plusFromNumber:' — implement it to support 'number + String' arithmetic

aVector + 5
// => Vector(6, 7, 8)

5 / 0
// => ERROR: bad arithmetic operation
```

(Two `// =>` lines shown for `5 + "not a vector"` deliberately — the first
is the bare DNU message every other DNU in this doc uses, the second is
the added hint `send_number_coercion/4` attaches to it, on the same
`format/1` line every DNU's hint already renders on: `"<message>\nHint:
<hint>"`. A real `.btscript` test asserts against the substring it needs,
not both.)

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
  `does_not_understand` when absent. `number.bt` is not modified.

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
  to know left-vs-right dispatch exists. The rough edge: a newcomer who
  defines `+` on their own value type and sees `aThing + 5` work will
  reasonably expect `5 + aThing` to work too, and has to separately learn
  the `plusFromNumber:` name and add it — Beamtalk doesn't derive one from
  the other.
- **Smalltalk developer:** Recognizes the shape (`aVector + 5` and
  `5 + aVector` both dispatching to `Vector`) even though the underlying
  mechanism (`plusFromNumber:` reflected methods vs. `generality`/
  `adaptTo:andSend:`) is different from what they know. The departure is
  documented here with its rationale (no scalar tower on BEAM) rather than
  left as an unexplained gap from Pharo.
- **Erlang/Elixir developer:** The generated code is exactly the
  `try`/`catch` idiom they'd write by hand for "try the fast numeric op,
  fall back to a dispatch on failure" — no magic, inspectable in the
  compiled `.core`/`.beam` output. The one surprise: Erlang's `badarith` is
  overloaded — a non-numeric operand, a zero divisor, and float overflow
  all raise the identical reason — so the generated code has to re-inspect
  `Right` inside the handler to tell them apart (§ Dispatch mechanism
  above); a BEAM veteran writing this by hand for the first time would
  plausibly miss that too.
- **Production operator:** The `try`/`catch` is scoped tightly around the
  single BIF call and re-checks `is_number(Right)` before deciding what to
  do, so an unrelated numeric failure (`5 / 0`) keeps surfacing through the
  existing `badarith` classification (ADR 0028) with its original class,
  reason, and stacktrace — it is never silently reinterpreted as a
  does-not-understand. Only a genuine non-numeric right operand takes the
  new dispatch path.
- **Tooling developer:** `plusFromNumber:` is an ordinary typed method, so
  LSP completion, hover types, and static DNU checking all work on it with
  zero special-casing — the same infrastructure that already handles
  `+`/`-`/`*`/`/` covers the reflected names when a caller writes
  `x plusFromNumber: y` directly. What doesn't carry over for free: the
  xref index (ADR 0087) is a syntactic AST walk over *literal* selectors in
  source, and `5 + aVector` never spells `plusFromNumber:` in the user's
  code — it's synthesized inside the `try`/`catch` at codegen time.
  `senders_of: #plusFromNumber:` will not surface that call site, so "find
  all callers of `Vector>>plusFromNumber:`" is incomplete for the
  number-on-the-left case until a tool special-cases it (out of scope here,
  but worth flagging for future `SystemNavigation`/xref work).

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

### Alternative: Always-on runtime guard on the right operand's type
- 🧑‍💻 **Newcomer:** "Predictable — the runtime always checks both sides
  before doing anything, so there's no separate 'this crashed, but only
  because of an unrelated float overflow' story to learn."
- 🎩 **Smalltalk purist:** "Neutral — Smalltalk's own generality dispatch
  is retry-based, not a proactive dual guard, so this isn't more faithful
  to the tradition either way."
- ⚙️ **BEAM veteran:** "A guard is the idiomatic BEAM shape — `case
  is_number(X) of ... end` reads better to most Erlang programmers than a
  `try`/`catch` used for control flow instead of genuine error handling."
- 🏭 **Operator:** "No exception machinery on the arithmetic hot path at
  all, even in the failing case — a guard-based miss is a plain branch, not
  a raised-and-caught error, which is simpler to reason about under load."
- 🎨 **Language designer:** "Symmetric: both operands get the same
  treatment, rather than the left operand's fast path and the right
  operand's catch handler being different mechanisms."

  (This is the *unconditional runtime guard* variant, rejected below. The
  compile-time skip — reusing `receiver_is_statically_numeric` on the right
  operand so a typed/literal right never enters a `try` at all — is instead
  adopted as part of the chosen design; see § Dispatch mechanism's "Trigger
  condition, refined" and Alternatives Considered.)

### Tension points
- The Smalltalk-purist case for both the generality tower and the
  `perform:`-based alternative is genuinely the strongest of any cohort —
  both are more faithful to Pharo/Squeak than the chosen design. Beamtalk's
  departure is justified by constraint 2 (static typing must stay
  informed), which is specific to this codebase's gradual typing design
  (ADR 0025/0068), not a claim that generality or `perform:` are bad ideas
  in general.
- Language designers and Smalltalk purists would pick the generality tower
  or `perform:`-retry for their elegance/faithfulness; BEAM veterans and
  operators are indifferent-to-negative on both, and side with the chosen
  design once static-typing preservation is on the table.
- The always-on-guard alternative is the one place a BEAM veteran's *first*
  instinct (a guard, not a catch) points away from the chosen design for the
  cases that still need a runtime mechanism at all — resolved not by
  rejecting the veteran's instinct outright but by narrowing where it would
  even apply: `receiver_is_statically_numeric` already removes the
  `total + delta` majority case at compile time (no guard, no `try`, no
  runtime cost), so the guard-vs-`try`/`catch` question only remains live
  for the residual, genuinely-unknown-right-operand call sites — a smaller
  and more defensible surface for the `try`/`catch` design to own.

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

### Always-on runtime guard on the right operand's type
Instead of catching `badarith`, emit a proactive runtime check on the right
operand (e.g. `is_number(Right)`) alongside the existing left-operand guard
*unconditionally* — every call site with a numeric left operand pays a
right-operand check, regardless of whether the right operand's type is
already known at compile time. Rejected in this unconditional form: the
common case is `total + delta` — a numeric literal or known-numeric
receiver on the left, and a right operand whose type is *already visible to
the compiler* (a `:: Number`-family param, another numeric literal, a
numeric/untyped field — the identical rule the left operand already uses).
Guarding that right operand at runtime would regress it from a zero-cost
bare BIF to an always-checked one, for information the compiler already
has for free.

**Not fully rejected, though — adopted as a compile-time skip instead of a
runtime guard.** The chosen design reuses exactly this idea
(`receiver_is_statically_numeric` applied to the right operand), but as a
*codegen-time* condition that removes the `try`/`catch` from
`total + delta`-shaped call sites entirely, rather than as a *runtime*
`is_number` check both operands pay on every call. Only a right operand
whose type is genuinely unknown at compile time (`5 + aVector`) reaches the
`try`/`catch` mechanism at all — see § Dispatch mechanism's "Trigger
condition, refined." The remaining design question is narrower than the
original framing suggests: not "guard vs. catch, always," but "guard vs.
catch, only for the already-dynamically-dispatched residual" — and for that
residual, the catch-on-`badarith` approach still only pays a cost when the
fallback actually fires, which an always-on runtime guard would not.

### Status quo — leave `5 + aVector` unhandled
Do nothing: keep the receiver-dispatch-only behavior from BT-2709/2710 and
document number-on-the-left arithmetic as unsupported. Rejected: it leaves
the value-type ergonomics the epic (BT-2708) promised half-finished —
`aVector + 5` working but `5 + aVector` crashing with a raw `badarith` is
an asymmetry with no principled justification a user could discover short
of hitting the crash, and "arithmetic operators are overloadable" becomes
a claim with an asterisk. The cost of the fix (one codegen change, opt-in
per type) is low enough that "leave it broken" doesn't clear the bar of a
genuine alternative, only a fallback if the chosen design had turned out to
be infeasible.

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
- `total + delta`-shaped call sites — a numeric left operand and a right
  operand whose type is already statically known — never enter a `try` at
  all (§ Dispatch mechanism's compile-time skip): identical generated code
  to today, not merely "a `try` that happens not to unwind."
- Reflected methods are ordinary typed methods: return types are known
  statically, so covariant-return inference (ADR 0068) and compile-time DNU
  checking work on them for free.
- No new language-level concept (no `generality`, no reflective `perform:`)
  — the mechanism is "a message send from inside a catch handler," which
  the runtime already fully supports.
- A type author who doesn't need number-on-the-left arithmetic pays zero
  cost and needs zero code — the hooks are opt-in.
- A type that doesn't implement the reflected hook at all still gets a
  more useful error than a bare DNU: `send_number_coercion/4`
  (§ Dispatch mechanism) adds a hint naming the original operator, so
  `5 + aThing` fails with something closer to "`Thing` has no
  `plusFromNumber:` — implement it to support `number + Thing`
  arithmetic" than an unexplained `"Thing does not understand
  'plusFromNumber:'"` referencing a selector the caller never typed.

### Negative
- Per-type boilerplate: a value type wanting full bidirectional arithmetic
  writes up to 8 small methods (`+ - * /` and their `…FromNumber:`
  counterparts) instead of 4 — and, per § Reflected method protocol's
  "implement it anyway, to reject with intent" guidance, the honest
  estimate is closer to 8 in practice than to some smaller subset: even an
  operator that doesn't apply (`divFromNumber:` on a point-like type) is
  better implemented as a deliberate rejection than left to the generic
  DNU-plus-hint fallback. Acceptable per the ticket's own framing — a
  future `deriving`/macro mechanism (BT-2714, synthesized-method deriver
  framework) is the right place to reduce this, not a reason to weaken the
  type-checker guarantee now.
- `try`/`catch` around a BIF call has *some* nonzero runtime cost even when
  it never triggers (frame setup for the catch) — the de-risking spike
  (§ Implementation) confirmed this cost is in the same range as the
  already-accepted `is_number` guard's, not dramatically worse, and that
  the compile-time skip means it's paid only on the same call sites that
  already pay a guard-shaped cost today, never on the true bare-BIF fast
  path. The one thing the spike couldn't produce is a portable absolute
  ns/add number — the project's real benchmark numbers
  (`docs/development/benchmarks.md`) need re-measuring on whatever
  hardware they're normally recorded on as part of implementation, since
  this spike's own reproduction of the existing, already-shipped
  `bench_guard/0` came out well outside that doc's recorded range on this
  sandbox (matching its own "run-dependent" caveat).
- Asymmetric operators (`5 * aMatrix` vs. `aMatrix * 5` meaning different
  things mathematically) place the burden on each type's author to document
  the semantic difference between its `*` and its `timesFromNumber:` —
  the language enforces nothing here beyond "both exist and are callable."
  `minusFromNumber:`/`divFromNumber:` make this concrete: `n minusFromNumber:`
  computes `n - self`, not `self - n` — the reflected method's parameter is
  always the number that was on the left, so the receiver is the second
  operand, an order easy to get backwards without a worked non-commutative
  example (added to § Reflected method protocol).
- **Existing type-checker warning becomes a false positive on newly-valid
  code.** `check_binary_operand_types`
  (`semantic_analysis/type_checker/validation.rs:2016`) already warns today
  on `5 + aVector` ("`+` on Integer expects a numeric argument, got
  Vector") whenever the receiver is numeric and the argument isn't. That
  check doesn't know about `plusFromNumber:`, so a type correctly
  implementing the reflected hook still gets a spurious warning at every
  number-on-the-left call site. This ADR is codegen-only and does not fix
  it — teaching the type checker that a `<verb>FromNumber:` method on the
  argument type suppresses the warning is left as follow-up work, tracked
  against this ADR rather than silently left for someone to rediscover.
- **`self.<field>` / untyped-param trust boundary interacts with the new
  catch, not just the guard.** `receiver_is_statically_numeric` already
  trusts an untyped `self.<field>` or `:: Number`-typed identifier as
  numeric without a runtime check (BT-2709's existing, accepted gradual-
  typing gap — not new here). If that trust is wrong at runtime (the field
  actually holds a non-numeric object), the resulting `badarith` is now
  caught by this mechanism too: the handler checks `is_number` on the
  *right* operand, concludes it's a genuine coercion miss or a real
  numeric error based on the right operand alone, and never reconsiders
  whether the *left* operand was the actual problem. The user-visible
  outcome is still reasonable (a `does_not_understand` or the existing
  `badarith` classification, not a worse crash), but it's a new place this
  pre-existing trust gap surfaces, worth a one-line callout rather than
  silence.

### Neutral
- Comparison operators (`< > <= >=`) are explicitly out of scope; the
  existing silent-wrong-order behavior for `5 < aVector` is unchanged by
  this ADR and remains tracked as a separate follow-up. Its eventual
  reflected-method convention (if any) should reuse `<verb>FromNumber:`
  naming, not invent a second scheme — noted here so that follow-up doesn't
  drift.
- `%` (`rem`) and `**` (`math:pow`) were never part of BT-2709/2710's
  dispatchable-operator set to begin with (`generate_binary_op` special-
  cases `**` and never routes `%` through the guard/dispatch machinery at
  all) — `5 % aVector` and `5 ** aVector` are unaffected by this ADR in
  either direction, not a new asymmetry it introduces.
- No generality/coercion hook is exposed for a hypothetical future numeric
  tower type — if one is ever added, it implements `plusFromNumber:` etc.
  like any other type, and any tower-specific promotion logic lives inside
  those method bodies.
- No hot-code-reload or supervision implications: `plusFromNumber:` is an
  ordinary method, redefined/reloaded exactly like `+` or any other method
  under the existing live-image reload path (ADR 0105) — this ADR adds a
  dispatch target, not a new reload code path.
- The xref index (ADR 0087/0115) does not see the synthesized
  `plusFromNumber:` send inside the generated `try`/`catch` — a known,
  accepted gap for `SystemNavigation`-style tooling (§ User Impact, Tooling
  developer), not something this ADR's codegen-only scope fixes.
- If the reflected method's receiver is an actor (not a Value), the
  dispatch goes through the same `beamtalk_message_dispatch:send/3` path as
  any other actor message — including the existing `dispatch_error` rule
  for a block sent from inside a class method (CLAUDE.md § Blocks into
  class methods). This ADR doesn't change that rule, only adds another
  call site that can hit it, same as `aVector + 5` already can today.

## Implementation

Affected components: codegen, plus one small runtime addition (no parser,
type-checker, or new runtime protocol/exception-class changes) — the type
checker's existing `check_binary_operand_types` warning is left as-is,
including its now-known false positive on newly-valid number-on-the-left
code (§ Consequences, Negative), rather than folded into this ADR's scope.
The runtime addition is `beamtalk_message_dispatch:send_number_coercion/4`
(§ Dispatch mechanism's DNU-hinting subsection) — a thin wrapper around the
existing `send/3`, not a new dispatch mechanism or exception class.

### De-risking spike (done — results below)

Mirroring BT-2709's own precedent for this exact call site, run before
writing this ADR's Implementation section into real codegen:

**1. Hand-written Core Erlang, compiled with `erlc`.** The exact shape in
§ Dispatch mechanism was hand-written as a standalone `.core` module and
compiled directly. Two real issues surfaced that the illustrative Erlang in
earlier drafts of this ADR got wrong, both fixed in that section now:

- `try Expr catch ... end` **without** an `of` clause is a syntax error in
  Core Erlang — unlike Erlang source, where `try/catch` without `of` is
  legal sugar, Core Erlang requires the pass-through clause explicitly
  (confirmed: `erlc` rejects the omission with `syntax error before:
  'catch'`).
- Every `case` clause requires an explicit `when` guard — `<'true'> -> ...`
  without `when 'true'` is also a syntax error (confirmed the same way).

With both fixed, the module compiles cleanly. `core_lint` passes; the only
diagnostic is the *expected* one — `erlc`'s own note that the inner
`is_number` case's defensive third arm can't match, confirmed by removing
that arm and observing the warning disappear entirely. This is the exact
situation `guarded_op_doc`'s doc comment already describes for the
identical pattern (an `is_number`-driven boolean case the compiler can
prove exhaustive without a wildcard, defensively given one anyway) — not a
new problem, a confirmed instance of an already-accepted one.

**2. Functional verification, end to end.** The compiled module (with a
one-function stub standing in for `beamtalk_message_dispatch:send/3`) was
exercised directly for all three branches: a plain numeric add returns the
correct sum; a non-numeric right operand produces the expected
`plusFromNumber:` dispatch call with operands in the documented order; and
`1.0e308 + 1.0e308` (float overflow, both operands genuinely numeric)
re-raises `badarith` with its *original* stacktrace showing `erlang:'+'`
as the failing call — bit-for-bit the same shape as today's uncaught
propagation, confirming the re-raise path doesn't fabricate or lose
information.

**3. Benchmark, with an important caveat.** Extending the BT-2709
methodology (same tight-loop shape, same `N`/`Reps`, same `min_us` best-of
sampling as `bench_guard/0`) to compare bare `erlang:'+'` against this
ADR's full `try`/`catch`/`is_number` shape gave a ratio in the same
ballpark as the *already-shipped* `is_number` guard measured the identical
way, on the identical hardware, in the same run — not dramatically worse,
and isolating the `try`/`catch` wrapper alone (no `is_number` re-check)
showed it contributes a small fraction of that cost; the `is_number` check
itself, not the `try`/`catch`, is where most of the overhead already lives
in the *existing, accepted* mechanism this ADR reuses that check from.

The one thing this spike does **not** give: a trustworthy absolute ns/add
number. Reproducing `bench_guard/0` completely unmodified, on this
sandbox, gave a ratio well outside the `~2.7–3.0×` this repository's own
`docs/development/benchmarks.md` records for that exact same benchmark —
confirming the doc's own "(run-dependent)" caveat rather than contradicting
it. The relative comparison (this ADR's mechanism vs. the guard it's
layered next to, same run, same hardware) is sound; the absolute number is
not portable off this sandbox and needs re-measuring wherever the project's
real benchmark numbers get recorded (`docs/development/benchmarks.md`) as
part of implementation, not asserted from this spike.

**Conclusion:** the mechanism is real, syntactically valid once corrected,
functionally correct on all three branches, and costs roughly what the
already-accepted `is_number` guard costs on the same call sites — which,
thanks to the compile-time skip (§ Dispatch mechanism), are exactly the
call sites that already pay a guard-shaped cost today. It does **not**
touch the true zero-cost fast path (`total + delta`, `2 + 2`) at all —
that remains a structural guarantee (no `try` emitted there), not a
benchmark claim. Safe to proceed to the codegen wiring below.

- `crates/beamtalk-core/src/codegen/core_erlang/operators.rs`:
  - The `receiver_is_statically_numeric(right)` skip check gates **both**
    call sites the left operand can reach a numeric outcome through, not
    just one of them:
    - The always-bare arithmetic path (the `else` branch in
      `generate_binary_op` when `receiver_is_statically_numeric(left)` is
      true): check `receiver_is_statically_numeric(right)` first — if
      `true`, emit the bare BIF exactly as today (no change, no `try`).
    - The `is_number`-true branch of `guarded_op_doc`'s
      `OperatorGuard::Arithmetic` case (a *dynamically* numeric left
      operand): apply the identical `receiver_is_statically_numeric(right)`
      check before deciding whether to wrap that branch's bare BIF call —
      if `true`, that branch also stays a bare BIF with no `try`, exactly
      mirroring the always-bare path's treatment. Skipping this check here
      would silently wrap every guarded-left call site in an unneeded
      `try` even when the right operand's type is already known, which
      would contradict the zero-cost claim in Consequences → Positive.
  - Only when `receiver_is_statically_numeric(right)` is `false` — on
    either call site above — does the new mechanism engage. There,
    `let`-bind both operands *before* the
    `try` (mirroring `guarded_op_doc`'s own `left_var`/`right_var`
    binding — referencing the inlined operand documents a second time
    inside the catch handler would double-evaluate a non-trivial right
    operand), wrap the BIF call in `try`/`catch`, bind the catch clause's
    type/error/stack as variables (not literal patterns — matching
    `on_do_catch_preamble`'s convention), `case`-match on `{Type, Error}`
    with an explicit fallback arm, and use `primop 'raw_raise'` — not
    `erlang:raise/3`, which expects a pre-built stacktrace term rather than
    the raw trace a catch clause binds — for both the "not badarith" and
    "badarith but numeric" re-raise arms. Apply BT-3163's
    `case_clause_fallback` convention to the inner `is_number` `case`, for
    the same uniformity `guarded_op_doc`'s own doc comment argues for.
  - New helper mapping `+ - * /` → `plusFromNumber:` / `minusFromNumber:` /
    `timesFromNumber:` / `divFromNumber:`, built with `Document`/`docvec!`
    typed leaves (CLAUDE.md, ADR 0089) — no `format!()`.
  - The dispatch branch calls `beamtalk_message_dispatch:send_number_coercion/4`
    (below), not bare `send/3`, passing the operator symbol as the fourth
    argument for the hint text.
- `runtime/apps/beamtalk_runtime/src/beamtalk_message_dispatch.erl`:
  - New `send_number_coercion/4` (§ Dispatch mechanism's DNU-hinting
    subsection): wraps `send/3`, catches only a `does_not_understand` whose
    class and selector match this exact call's `Right`/`Selector`, and
    re-raises it with a hint added via `beamtalk_error:with_hint/2` naming
    the original operator. Any other exception — including a DNU with a
    *different* selector or class, e.g. a bug inside the reflected method's
    own body — passes through `erlang:raise/3` unchanged.
  - EUnit tests: the hint is added when the reflected method is genuinely
    absent; a DNU raised *inside* a present `plusFromNumber:` (for an
    unrelated selector) is not rewritten, confirming the class/selector
    match is doing its job and not over-catching.
- Codegen regression tests (mirroring the existing
  `tests/expressions.rs` guard tests from BT-2709/2710): assert the
  `try`/`catch`/`is_number`/re-raise shape is emitted only when the right
  operand's type is statically unknown; assert a `total + delta`-shaped
  call site (right operand `:: Number`-typed or a literal) still emits the
  exact bare-BIF Core Erlang it does today, with no `try` at all; assert
  the right operand isn't evaluated twice for a non-trivial right-operand
  expression (e.g. a message send), since that would silently duplicate a
  side effect; assert the dispatch branch calls `send_number_coercion/4`
  with the correct operator symbol.
- `stdlib/test/*.bt` (BUnit):
  - A `Vector`-style value type implementing `plusFromNumber:`/
    `timesFromNumber:`, asserting `5 + aVector` dispatches correctly.
  - A type with no reflected hook, asserting `5 + aThing` raises
    `does_not_understand` *with the added hint* (not a raw `badarith`, and
    not a bare hint-less DNU).
  - A type implementing `divFromNumber:` purely to reject
    (§ Reflected method protocol's "implement it anyway" guidance),
    asserting the custom message surfaces, not the generic DNU hint —
    confirming an intentional rejection isn't accidentally intercepted by
    `send_number_coercion/4`'s own catch (it only fires on DNU, and a
    `self error:` body doesn't raise one).
  - `5 / 0` and `1.0e308 + 1.0e308` (a zero divisor and a float-overflow
    add, exercising both known numeric `badarith` causes on both call
    sites) still raise the existing `TypeError`/"bad arithmetic operation"
    classification, not a `…FromNumber:` `does_not_understand` — the
    regression case that motivated the `is_number(Right)` re-raise check.

No `number.bt` changes — reflected methods are declared only on the types
that opt in, not as an abstract protocol entry, since there is no receiver
to declare `subclassResponsibility` against (the hook is looked up
dynamically on whatever the right operand turns out to be).

## Migration Path

No changes required for existing code — this only changes behavior for
expressions that currently crash with a raw `badarith` (number-on-the-left
arithmetic against a non-numeric right operand). No previously-successful
expression's result changes.

One narrow, existing-code-visible effect is worth naming rather than
glossing over: code that wraps such an expression in `on: TypeError do:`
(or a broader `on: Error do:`) specifically to recover from today's
`badarith` will observe a different outcome after this ADR ships, split by
whether the right operand's type gained the reflected hook —
- If the right operand's type now implements `plusFromNumber:` (etc.), the
  expression succeeds and the handler simply doesn't fire — the intended
  fix, not a regression, though it does mean a handler written as a
  workaround for the crash becomes dead code worth removing.
- If it doesn't, the expression still fails, but as `does_not_understand`
  (`RuntimeError`) instead of `badarith` (`TypeError`) — an `on: TypeError
  do:` handler that used to catch this specific failure no longer does,
  and the exception propagates further than it did before. An `on: Error
  do:` handler (covering both) is unaffected either way.
This is the same class of change any DNU-instead-of-crash fix produces (the
exception's class name changes, not just its message), not something
specific to this ADR's mechanism — noted here because § User Impact
(Newcomer) already establishes `does_not_understand` as the intended,
improved failure mode.

## Addendum (2026-08-25): BT-3266 — right-operand untyped `self.<field>` gap, accepted

`receiver_is_statically_numeric` (BT-2709) treats an untyped `self.<field>`
as statically numeric to keep `self.count := self.count + 1`-shaped counter
arithmetic on the bare-BIF fast path. § Dispatch mechanism's "Trigger
condition, refined" deliberately reuses that same predicate, unmodified, to
gate this ADR's *right*-operand coercion check — so the identical leniency
reproduces on the right side: `self.total + self.extra` with `extra`
untyped stays a bare `erlang:'+'`, no guard and no `try`/`catch`, and raw-
crashes on `badarith` if `extra` ever holds a non-number. BT-3263 code
review (PR #3515) flagged this as a real, if narrow, gap and filed it as
BT-3266 rather than folding a fix into that PR.

**Decision: accept the gap as specified; do not diverge the right-operand
check from the left-operand one.** Concretely, this addendum closes BT-3266
without a code change to the trigger condition — the codegen already
matches what's decided here (see "What already shipped" below).

**Rationale:**
- **Symmetry.** A right-operand-specific stricter check would make
  `self.a + self.b` guard asymmetrically depending on which side of `+`
  each field sits — the same untyped field trusted as a left operand and
  guarded as a right operand at different call sites. That's a more
  confusing model than "both operands get the same rule," for a gap that's
  already accepted on the left (§ Consequences, Negative — the `self.
  <field>`/untyped-param trust boundary bullet already covers exactly this
  class of risk, just from the left-operand side).
- **Performance.** Closing the gap taxes precisely the accumulator/counter
  call shape (`self.total + self.extra`, `self.total + self.count`) that
  BT-2709 designed the untyped-field leniency around in the first place —
  paying a guard on every untyped-field read on *either* side of an
  operator undoes that trade-off for hot self-field arithmetic, not just
  for the narrow case this issue names.
- **Scope.** Closing the gap for the right operand only, while leaving the
  left operand as-is, is the one option this ADR's dispatch mechanism
  doesn't already support cleanly — it would need `receiver_is_statically_
  numeric` split into left/right variants, and raises its own unresolved
  question of whether the left operand should then tighten too for
  consistency. That's a wider redesign than a follow-up issue's scope, not
  a decision to make silently inside this addendum.
- **Blast radius stays the accepted one.** When the trust is wrong, the
  user-visible outcome is the same `badarith` this ADR's own Negative
  consequences section already accepts for the left operand — not a new
  failure mode, just the existing one reachable from one more operand
  position.

**What already shipped** (landed in BT-3263, commit 871eac5, ahead of this
addendum): `receiver_is_statically_numeric`'s own doc comment
(`operators.rs`) names this exact asymmetry and cross-references BT-3266;
`test_number_coercion_untyped_self_field_right_operand_stays_bare`
(`codegen/core_erlang/tests/expressions.rs`) pins the current, accepted
behavior down as a regression test, and a second test using `self.<field>`
on *both* operands (`self.total + self.extra`, this issue's literal
example) was added closing out BT-3266's coverage criterion.

If untyped-field arithmetic's silent-badarith risk ever becomes a real
problem in practice, revisit both operands together as one design pass —
not the right operand alone.

## Implementation Tracking

**Parent:** BT-2712 (Phase 4 of BT-2708, already tracked this ADR's spec —
reused as the parent for the issues below rather than creating a
duplicate Epic)
**Issues:**
- BT-3262 — Add `send_number_coercion/4` to `beamtalk_message_dispatch.erl` (Phase 1, runtime)
- BT-3263 — Wire number-on-the-left coercion into `operators.rs` codegen (Phase 2, codegen; blocked by BT-3262)
- BT-3264 — BUnit tests for number-on-the-left arithmetic coercion (Phase 3; blocked by BT-3263)
- BT-3265 — Benchmark + language docs for number-on-the-left arithmetic (Phase 4; blocked by BT-3263)
- BT-3266 — Right-operand untyped `self.<field>` gap: design decision (accepted, not fixed — see Addendum above)

**Status:** Planned

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
