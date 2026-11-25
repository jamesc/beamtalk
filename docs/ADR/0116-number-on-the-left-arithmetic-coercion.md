# ADR 0116: Double-Dispatch Coercion for Number-on-the-Left Arithmetic

## Status
Implemented (2026-08-25) — Phases 1–4 landed (BT-3262–BT-3266, parent BT-2712); 2026-10-18 addendum follow-ups BT-3892, BT-3924, BT-3925 also Done. The 2026-11-25 addendum (comparison operators, BT-5176) is **Accepted (2026-11-25)** and not yet implemented.

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
   crash) tracked as a separate follow-up, not designed here. (That
   follow-up is the 2026-11-25 addendum below, Accepted.)

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
            %% Hint wording as reworded by the 2026-11-25 addendum
            %% (decision 4); the original "~s has no '~s' — implement it to
            %% support 'number ~s ~s' arithmetic" read as advice to add a
            %% method to `nil`.
            Hint = iolist_to_binary(io_lib:format(
                "'~s' cannot combine a number with ~s — the right operand "
                "must be a number, or a value whose class implements '~s'",
                [OrigOp, RightClass, Selector])),
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
// => ERROR: '+' cannot combine a number with String — the right operand must be a number, or a value whose class implements 'plusFromNumber:'

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
(Designed in the 2026-11-25 addendum below, Accepted.)

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
  `5 + aThing` fails with "'+' cannot combine a number with Thing — the
  right operand must be a number, or a value whose class implements
  'plusFromNumber:'" (wording from the 2026-11-25 addendum, decision 4)
  rather than an unexplained `"Thing does not understand
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

## Addendum (2026-10-18): BT-3892 — untyped/union `self.<field>` trust revisited, both operands

The risk above became real: `field: p = nil` set to a `Point` in `setUp`, then
`self.p + q`, skipped the left guard, hit `badarith`, and was routed to
`plusFromNumber:` on `q` — a DNU that never sent `+` to the `Point`. Union-typed
fields (`:: Point | Nil`) had the same failure because only simple annotations
were recorded. Per the addendum above, both operands were revisited together,
by changing what `receiver_is_statically_numeric` (and its comparison twin)
know about a field, not by splitting the predicate:

- A declared field records its full type; a union is bare only when every
  non-`Nil` member is in the operator's primitive set (and, for comparisons,
  only when it has no `Nil` member; see the last paragraph).
- An untyped field (`state:`, `field:` or `classState:`, of any class kind)
  is always guarded, whatever its default. This replaced, in BT-3925, a first
  rule that trusted an untyped Actor `state:` field (or a sealed class's
  `classState:`) as numeric on evidence from the class's own source — a
  numeric-literal default and only numeric in-class writes. That evidence
  could not see the open-world writers: `spawnWith:` (`Ctr spawnWith:
  #{#count => aPoint}`), `fieldAt:put:`, a subclass assigning an inherited
  field, an inherited class method over a redeclared `classState:`, a
  class-side extension, or a Value's keyword constructor. Any of them made
  `self.count + 1` a raw `badarith` instead of a send to the object's `+`.
- `:: Integer` / `:: Float` / `:: Number` is the fast path: an annotated
  field keeps the bare BIF. BT-3925 measured the cost of guarding the untyped
  Actor counter (`runtime/perf/self_send_bench`, `SsbMain counters`): the
  guarded loop was no slower than the bare one (see
  `docs/development/benchmarks.md`), and the guard wraps only the operator,
  so an `ifTrue:` on a guarded `self.count < n` still compiles to the same
  inline state-threading `case`.
- A field with no fact (inherited untyped, an extension target's untyped
  field) is guarded. Class methods consult `classState:` facts built the same
  way.

Both operands still share one rule, so the right-operand gap closes with the
left. The implementation is
`crates/beamtalk-codegen/src/core_erlang/field_types.rs`.

A union with a `Nil` member (`:: Integer | Nil`) stays bare for arithmetic,
where the BIF already raises `badarith` on `nil`, but is guarded for
comparisons. BT-3924 inverted the runtime comparison guard from "is this an
object?" (`beamtalk_primitive:is_object/1`) to an inline primitive-comparable
test (`is_number` or `is_binary`): numbers, characters and strings take the
bare BIF and every other receiver is sent the operator as a message. So
`nil < 5` raises `does_not_understand` wherever it appears — an untyped
local or parameter, or a nilable field — instead of silently answering
`false`. `Symbol` gained `< > <= >=` primitives so `#a < #b` keeps working
through dispatch.

## Addendum (2026-11-25): BT-5176 — number-on-the-left comparison (`< <= > >=`)

**Status: Accepted (2026-11-25).** Nothing in this addendum is implemented yet. It supersedes
constraint 4, the "No comparison-operator reflection" bullet of § What this
ADR does not do, the deferred alternative "Extend reflection to comparison
operators now" and the first Neutral consequence, all of which deferred
comparison to "a separate follow-up". This is that follow-up.

### Context

`3 <= nil` answers `true`. A numeric `<`, `<=`, `>` or `>=` whose **right**
operand is not a number is handed to the Erlang comparison BIF, and Erlang's
term order is total: every number sorts below every atom, tuple, map, list
and binary, and nothing raises. `max:`, `min:` and `between:and:`
(`stdlib/src/number.bt`) are written with those operators, so they inherit
it: `3 max: nil` answers `nil`. So does a class that takes `>` from
`uses: Comparable`, whose derived `>` is `other < self`: `aRank > 5` runs
`5 < aRank` and answers `true`. (All three observed on 0.6.0-edge while
writing this.)

The receiver side was closed in two steps. BT-3924 (2026-10-18 addendum)
made a non-comparable **receiver** dispatch, so `nil < 5` raises
`does_not_understand`. BT-4823 then stopped trusting a *declared* receiver
(`x :: Integer`), because a declaration is not enforced at run time and `<`
cannot raise: only a receiver the compiler **proves** (a literal, `self` in
a builtin class) keeps the bare BIF. The argument side was left open on
purpose by BT-3928 (2026-10-19), which documented the term-order fallback,
widened the static warning and pinned the behaviour with two tests.

Every path a numeric comparison takes tests the receiver only:

| Path | Where | Today |
|---|---|---|
| Inlined, proven receiver | `generate_binary_op` (`crates/beamtalk-codegen/src/core_erlang/operators.rs`), `ComparisonReceiver::Proven` | bare `erlang:'<'(L, R)` |
| Inlined, declared receiver | `OperatorGuard::DeclaredComparison` | kind test on `L`, then bare BIF or dispatch |
| Inlined, unknown receiver | `OperatorGuard::Comparison` | `is_number(L) or is_binary(L)`, then bare BIF or dispatch |
| Primitive method body | `generate_comparison_bif` (`primitives/mod.rs`), used by `Integer`, `Float`, `Character` and `String` | bare `erlang:'<'(Self, P0)` — every dynamic send and `perform:` |

**Why BT-3928 is being reversed.** It gave three reasons for a static-only
fix. Each has since changed:

1. *"A runtime check adds a branch to every bare-BIF comparison, including
   the statically numeric hot paths, and no operator benchmark exists."*
   The check below is elided when the right operand is proven numeric, the
   BT-5176 research measured the remaining cost at +0.1–0.3 ns per
   comparison (the same order as the receiver guard those sites already
   pay), and `benchmarks/vs-elixir` now has comparison rows (11d–11g,
   added for BT-4823) to measure against.
2. *"Arithmetic doesn't raise `type_error` for a wrong-kind argument; it
   goes through reflected dispatch and ends in `does_not_understand`, so a
   comparison `type_error` would be a new, inconsistent precedent."* This
   addendum does not add that precedent for *numeric operators*. It uses
   the same reflected dispatch and the same `does_not_understand`. (The
   objection was narrower than it sounds: stdlib value types already
   raise `type_error` from their own comparisons — `aDuration < 5` is
   "Argument must be a Duration", `beamtalk_duration.erl`. See
   § Consequences.)
3. *"The static checker already catches the numeric case."* Only when the
   argument's static type is known. BT-5175 is the counter-example: `1 to:
   nil do: [...]` compiled, and the pure `Integer>>to:do:` body
   (`self <= end`, `end :: Integer`) then looped forever, because every
   number is below `nil`. A silent wrong answer in a loop condition is a
   hang, not a cosmetic defect.

The owner's decisions going in (BT-5176): comparisons get reflected hooks
for consistency with this ADR rather than a bare `type_error`; `String`,
`Character` and `Symbol` wrong-kind ordering is a separate follow-up; and
this is an addendum, not a new ADR.

### Decision

1. **Rule.** When the left operand of `<`, `<=`, `>` or `>=` is a number at
   run time and the right operand is not, the right operand is sent a
   **reflected comparison method**. The four operators no longer
   term-order a number against a non-number. `max:`, `min:`,
   `between:and:` and every other caller of the four operators inherit
   the rule. Equality (`==`, `=:=`,
   `/=`, `=/=`) is untouched and stays total: `3 == nil` is `false`.
2. **Hook shape: one reflected method per operator**, named on this ADR's
   `<verb>FromNumber:` scheme — not a single three-way
   `compareFromNumber:`.
3. **Missing hook: `does_not_understand` with a hint**, the same kind this
   ADR already raises for a missing `plusFromNumber:` — not a `type_error`.
4. **Trigger: a proactive `is_number` test on the right operand**, on both
   the inlined and the primitive path, elided only where the compiler
   *proves* the right operand numeric. The failing arm is a single call to
   a runtime helper that shares the arithmetic one's implementation.
5. **Type checker:** the hook is an ordinary declared method; the checker
   looks it up on the argument's class, so a present hook silences the
   existing warning and supplies the send's type.

### Reflected comparison protocol

| Expression | Sent to the right operand | The hook answers |
|---|---|---|
| `n < x`  | `x lessThanFromNumber: n`       | `n < x`  |
| `n <= x` | `x lessOrEqualFromNumber: n`    | `n <= x` |
| `n > x`  | `x greaterThanFromNumber: n`    | `n > x`  |
| `n >= x` | `x greaterOrEqualFromNumber: n` | `n >= x` |

As with `minusFromNumber:`, the receiver is the value that was on the
**right** and the parameter is the number that was on the **left**: the
method name reads in source order (`n` *less than* `self`).

```beamtalk
Value subclass: Money
  field: cents :: Integer = 0

  // receiver-dispatch (unchanged): aMoney < bMoney
  < other :: Money -> Boolean => self.cents < other cents

  // number-on-the-left: 500 < aMoney
  lessThanFromNumber:       n :: Number -> Boolean => n <  self.cents
  lessOrEqualFromNumber:    n :: Number -> Boolean => n <= self.cents
  greaterThanFromNumber:    n :: Number -> Boolean => n >  self.cents
  greaterOrEqualFromNumber: n :: Number -> Boolean => n >= self.cents
```

Three points of guidance go into the language documentation:

- **Write the body against the type's own state, never by flipping the
  operator on `self`.** `lessThanFromNumber: n => self > n` looks
  equivalent and is a trap: when the class takes `>` from
  `uses: Comparable`, that `>` is `other < self` (`stdlib/src/comparable.bt`),
  which is `n < self`, which sends `lessThanFromNumber:` again and never
  terminates.
- **Implement all four or none.** `max:` needs `greaterThanFromNumber:`,
  `min:` needs `lessThanFromNumber:`, and `between:and:` needs the two
  inclusive ones. A type for which ordering against a number has no meaning
  implements none, and the hinted `does_not_understand` below is the right
  error. (This differs from the arithmetic guidance, "implement it anyway,
  to reject with intent": arithmetic operators are independent, so a type
  can support `+` but not `/`; the four orderings stand or fall together.)
- **`n` arrives unconverted** — an `Integer` or a `Float`. (A `Character`
  on the left arrives as its codepoint, and the hint calls it a number;
  see `Character` below.) The BEAM has no NaN or infinite float (float overflow raises
  `badarith`), so the number side never brings an unordered value. A user
  type that has one (an undefined `Decimal`, an empty `Interval`) answers
  `false` from each hook.

### Missing hook: `does_not_understand`, with a hint

A right operand whose class has no hook raises the ordinary
`does_not_understand` for the synthesized selector, with a hint that names
the operator the caller actually typed. Kind `does_not_understand`
(`RuntimeError`), class the right operand's, selector the hook:

```beamtalk
3 <= nil
// => ERROR: UndefinedObject does not understand 'lessOrEqualFromNumber:'
// => ERROR: '<=' cannot order a number against UndefinedObject — the right operand must be a number, or a value whose class implements 'lessOrEqualFromNumber:'

500 < aMoney     // => true    (aMoney lessThanFromNumber: 500)
aMoney < bMoney  // receiver dispatch to Money>>'<', unchanged
3 max: nil       // => ERROR: UndefinedObject does not understand 'greaterThanFromNumber:'
3 == nil         // => false   (equality stays total)
nil < 3          // => ERROR: UndefinedObject does not understand '<'   (unchanged, BT-3924)
```

(Two `// =>` lines for one error, as in § REPL example: the message, then
the hint that `format/1` renders on the `Hint:` line.)

The two failures are now symmetric: `nil < 3` is a `does_not_understand`
for `<` on `UndefinedObject`, and `3 < nil` is a `does_not_understand` for
`lessThanFromNumber:` on `UndefinedObject`. One `on: RuntimeError do:`
handler covers both, and covers `3 + nil` too. The symmetry is between a
number and a value with no ordering; it does not extend to types that
validate their own argument (`aDuration < 5` raises `TypeError`, and
`TypeError` and `RuntimeError` are siblings under `Error`).

The hint comes from the same code as the arithmetic one. The body of
`beamtalk_message_dispatch:send_number_coercion/4` is kept as the one
implementation, and a second entry point, `send_number_comparison/4`,
calls it with the ordering wording; codegen chooses the entry point, so
the runtime holds no table of which operators are comparisons (a rule that
would otherwise be duplicated across the Rust/Erlang boundary). The
ordering text differs from today's arithmetic template because that
template ("implement it to support …") reads as advice to add a method to
`nil`, the common case here and the wrong advice. The arithmetic hint is
reworded to match (decision 4 below): `3 + nil` gets "'+' cannot combine a
number with UndefinedObject — the right operand must be a number, or a
value whose class implements 'plusFromNumber:'", which replaces the text
in § Hinting the DNU when the reflected method is missing. The narrow
catch — only a `does_not_understand` for *this* class and *this* selector
— is what keeps a bug inside a present hook from being relabelled.

### Trigger and codegen

There is no `badarith` to catch, so the mechanism is the proactive test the
deferred alternative predicted: `is_number` on the right operand, placed in
front of every bare comparison BIF whose left operand is a number.

**Inlined path** (`generate_binary_op`). `R?` below stands for "unless the
right operand is proven numeric" (§ Elision):

| Receiver | Today | With this addendum |
|---|---|---|
| Proven number (`5 < x`) | bare BIF | `R?` `case is_number(R)`: BIF / hook |
| Proven `String` (`"a" < x`) | bare BIF | unchanged (follow-up) |
| Declared number or `Character` (`n :: Integer`) | kind test on `L`: BIF / send | true arm gains `R?` `case is_number(R)`: BIF / hook |
| Declared `String` | `is_binary(L)`: BIF / send | unchanged |
| Declared union with `String` (`:: Integer \| String`) | `is_number(L) or is_binary(L)`: BIF / send | split by kind like the unknown receiver: only the number arm gains the test |
| Unknown | `is_number(L) or is_binary(L)`: BIF / send | number arm gains the `R?` test; binary arm unchanged |

Reference shape for the unknown receiver, hand-compiled with `erlc +clint`
and exercised for the number, string and non-number-right cases (the
`case_clause` fallback arms that `guarded_op_doc` adds to every boolean
`case` are omitted here):

```erlang
let <BinLeft> = <left operand> in
let <BinRight> = <right operand> in
case call 'erlang':'is_number'(BinLeft) of
  <'true'> when 'true' ->
    case call 'erlang':'is_number'(BinRight) of
      <'true'> when 'true' ->
        call 'erlang':'<'(BinLeft, BinRight)
      <'false'> when 'true' ->
        call 'beamtalk_message_dispatch':'send_number_comparison'(
            BinRight, 'lessThanFromNumber:', [BinLeft], '<')
    end
  <'false'> when 'true' ->
    case call 'erlang':'is_binary'(BinLeft) of
      <'true'> when 'true' ->            %% String receiver: unchanged
        call 'erlang':'<'(BinLeft, BinRight)
      <'false'> when 'true' ->           %% BT-3924: dispatch the operator
        call 'beamtalk_message_dispatch':'send'(BinLeft, '<', [BinRight])
    end
end
```

Both operands are `let`-bound before the test, as `guarded_op_doc` and
`number_coercion_bare_path` already do, so the right operand is evaluated
once. Everything is built from `form::*` and typed leaves (ADR 0089). This
is expression codegen, like the existing guard: it threads no state and
does not go through `ThreadedIr`.

**Primitive path** (`generate_comparison_bif`). The `@primitive` bodies of
`Integer`, `Float` and `Character` `<`/`<=`/`>`/`>=` become `case
is_number(P0) of true -> BIF; false -> hook`. `String`'s stay bare. The
call is in tail position in the method body, so it costs nothing on the
numeric arm. This is what makes `perform:` and every other dynamic send
agree with the inlined path. `generate_comparison_bif` takes no class and
is shared with `String` (`string.rs`) and `reflection.rs`, and also emits
the equality BIFs, so it is split: a numeric-ordering variant for the three
classes, the existing one for everything else. The unit tests in
`primitives/integer.rs` that pin the bare output change with it.

**`Character`.** A `Character` is a codepoint integer at run time
(`beamtalk_primitive:class_of/1` answers `Integer`), so a character held in
an untyped variable is indistinguishable from a number and necessarily
takes this rule. The rule is therefore stated on the runtime
representation and applied to statically known `Character` receivers too,
so that `$a < nil` behaves the same whether or not the compiler can see the
`$a`. What stays for the follow-up is `String` and `Symbol` receivers, and
number-versus-character confusion (`$a < 5`), which no runtime test can
see.

**Four places read "a comparison on a proven receiver cannot run user
code and answers a Boolean". Each must take the right operand into
account, from one shared predicate:**

- `classify_binary` (`synced_call.rs`): such a comparison becomes
  `GuardedOperator` unless the right operand is proven numeric, exactly as
  arithmetic already is for its coercion.
- The BT-4279 fast-path test offered by `guarded_op_doc` must imply the
  bare BIF arm: `is_number(L) and is_number(R)` for the number arm (plus
  the unchanged `is_binary(L)` case).
- The guard-return rewrite (`guard_returns.rs`, `typed_comparison`,
  BT-4452) treats a literal-receiver comparison as a Boolean by
  construction. That holds only with a proven right operand; otherwise it
  decides through `beamtalk_guard_return`, as declared comparisons do.
- `record_comparison_lowering(left, guard.is_none())` (`operators.rs`)
  reports "lowered bare" to the BT-4452 backstop. A proven receiver with a
  tested right operand has no receiver guard but is no longer bare; the
  flag must come from the shared predicate, with a test that fails when it
  does not.

A proven-receiver site that gains the test should also offer the BT-4279
fast path (`is_number(R)`), which the bare path has never needed to.

#### Elision: proven, not declared

The right-operand test is omitted only when the compiler **proves** the
right operand is a number. This is a new predicate, "proven numeric". It
is not `ComparisonReceiver::Proven`, which also accepts a `String` literal
and `self` in `String` and records no kind (`5 < "abc"` must be tested):

- a numeric or character literal (`x < 10`);
- `self` inside `Integer`, `Float` or `Character`;
- later, any other operand the compiler itself constructs as a number (a
  counted loop's induction variable is the obvious candidate). New sources
  must be proofs.

A **declared** right operand (`n :: Integer` parameter or field) is
tested. This deliberately differs from arithmetic, which reuses
`receiver_is_statically_numeric` and trusts a declaration, and from the
BT-5176 research proposal, which suggested the same for comparison. The
reason is the one BT-4823 established for the receiver: arithmetic can
trust a declaration because a wrong value still raises `badarith`;
comparison cannot, because a wrong value answers silently. Trusting
declared right operands would leave exactly the motivating cases broken —
`Number>>max:`, `min:` and `between:and:` all declare `other :: Number`,
and `Integer>>to:do:` declares `end :: Integer`, so `3 max: nil` would
still answer `nil` and `1 to: nil do:` would still depend on a local guard.
The right operand gets the receiver's standard of evidence (only a proof
removes the test), not a second, looser rule.

Counted loops are unaffected: an inlined `to:do:` / `timesRepeat:` emits its
own `erlang:'=<'` against a limit it has already number-checked once in the
loop preamble (`counted_loops.rs`, BT-5136, BT-5167), and never goes
through `generate_binary_op`. BT-5175 (in flight, PR #5762; not on `main`
as this is written) adds the same check to the pure `Integer>>to:do:`,
whose body today is a bare `self <= end`. That guard is worth keeping
after this addendum: it raises a `type_error` that names the limit, once,
before the first iteration, matching the inlined path. With this addendum
the comparison inside the loop would raise as well, so neither depends on
the other.

#### The failing arm, and what it costs

The BT-5176 research found that the shape of the failing arm matters more
than the test: an arm that ends in `erlang:error/1` is free, while an arm
that makes an ordinary call forces a stack frame onto the whole loop
iteration (1.4 → 8.3 ns per comparison on a literal-receiver loop). It
recommended `erlang:error(Term)`. A reflected hook cannot do that: the hook
answers a value, so its arm must be a call that returns. Measured for this
addendum (hand-written Erlang, OTP 28, loop with five live values, 50M
iterations, median of 9, two runs; the absolute numbers are this sandbox's
and only the ratios are meaningful):

| Shape | ns/iter |
|---|---|
| bare `N < B` | 4.3 – 4.6 |
| `is_number(B)`, failing arm `erlang:error/1` | 4.7 – 5.0 |
| `is_number(B)`, failing arm a hook call | 17.1 – 17.5 |
| bare `N + B` | 4.5 – 4.9 |
| `N + B` in this ADR's `try`/`catch` (what `5 + x` compiles to today) | 17.9 – 18.0 |
| unknown receiver today (guard with a dispatch arm) | 14.2 – 17.3 |
| unknown receiver with the right-operand test and hook arm | 14.6 – 14.7 |

Reading it:

- **Unknown and declared receivers pay nothing new.** Their guard already
  has a dispatching arm, so the frame is already there; the extra
  `is_number` is inside the noise. This is every comparison whose left
  operand is a variable, parameter or field — the normal way to write one
  (`x > 0`, `i < n`).
- **A proven right operand pays nothing at all** (no test emitted).
- **A proven receiver with an unproven right operand (`0 < x`) newly pays a
  frame**, in code that otherwise makes no call. That is the same cost,
  on the same call-site shape, that this ADR's arithmetic has paid since it
  shipped (`5 + x` with `x` untyped: 4.5 → 18 above; the coercion `try`
  is also what `vs-elixir` row 11a's loop body carries). It is the price
  of a hook that can answer.

So the "failing arm must be `erlang:error/1`" requirement is met in spirit
only: the failing arm is **one** call to a runtime helper — never the
inline `beamtalk_error` construction chain of `type_error_raise_doc`, and
the raise itself happens inside the helper — but it is a call, and the
literal-on-the-left shape pays for it. The owner accepted that trade
(decision 1 below).

#### How the type checker sees the hook

Constraint 2 holds as it does for arithmetic: the hook is an ordinary
method with a declared signature, and nothing is looked up through
`perform:`.

- **One selector table.** The operator → reflected-selector mapping moves
  from `coercion_selector` (`operators.rs`) into `beamtalk-core`, below
  both codegen and the type checker, and covers all eight operators. The
  runtime needs no copy; the selector is passed to it.
- **`check_binary_operand_types`** (`type_checker/validation/arguments.rs`).
  For a numeric receiver and an argument whose static type is known and not
  numeric, look the reflected selector up on the argument's class:
  - found — no diagnostic, and the hook's parameter is checked against
    the receiver's type like any other argument. This function only emits
    diagnostics (it returns a `bool` to its caller in `receiver.rs`); the
    send's *type* is inferred separately from `Integer>><`'s declared
    `-> Boolean`. Phase 3 must find that inference site and make it use
    the hook's declared return type;
  - not found — today's warning (`` `<` on Integer expects a numeric
    argument, got Nil ``), with its hint changed from "Comparison operators
    require compatible types" to say that it raises `does_not_understand`
    at run time and names the selector to implement.
- Severity stays a warning; an untyped argument stays silent and a
  generic one keeps its hint-level diagnostic, as today. The check is the same code for arithmetic, so the
  false positive this ADR recorded for `5 + aVector` (§ Consequences,
  Negative) is closed by the same change.
- **Not covered:** `3 max: aMoney`, `min:` and `between:and:` are keyword
  sends checked against `other :: Number` (`number.bt`). They work at run
  time through the hooks but still warn at the call site and infer
  `Number`. Widening those signatures is not proposed.

### What this addendum does not do

- **`String` and `Symbol` receivers** with a wrong-kind argument
  (`"a" < 5`, `#a < 5`) still term-order. Separate follow-up, with its own
  test (`is_binary`) and its own decision about hooks.
- **Match-arm guards.** A `<` written in a pattern guard lowers to an
  Erlang guard BIF (`patterns/match_lowering.rs`), where nothing can be
  dispatched or raised. Guards keep term order on both operands, as they do
  today for the receiver.
- **No derivation.** `uses: Comparable` does not supply the hooks, and
  `aMoney > 5` does not imply `5 < aMoney`.
- **Default sort order.** `List sort`, `Dictionary` and `Set` ordering use
  Erlang term order directly, never `<`.
- **Collection `max` / `min`.** These call `lists:max` / `lists:min`
  (`beamtalk_collection.erl`), so `#(3, nil) max` still answers `nil` and
  ignores hooks, while `3 max: nil` raises. Follow-up.
- **The start of an inlined `to:do:`.** The inlined loop number-checks its
  limit, not its start (`counted_loops.rs`). Pre-existing; not changed
  here.

### Prior art

| Language | Mixed ordering | Verdict |
|---|---|---|
| **Python 3** | `3 < None` raises `TypeError`; Python 2 ordered mixed types arbitrarily and Python 3 removed that. `a < b` tries `a.__lt__(b)`, and on `NotImplemented` the *reflection* `b.__gt__(a)`; there is no `__rlt__`. The three-way `__cmp__` was removed in favour of per-operator methods. | Same behaviour change, for the same reason. Per-operator adopted. The reuse of `>` as the reflection of `<` is not (see "Mirror dispatch"). |
| **Ruby** | `3 < nil` raises `ArgumentError` ("comparison of Integer with nil failed"). Numeric operators ask the right operand to `coerce`; `Comparable` derives the operators from one `<=>`, which answers `nil` for incomparable values. | The three-way design at its best — and it needs a fourth answer (`nil`) to be honest about partial orders. |
| **Pharo / Squeak** | `3 < nil` raises `doesNotUnderstand:` on `nil`, for the coercion selector the number's failed primitive retries with (the `adaptTo…` family; the exact selector varies by image). `3 = nil` is `false`. | The same outcome as this addendum: a DNU on the right operand for a selector the caller never typed, equality total. Recalled, not run. |
| **Erlang / Elixir** | Total term order; `3 < nil` is `true`. Elixir documents it as structural comparison and gives `Date.compare/2` and friends for semantic ordering. | The behaviour being removed. It is right for a term language and wrong for one whose `<` is a message. |
| **Gleam** | `<` is `Int`-only and `<.` is `Float`-only; anything else is a compile error. | Not available to a gradually typed language, but the type-checker warning is the same idea. |
| **Rust** | `PartialOrd::partial_cmp` answers `Option<Ordering>`; `lt`/`le`/`gt`/`ge` are derived but individually overridable; mixed-type ordering is an explicit `impl PartialOrd<Money> for i32`. | Opt-in per type pair, as here. Again the three-way form needs a "no answer" case. |

### User impact

- **Newcomer.** `3 <= nil` stops answering `true`, which nobody would
  guess, and raises an error that says what is wrong with the right
  operand. The cost: the error names `lessOrEqualFromNumber:`, a selector
  they did not write; the hint exists to bridge that.
- **Smalltalk developer.** Gets the Pharo outcome (a DNU on the argument,
  equality total) through a mechanism they already met in this ADR. The
  departure is that the hook is per operator, not one `adaptTo…andSend:`.
- **Erlang/Elixir developer.** Loses term order through `<` on numbers,
  which is a real BEAM idiom (number < atom). It remains available where
  they would look for it: default `sort`, map and set ordering, and Erlang
  FFI. The generated code is a plain `case is_number(R)`.
- **Production operator.** A class of silent wrong answers, and the
  infinite loop of BT-5175, become exceptions with a class, a selector and
  a stack. The risk is the reverse: code that has been quietly relying on
  a `nil` sentinel in a comparison starts raising after upgrade (§
  Migration).
- **Tooling developer.** The hooks are ordinary methods: completion, hover
  and static DNU checks work. As with `plusFromNumber:`, `sendersOf:` does
  not see the synthesized send.

### Steelman analysis

**Alternative: a single `compareFromNumber:`**

- 🧑‍💻 **Newcomer:** "One method to write and it cannot disagree with
  itself. Four near-identical one-liners is where I make a typo."
- 🎩 **Smalltalk purist:** "One coercion point is closer to
  `adaptToNumber:andSend:` than four reflected selectors are."
- ⚙️ **BEAM veteran:** "Neutral. The failing arm is a call either way."
- 🏭 **Operator:** "One method means `<` and `>=` can never both be true in
  production."
- 🎨 **Language designer:** "Ruby, Rust, C++20 and Kotlin all converged on
  a three-way primitive. Consistency by construction beats consistency by
  convention."

**Alternative: plain `type_error`, no hooks**

- 🧑‍💻 **Newcomer:** "`TypeError: Comparison argument must be a number` is
  the error I expect from every other language, and it names nothing I
  didn't type."
- 🎩 **Smalltalk purist:** "None; operators are messages and the argument
  should get a say."
- ⚙️ **BEAM veteran:** "The failing arm is `erlang:error/1`, so even
  `0 < x` stays frame-free. That is the only shape of this change that is
  free everywhere."
- 🏭 **Operator:** "Same error class as the counted-loop guards; one thing
  to alert on."
- 🎨 **Language designer:** "Nobody has asked for `5 < aMoney`. Ship the
  safety fix and add hooks when a user wants them."

**Alternative: status quo (BT-3928)**

- 🧑‍💻 **Newcomer:** "None."
- 🎩 **Smalltalk purist:** "None."
- ⚙️ **BEAM veteran:** "Term order is the platform's semantics; fighting it
  costs a test on every comparison and surprises people reading `.core`."
- 🏭 **Operator:** "It is not a behaviour change, and behaviour changes in
  comparisons are the kind that page me."
- 🎨 **Language designer:** "The static warning is the right layer in a
  gradually typed language."

**Tension points.** The BEAM veteran's case for `type_error` is the
strongest argument against this design, and it is a measured one: hooks
cost a frame on literal-receiver sites that `type_error` would not. The
language designer's case for `compareFromNumber:` is strong for *total*
orders and weak exactly where a user type differs from a number. And the
newcomer is better served by a `TypeError` message than by a hinted DNU —
the hint narrows that gap but does not close it.

### Alternatives considered

**Single `compareFromNumber:` answering -1, 0 or 1.** One method instead of
four, consistent by construction, and it would cover `max:`, `min:` and
`between:and:` at once. Rejected:

- It reflects four messages with one, where the receiver side has four.
  `aVector < 5` may answer whatever `Vector>><` declares; a three-way hook
  forces `5 < aVector` to be a Boolean derived by the compiler. Per-operator
  hooks keep each call site's type a direct signature lookup (constraint 2).
- It cannot say "unordered". Three answers force a partial order to raise
  or to lie; Ruby and Rust both had to add a fourth answer, and then need a
  per-operator rule for it anyway.
- Beamtalk has no three-way comparison or `Ordering` anywhere: `Comparable`
  derives from `<`, not from `<=>`. This would introduce the concept for
  one hook.
- Its `0` reads as "equal", but `5 == aMoney` stays `false` (equality is
  not dispatched), so the single method would promise a trichotomy the
  language does not keep.
- This ADR already asked that a comparison hook "reuse `<verb>FromNumber:`
  naming, not invent a second scheme".

**One hook that carries the operator** (Squeak's
`adaptToInteger:andCompare:` shape, e.g. `compareFromNumber: n op: #<`).
One method, any answer, and no trichotomy problem. Rejected for the reason
this ADR rejected `adaptTo:andSend:`: the operator becomes data, the body
is a `perform:` or a four-way branch, and the return type no longer depends
on a selector the checker can look up.

**Convert, then re-send** (Ruby's `coerce`: ask the right operand to turn
the number into its own kind, then send the operator receiver-side). One
method, it reuses `Money>><` and `Comparable`, and it has no recursion
trap — the strongest alternative on boilerplate. Rejected: it only fits
types a number converts *into* (is `500` five hundred cents or dollars? a
`Vector` has no such conversion), which is the generality-tower model this
ADR rejected for arithmetic; and it would give comparison a different
protocol from the `plusFromNumber:` family it sits beside. A type that
does have a natural conversion can write each hook as one line through it.

**Fix only the primitive path and the stdlib callers** (`is_number` checks
in the `@primitive` bodies and in `max:`, `min:`, `between:and:`,
`to:do:`), leaving inlined codegen alone. Far smaller. Rejected: almost
every comparison in user code is inlined, so `3 <= nil` and `x < y` would
keep term-ordering and the two paths would disagree, which is the state
BT-4823 removed for receivers.

**Plain `type_error` (the BT-5176 research recommendation).** Raise
`type_error` "Comparison argument must be a number" with a frame-free
`erlang:error/1` arm. It is cheaper and smaller. Rejected by the owner for
consistency, and the consistency argument is concrete: BT-3928's own
objection to a runtime check was that a comparison `type_error` would
diverge from arithmetic; `3 + nil` and `3 < nil` would raise different
exception classes for the same mistake (though the language is not
uniform here either way: `aDuration < 5` already raises `TypeError` from
`Duration`'s own check, so with hooks `5 < aDuration` and `aDuration < 5`
differ instead); and `aMoney < 5` working while
`5 < aMoney` is a hard error with no opt-in is the asymmetry this ADR
exists to remove. Shipping `type_error` now and hooks later is not a
cheap middle path either: the second step would change the error's class
again, a second breaking change for anyone who had started catching the
first. It remains the fallback if the frame cost on literal-receiver sites
were ever judged unacceptable; the owner accepted that cost (decision 1
below).

**Mirror dispatch (Python's rule): send `x > n` for `n < x`.** No new
selectors at all. Rejected: a type's `>` is declared for its own kind
(`other :: Money`), so a number would arrive inside a method that never
expected one and fail somewhere in its body instead of at a named hook; a
type could not opt out; and with `uses: Comparable` the derived `>` is
`other < self`, so `5 < aRank` → `aRank > 5` → `5 < aRank` recurses
forever.

**Trust a declared right operand** (elide the test for `n :: Integer`).
Cheaper on literal-receiver sites. Rejected in § Elision: it leaves
`max:`, `min:`, `between:and:` and `to:do:` silently wrong.

**Raise `type_error` when the hook is missing, hooks otherwise.** Gives the
newcomer the friendlier error. Rejected: the helper would have to probe
for the method before sending, the same mistake would raise `TypeError`
for comparison and `RuntimeError` for arithmetic, and a missing method
*is* a `does_not_understand` everywhere else in the language.

**Status quo.** Rejected: see "Why BT-3928 is being reversed".

### Consequences

**Positive**

- No number is term-ordered against a non-number through `<`, `<=`, `>`,
  `>=`, `max:`, `min:` or `between:and:`, on the inlined or the primitive
  path. (The exceptions are listed in § What this addendum does not do.)
- `5 < aMoney` can be made to work, with a checked signature.
- One mechanism, one helper and one error kind for both operator families.
- The receiver and argument sides now follow the same proven / declared /
  unknown rule.

**Negative**

- **Breaking change** for code that relied on term order (§ Migration).
- Literal-receiver comparisons with an unproven right operand pay a stack
  frame in otherwise call-free code (§ The failing arm).
- A comparison that becomes a `GuardedOperator` can change a loop's
  shape: a field-writing loop whose condition is `0 < self.n` moves from
  the hybrid shape to `StateAcc` (BT-4121), which costs a store and reload
  per iteration, not just a frame. Variable-first conditions
  (`self.n > 0`) are already guarded and do not change. Phase 0 adds a row
  for it.
- Four more one-line methods for a type that wants full number interop: up
  to sixteen with arithmetic. BT-2714 (derivers) is still the place to
  reduce that.
- The error for a missing hook names a selector the caller did not type,
  and through `Comparable` it can name a different operator: for a class
  with a derived `>`, `aRank > 5` runs `5 < aRank` and reports
  `lessThanFromNumber:`.
- Only a class's author can opt it in. Cross-file extension methods are
  gone (ADR 0152), so a hook cannot be added to a builtin or to a
  dependency's class; ordering a number against such a value needs an
  explicit conversion at the call site.
- A hook can disagree with equality: `5 <= aMoney` and `5 >= aMoney` may
  both be `true` while `5 == aMoney` is `false`.
- Counted loops raise `type_error` for a non-number bound, stdlib value
  types raise `type_error` from their own comparisons (`aDuration < 5`),
  and number operators raise `does_not_understand` (`5 < aDuration`). Both are deliberate (a validated keyword
  argument versus a message the argument does not understand), but they
  are two sibling classes for what a user may see as one mistake, and
  only `on: Error do:` catches all of them.

**Neutral**

- `sort:` with a `[:a :b | a < b]` block now raises on a list mixing
  numbers and non-numbers; default `sort` is unchanged.
- A hook on an actor is an ordinary synchronous send, with the same rules
  as `plusFromNumber:`.
- Hot reload, supervision and the xref gap are as for arithmetic.

### Migration

**CHANGELOG** (with the codegen change), following the file's `Breaking:`
form:

> **Breaking: a number is no longer ordered against a non-number
> (BT-5176, ADR 0116 addendum).** `<`, `<=`, `>` and `>=` with a number on
> the left and a non-number on the right — and so `max:`, `min:` and
> `between:and:` — used to fall back to Erlang term order (`3 <= nil` was
> `true`, `3 max: nil` was `nil`). They now send the right operand a
> reflected method (`lessThanFromNumber:`, `lessOrEqualFromNumber:`,
> `greaterThanFromNumber:`, `greaterOrEqualFromNumber:`) and raise
> `does_not_understand` when it has none.
> - Unchanged: `==`, `=:=`, `/=`, `=/=`; `String` and `Symbol` receivers;
>   default `sort` and `Dictionary`/`Set` order.
> - Migration: test for `nil` before comparing (`x isNil`, `ifNil:`), and
>   for a symbol sentinel before comparing against it (`elapsed < timeout`
>   where `timeout` may be `#infinity`); for a
>   deliberate mixed order use default `sort` or an explicit comparator; to
>   make a value type comparable with numbers, implement the four hooks.
> - Reverses BT-3928.

**Pinned tests** (both pin the BT-3928 fallback on purpose):

- `stdlib/test/nil_comparison_test.bt`,
  `testMismatchedArgumentFallsBackToTermOrder` (line 105): the
  `lt: 5 with: nil` assertion on line 106 becomes
  `should:raise: #does_not_understand`. The `lt: #a with: 5` line has a
  `Symbol` receiver and stays.
- `stdlib/test/typed_comparison_receiver_test.bt`,
  `testArgumentSideIsStillUnchecked` (line 112): both assertions (lines
  113–114) flip, and the test is renamed. The `String`-receiver assertion
  at line 104 stays.

**Documentation.** `docs/beamtalk-language-features.md` § Comparison
(around line 1113): replace the paragraph "The **argument** is not checked
at run time …" with the rule, the hook table, the error and the three
points of guidance; drop "Comparison operators have no number-on-the-left
coercion". Add the four selectors to § Number-on-the-left coercion. Reword
the comments that cite BT-3928 (`arguments.rs` near line 1141,
`stdlib/test/fixtures/typed_comparison_probe.bt`,
`comparison_receiver`'s doc comment in `operators.rs`). ADR 0148 (Deferred)
records that "the comparison *argument* stays unchecked by decision
(BT-3928)"; it gets a pointer to this addendum in the same change.

**Downstream.** The BT-5176 research ran a prototype of the rule (as
`type_error`) against the repository: 10,105 of 10,107 BUnit tests passed,
the two failures being the pins above, and the stdlib, bootstrap and
example suites passed. A grep of 17 satellite repositories found no
reliance on mixed ordering. The implementation must repeat this with the
real change and run the cross-repo CI.

### Implementation plan

The de-risking this ADR
asked of its own mechanism is already done for this one: the Core Erlang
shapes above (unknown receiver, proven receiver, primitive body) were
hand-compiled and run, the `Money` example passes `beamtalk check`, and the
cost table in § The failing arm is measured.

| Phase | Size | Content | Acceptance |
|---|---|---|---|
| **0. Baseline** | S | Add `benchmarks/vs-elixir` rows before any behaviour change: `5 < b` with `b` untyped (proven receiver, unknown right — the new row BT-5176 asked for), `a < b` both untyped, `a :: Integer < b` untyped, `5 + b` untyped as the arithmetic reference, and an actor loop `[0 < self.n] whileTrue:` that writes a field. Record them together with the existing rows 11d–11g, which are in the harness (`vs_bench.erl`) but in no committed results file. | Rows run under `just bench-vs-elixir`; baseline committed under `benchmarks/vs-elixir/results/`. |
| **1. Runtime** | S | `send_number_comparison/4`, sharing `send_number_coercion/4`'s body. The arithmetic hint is reworded to match (decision 4), with the tests and language-features text that pin it. EUnit: both hint texts; a DNU raised inside a present hook is not relabelled. | `just test` and `just dialyzer` green. |
| **2. Codegen and behaviour** | M–L, `ci:corpus` | Shared selector table in `beamtalk-core` (replaces `coercion_selector`). Right-operand test on both inlined arms and in `generate_comparison_bif` for `Integer`/`Float`/`Character`. The shared "bare and Boolean" predicate for `classify_binary`, the BT-4279 fast path and the guard-return rewrite. Flip the two pins. New `stdlib/test/number_comparison_arg_test.bt`. CHANGELOG entry and language-features rewrite in the same PR. | See below. |
| **3. Type checker** | S | Hook lookup in `check_binary_operand_types` for all eight operators; new hint text; inferred type from the hook. Blocked by phase 2. | Tests: a class with the hook produces no diagnostic and the send has the hook's return type; a class without it warns with the new hint; an untyped argument is silent; `5 + aVector` with `plusFromNumber:` no longer warns. |
| **4. Follow-ups** (separate issues) | — | `String`/`Symbol` receivers; proving more right operands (loop counters, arithmetic results); match-arm guards. | — |

**Phase 2 acceptance.**

- BUnit, each of the four operators × receiver (literal, `self` in
  `Integer`, typed parameter, untyped, `perform:`, `Float`, `Character`) ×
  right operand (`nil`, symbol, string, list, boolean, object without
  hooks): hinted `does_not_understand`, asserting the selector and the
  hint.
- A value type with the four hooks: all four operators, `max:`, `min:`,
  `between:and:`, and a hook whose body raises (its error surfaces
  unrelabelled).
- Unchanged behaviour pinned: `3 < 4.0`; `aMoney < bMoney`; equality total
  on mixed operands; `"a" < 5` and `#a < 5`; default `sort` of a mixed
  list.
- Rust codegen tests: no test emitted for a proven right operand; the
  right operand evaluated once; the fast-path test includes the right
  operand; a declared right operand is tested.
- A generated corpus over receiver kind × right-operand kind × position
  (plain expression, loop condition, `ifTrue:` receiver, guard-return
  shape, actor-state bracket), agreeing with a reference evaluation.
  `just test-corpus` and `just verify-threaded-ir` green.
- Cross-repo CI green.
- **Benchmark gate**, against the phase 0 baseline. Rows 11d and 11e
  (`a > 0`, `a < 10`: proven right operand) compile to the same code, so
  reductions, heap words and ns are unchanged. Rows 11f and 11g
  (`[a > i] whileTrue:`, `a max: 3`) and the two new unknown/declared-
  receiver rows gain one `is_number` test: reductions and heap words
  identical, ns inside the run-to-run spread. The new `5 < b` row is no
  slower than the `5 + b` row. The actor-loop row is reported with its
  before and after loop shape (see § Consequences); a regression there is
  a decision for the owner, not an automatic stop. All of it is recorded in
  `docs/development/benchmarks.md`. A result outside those bounds stops
  the PR and goes back to the owner (decision 1).

### Decisions on the open questions

The draft put four questions to the owner, who answered them on
2026-11-25 when accepting this addendum:

1. **The frame on literal-receiver sites: accepted.** Hooks make the
   failing arm a returning call, so `0 < x` (proven receiver, unproven
   right) pays a stack frame in call-free code — what `5 + x` already
   pays — and can move a field-writing actor loop to `StateAcc`. The
   frame-free `type_error` without hooks is not taken. The benchmark gate
   in phase 2 still applies.
2. **Declared right operands are tested, not trusted.** Confirmed, as in
   § Elision, so that `max:`, `min:`, `between:and:` and `to:do:` raise.
3. **`Character` receivers are covered.** The wrong-kind follow-up is
   narrowed to `String` and `Symbol` receivers.
4. **The arithmetic hint is reworded to match the ordering wording.**
   `3 + nil` and the other three arithmetic operators take the new
   template (§ Missing hook). The change is made in phase 1, not in the
   pull request that added this addendum.

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
- BT-3892, BT-3924, BT-3925 — 2026-10-18 addendum follow-ups (untyped/union `self.<field>` guarding, comparison guard inversion, Actor `state:` fields)

- BT-5176 — 2026-11-25 addendum (comparison operators): Accepted 2026-11-25; implementation issues to be filed. Related: BT-5175 (counted-loop limit guard), BT-3928 (the fallback this reverses), BT-4823 (declared receivers are tested)

**Status:** Complete for the arithmetic design — all issues Done (BT-3262–BT-3266 closed 2026-08-25, BT-2712 closed 2026-08-25; addendum issues closed 2026-10-18). The 2026-11-25 comparison addendum is Accepted and not yet implemented.

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
- 2026-11-25 addendum: BT-5176 (issue and research comment), BT-5175
  (counted-loop limit guard, PR #5762), BT-3928 (term-order fallback,
  reversed), BT-3924 and BT-4823 (receiver side), BT-5136 / BT-5167
  (counted-loop number guards), BT-4279 (fast path), BT-4452 (guard-return
  rewrite); ADR 0002 (comparison operators are the Erlang BIFs — amended
  here for a number against a non-number), ADR 0127 (`Comparable`), ADR 0137
  (cell brackets), ADR 0152 (no cross-file extensions: a hook can only be
  declared in the class's own file)
