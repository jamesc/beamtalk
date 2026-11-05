# ADR 0145: Cascade Semantics for Immutable Values — Keep `;` Fan-Out, Add a `|>` Pipeline

## Status
Proposed (2026-11-05)

Decision record for the "Cascade Semantics" question that ADR 0042 deferred
(to a planned "ADR 0044" that was never written — 0044 became
comments-as-AST-nodes). Successor to the canceled research issue BT-506.
Driving issue: BT-4352.

## Context

### The problem

A Smalltalk cascade sends every message to the **same receiver** and answers
the last message's result:

```beamtalk
Transcript show: "Hello"; cr; show: "World"
// ≡  Transcript show: "Hello".  Transcript cr.  Transcript show: "World"
```

That is the right meaning for anything whose messages act on the receiver in
place — an actor (ADR 0042), a `ClassBuilder` (an `Actor` precisely so its
cascade works, ADR 0038), an `Ets` table, the `Transcript`. It is the wrong
meaning for an immutable value. A `Value`'s `with*:` answers a *new*
instance and leaves the receiver alone, so with the usual example value

```beamtalk
Value subclass: Point
  field: x :: Integer = 0
  field: y :: Integer = 0
```

```beamtalk
p := Point x: 1 y: 2
p withX: 5; withY: 10      // => a Point(1, 10) — the withX: result was dropped
```

`withX:`'s result is computed and discarded, and the cascade answers
`p withY: 10`. Nothing warns. The same trap applies to every built-in value
whose "update" messages answer a copy: `Dictionary at:put:`, `List` /
`String` building messages, `Integer max:`, and every user `Value subclass:`.

ADR 0042 § Cascade Semantics recorded the problem and a leading candidate —
**cascade-as-pipeline on values** (each message sent to the *previous
result*, giving Elixir-style `|>` "for free") — but left three questions
open:

1. **Type-dependent semantics** — pipeline on values, fan-out on actors, or
   one uniform rule?
2. **Dynamic receivers** — what happens when the receiver's kind is not
   known at compile time?
3. **ADR 0039 parity** — cascades are listed under "What We Keep"
   (`docs/beamtalk-syntax-rationale.md`: *Cascades — ✅ Same*). Does a
   change argue its way past that?

### Motivating example

beamtalk-buccaneer `src/exchange.bt`, `upgradeToWebSocket:`, threads four
functional updates through an exchange-state value and stores the result:

```beamtalk
self store: ((((st withWebsock: websock)
  withWebsockState: websock)
  withWebsocketOptions: opts)
  withRespState: #upgraded)
```

The reader has to count parentheses to see that this is a straight
left-to-right chain. The obvious "cleanup" —

```beamtalk
self store: (st withWebsock: websock; withWebsockState: websock;
  withWebsocketOptions: opts; withRespState: #upgraded)
```

— compiles, runs, and stores a state with only `respState` changed. Today the
only readable *correct* alternative is a run of temp locals.

### Current state

- **Parser.** `parse_cascade` (`crates/beamtalk-core/src/source_analysis/parser/expressions.rs`)
  parses a keyword message, then `; msg` repeatedly, producing
  `Expression::Cascade { receiver, messages }` where `receiver` is the
  *first message send* and the cascade target is that send's receiver.
- **Codegen.** `generate_cascade` (`crates/beamtalk-codegen/src/core_erlang/expressions.rs`)
  binds the target once, sends each non-final message to it, discarding the
  result, and answers the final message's result. Binary messages in a
  cascade are rejected. In actor instance context a cascade on `self`
  threads `State` message-by-message through `ThreadedIr`
  (`threaded_cascade_on_self`, ADR 0118); cascade operands that need a
  state-threading prelude are spliced through `ThreadedIr` too (BT-3791,
  BT-3802).
- **Type checker.** `infer_cascade` (`type_checker/inference/cascade.rs`)
  dispatches every message against the cascade target's type and types the
  cascade as the last message's return type.
- **Lints.** `cascade_candidate` (beamtalk-lint) suggests a cascade for 3+
  consecutive *statements* sending to the same simple receiver — that rewrite
  is meaning-preserving even on values, because each statement's result was
  already discarded. `check_testcase_setup_drops_field_assignments`
  (`semantic_analysis/validators/lint_validators.rs`) accepts a
  `setUp` that ends in "a cascade on `self` ending in a `with<Field>:`
  message" as one that "reliably yields the fully updated self". Because
  `TestCase` is a `Value subclass:`, a `setUp` ending
  `self withA: 1; withB: 2` keeps only `withB:` — the lint currently
  blesses exactly the trap this ADR is about.
- **Usage.** In `stdlib/src` and `stdlib/test`, cascades target actors
  (`self.bus subscribe: …; notify: …`), `ClassBuilder`, `Ets` tables and
  `Transcript`; a handful of codegen-regression fixtures cascade on an
  `Integer` (`0 max: 1; max: (…)`) or use the `x printString; yourself`
  idiom. None cascades `with*:` on a value — authors have learned to avoid
  the trap, at the cost of the nesting above.

### Constraints

- **No semantics that depend on information the compiler may not have.**
  Beamtalk is gradually typed; much code is `Dynamic`. The meaning of `;`
  must not change because an annotation was added or removed.
- **Value / Actor is not the whole picture.** ADR 0067 has three class
  kinds. Method-only `Object` subclasses (`Ets`, `AtomicCounter`, FFI
  wrappers) and raw Erlang terms (pids, refs, ETS tids) mutate *outside* the
  receiver. "Pipeline on values, fan-out on actors" has no correct answer
  for them without effect information Beamtalk does not track.
- **ADR 0039:** departures from Smalltalk need a friction argument; cascade
  is listed as identity-defining, not friction.
- **State-threading codegen** (CLAUDE.md, ADR 0111): any new chaining form
  must lower through existing `ThreadedIr` paths, not add a new threading
  family.

## Decision

Three parts, each independent enough to accept or reject on its own.

### 1. `;` keeps uniform fan-out semantics for every receiver

A cascade always sends each message to the cascade target and answers the
last message's result, whatever the target is — actor, value, `Object`
instance, Erlang term or `Dynamic`. No change to the parser, codegen,
runtime or type checker.

This settles ADR 0042's three questions:

| Question | Answer |
|---|---|
| (1) Type-dependent semantics? | **No.** One rule: `;` is fan-out, everywhere. |
| (2) Dynamic receivers? | **Nothing to decide** — the meaning of `;` does not depend on the receiver's kind, so an unknown kind changes nothing. (The lint in part 3 is silent on them.) |
| (3) ADR 0039 parity? | **Preserved.** `;` means exactly what it means in Pharo/Squeak/Newspeak; pipeline chaining gets its own syntax instead of overloading a kept Smalltalk form. |

### 2. A new pipeline operator `|>` chains messages on the previous result

```
pipeline := cascade ( '|>' message )*        // message = unary | binary | keyword
```

`recv m1 |> m2 |> m3` sends `m2` to the result of `recv m1`, and `m3` to the
result of that — it is **exactly** `((recv m1) m2) m3`, and is defined by that
rewrite. There is no new runtime behaviour, no type dependence, and it works
the same on values, actors and `Dynamic` receivers.

The motivating example becomes:

```beamtalk
self store: (st withWebsock: websock
  |> withWebsockState: websock
  |> withWebsocketOptions: opts
  |> withRespState: #upgraded)
```

Other shapes it reads well in:

```beamtalk
// Collection pipelines (BT-506's original motivation)
total := orders select: [:o | o isPaid]
  |> collect: [:o | o amount]
  |> inject: 0 into: [:sum :a | sum + a]

// Building a value inside a block / HOM (ADR 0042's "sweet spot")
moved := points collect: [:p | p withX: 0 |> withY: p y + 1]

// Mixed unary and keyword stages
name := user profile |> at: #displayName |> trim
```

REPL:

```
> p := Point x: 1 y: 2
// => a Point(1, 2)
> p withX: 5 |> withY: 10
// => a Point(5, 10)
> p withX: 5; withY: 10
// => a Point(1, 10)        (fan-out: unchanged, and now warned — part 3)
```

**Precedence and layout.**

- `|>` binds looser than keyword messages, like `;`. Each stage is a complete
  unary, binary or keyword message whose receiver is the previous stage's
  value; a keyword stage's arguments end at the next `|>`.
- The first segment is an ordinary expression (`recv m1`, or just `recv`):
  `st |> withX: 1` is legal and equals `st withX: 1`.
- A line that **starts** with `|>` continues the expression above, by the
  same rule that lets a line starting with a keyword part continue
  (language features § Splitting a Message Send Across Lines): `|>` can
  never begin a statement, so the continuation is unambiguous.
- **`;` and `|>` do not mix without parentheses.** `a m1 |> m2; m3` is a
  parse error: *"cannot mix cascade `;` and pipeline `|>` in one expression;
  parenthesise the part you mean"*. Both readings are plausible, and the
  rare code that wants both is clearer with explicit parentheses:
  `(a m1 |> m2) m3; m4` or `a m1 |> (…)`-free temp locals.
- `|>` is not a binary selector and cannot be defined as a method. (Today
  `|` always lexes as `TokenKind::Pipe`, so no existing user selector is
  `|>`; the lexer gains one two-character token.)

**Error examples.**

```beamtalk
p withX: 5 |> withY: 10; withZ: 3
// error: cannot mix cascade `;` and pipeline `|>` in one expression;
//        parenthesise the part you mean

p withX: 5 |>
// error: expected a message after `|>`

p withX: 5 |> 10
// error: expected a message after `|>` (found a literal) — `|>` sends a
//        message to the previous result; it does not apply a function
```

A stage the previous result does not understand raises
`does_not_understand` exactly as the nested form would, and the type checker
reports it at the stage's span (it sees an ordinary send).

**AST representation.** The parser produces ordinary left-nested
`Expression::MessageSend` nodes; each send that came from a `|>` stage
carries a source-form marker (as `Parenthesized` preserves source
parentheses today) so `unparse`, `beamtalk fmt`, reflected method source and
the LSP reproduce `|>` as written. Every semantic pass — name resolution,
type inference, the `ThreadedIr` lowering, codegen, the language-service
send queries — sees a plain message send and needs no new arm. This is what
makes `|>` free of state-threading risk: a pipeline *is* the nested send the
compiler already lowers.

### 3. A `cascade-drops-update` warning for value cascades

A cascade gets a warning, at the dropped message, when **all** of:

- the cascade target's inferred type is statically a value kind (a
  `Value subclass:` or a built-in immutable class — `Integer`, `String`,
  `Symbol`, `List`, `Dictionary`, `Set`, `Tuple`, …), and
- a **non-final** message's declared return type is the target's own type
  (`Self`, or the target's class) — i.e. it answers an updated copy, which
  the cascade then throws away.

```
warning[cascade-drops-update]: the result of `withX:` is discarded
  p withX: 5; withY: 10
    ^^^^^^^^^
  = note: `Point` is a value: `withX:` answers a new Point and leaves `p`
          unchanged, and a cascade sends `withY:` to `p`, not to that result
  = help: to send `withY:` to the result, use a pipeline:
          p withX: 5 |> withY: 10
```

It does **not** fire on `Dynamic` targets, actors, `Object` instances,
non-final messages answering some other type (`printString`, `size`), or
`yourself`-terminated cascades whose earlier messages are all of that kind.
It is a lint with the usual `@expect cascade_drops_update` /
`[diagnostics] cascade-drops-update = "off"` controls, not a guarantee —
a dynamic receiver still fans out silently, as it always has.

The `with*:` recogniser reuses `crate::synthetic_selectors::with_star_selector`
and the class hierarchy's declared return types; no new selector table.
The same change corrects `check_testcase_setup_drops_field_assignments`:
a `setUp` ending in a `self with*:` cascade with **more than one** `with*:`
message is no longer accepted as self-producing (it drops all but the last);
its help text suggests the `|>` form.

### What stays valid regardless

- Explicit nesting — `((st withA: 1) withB: 2)` — is and remains the
  canonical spelling; `|>` is sugar for it, never a replacement.
- Temp locals remain the recommendation when intermediate values deserve
  names or when stages need debugging (`|>` stages have no names to inspect).
- Every existing cascade keeps its exact meaning. No source changes are
  required by this ADR.

## Prior Art

| Language | Same-receiver | Chain-on-result | Notes |
|---|---|---|---|
| **Smalltalk-80 / Pharo / Squeak** | `;` | none (parens, temps) | `; yourself` idiom exists *because* `;` is fan-out. Recurring pharo-dev threads (2010, 2014) asked for `\|>` / "pipe operator"; answers pointed to library approaches (Vassili Bykov's `asPipe` proxy, a DNU-based `chain` in PharoFunctional) and nothing was merged. |
| **Cuis Smalltalk** | `;` | `::` | Cuis added `::` to its compiler so keyword messages chain like unary ones: `#(1 2 3) raisedTo: 2 :: max: [:x \| x] :: negated` — each message goes to the previous result. The closest prior art to this ADR: a Smalltalk keeping `;` and adding a *separate* chain token. We adopt the semantics, not the token (`::` is Beamtalk's type-annotation operator, including `x :: T := e` in expression position). |
| **Newspeak** | `;` | none | Keeps Smalltalk cascades unchanged. |
| **Dart** | `..` (cascade) | `.` | Cascade answers the *receiver*, not the last result; ordinary `.` chains. Two operators, two meanings. |
| **Elixir** | — | `\|>` | Pipes the previous value as the *first argument* of a function. Beamtalk's `\|>` pipes it as the *receiver* of a message — the message-send analogue. |
| **F# / OCaml** | — | `\|>` | Same token, function application. |
| **Gleam** | — | `\|>` | Same token; BEAM peers will recognise it. |
| **Kotlin** | `apply`/`also` | `let`/`run` | Library scope functions — the "do it with blocks" alternative (see Alternatives). |
| **C# LINQ** | — | `.` | ADR 0042 noted LINQ as "pipeline cascade in expression context"; `\|>` covers the same ground. |

What we adopt: Cuis's split — `;` for fan-out, a distinct token for chaining.
What we adapt: the token, choosing `|>` because BEAM developers (Elixir,
Gleam) already read it as "the thing on the left flows into the right".
What we reject: changing `;` itself (no surveyed language with a cascade
redefines it by receiver type).

## User Impact

**Newcomer (Python/JS/Ruby).** Method chaining (`a.b().c()`) is native to
them; `p withX: 5 withY: 10` failing (it is one `withX:withY:` message) and
`;` not chaining are both surprises. `|>` gives them the chain they expect,
and the `cascade-drops-update` warning turns the silent `;` mistake into a
pointed message that names `|>`. They now have two operators to learn; the
warning teaches the difference at the moment it matters.

**Smalltalk developer.** `;` behaves exactly as in Pharo; `yourself` keeps
its meaning. `|>` is a departure, but Cuis has shipped the same idea (as
`::`), and it removes a Smalltalk pain point that ADR 0042's immutable values
make far more frequent than in an image of mutable objects.

**Erlang/Elixir developer.** `|>` reads like Elixir's pipe, with one
difference worth a sentence in the docs: the value flows into the
*receiver*, not the first argument. Generated Core Erlang is identical to
the nested form, so debugging and stack traces are unchanged.

**Production operator.** No runtime change: same BEAM code as hand-nested
sends, no new dispatch, no hot-reload implications.

**Tooling developer.** Because a pipeline is an ordinary nested send with a
source marker, hover, go-to-definition, completion after `|>` (complete
against the previous stage's type) and senders/references work without new
AST arms. The formatter needs one layout rule (one stage per line when
broken).

## Steelman Analysis

### Option B — type-dependent cascade (ADR 0042's leading candidate)

- 🧑‍💻 **Newcomer**: "I never have to know which operator to use — `;`
  just does the right thing for the receiver in front of me."
- 🎩 **Smalltalk purist**: "Cascade's *intent* was 'configure this thing
  with several messages'. For a value, configuring *is* chaining; pipeline
  is the faithful translation of the intent, not a betrayal of it."
- ⚙️ **BEAM veteran**: "Elixir's `|>` with zero new syntax."
- 🎨 **Language designer**: "One construct, one intent; the compiler picks
  the mechanism."

Why it still loses: the mechanism is chosen from information the compiler
does not reliably have (question 2). A `Dynamic` receiver needs a run-time
kind test at every cascade, and `Object` instances and Erlang terms have no
right answer at all (an `Ets` table must fan out; it is neither a Value nor
an Actor). Adding a type annotation could silently flip a cascade's meaning.
`x foo; yourself` becomes `x foo` on values. Existing fixtures such as
`0 max: 1; max: n` change results.

### Option C — uniform pipeline `;`

- 🧑‍💻 **Newcomer**: "Every other language's chaining operator chains."
- 🎨 **Language designer**: "One rule, no type dependence — the honest
  fix for question 2 *and* question 1."
- 🏭 **Operator**: "Predictable: no receiver-dependent behaviour."

Why it loses: it breaks every actor/builder cascade whose messages answer
something other than the receiver — `ClassBuilder name:` answers the
`Symbol`, `Transcript show:` answers `nil`, `Ets at:put:` answers the value —
so `Object classBuilder name: #Foo; superclass: Object; register` would send
`superclass:` to `#Foo`. It discards the Smalltalk meaning ADR 0039 kept, and
every existing cascade would need auditing.

### Option A — status quo (fan-out; nesting and temps)

- 🎩 **Smalltalk purist**: "No new syntax. Temps are what Smalltalkers
  actually write, and they name the intermediate steps."
- 🏭 **Operator**: "Nothing to learn, nothing to break."
- 🎨 **Language designer**: "Every operator is forever; four parentheses is
  not a crisis."

Why it is not enough on its own: the trap is silent. Part 3 (the warning)
is the piece of this ADR closest to Option A and could be accepted alone;
the `|>` operator is the part that removes the reason people reach for `;`.

### Option E — library-level chaining (`then:` blocks / `asPipe` proxy)

- 🎩 **Smalltalk purist**: "Message passing all the way down — no grammar
  change, just a method on `Object`."
- 🎨 **Language designer**: "Composable, first-class, overridable."

Why it loses: `st then: [:s | s withA: 1] then: [:s | s withB: 2]` is longer
than the nesting it replaces; a DNU-forwarding proxy (`st asPipe withA: 1;
withB: 2; value`) is slow, hides the real receiver from the type checker and
LSP, and relies on `;` meaning something else on the proxy.

### Tension points

- Newcomers and language designers who value "one construct per intent" lean
  to B; Smalltalk purists, operators and tooling authors lean to A + D. The
  deciding factor is question 2: only syntax-directed rules (A, C, D) are
  meaning-stable under gradual typing.
- Purists may accept the warning (part 3) but not the operator (part 2).
  The parts are separable so the maintainer can take one without the other.

## Alternatives Considered

The motivating example under each candidate:

```beamtalk
// A. Status quo — nested parentheses
self store: ((((st withWebsock: websock) withWebsockState: websock)
  withWebsocketOptions: opts) withRespState: #upgraded)

// A'. Status quo — temp locals
st1 := st withWebsock: websock
st2 := st1 withWebsockState: websock
st3 := st2 withWebsocketOptions: opts
self store: (st3 withRespState: #upgraded)

// B / C. Cascade-as-pipeline (`;` chains on a value)
self store: (st withWebsock: websock; withWebsockState: websock;
  withWebsocketOptions: opts; withRespState: #upgraded)

// D. Dedicated pipe operator (this ADR)
self store: (st withWebsock: websock
  |> withWebsockState: websock
  |> withWebsocketOptions: opts
  |> withRespState: #upgraded)
```

### A. Status quo
Explicit nesting or temp locals. Correct and unambiguous, but the
readable-looking `;` form silently drops updates. Retained as always-valid
spellings; not chosen as the *whole* answer because nothing flags the trap.

### B. Type-dependent cascade (pipeline on values, fan-out on actors)
ADR 0042's leading candidate. Rejected: semantics depend on static type
information that gradual typing does not guarantee; `Dynamic` receivers
need a per-cascade run-time kind test (new runtime dispatch, slower, and
invisible in the source); `Object` instances and Erlang terms have no
correct classification; `; yourself` loses its meaning on values; existing
value cascades (`0 max: 1; max: n`) change results; and cascades on `self`
in a value method would no longer match cascades on `self` in an actor
method. Also forces a `ThreadedIr` decision for pipeline-on-`self` inside
value-type methods (`SelfVt` threading) that the nested form never raises.

### C. Uniform pipeline `;`
Rejected: breaks Smalltalk parity (ADR 0039), every builder/actor cascade
whose messages do not answer `self` (`ClassBuilder`, `Transcript`, `Ets`),
and the `yourself` idiom.

### D′. Other tokens for the pipeline operator
- **`::` (Cuis)** — the best Smalltalk precedent, but `::` is Beamtalk's
  type-annotation operator, including in expression position
  (`x :: Integer := 3`); `x :: foo` would be ambiguous with an annotation
  until the parser sees whether `:=` follows.
- **`;;`** — visually close to `;` (which is the confusion being fixed) and
  reads as "more cascade", not "different".
- **`~>`, `>>=`, `->`** — `~` and `>` are binary-selector characters, so
  these are (or could be) user-definable binary selectors; `->` is already
  a token (return types, `match:` arms). `>>` is the method-lookup selector.
- **`|>`** — not currently lexable as a selector (`|` is always
  `TokenKind::Pipe`), recognised by BEAM developers. Chosen.

### E. Library-level chaining
`Object >> then: aBlock` or a DNU-forwarding `asPipe` proxy. Rejected as the
primary answer (see Steelman). Nothing stops a library from adding `then:`.

### F. Multi-field functional update
A single message that sets several fields at once (e.g. a generated
`with: #{#websock => ws. #respState => #upgraded}` or a keyword form
`withWebsock: … respState: …`). It would serve the motivating example
directly — arguably better than any chain, because it allocates one copy —
but only for field updates of a single value; it does nothing for
collection pipelines or non-`with*:` messages. Complementary, out of scope;
listed as an open question.

## Consequences

### Positive
- The meaning of `;` never depends on types: gradual typing, `Dynamic`
  receivers and annotation changes cannot alter it.
- Smalltalk parity for cascades is preserved (ADR 0039).
- Left-to-right chaining of value updates and collection pipelines without
  parentheses or temps — the BT-506 goal — with zero codegen or runtime
  change: a pipeline lowers to the nested sends the compiler already
  handles, including all state-threading paths.
- The silent value-cascade trap becomes a warning with a fix-it, and the
  `setUp` lint stops endorsing it.

### Negative
- A new operator and a new concept: users must learn `;` (same receiver)
  versus `|>` (previous result). Two chaining spellings exist for the same
  computation (`|>` and nesting).
- `|>` departs from Smalltalk syntax; Pharo code does not use it, and its
  receiver-not-first-argument meaning differs subtly from Elixir's.
- The warning only covers statically-typed value targets; `Dynamic` value
  cascades still drop updates silently.
- Pipelines hide intermediate values from the debugger in the same way
  nesting does; temps remain better for step-debugging.

### Neutral
- Existing source is unaffected; no migration required.
- `yourself` keeps its Smalltalk meaning and stays useful only with `;`.
- The `cascade_candidate` lint is unchanged (its rewrite is
  meaning-preserving on values).

## Implementation

Sketch for `/plan-adr` once accepted; sizes are rough.

1. **Warning only (S)** — `cascade-drops-update` in
   `semantic_analysis` using `infer_cascade`'s resolved target type and the
   hierarchy's declared return types; fix
   `check_testcase_setup_drops_field_assignments`'s multi-`with*:` cascade
   acceptance. Independent of the operator; ships first.
2. **Lexer + parser (M)** — `TokenKind::PipeGt` for `|>`
   (`source_analysis/lexer.rs`); `parse_pipeline` above `parse_cascade` in
   `parser/expressions.rs`, producing left-nested `MessageSend`s with a
   pipeline source marker; the `;`/`|>` mixing error; line-continuation for
   a leading `|>`.
3. **Source fidelity (S/M)** — `unparse`, `beamtalk fmt` layout,
   reflected method source, tree-sitter / editor grammars under `editors/`.
4. **Language service (S)** — completion after `|>` against the previous
   stage's type; hover/definition already work through `MessageSend`.
5. **Docs & tests (S)** — `docs/beamtalk-language-features.md`
   (Cascades + new Pipelines section), `docs/beamtalk-syntax-rationale.md`
   (new departure row), BUnit tests in `stdlib/test/` for values, actors and
   `Dynamic` receivers, parser tests for precedence, layout and errors.
   `docs/development/surface-parity.md` needs no change (no new operation).

No codegen, `ThreadedIr`, or runtime changes are expected; if one turns out
to be needed, the source-marker design has failed and should be revisited.

## Open Questions (for the maintainer)

1. **Accept all three parts, or a subset?** Part 3 (warning + `setUp` lint
   fix) is valuable even if `|>` is rejected.
2. **Token.** `|>` (proposed) versus another spelling. A pure-Smalltalk
   alternative would be Cuis's `::`, at the cost of the annotation clash.
3. **Mixing `;` and `|>`.** Proposed: parse error without parentheses.
   Alternative: allow `a m1 |> m2; m3` with `;` binding tighter (cascade on
   the pipeline's last stage's receiver), as Cuis does with `::` and `;`.
4. **Binary stages.** Proposed: allowed for uniformity (`x |> + 1`), since
   the rewrite is trivial; could instead be restricted to unary and keyword
   stages for readability.
5. **Warning breadth.** Proposed: only non-final messages that answer the
   target's own type. Could widen to any discarded non-`nil` result on a
   value target (catches more, more false positives on value methods with
   side effects such as logging).
6. **Multi-field update (Alternative F).** Left undecided; worth a separate
   issue if `with*:` chains remain the dominant use of `|>`.

## References
- Related issues: BT-4352 (this ADR), BT-506 (canceled pipeline-chaining
  research; this ADR is its decision-record successor)
- Related ADRs: ADR 0042 § Cascade Semantics (deferred this question),
  ADR 0039 (Syntax Pragmatism vs Smalltalk — cascades kept),
  ADR 0038 (ClassBuilder as Actor so cascades fan out), ADR 0067 (three
  class kinds), ADR 0111 / ADR 0118 (state-threading through `ThreadedIr`,
  cascade-on-`self` lowering)
- Documentation: `docs/beamtalk-language-features.md` (Message Syntax,
  Splitting a Message Send Across Lines), `docs/beamtalk-syntax-rationale.md`
  § Cascades
- Code: `crates/beamtalk-core/src/source_analysis/parser/expressions.rs`
  (`parse_cascade`), `crates/beamtalk-codegen/src/core_erlang/expressions.rs`
  (`generate_cascade`), `crates/beamtalk-codegen/src/core_erlang/util.rs`
  (`threaded_cascade_on_self`),
  `crates/beamtalk-core/src/semantic_analysis/type_checker/inference/cascade.rs`,
  `crates/beamtalk-core/src/semantic_analysis/validators/lint_validators.rs`
  (`check_testcase_setup_drops_field_assignments`),
  `crates/beamtalk-lint/src/cascade_candidate.rs`
- External: Cuis Smalltalk `::` chaining
  (cuis-dev, March 2023: https://lists.cuis.st/mailman/archives/cuis-dev/2023-March/007062.html);
  pharo-dev "pipe operator" threads
  (https://lists.pharo.org/hyperkitty/list/pharo-dev@lists.pharo.org/thread/D6MQGICN6MQR4ZTODJRUBCRIT3OHZVMM/);
  Elixir `Kernel.|>/2`; Dart cascade notation
