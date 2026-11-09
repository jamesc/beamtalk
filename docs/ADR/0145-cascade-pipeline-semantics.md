# ADR 0145: Cascade Semantics for Immutable Values — Keep `;` Fan-Out, Add a `|>` Pipeline

## Status
Accepted (2026-11-06); proposed 2026-11-05; amended 2026-11-09 (review: stage spans; the parser desugars `|>`, with a re-sugaring side table).

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
p withX: 5; withY: 10      // => Point(x: 1, y: 10) — the withX: result was dropped
```

`withX:`'s result is computed and discarded, and the cascade answers
`p withY: 10`. Nothing warns. The same trap applies to every built-in value
whose "update" messages answer a copy: `Dictionary at:put:`, `List` /
`String` building messages, `Integer max:`, and every user `Value subclass:`.

Verified on `main` (2026-11-05) with a throwaway BUnit test: the cascade
above answers `Point(x: 1, y: 10)`; `#{} at: #a put: 1; at: #b put: 2` answers
a one-entry dictionary; and a `TestCase` whose `setUp` is
`self withA: 1; withB: 2` runs its tests with `a = 0` — compiled without
any warning.

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
- **How often the nesting occurs.** A rough grep for a left-nested keyword
  send used as the receiver of another keyword send finds 5 lines in
  `stdlib/src`, 1 in `examples/`, and a few dozen in `stdlib/test` (most
  are `self assert: ((x foo: y) bar: z)`-shaped, two levels deep); only
  about 7 are `with*:` chains. Inside this repository, deep chains are
  uncommon. The motivating case comes from an application
  (beamtalk-buccaneer) whose request-state values carry many fields; how
  representative that is outside the repo is not measured.

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
statement-expr := keyword-expr                         // a plain send
                | keyword-expr ( ';'  message )+       // cascade (unchanged)
                | keyword-expr ( '|>' message )+       // pipeline (new)
message        := unary | keyword                      // a send without its receiver
```

A `;` after a pipeline stage, or a `|>` after a cascade message, is the
mixing error below. `|>` is one token only when the two characters are
adjacent; `| >` stays `Pipe` followed by a binary `>`.

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
// => Point(x: 1, y: 2)
> p withX: 5 |> withY: 10
// => Point(x: 5, y: 10)
> p withX: 5; withY: 10
// => Point(x: 1, y: 10)        (fan-out: unchanged, and now warned — part 3)
```

**Precedence and layout.**

- `|>` binds looser than keyword messages, like `;`. **A stage is
  everything up to the next `|>` (or the end of the expression), parsed as
  an ordinary keyword expression whose receiver is the previous stage's
  value**: `stage := unary-message+ binary-tail? keyword-message? |
  keyword-message`. So `|> keys sorted` sends `keys` and then `sorted`,
  `|> foo at: 1` sends `foo` and then `at: 1` to that result, and
  `|> size + 1` adds 1 to the size. Each is exactly what the same text
  means with the previous value written in front of it. This differs
  deliberately from a cascade part, which is a single message: today
  `x foo; size reversed` parses `reversed` as a separate statement on the
  same line (verified; BT-4508), and pipelines must not
  copy that trap.
- **No stage may start with a binary message.** `x |> + 1` is a parse
  error with the help "parenthesise the left side: `(…) + 1`". Unary
  binds tighter than binary, so `a foo + 1` already chains; a stage that
  starts with a binary operator would only help after a keyword stage
  (`a at: k |> + 1`), which reads worse than `(a at: k) + 1`. A binary
  message *after* a stage's unary messages (`|> size + 1`) is fine. This
  can be relaxed later without breaking code.
- The first segment is an ordinary expression (`recv m1`, or just `recv`):
  `st |> withX: 1` is legal and equals `st withX: 1`.
- A line that **starts** with `|>` continues the expression above, by the
  same rule that lets a line starting with a keyword part continue
  (language features § Splitting a Message Send Across Lines): `|>` can
  never begin a statement, so the continuation is unambiguous. **In the
  REPL** the line above is already complete when Enter is pressed
  (`is_input_complete`, `parser/mod.rs`), so a leading-`|>` layout must be
  wrapped in `( … )` there; a line that *ends* in `|>` is incomplete
  (`PipeGt` joins the trailing-incomplete tokens) and continues as usual.
- **`;` and `|>` do not mix without parentheses.** `a m1 |> m2; m3` is a
  parse error: *"cannot mix cascade `;` and pipeline `|>` in one expression;
  parenthesise the part you mean"*. Both readings are plausible, and the
  rare code that wants both is clearer with explicit parentheses
  (`(a m1 |> m2) m3; m4`) or temp locals.
- **Where a pipeline may appear: anywhere a full keyword expression
  parses.** That is every `parse_cascade` site (a statement, an
  assignment's right-hand side, a `^` value, the inside of parentheses,
  and so a block body's statements), plus the eight sites that call
  `parse_keyword_message` directly:
  - string interpolation: `"total: {orders select: [...] |> sum}"`,
    where Elixir users will try it first;
  - the three `match:` arm-body sites:
    `[1 -> a withX: 1 |> withY: 2; _ -> b]`;
  - map values and list, array and tuple literal elements:
    `#(a foo |> bar, b)` (map keys are not parsed at keyword level and
    keep needing parentheses).

  `|>` is its own token, so it never collides with the arm separator `;`,
  the element separator `,` or the list cons `|`. The `;`/`|>` mixing
  rule applies only where `;` would otherwise start a cascade; in an arm
  body `;` still ends the arm. The one place that still needs parentheses
  is a **keyword argument**, which is parsed at binary level for every
  keyword message: `x at: k put: (v withA: 1 |> withB: 2)`. Without them,
  `|>` ends the argument and pipes the whole `at:put:` send, the same as
  for any keyword message.
- **Casts.** A pipeline cannot end in `!`, just as a cascade cannot
  today. `!` is a statement terminator that marks the statement's send as
  a cast only when the statement is a plain `MessageSend`
  (`parse_statements`, `expressions.rs`); a pipeline statement gets the
  existing "cast in expression" error, with the help "bind the pipeline's
  result to a local, then cast to it on the next line". (`a m1! |> m2`
  ends the statement at `!`, so its error lands on `|>` as "a statement
  cannot start with `|>`".) Allowing a cast on the final stage would need
  every terminator site taught about pipelines and would make pipelines
  and cascades differ; it can be revisited if asked for.
- `|>` is not a binary selector and cannot be defined as a method. `|`
  is not a binary-selector character (it always lexes as
  `TokenKind::Pipe`), so no existing user selector is `|>` and no current
  grammar puts `>` right after `|`; the lexer gains one two-character
  token. `|` is also the union-type operator (ADR 0102), including in
  expression-position annotations (`x :: A | B := …`); taking `|>` reserves
  that spelling against any future type-operator use.

**For Elixir and Gleam users.** The `|>` spelling is chosen so BEAM
developers recognise it on sight: the value on the left flows into the step
on the right, read top to bottom. The one difference to learn is *where*
the value lands. In Elixir and Gleam it becomes the first argument of a
function; in Beamtalk it becomes the receiver of a message. Because a
Smalltalk receiver sits where an Elixir first argument sits, the shapes
line up one-for-one (each row is a separate stage, not one chain). The
first row shows the one stylistic difference: idiomatic Beamtalk does not
pipe the head, because `orders select: …` already sends to `orders`;
Elixir needs `orders |> Enum.filter(…)` only because `Enum.filter` is a
function, not a message to `orders`.

| Elixir | Gleam | Beamtalk |
|---|---|---|
| `orders \|> Enum.filter(&paid?/1)` | `orders \|> list.filter(is_paid)` | `orders select: [:o \| o isPaid]` |
| `\|> Enum.map(& &1.amount)` | `\|> list.map(fn(o) { o.amount })` | `\|> collect: [:o \| o amount]` |
| `\|> Enum.sum()` | `\|> int.sum` | `\|> sum` |
| `\|> Map.put(:a, 1)` | `\|> dict.insert("a", 1)` | `\|> at: #a put: 1` |

The function name (`Enum.map`) becomes a selector on the value
(`collect:`); the module disappears because the value's class decides
which method runs. There is no capture syntax (Elixir's `&`, Gleam's `_`)
for putting the value anywhere but the receiver; when it must be an
argument (`Logger info: x`), use `then:` (below), a temp local or the
nested form. The language-features
guide gets this table, and the parse error for a non-message stage
(`x |> 10`) points at it.

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

d at: #k |> + 1
// error: a pipeline stage cannot start with a binary message;
//        write `(d at: #k) + 1`

x |> keys sorted
// fine: `(x keys) sorted`; a stage is a whole message chain, not one message
```

**`then:` for the value-as-argument case.** Elixir's `&` and Gleam's `_`
can put the piped value anywhere; `|>` only makes it the receiver. For the
other cases, `Object` gains one method, named after Elixir's
`Kernel.then/2`:

```beamtalk
/// Answer the result of evaluating aBlock with the receiver.
then: aBlock :: Block => aBlock value: self
```

```beamtalk
average := orders collect: [:o | o amount]
  |> then: [:xs | xs sum / xs size]
```

It is an ordinary message, so it works without `|>` too (`x then: [...]`).
Its declared signature follows `Object >> ifNotNil:`
(`notNilBlock :: Block => notNilBlock value: self`), which leaves the
block bare for the same reason (BT-2834). Pharo spells the same method
`in:`; the Elixir name is chosen for this ADR's audience. Nothing in the
stdlib, the runtime or a current ADR defines `then:` today. (ADR 0034's
Future `then:` is superseded.) Forwarding proxies that rely on
`doesNotUnderstand:` (`TimeoutProxy`, the `TransparentProxy` example) would
answer `then:` themselves rather than forward it. BT-4515 either forwards
it explicitly in those proxies or documents the behaviour. `then:` ships
after `|>` (blocked by BT-4514): on its own, `then:` written after a
keyword message merges into it (see Option E). It is deliberately **not** `sealed`, so a
user class that already defines `then:`, such as a promise type, keeps
compiling and overrides it.

Unlike `ifNotNil:`, `then:` is not inlined by the compiler. That has
three consequences, which the docs state:

- **Types.** The type checker gets one rule, modelled on
  `infer_args_for_if_not_nil`: a literal block's parameter is typed as
  the receiver's type, and `then:` answers the block's result type. Without
  it, `xs` in `then: [:xs | …]` would be `Dynamic` and the rest of the
  pipeline would lose types and completion.
- **Captured locals.** A literal block that assigns a captured local is
  an escaping stateful block (ADR 0133), the same compile error as for any
  block passed to an ordinary method. Use a temp local instead.
- **Actors and class methods.** `then:` is a message, so on an actor
  receiver the block runs in the actor's process. A block that sends back
  to the calling actor deadlocks (`deadlock_detected`). On a class
  receiver whose class has class variables, it runs in the class process
  (ADR 0142). Elixir's `then` always runs in the caller. To get that,
  bind the value to a temp local. Inlining a literal-block `then:` so it
  runs in the caller for every receiver, as `ifNotNil:` does, would need
  its own `ThreadedIr` treatment for blocks that write locals. It is a
  possible follow-up, not part of this ADR.

A stage the previous result does not understand raises
`does_not_understand` exactly as the nested form would, and the type checker
reports it at the stage's span (it sees an ordinary send).

**AST representation (decided 2026-11-09): the parser desugars, and a
side table restores the source as written.** No `Expression::Pipeline`
variant exists. The parser builds each pipeline directly as the
left-nested `MessageSend`s it means, and the tree is identical to the one
the hand-nested form `((recv m1) m2) m3` produces.
- **The stage parser.** It is the keyword-expression parser with the
  receiver supplied, so `parse_cascade_message`, which parses one message
  only, cannot be reused.
- **Guards it keeps.** It keeps the `should_stop_keyword_continuation`
  guard, which stops a final keyword stage from swallowing the next method
  header.
- **Stages it rejects.** It rejects a stage that starts with
  `BinarySelector`, `Arrow` or `GtGt`.
- **Parser-only errors.** The `;`/`|>` mixing error and the other pipeline
  errors are parser errors, raised before the tree exists.

Alongside the tree, the parser records which sends were written with
`|>`: the span of each send whose message followed a `|>` token. The
record is carried on the definition that owns the code (the
`MethodDefinition`, and the `Module` for top-level and REPL expressions),
not passed as a separate argument. So every `unparse_method` or
`unparse_module` call sees it without a caller having to remember to
pass it. Spans are unique (§ Spans), and normalisation and cloning keep
them, so the record still matches after the operand-paren peel.

Consequences of this design:

1. **Analysis, codegen and most tooling see ordinary sends.** This covers:
   - name resolution, type inference, the ADR 0131 recognisers,
     `ThreadedIr` and codegen;
   - the LSP's position features (definition, hover, signature help);
   - the senders, references, announce-sites and field-accesses queries;
   - `@expect` collection and the lints.

   None of them gains an arm, and none can silently skip a pipeline,
   because nothing exists to skip. This is what keeps `|>` free of
   state-threading risk: from the parser onward, a pipeline *is* the
   nested send the compiler already lowers. The normalisation pass, its
   `has_parenthesised_operand` gate and `assert_operands_normalised` are
   unchanged.
2. **Re-sugaring is the only new consumer work.** It is needed in three
   places:
   - `unparse`, used by `beamtalk fmt`;
   - `as_written_method_source` (codegen `generator/mod.rs`);
   - the REPL `ClassBuilder` path.

   The last two rebuild a method's text from the normalised tree, then
   restore the parentheses recorded in `PeeledParens`. They must also
   print `|>` before each recorded send. Otherwise `(MyClass >> #sel)
   source` shows nested sends, and the workspace, which compares that
   text with the file on disk, reports the method as modified.
   - **The cost of this design.** A re-sugaring path that ignores the
     record prints the equivalent nested form instead of failing to
     compile.
   - **Mitigation.** The record lives on the definition, so the usual
     unparse entry points get it automatically. A round-trip corpus test
     (`unparse/tests/canonical_source.rs`) requires parse → fmt and
     parse → reflected source to reproduce every pipeline fixture byte for
     byte. The fixtures include comments between stages and pipelines in
     blocks, `match:` arms, string interpolation and collection literals.
3. **Token-level tooling.** Two things work on tokens, not the AST:
   - Completion right after `|>`: the language service sees a `PipeGt`
     token before the cursor and completes against the type of the
     expression that ends there.
   - The editor grammars and the liveview highlighter: they lex `|>` as
     one token.

**Comments between stages.** A multi-line pipeline invites a trailing
`// …` on a stage. Comments currently attach only at statement level, and
the workspace compares reflected source with the file on disk. So fmt and
the reflected-source restore must keep a comment between stages, or every
method that has one is reported as modified. BT-4514 decided where such a
comment attaches: to the record entry of the send whose `|>` follows it.
An end-of-line comment after the previous stage is that entry's trailing
comment, and comments on their own lines before the `|>` are its leading
comments. A comment after the last stage stays the statement's own trailing
comment. The round-trip corpus above covers it.

**Rejected: an `Expression::Pipeline` variant desugared in
normalisation.** This was the original design. It gives the formatter the
pipeline's structure directly, and the compiler forces every match site to
decide what to do with a pipeline. Its costs:
- a `Pipeline` arm across the raw-tree consumers: 42 non-test files match
  on `Expression::Cascade`, and catch-all `_ =>` arms would skip the new
  variant silently;
- a broader normalisation gate at five call sites;
- span reconstruction in every position-based LSP feature;
- handling `Pipeline` in the queries that parse reflected source.

All of that protects the few re-sugaring paths, which the round-trip test
protects more cheaply. A defaulted `SendForm` marker field on
`MessageSend` was rejected too, because it touches the ~480 `MessageSend`
construction sites.

**Spans (amended 2026-11-09).** Each synthesised send gets the span the
hand-nested form would give it: from the start of its receiver (the
pipeline head) to the end of its message, `receiver.span().merge(…)` as in
`parse_keyword_message`. It does **not** get just the stage as written.
Span-keyed analysis facts depend on this: `dispatch_kinds` and
`state_effects` (`semantic_analysis/facts.rs`) and the type map.
- If sends took the stage span, a stage of several messages
  (`|> keys sorted`) would produce two sends with the same span. BT-4122
  hit this class of aliasing for block profiles.
- An outer send's span would then no longer contain its receiver's span,
  which every node-at-position lookup assumes.

The wire check adds a test that every span in a desugared pipeline is
unique and contains its receiver's span.

**Diagnostics.** A diagnostic on a pipeline stage is reported at that
stage's message span. That is the message's own selector-and-arguments
span inside the synthesised send, so `withY: 10`, not the whole chain. It
points at the right place without special wording. A
message that says "the receiver of this send" means the previous stage's
value, and the span shows which one.

### 3. A `cascade-drops-update` warning for value cascades

A cascade gets a warning, at the dropped message, when **all** of:

- the cascade target's inferred type is statically a value kind (a
  `Value subclass:` or a built-in immutable class — `Integer`, `String`,
  `Symbol`, `List`, `Dictionary`, `Set`, `Tuple`, …), and
- a **non-final** message resolves to a method whose **defining class is
  itself a value kind** (not `Object`), and whose declared return type is
  `Self` or that defining class, **ignoring type arguments**. In other
  words it answers an updated copy, which the cascade then throws away.
  - Keying on the *defining* class handles inheritance: a synthesized
    `with*:` declares its defining class as the return type, so an
    inherited `withX:` on a `Point3D` target is defined on `Point`,
    returns `Point`, and counts.
  - Ignoring type arguments makes `Dictionary(K, V) at:put:` (declared
    `-> Dictionary(K, V)`) count.
  - Excluding methods defined on `Object` removes the protocol every value
    inherits. That covers `yourself` and the printing helpers `show:` and
    `showCr:` (declared `-> Self`, cascaded for their output). It also
    covers the `-> Object` methods, which a bare "superclass of the target"
    rule would have matched on every target.

```
warning[cascade-drops-update]: the result of `withX:` is discarded
  p withX: 5; withY: 10
    ^^^^^^^^^
  = note: `Point` is a value: `withX:` answers a new Point and leaves `p`
          unchanged, and a cascade sends `withY:` to `p`, not to that result
  = help: to send `withY:` to the result, use a pipeline:
          p withX: 5 |> withY: 10
```

Part 3 ships before part 2 and does not depend on it. Until `|>` exists,
or if it is rejected, the help suggests the nested form
(`(p withX: 5) withY: 10`) instead.

Two cases the rule above leaves open (added 2026-11-09):
- **Union-typed targets (ADR 0102).** The warning fires only if every
  member of the union is a value kind. The simplest conforming
  implementation does not fire on unions at all.
- **Protocol-provided methods** (e.g. `Comparable >> max: -> Self`).
  `Integer max:` must warn, so the implementation decides how "defining
  class" reads for a method that a protocol supplies, and records that
  choice in BT-4512.

**REPL limitation.** REPL bindings carry names, not types, so in the
REPL `p withX: 5; withY: 10` has a `Dynamic` target and does **not** warn.
The REPL transcript in part 2 shows the result, not a diagnostic. A
type-free trigger (two or more synthesised `with*:` selectors in one
cascade) would cover that case. It is a possible follow-up.

It does **not** fire on `Dynamic` targets, actors, `Object` instances,
non-final messages answering some other type (`printString`, `size`), or
methods inherited from `Object` (`yourself`, `show:`).
It is a warning with the usual `@expect cascade_drops_update` /
`[diagnostics] cascade-drops-update = "off"` controls, not a guarantee —
a dynamic receiver still fans out silently, as it always has. Known false
positives: a value method declared `-> Self` that also has a side effect
(logs, validates, sends to an actor) and is cascaded for that effect;
`@expect` covers them.

The check needs inferred types, so it lives in the type checker beside
`infer_cascade` (`type_checker/inference/cascade.rs`), which already
resolves the cascade target's type and each message's dispatch — not in
the AST-only validators of `lint_validators.rs`. It reads declared return
types from the class hierarchy; no new selector table. The `setUp` lint
fix below is syntactic and uses the existing `with*:` recogniser
(`crate::synthetic_selectors::with_star_selector`).

The `cascade_candidate` lint (beamtalk-lint) suggests turning 3+
consecutive statements to one receiver into a cascade. Left alone, on a
value receiver it would suggest exactly the cascade the new warning then
flags. Those statements were already discarding each update, so the
rewrite would preserve meaning but suggest a shape that is now warned. The
lint therefore skips a run of sends that the new warning would flag.
Such a run is itself a dropped-update bug (`p withX: 5.` alone discards
its result). Catching discarded copies at statement level is a separate,
broader lint and is out of scope here; it is listed as a follow-up.
The same change corrects `check_testcase_setup_drops_field_assignments`:
a `setUp` ending in a `self with*:` cascade with **more than one** `with*:`
message is no longer accepted as self-producing (it drops all but the last);
its help text suggests the `|>` form.

### What stays valid regardless

- Explicit nesting — `((st withA: 1) withB: 2)` — is and remains the
  canonical spelling; `|>` is sugar for it, never a replacement.
- Temp locals remain the recommendation when intermediate values deserve
  names or when stages need debugging (`|>` stages have no names to inspect).
- Every existing cascade keeps its exact meaning. The only source changes
  are warning suppressions: cascades that deliberately fan out on a value
  and now warn — e.g. the codegen-regression fixtures'
  `0 max: 1; max: (…)` (Integer target, `max:` answers Integer) — get an
  `@expect cascade_drops_update`. Phase 1 runs the warning over stdlib and
  the test corpus and lists every hit before it lands.

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

**Erlang/Elixir/Gleam developer.** `|>` is the same token and the same
top-to-bottom reading as in Elixir and Gleam — a design goal, not an
accident — with one difference the docs state up front: the value flows
into the *receiver*, not the first argument (see "For Elixir and Gleam
users" above). Generated Core Erlang is identical to the nested form, so
debugging and stack traces are unchanged.

**Production operator.** No runtime change: same BEAM code as hand-nested
sends, no new dispatch, no hot-reload implications.

**Tooling developer.** The parser desugars `|>`, so every AST consumer
sees an ordinary nested send. That includes type inference, lints, hover,
definition, signature help, and the senders and references queries. Only
the re-sugaring paths need new work: `unparse`/fmt, reflected method source
and the REPL `ClassBuilder` path. They read the per-definition record of
`|>` sends. Completion after `|>` is token-driven and offers the previous
stage's selectors. Editor grammars lex `|>` as one token. The formatter
needs one layout rule (one stage per line when broken). Re-sugaring is the
bulk of the operator's tooling cost (§ AST representation).

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
- 🎨 **Language designer**: "Composable, first-class, and an ordinary
  method a class can override."

Why it loses as the primary answer: `(st then: [:s | s withA: 1]) then:
[:s | s withB: 2]` is longer than the nesting it replaces and needs the
parentheses, because without them it is one `then:then:` send. That
same hazard means a `then:` written after a keyword message without `|>`
(`xs inject: 0 into: […] then: […]`) is swallowed into
`inject:into:then:`. With `|>` in front, the stage boundary prevents this,
which is one more reason `then:` is adopted as a `|>` companion
(§ Decision) rather than a substitute; a DNU-forwarding proxy (`st asPipe withA: 1;
withB: 2; value`) is slow, hides the real receiver from the type checker and
LSP, and relies on `;` meaning something else on the proxy.

### Tension points

- Newcomers and language designers who value "one construct per intent" lean
  to B; Smalltalk purists, operators and tooling authors lean to A + D. The
  deciding factor is question 2: only syntax-directed rules (A, C, D) are
  meaning-stable under gradual typing.
- Purists may accept the warning (part 3) but not the operator (part 2).
  The parts were kept separable for that reason. The maintainer took all
  three, with part 3 first (§ Resolved).

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
primary answer (see Steelman). `then:` is adopted as a complement to `|>`
for the value-as-argument case (§ Decision); the proxy is not.

### F. Multi-field functional update
A single message that sets several fields at once (e.g. a generated
`with: #{#websock => ws. #respState => #upgraded}` or a keyword form
`withWebsock: … respState: …`). It would serve the motivating example
directly — arguably better than any chain, because it allocates one copy —
but only for field updates of a single value; it does nothing for
collection pipelines or non-`with*:` messages. Complementary and out of
scope: it becomes a separate, low-priority issue (§ Resolved).

### G. Warning plus multi-field update, no new operator
Accept part 3 and Alternative F, reject part 2. This fixes the trap and the
motivating example with no new syntax; collection chains of *unary*
messages already read left-to-right without parentheses. This is the
strongest rival to the full proposal and the right answer if the
in-repo frequency of deep keyword chains (see Context § Current state:
low) is taken as representative. Not taken (§ Resolved): F does not
cover keyword-heavy collection pipelines (`select:` → `collect:` →
`inject:into:`), which is the case BT-506 was opened for, and the
maintainer wants `|>` for Elixir and Gleam users.

### H. Formatter-only: lay out nested chains one send per line
Teach `beamtalk fmt` to break a left-nested keyword chain into one send per
line with aligned parentheses. Improves readability with no language
change, but the reader still counts parentheses to find where each stage
ends, and the `;` trap is untouched. Compatible with any option; not a
substitute for part 3.

### I. Make the warning an error where it is certain
For a cascade whose non-final message is a compiler-synthesized `with*:`
setter on a statically-known `Value` target, the dropped update is certain
(the setter has no side effects), so the compiler could reject it outright.
Not proposed: errors on well-formed Smalltalk are a bigger step than ADR
0039 allows without evidence the warning is ignored; the warning can be
promoted later.

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
- The tree no longer records how a send was written. A re-sugaring path
  (unparse, fmt, reflected source) that ignores the per-definition `|>`
  record prints the equivalent nested form instead of failing to compile.
  The round-trip corpus test is the guard. Most of the operator's
  implementation work is this re-sugaring and the parser.
- Diagnostics on desugared stages use generic send wording ("the receiver
  of this send") at the stage's span, rather than pipeline-specific
  phrasing.
- `|>` takes a spelling from the `|` family that ADR 0102 uses for union
  types, closing it to future type-operator syntax.
- The warning has false positives on side-effecting `-> Self` methods
  defined on value classes and fires on a few existing intentional value
  cascades, which need `@expect`.
- Multi-line pipelines laid out with a leading `|>` need parentheses in
  the REPL, unlike in files.
- The motivating evidence is one application; in-repo deep chains are
  rare. If that holds, the operator's cost may outweigh its use
  (Alternative G).

### Neutral
- Existing cascades keep their meaning; the only source edits are
  `@expect` suppressions on intentional value cascades.
- `yourself` keeps its Smalltalk meaning and stays useful only with `;`.
- The `cascade_candidate` lint stops suggesting a cascade where the new
  warning would fire on it.

## Implementation

Phases for `/plan-adr`; sizes are rough.

1. **Warning only (S/M)** — `cascade-drops-update` in the type checker
   beside `infer_cascade`, using its resolved target type and the
   hierarchy's declared return types (defining class a value kind, type
   arguments ignored); its help suggests the nested form until `|>` ships;
   run it over stdlib and the test corpus, list the hits and add `@expect`
   where the fan-out is intended; fix
   `check_testcase_setup_drops_field_assignments`'s multi-`with*:` cascade
   acceptance; make `cascade_candidate` skip runs the warning would flag.
   Independent of the operator; ships first.
2. **Wire check (S/M).** This comes before the full parser work.
   - Lex `|>`.
   - Parse a pipeline at statement level directly into nested
     `MessageSend`s, with hand-nested spans.
   - Record the `|>` sends on the owning `MethodDefinition` / `Module`.
   - Teach `unparse` and `as_written_method_source` to re-sugar.
   - Tests:
     - one end-to-end BUnit test on value, actor and `Dynamic` receivers;
     - `just verify-threaded-ir`;
     - a span-uniqueness test;
     - a check that `(Cls >> #sel) source` and fmt round-trip the `|>`.

   This proves the "no codegen change" assumption and the re-sugaring
   design before the rest lands.
3. **Lexer + parser (M)** — `TokenKind::PipeGt` (adjacent `|>` only) in
   `source_analysis/lexer.rs`; `|>` stages at every `parse_cascade` site in
   `parser/expressions.rs`; the `;`/`|>` mixing error; the "cast in expression" error for a
   pipeline ending in `!`; `|>` stages at the eight
   `parse_keyword_message` sites (string interpolation, `match:` arm
   bodies, collection literals), with `;` keeping its arm-separator
   meaning in arms; the binary-stage error; line-continuation for a leading
   `|>`; `PipeGt` in `is_input_complete`'s trailing-incomplete tokens.
   Parser tests for each, plus a `tests/repl-protocol/cases/` case for
   multi-line REPL input. The stage parser is the keyword-expression
   parser with an implicit receiver (not `parse_cascade_message`), with
   tests that `|> keys sorted` and `|> foo at: 1` stay one stage and that
   a stage starting with `BinarySelector`/`Arrow`/`GtGt` is rejected.
   `Object >> then:` in `stdlib/src/object.bt` (not sealed), with BUnit
   tests on value, actor and class receivers, plus the type-checker rule
   that types a literal block's parameter as the receiver (next to
   `infer_args_for_if_not_nil`).
4. **Source fidelity (M).**
   - `beamtalk fmt` layout: one stage per line when broken.
   - Re-sugaring on the REPL `ClassBuilder` path.
   - Comments between stages.
   - The full round-trip corpus in `unparse/tests/canonical_source.rs`:
     blocks, `match:` arms, string interpolation, collection literals and
     comments.
   - The editor grammars under `editors/`, including the liveview
     highlighter, which treats `|` as a binary character today.
5. **Language service (S)** — completion after a `PipeGt` token against
   the type of the expression before it. Tests show that hover,
   definition and signature help on a stage already work, because they
   see the nested sends; no new arms are expected.
6. **Docs & tests (S)** — `docs/beamtalk-language-features.md`
   (Cascades + new Pipelines section), `docs/beamtalk-syntax-rationale.md`
   (new departure row), BUnit tests in `stdlib/test/` for values, actors and
   `Dynamic` receivers, parser tests for precedence, layout and errors.
   `docs/development/surface-parity.md`: no new operation, but two
   existing rows change. `complete` (shared by the REPL, MCP, LiveView and
   LSP) must offer the same candidates after `|>` on every surface.
   `browse-method-source` (LiveView) and the REPL's `(Cls >> #sel) source`
   must show `|>` as written.

No codegen, `ThreadedIr`, or runtime changes are expected. If the wire
check (phase 2) needs one, the desugar-to-nested-sends design has failed
and the ADR should be revisited before phases 3–6.

## Migration Path

Nothing breaks: every existing program compiles and runs with the same
meaning, and `|>` was not previously valid syntax. `Object >> then:` is
not sealed, so a user class that already defines `then:` keeps its own
method. Two changes do surface
*new diagnostics* on code that compiles cleanly today, and each lands with
the fix-up in the same PR:

- **`cascade-drops-update`** fires on existing cascades whose target is a
  value and whose non-final message answers an updated copy. For each hit,
  either the cascade is a latent bug (rewrite it with `|>`, nesting or temp
  locals) or the fan-out is intended (add `@expect cascade_drops_update`).
  Phase 1 lists the stdlib and test-corpus hits up front, such as the
  `0 max: 1; max: (…)` codegen fixtures, which are intended.
- **The `setUp` lint** stops accepting a `setUp` that ends in a `self`
  cascade with more than one `with*:` message. Every such `setUp` is
  already broken (all but the last field are dropped, as verified above);
  the fix is `self withA: 1 |> withB: 2`, or the nested form if `|>` has
  not shipped yet.

Projects that build with `--warnings-as-errors` see both as failures until
they fix or suppress the hits; a build without that flag lists every
hit as a warning first.

## Open Questions (for the maintainer)

None remain. The 2026-11-09 review reopened the AST design, and the
maintainer settled it the same day (below). Every earlier question was
settled on 2026-11-06.

### Resolved (2026-11-09)

- **AST design for `|>`.** The parser desugars `|>` into nested sends and
  records the `|>` sends on the owning definition for re-sugaring. There is
  no `Expression::Pipeline` variant (§ AST representation).
- **Stage spans.** Hand-nested spans, unique by test (§ Spans).
- **`then:` ordering.** Blocked by BT-4514.

### Resolved (2026-11-06)

Each of these was an open question in an earlier draft. Where the
decision is the stricter choice, loosening it later is cheap; tightening
it later would break code.

- **Scope:** all three parts. Part 3 (the warning and the `setUp` lint
  fix) ships first, because it fixes a live bug and does not depend on
  `|>`. Alternative G (no operator) is not taken.
- **`then:` vs `in:`:** `then:`, Elixir's name, for this ADR's audience.
  Pharo's `in:` is not added.
- **Token:** `|>`, so Elixir and Gleam users recognise it. Cuis's `::` is
  ruled out by the type-annotation clash.
- **Mixing `;` and `|>`:** a parse error without parentheses. Both
  readings are plausible, and code that needs both is rare.
- **Binary stages:** not allowed (§ Precedence and layout). Unary already
  chains through binary; the error suggests parentheses.
- **Warning breadth:** narrow. It flags only a dropped updated copy (a
  method defined on a value class answering `Self` or that class). A
  general "discarded result" lint is a separate follow-up.
- **Multi-field update (Alternative F):** a separate, low-priority issue,
  independent of this ADR.
- **`match:` arms, string interpolation and collection literals:** accept
  a bare `|>`. The rule is "anywhere a full keyword expression parses";
  only keyword arguments need parentheses.
- **Error vs warning for a certain drop (Alternative I):** a warning for
  now. It can be promoted to an error later if the warning is ignored.
- **Value-as-argument escape hatch:** ship `Object >> then: aBlock` with
  the operator. It is an ordinary, overridable, non-inlined method with a
  typing rule; inlining it to run in the caller is a possible follow-up.
- **Casts:** a pipeline cannot end in `!`, matching cascades.

## Implementation Tracking

**Epic:** BT-4511
**Status:** Planned

| Phase | Issue | Size | Blocked by |
|---|---|---|---|
| 1 | BT-4512: `cascade-drops-update` warning | M | — |
| 1 | BT-4513: `setUp` lint rejects multi-`with*:` cascades | S | — |
| 2 | BT-4514: wire check (lex `\|>`, parser desugar, re-sugaring record) | M | — |
| 2 | BT-4515: `Object >> then:` and its typing rule | S | BT-4514 |
| 3 | BT-4516: complete `\|>` parsing and errors, REPL multi-line | M | BT-4514 |
| 3 | BT-4517: source fidelity (unparse, fmt, reflected source, editors) | M | BT-4514 |
| 3 | BT-4518: completion after `\|>` | S | BT-4514 |
| 4 | BT-4519: docs and end-to-end REPL-protocol case | S | BT-4512, BT-4515, BT-4516, BT-4517, BT-4518 |

Follow-ups outside the epic (low priority, `needs-spec`):

- BT-4520: inline `then:` so it runs in the caller
- BT-4521: multi-field functional update (Alternative F)
- BT-4522: statement-level lint for discarded copies
- BT-4508: cascade parts silently drop trailing tokens

## References
- Related issues: BT-4352 (this ADR), BT-506 (canceled pipeline-chaining
  research; this ADR is its decision-record successor), BT-4508 (cascade
  parts silently drop trailing tokens; the trap pipeline stages avoid)
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
