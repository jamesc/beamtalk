# ADR 0133: Scope of Captured-Variable Mutation in Blocks

## Status
Accepted (2026-10-18); Proposed 2026-10-18

Decides between two destinations for the state-threading compiler. If accepted
as written, this ADR **supersedes ADR 0132** (callee-side threaded entry points)
and **ADR 0128** (opaque-callable fold threading for collection HOMs), and
**amends ADR 0041**'s "local mutations work in all blocks" to the rule in
§Decision. ADR 0131 (outer-local rebinds through compiler-known constructs),
ADR 0130 (class variables), ADR 0120 (value-type `self` threading), ADR 0118
(expression-level preludes) and ADR 0111 (the lowered-IR verifier) are kept.

Tracking: BT-3899. The hold recorded on BT-3865 (2026-10-18) lifts when this
ADR is Accepted or Rejected.

## Context

### Problem statement

A Smalltalk block that assigns a variable it captured is a store into a
context object the block shares with its home method, on a heap. The BEAM has
neither half of that: variables are immutable, and no heap is shared between
processes. Beamtalk therefore compiles every such assignment into **versioned
state threaded through every construct** and, since ADR 0132, through every
method call that takes a block.

The compiler has become the cost of that simulation. Measured on `main` at
`0b7cb7a` (2026-10-18):

| Measure | Value |
|---|---|
| `beamtalk-codegen`, non-test lines | 88,900 |
| of which state-threading machinery (`control_flow`, `threaded_ir`, `threaded_entry`, `threaded_expr`, `threading_analysis`, `sequencing`, `nlr`) | about 34,000 (40%) |
| `ThreadedIr` verifier invariants (`VerifyError` variants) | 34 |
| storage families threaded (`State`, `SelfVt`, `Acc`, `Local`) | 4 |
| producer kinds dispatched by `threaded_expression` | 6 |
| ADRs about this one mechanism | 10 of 136; the three most-cited ADRs in codegen are 0131 (261 citations), 0118 (211), 0111 (149) |
| Rust corpus proof tier (`just test-corpus`) | about 9 minutes per run |

Size alone would be acceptable if the machinery were converging. It is not:

- **The escaping case (ADR 0132) ships agreement bugs.** The post-merge
  review of #4361 (Phase 1c) found five in one phase, fixed in #4372: a
  second threading parameter used inside the first's operand failed to
  compile (BT-3890); every non-list collection receiver (`Dictionary`,
  `Stream`, `String`, user collections) was folded through `to_list`, so a
  `Dictionary collect:` answered a list and an infinite `Stream
  detect:ifNone:` hung; a non-block `ifNone:` handler crashed; a
  `ClassBuilder` fun inside a threaded body inherited the body's threaded
  state and answered `{42, plain}`; a mention of a parameter in a conditional
  argument was admitted by the index and refused by codegen. Each is the same
  genus: two walkers that must agree but are written twice (the threading
  index in `beamtalk-core`, `collect_root_uses` in codegen), or per-body
  state kept as one global slot on the generator. The review bot found a
  sixth (the `ifNone:` handler compiled twice) on the fix.
- **The inline case (ADR 0131) is not stable either.** Its residuals Epic
  BT-3883 has 13 open children: BT-3841 (of 733 measured statement shapes,
  552 still drop writes silently behind a position diagnostic), BT-3860 (an
  assignment in value position, `x := (t := t + 1) + 1`, never rebinds the
  local, a silent wrong answer in every context), BT-3891 (a write inside a
  conditional nested in a `whileTrue:` condition emits an unbound variable),
  BT-3875, BT-3897, BT-3878, BT-3861 (writes in `match:` arms, `groupBy:`
  blocks, and `detect:ifNone:` handlers inside loops, dropped or mis-shaped).
  Every one is a construct-times-position cell the AST-directed lowering
  never visited.
- **Demand for the escaping case is unmeasured, and the first measurement is
  zero.** ADR 0132's Open Question 5 asked for a count of real §6 hits before
  Phases 2 to 6 were scheduled. A scan of all 56 `.bt` files of
  `jamesc/beamtalk-symphony` (2026-10-18) found about 20 writes to captured
  locals, every one inside a `do:`, `ifTrue:`, `ifFalse:` or `ifNotNil:`
  literal block, and **zero** writing blocks that are stored, returned,
  passed to a user-defined method or sent to another process. The same
  codebase does write higher-order methods, five of its own (`logged:`, a
  middleware wrapper; `HTTPRouter build:`, a DSL block; `onEvent:`,
  `onExit:`, `onReload:`, callbacks stored on an actor and run later by
  another process) plus stdlib callbacks such as `Timer after:do:`. Every one
  takes its block as a value to run later, where assigning a local of the
  registering method would be meaningless in any Smalltalk; state goes into
  the actor through `self … !`. The HOM shape that *would* want a captured
  write, a user-defined collection class with its own `do:`, does not occur:
  data is maps, lists, value objects and `Result`s.

### Current state

Today's line, drawn by ADR 0131 §6 and narrowed by ADR 0132 Phase 1:

- A block literal written directly as the argument of a compiler-known
  construct threads its writes: `ifTrue:`, `ifFalse:`, `ifTrue:ifFalse:`,
  `ifNil:`, `ifNotNil:` and their two-block forms, `and:`, `or:`,
  `whileTrue:`, `whileFalse:`, `timesRepeat:`, `to:do:`, `to:by:do:`,
  `on:do:`, `ensure:`, `Result tryDo:`, the collection messages `do:`,
  `collect:`, `select:`, `reject:`, `detect:`, `detect:ifNone:`,
  `inject:into:`, `count:`, `anySatisfy:`, `allSatisfy:`, `eachWithIndex:`,
  `do:separatedBy:`, and the `Dictionary` lookup handlers
  (`state_threading_selectors.rs` is the one list). On a receiver whose type
  is known only at run time, the stdlib's own collections honour the Tier 2
  block protocol (ADR 0041): a Tier 2 block is an arity-N+1 fun carrying a
  `StateAcc` map and answering `{Result, StateAcc1}`.
- A block that **escapes**, by being stored (`f := [t := t + 1]`), returned,
  passed to a user-defined method (`Twice twice: [t := t + 1]`), or sent to a
  class in another process, is a compile error in class and value-type
  methods (ADR 0131 §6), and a structured runtime error
  (`stateful_block_dispatch`) wherever the compiler could not see it.
- ADR 0132 Phase 1c (merged 2026-10-18) gives every class method with a
  "threading parameter" a second entry `'__t2_class_<sel>'/n+2` over a fourth
  family `Acc`. **Nothing in Beamtalk calls it yet**: Phase 1e, the caller
  side, has not landed, so the entries are reachable only from EUnit. Phase
  1d (value types, BT-3872, #4374) is In Review and on hold.

### Constraints

- `docs/beamtalk-principles.md` §4: "when a Pharo habit conflicts with
  idiomatic BEAM behaviour, BEAM wins, and the Smalltalk *name* is kept where
  possible."
- No silent wrong answer, ever (ADR 0132's first goal, inherited here): every
  shape is either correct, a compile error, or a structured runtime error.
- Actor `State`, value-type `self` and class variables are the language's
  real shared state; whatever is decided for captured locals must leave them
  threaded, because that threading is idiomatic BEAM (a `gen_server` returns
  its state).
- `ThreadedIr` and its verifier (ADR 0111) stay the home of state-threading
  invariants; CLAUDE.md forbids new ad-hoc checks at call sites.
- The window in which ADR 0132 can be removed by deletion rather than by
  migration closes when Phase 1e lands.

## Decision

**A block may assign a captured local, a field or a class variable only where
the compiler lowers the block in place: as the direct argument (or receiver,
for `whileTrue:`/`whileFalse:`) of a control construct or a compiler-known
collection message. A block that escapes may read captured variables but may
not assign them.** The compiler reports the assignment at compile time; the
stdlib keeps the Tier 2 block protocol for its own collection messages on
dynamic receivers, so the inline rule holds whatever the receiver's run-time
type.

"Escapes" means any of: the block is assigned to a variable or field, returned
from its method, passed as an argument to a method the compiler does not lower
in place (any user-defined method, any stdlib method outside the known list),
sent as a message argument to a class or actor, or stored in a collection or
literal.

Consequently:

- ADR 0132 is superseded in full. The `__t2_` and `__t2_class_` entries, the
  `Acc` family, `MethodInfo::threading_params` and the threading index
  (`tier2_callee`), `tier2_methods` / `tier2_class_methods` metadata,
  `beamtalk_dispatch:send_threaded/4`, `call_threaded/3`, the Tier 2 fun mark
  (`threading_abi.rs`, BT-3885) and their tests are removed.
- ADR 0128 is superseded. An actor method that forwards a non-literal
  callable to a collection message sends it plainly: under this rule the
  caller could not have handed over a writing block, so there is no state to
  thread through the fold.
- ADR 0041's claim "local mutations work in all blocks" is amended to this
  rule. Its block protocol (Tier 1 and Tier 2 funs) stays, scoped to the
  compiler-known constructs and the stdlib collections.
- ADR 0131 §6's compile error becomes the permanent, documented semantics
  rather than a phase boundary, with the message below.

### Code examples

What stays exactly as it is, and is what Beamtalk code actually writes:

```beamtalk
total := 0
orders do: [:each | total := total + each price]
total                                    // => sum, threaded by the inline fold

count := 0
[count < 10] whileTrue: [count := count + 1]

found := nil
items do: [:x | (x > 3 and: [found isNil]) ifTrue: [found := x]]

// On a dynamic receiver the stdlib's own `do:` runs the Tier 2 block:
summary := 0
aDictionary do: [:v | summary := summary + v]
```

What becomes a permanent compile error, with the fix the message names:

```beamtalk
sealed Object subclass: Twice
  class sealed twice: aBlock =>
    aBlock value
    aBlock value

Object subclass: Caller
  class run =>
    t := 0
    Twice twice: [t := t + 1]            // error (see below)
    t

  class runWell =>
    t := Twice twice: [1]                // the block answers a value instead
    t + (Twice twice: [1])
```

### REPL session

```text
beamtalk> total := 0. #(1, 2, 3) do: [:x | total := total + x]. total
6
beamtalk> t := 0. Twice twice: [t := t + 1]
error: block assigns captured variable `t`, but `twice:` is not a construct
       the compiler threads it through
  ┌─ repl:1:21
1 │ t := 0. Twice twice: [t := t + 1]
  │                      ^^^^^^^^^^^^ `t` is assigned here
  = help: return the new value from the block and assign it where it is called:
          `t := Twice twice: [t + 1]`
  = note: a block passed to a user-defined method, stored, or returned may read
          captured variables but not assign them (ADR 0133). The messages that
          thread assignments are the control constructs (ifTrue:, whileTrue:,
          to:do:, on:do:, ...) and the collection messages (do:, collect:,
          inject:into:, detect:ifNone:, ...).
```

### Error examples

The same diagnostic fires for every escaping shape, naming the shape:

| Shape | Message fragment |
|---|---|
| `f := [t := t + 1]` | "`t` is assigned in a block stored in `f`" |
| `^[t := t + 1]` | "`t` is assigned in a block returned from `run`" |
| `self forEach: [:o \| t := t + o]` | "`t` is assigned in a block passed to `forEach:`, which is not a construct the compiler threads it through" |
| `Erlang lists foreach: [:e \| t := t + e] with: xs` (passed as an FFI argument) | "`t` is assigned in a block passed to Erlang `lists` `foreach:with:`, which calls it as a plain fun" |
| `Logger onEvent: [:e \| seen := seen + 1]` (other process) | "`seen` is assigned in a block passed to a class method; it would run in another process" |

The run-time side keeps one structured error for the cases the compiler
cannot see (a block stored first and then laundered through FFI, a
`perform:` with a runtime-built argument list): the existing
`stateful_block_dispatch`, which already says "the block was not run".

**Amendment (BT-3936): a self-send in a closure that changes actor state.**
A block that assigns nothing can still contain `self record: x`, where
`record:` writes a field. When that block is a real closure (a block passed to
the actor's own method, or stored), the send dispatches against the `State`
the closure captured at creation, and the callee's new `State` has nowhere to
go: the closure answers one value. This is not an escaping-block assignment, so
the compile-time rule above does not see it. It is a run-time check in the one
place an actor self-send dispatch is generated (`dispatch_codegen.rs`; every
form: threaded, discarding, sealed), active only inside a closure
(a Tier 1 closure). It compares the callee's returned `State` with the
`State` it was dispatched against using `=:=` (pointer identity first, so the
read-only case is cheap). A different map is compared once more with the
codegen-internal `__local__<name>` threading keys stripped
(`beamtalk_actor:strip_local_temps/1`, the helper the gen_server boundary
uses), because a read-only callee that rebinds a local through a conditional,
`on:do:` or `ensure:` changes only those keys. Equal answers normally; different raises
`#beamtalk_error{kind = stateful_block_dispatch}` carrying the selector and the
hint "A self-send inside a block changed actor state, but the block cannot
return it; return the value and assign it in the method instead". No
threading is reintroduced and no surrounding lowering changes. A literal block
the compiler inlines into the method (`self.items do: [...]`) is not a closure
and still threads `State`, so its self-sends keep their changes. A Tier 2
closure (`StateAcc` in, `{Result, StateAccN}` out) carries `State` back to its
caller, so it is exempt; a consumer that cannot take that state back already
raises `stateful_block_dispatch` itself.

The error is an ordinary `Error`: an `on: Error do:` written inside the closure
around the self-send catches it. That is the user's explicit choice to catch
every `Error`, and the error stays catchable rather than being made special.

Value types and class methods do not need the check: a `Value` method cannot
assign a slot, so a self-send never changes anything to lose, and a class
method has no `State` (class variables are written in place, ADR 0130). Not
covered: a user-defined binary selector sent to `self`, which is not compiled
as a self-dispatch at all and drops the change at method level too (BT-3985).

## Prior Art

**Smalltalk-80 and Cog.** Smalltalk-80 blocks were not real closures: a block
shared its home context's temporaries and could not be re-entered. Full
closures came later, and modern Pharo/Squeak compilers (Cog) classify blocks
as clean, copying or full, find the temps a block *writes*, and hoist them
into a heap-allocated "remote temp vector" shared by reference between block
and home. Beamtalk's Tier 1/Tier 2 split and its threading analysis are the
same classification. The difference is the last step: Cog allocates one
shared array; the BEAM cannot share by reference, so Beamtalk must thread the
vector through every return value. This ADR keeps Cog's analysis for the
inline case and declines to simulate the shared heap for the escaping case.

**Erlang and Elixir.** Variables are immutable; a closure captures values.
Idiomatic accumulation is `lists:foldl` / `Enum.reduce` returning the new
accumulator, exactly the "return a value from the block" idiom the diagnostic
recommends. Elixir users expect `Enum.each(xs, fn x -> total = total + x end)`
to warn and do nothing; this ADR makes the Beamtalk equivalent a compile
error with a fix, which is stricter and kinder.

**Gleam.** No mutation at all; `list.fold` is the only idiom. Beamtalk keeps
more than Gleam (inline assignment in loops and conditionals) because
`do:`-with-accumulator is the dominant Smalltalk shape and compiles to a fold
anyway.

**Newspeak.** Full closures with shared mutable contexts, like Pharo; nothing
to borrow on this point beyond the diagnostic style.

**Pony and Akka.** Actor languages that forbid a closure sent to another
actor from capturing mutable state (Pony's reference capabilities make it a
type error). The "sent to another process" row of the error table is the
same rule.

Adopted: Cog's write-analysis for the inline case; Erlang's return-the-value
idiom as the recommended fix; Pony's rule for blocks crossing an actor
boundary. Rejected: simulating a shared mutable heap across method calls and
processes (ADR 0132's approach).

## User Impact

**Newcomer (Python/JS/Ruby).** Loops and conditionals that accumulate into a
local work as expected. Passing a mutating lambda to a user function is the
one thing that errors, with a message that shows the fix. Python's
`nonlocal` and JS closure mutation make this surprising for a moment; the
error is discoverable at compile time and in the REPL, never at run time.

**Smalltalk developer.** This is a real departure: in Pharo, `Twice twice:
[t := t + 1]` answers 2. The justification is §4 of the principles: the
behaviour depends on a shared heap the platform does not have, and the
Smalltalk *name* (blocks, `do:`, `inject:into:`, `^` from a block) is kept
everywhere. Everything in Symphony's 56 files already fits the rule.

**Erlang/Elixir developer.** This is how they already think. Generated code
for a user HOM becomes a plain fun call, debuggable with `dbg`, with no
`{Result, Acc}` tuples or `__t2_` entries in stack traces.

**Production operator.** Fewer generated functions per module (no second
entry per block-taking class method), no metadata maps to keep in sync across
hot reload, and the runtime dispatch (`beamtalk_dispatch`) loses two gates.

**Tooling developer.** The LSP can report the diagnostic at the assignment
with a code action ("return the value instead"). Static analysis gets simpler:
"does this block escape" is a syntactic question with one answer, not a
fixpoint over the unit.

## Steelman Analysis

### A. Keep the promise; consolidate onto one `ThreadedIr` pass (rejected)
- 🧑‍💻 **Newcomer**: "I never want a compile error for something Python does
  fine; make it work everywhere."
- 🎩 **Smalltalk purist**: "`Twice twice: [t := t + 1]` answering 2 *is*
  Smalltalk. Any rule that makes it an error is a different language."
- ⚙️ **BEAM veteran**: "If you are going to thread state at all, do it once,
  properly, as closure conversion in a real IR; then the escaping case is
  just another scope kind, not a special protocol."
- 🏭 **Operator**: "One lowering pass is easier to trust than two walkers
  that must agree; the verifier becomes complete."
- 🎨 **Language designer**: "The semantics are uniform; the implementation
  problem is real but it is an implementation problem."

The strongest point here is the BEAM veteran's, and this ADR adopts it for
the inline case (§Implementation Phase 3). It is rejected for the escaping
case because the IR does not remove the two costs that are intrinsic to it:
a cross-method calling convention (two entries per block-taking method,
metadata, a runtime resolution walk, Phases 2 to 6 of ADR 0132) and a
cross-process question that has no good answer (a block sent to another
process assigning a local of a method that may have returned is a race, not a
feature). The measured demand is zero.

### B. Inline-only mutation, escaping blocks read but do not assign (chosen)
- 🧑‍💻 **Newcomer**: "Loops work, and when I do the one wrong thing the
  compiler tells me the fix in one line."
- 🎩 **Smalltalk purist**: "Smalltalk-80 blocks could not do this either;
  full closures came later. The idioms I actually use (`do:`, `inject:into:`,
  `detect:ifNone:`, `^` from a block) are untouched."
- ⚙️ **BEAM veteran**: "This is the BEAM's own rule, stated in Smalltalk
  vocabulary. Generated code is what I would write by hand."
- 🏭 **Operator**: "40% of the compiler is this mechanism; removing a third of
  it and freezing the rest is the single biggest reliability win available."
- 🎨 **Language designer**: "One syntactic question (does the block escape)
  replaces a greatest-fixpoint index, a runtime protocol and a fourth
  storage family."

### C. Mutable reference cells (process-dictionary or `atomics` boxes)
- ⚙️ **BEAM veteran**: "A `make_ref()`-keyed pdict cell per written local
  gives you Cog's remote temp vector within one process at near-zero
  compiler cost." Rejected: it does not cross processes (the class-method and
  actor cases), it leaks on NLR and exceptions without cleanup code at every
  boundary, and it makes block semantics depend on which process runs the
  block, which §4 of the principles forbids.

### D. Status quo: keep lowering per construct, finish ADR 0132 as planned
- 🎨 **Language designer**: "Each phase is small and reviewable." Rejected by
  the evidence: the per-construct approach produced BT-3883's 13 open cells
  and #4361's five agreement bugs; each new phase multiplies the cells.

### Tension points
- Smalltalk purists prefer A; BEAM veterans, operators and tooling
  developers prefer B. The principles' §4 tiebreaker resolves this toward B.
- Newcomers are split: A for "it just works", B for "the error tells me the
  fix". The REPL example decides it: B's error is one line and names the fix.
- The language designer's case for A (uniform semantics) is real; B answers
  it with a uniform *rule* (a syntactic escape test) instead of uniform
  mutation.

## Alternatives Considered

### A. Keep the promise and consolidate the lowering onto one `ThreadedIr` pass
Described above. Its implementation half is adopted for the inline case
(Phase 3); its semantic half is rejected.

### C. Mutable reference cells
Described above; rejected.

### D. Status quo
Described above; rejected.

### E. A declared annotation (`threads: aBlock`) instead of a rule
A method could declare which block parameter it threads, making the escaping
case opt-in. Rejected: it keeps the whole ADR 0132 mechanism for the
annotated methods, so it removes none of the cost, and it moves the burden to
library authors for a feature no scanned codebase uses.

### F. Call-site specialization (inline a sealed same-module callee)
ADR 0128 Option 4 / BT-3610. Rejected: it covers only sealed same-module
callees, needs the inlined body to be re-lowered per call site, and still
leaves every other callee at the §6 error, so it is a narrower B with more
code.

## Consequences

### Positive
- About a third of the state-threading machinery is deleted while nothing in
  Beamtalk depends on it: `threaded_entry.rs` (864 lines),
  `tier2_callee.rs` (1,144), `threading_abi.rs` and its generated header, the
  `Acc` family, `opaque_fold.rs` (777), the runtime gates, the `tier2_*`
  metadata, and about 3,200 lines of tests. Three families remain.
- One syntactic rule replaces a greatest-fixpoint index whose agreement with
  codegen had to be tested rather than guaranteed.
- The inline case gets the IR consolidation on a smaller problem, with
  BT-3883's open list as its acceptance criteria.
- Generated code for user HOMs is a plain fun call again; no second entry per
  method; hot reload has no entry metadata to keep consistent.
- Error messages improve: a compile-time diagnostic at the assignment with
  the fix, instead of `stateful_block_dispatch` at run time.

### Negative
- A documented departure from Pharo: `Twice twice: [t := t + 1]` is an error.
  Smalltalk developers will notice; the message must carry the idiom.
- Reverses two Accepted ADRs and a merged Epic (BT-3865 Phases 0 to 1c),
  about three weeks of work. The alternative was carrying it forward
  indefinitely.
- The known-selector list becomes a language-level contract rather than an
  optimisation table: adding a collection message to the stdlib that should
  thread writes means adding it to the list (one leaf module,
  `state_threading_selectors.rs`), and user-defined collections never join it.
- `Collection>>detect:ifNone:`'s lost threading parameter and the other 33
  stdlib `threading_params` entries become moot rather than fixed.
- **User-defined collection classes.** The Smalltalk idiom "subclass
  `Collection`, define `do:`, inherit the rest" meets the rule at its one
  sharp edge: `myOrders do: [:o | total := total + o price]` is refused when
  `do:` resolves to user code, because a hand-written `do:` cannot thread the
  block's writes. Today that shape is already a run-time
  `stateful_block_dispatch`; the rule makes it a documented limit instead of a
  gap to close. The idioms that remain: iterate the inner stdlib collection
  (`myOrders items inject: 0 into: [:acc :o | acc + o price]`), expose a
  folding method (`myOrders sumOf: [:o | o price]`) whose body uses the
  stdlib fold, or declare `uses: Enumerable` and implement only `do:`, in
  which case the protocol-provided `inject:into:`, `collect:` and `select:`
  are stdlib code expanded into the class (see Phase 1 for what they may
  carry). Neither Symphony nor the stdlib has such a class today; the
  `Enumerable` users in the stdlib (`Ets`, `ChangeLog`) are stdlib code.

  > **Amendment (2026-10-18, BT-3903): honour.** Measured on `main`, the
  > premise above is wrong in the user's favour: a block literal that
  > writes, sent with a compiler-known collection message (`do:`,
  > `inject:into:`, `collect:`, `select:`, `detect:ifNone:`, `anySatisfy:`,
  > …), is lowered in place as a fold over `beamtalk_collection:to_list/1`,
  > which drives the receiver's own `do:` with a pure block. So the write
  > threads for any receiver with a working `do:`, whether that `do:`
  > forwards to a stdlib collection or walks by hand, and whether the other
  > messages are the class's, a trait's provisions or absent. The decision
  > is to keep that: a user collection with `do:` honours writing blocks
  > from these messages, and neither `trait_expansion.rs` nor the protocol
  > sources carry a gate of their own (a provision written as `self do:
  > [... acc := ...]` is the same in-place fold in a value or plain class;
  > not in an `Actor`'s own method, where the self-send is dispatched and
  > the block escapes, a compile error since BT-3908 (BT-3918); the reason
  > `Enumerable` is built on `elements`). An actor
  > with `do:` honours writing blocks sent from another process. With a
  > writing block,
  > these messages *mean* the fold over `do:` only when the receiver's
  > class resolves the message to the stdlib's own method (see the
  > BT-3916 receiver guard below). Pinned by
  > `stdlib/test/adr0133enumerable_user_test.bt`. Two
  > corrections to the bullet: `Enumerable` requires `elements`, not
  > `do:`; and a user package cannot yet `uses:` a stdlib protocol at all
  > (BT-3915), so the test's user collections use a package-local trait
  > over `do:`, and `Ets` stands in for a real `uses: Enumerable` class.

  > **Amendment (2026-10-19, BT-3916): guard.** The "honour" above left one
  > inconsistency: a class that overrides one of these messages (other than
  > `do:`) with different semantics never ran its override for a writing
  > block, and silent wrong answers are ruled out by this ADR's goals. The
  > in-place fold now asks the receiver first
  > (`beamtalk_collection:to_list_for_threading(Recv, Selector)`, in place of
  > `to_list/1`; `do:` is the fold's own source and is never guarded). The
  > selector checked is the one the user wrote: `eachWithIndex:` and
  > `do:separatedBy:` are lowered through a synthetic `inject:into:` fold but
  > guard under their own names. A writing block sent a compiler-known
  > collection message other than `do:` folds over `do:` only when **no
  > collection message is the class's own**:
  >
  > - The sent message must resolve to a stdlib method, judged by the
  >   **origin of the method**, not the module it lives in (`uses:` flattens a
  >   provision into the using class's own module, ADR 0127): a stdlib class's
  >   method, or a provision of a **stdlib** trait such as `Enumerable`
  >   (BT-3915). A message the class defines itself, or gets from a
  >   package-local trait or an extension, raises `stateful_block_dispatch`.
  > - And the class must define, outside the stdlib, **none** of the
  >   compiler-known collection messages (every state-threading collection
  >   selector but `do:`). The stdlib's own `collect:`, `select:`, `count:`,
  >   `eachWithIndex:` and `do:separatedBy:` are `self inject:into:`, and
  >   `reject:` is `self select:`; the pure send reaches an override of those,
  >   the fold would skip it. So a class with its own `inject:into:` refuses a
  >   writing `collect:` too, and one with its own `select:` refuses `reject:`.
  >   The rule is deliberately the conservative one, so no table of which
  >   message runs through which has to be kept in step with `collection.bt`.
  >
  > A receiver that does not respond to the message at all raises
  > `does_not_understand`, as the pure send does. Codegen records, per class,
  > its stdlib-trait provisions (`stdlib_trait_methods`, from
  > `MethodInfo::origin` joined with the `(class, protocol)` pairs that
  > resolved to the stdlib) and whether it defines a collection message
  > outside the stdlib (`overrides_collection_selector`, from
  > `state_threading_selectors::overridable_collection_selectors`, the
  > compiler's one list) in `__beamtalk_meta/0`; the runtime guard reads both
  > and holds no list of its own. The cost is accepted: a package-local
  > trait's `inject:into:` refuses a writing block rather than answering, even
  > when it is a faithful fold over `do:`; a class that wants the write to
  > thread `uses:` the stdlib `Enumerable`, or computes the value without
  > assigning from the block. `testOwnAnySatisfyWritePin` flipped to
  > `testOwnAnySatisfyWriteRaises`. A runtime extension method on a collection
  > message (BT-3951), found by the guard in the extension registry for each
  > class on the chain, refuse the same way. So does a `species` override or a
  > class-side `withAll:` (own or extension), the only class-side message
  > `Collection`'s provisions reach through `species`, but only for the
  > selectors whose `Collection` provision ends in `self species withAll:`
  > (`collect:`, `select:`, and `reject:` through `select:`), and only when the
  > receiver gets them from `Collection`: `Enumerable`'s provisions forward to
  > `self elements`, so a `uses: Enumerable` class with the usual `withAll:`
  > constructor still folds every writing send, and a `Collection` subclass's
  > `inject:into:`, `detect:` and `anySatisfy:` still fold. Codegen records this
  > apart, as `overrides_species_construction`. The runtime copies of the lists
  > (`beamtalk_collection_selectors`) are checked against the compiler's by a
  > Rust test.

  > **Amendment (2026-10-20, BT-3952): an escaping block may not read a field of
  > an actor.** "Reading captured variables in escaping blocks is unchanged"
  > holds for locals, which are immutable once captured, but not for an
  > actor's fields: the block captures the actor's `State` when it is made, so
  > a block that reads `self.total` and runs after the actor wrote `total`
  > answers the old value, silently. Measured on a BEAM: a block passed to the
  > actor's own method answers stale when that method writes the field before
  > running the block (`bumpThenRun: [self.n]` answers 0, not 10), and a block
  > that is stored, returned or placed in a literal answers stale once a write
  > follows; a block run before any write answers right, and the compiler
  > cannot tell the two apart across a method boundary. The first design made
  > the read live (publishing `State` on every field write); it was dropped as
  > a runtime mechanism every write path must keep complete, with a call on
  > every write, serving a shape with no occurrence in `stdlib/src` or
  > `examples`. The rule is a compile error instead, in the escaping-block
  > recognizer of this ADR (`validators/local_threading.rs`): in an actor
  > instance method a block literal whose body reads `self.field` (any field,
  > through nested blocks too) and that is passed to a message sent to `self`
  > or `super` (the actor's own method, sealed or not, including `self do:`),
  > assigned to a field, returned, or placed in a collection literal, is
  > reported at the read. Conservative by construction: a self-send whose
  > callee runs the block before any write would answer right, and is rejected
  > too. Unaffected: blocks the compiler runs in place (`ifTrue:`, `do:`,
  > `detect:ifNone:`, ... on a non-self receiver), blocks passed to a
  > non-self receiver. A block bound to a local is followed to the
  > local's uses: an escape (`^b`, `self foo: b`, `self.cb := b`, `#[b]`) is
  > an error, and so is any use after something that may change the actor's
  > state (a `self.field :=` write or any send to `self`/`super`, wherever
  > it sits in between) or inside a block that contains one (a loop body's
  > later iterations). `b := [self.n]. b value` with nothing in between
  > answers right and is accepted; with a write or self-send between, it
  > measured stale (3, not the 13 written) and is an error.
  >
  > The reflective read `self fieldAt: k` counts as a field read in every one
  > of these rules. A send to `self` or `super` in a block (a getter such as
  > `self total` dispatches against the captured `State`, measured: a stored
  > and a returned `[self total]` both answered the old value) is a second,
  > narrower error, reported at the send ("is sent in a block that runs
  > later"): only when the block is stored (assigned to a field, returned,
  > placed in a literal, or bound to a local that does one of those). A block
  > passed directly to the actor's own method is exempt, since BT-3936's
  > runtime check covers a state-changing callee there, and so is the later
  > call of a local-bound block. Compute the value first and capture it. A class method is unaffected (class variables read live
  > at home, ADR 0130 §3), as is a value type (its fields never change). The
  > idiom is to pass the value in: `[:v | ...]` with the callee doing
  > `block value: self.total`. Live read may be revisited after BT-3950 if real
  > code needs it.

### Neutral
- Reading captured variables in escaping blocks is unchanged (an actor's
  `self.field` read is the BT-3952 amendment's exception).
- Non-local return from any block, escaping or not, is unchanged.
- Actor `State`, value-type `self` and class variables keep their threading
  and their existing rules (ADR 0120, 0130).
- The Tier 2 block protocol survives inside the stdlib's collections for
  dynamic receivers; it just never crosses into user code.

## Implementation

Phases, each its own PR or small epic; `/plan-adr` turns these into issues.

**Phase 0: statuses and holds (S).** Mark ADR 0132 and ADR 0128 Superseded
by this ADR, amend ADR 0041's claim, update `docs/beamtalk-language-features.md`
§Blocks and `docs/agents/expanded.md`. Close BT-3872 (#4374 unmerged),
BT-3873, BT-3874, BT-3885 follow-ups and BT-3893; close BT-3865 as
superseded.

**Phase 1: the diagnostic (M).** One recognizer in `beamtalk-core`
(`semantic_analysis::block_escape`, extending the existing ADR 0131 §6 check)
answers "does this block literal escape, and which captured variables does it
assign". It emits the diagnostic above at the assignment span, with the
shape-specific fragment and the fix. The ADR 0132 §6 inventory's `PIN-BUG`
cases flip to positive diagnostic tests. The LSP reports it as a diagnostic
with a code action. The note's list of known constructs is generated from
`state_threading_selectors.rs` so the list has one source (there is no
`beamtalk explain` command; the note carries the list).
Decide here, with a test, what a `uses: Enumerable` class's expanded
`inject:into:` / `collect:` / `select:` do with a Tier 2 block: they are
stdlib code, so they may honour the Tier 2 protocol as the stdlib collections
do (the block then threads through the user's `do:` only if that `do:` is
also protocol-provided or forwards to a stdlib collection), or they refuse it
with `stateful_block_dispatch` like any user method. Either answer is
acceptable under this ADR; the one that is not is dropping the write.
*Decided (BT-3903): honour, with the BT-3916 receiver guard; see the amendments under § Consequences.*

**Phase 2: removal (L).** In dependency order: the caller-side hook that was
never emitted; `send_threaded/4`, `call_threaded/3`,
`call_threaded_with_arguments/3`, `is_tier2_block/1`, `lookup_threaded_*`,
`threaded_target` and the `tier2_*` metadata in the runtime, with their
tests; `threading_abi.rs` and the generated header; `threaded_entry.rs`, the
`__t2_` exports, specs and `Tier2Entry` metadata in codegen; the `Acc`
family and its verifier checks (`AccAsScratchSeed`, `OwnLocalKeyInAcc`,
`NlrInThreadedBody`, the `Acc` rows of `NlrSlotShapeMismatch`,
`ConstructFamilyDropped` for `Acc`); `tier2_callee.rs` and
`MethodInfo::threading_params` with the generated builtins regenerated;
`opaque_fold.rs` and `FoldCarrier`, so a non-literal callable in an actor
method is the plain send. The shared tier2 conformance fixture and the
`threaded_*` EUnit modules go with them. Stdlib `.beam` output must be
byte-identical except for the two removed empty metadata keys.

**Phase 3: consolidate the inline lowering onto `ThreadedIr` (XL).** Make
`ThreadedIr` the emission input for whole method bodies: every assignment is
a `Bind` wherever it sits (closes the BT-3860 class), every construct is a
frame with explicit families (closes BT-3891, BT-3875, BT-3897), the legacy
write-backs go (BT-3833), and `ThreadedBodyState`-style generator globals are
owned by the frame stack. Acceptance: every open child of BT-3883 is a passing
corpus case, and BT-3841's 733-shape measurement reports zero silent drops.
Expression codegen (sends, literals, primitives, the typed-leaf `Document`
emission) is unchanged.

Affected components: `beamtalk-core` (semantic analysis, one new recognizer,
one removed), `beamtalk-codegen` (removal, then the IR pass), the runtime
(`beamtalk_dispatch`, `beamtalk_class_dispatch`, `beamtalk_class_metadata`),
the LSP (diagnostic plus code action), docs. Parser and REPL protocol are
unchanged; the REPL inherits the diagnostic.

The one single-source-of-truth module this touches is
`state_threading_selectors.rs`: the known-construct list is already the one
list codegen, the index and the §6 check read, and Phase 1 makes `explain`
read it too rather than restating it. No Rust/Erlang conformance fixture is
needed after Phase 2 because nothing about this rule crosses the boundary; the
Tier 2 block *shape* that remains is already pinned by the existing block
protocol tests.

## Migration Path

- **Compiled code.** No compiled Beamtalk program calls a threaded entry
  (Phase 1e never landed), so removing the entries changes no behaviour of
  existing `.beam` files. Modules compiled with the two extra metadata keys
  keep loading; the runtime already treats missing keys as "no entries".
- **Source code.** A block that assigns a captured variable and escapes is
  today a compile error in class and value-type methods (ADR 0131 §6) and a
  run-time error elsewhere; after Phase 1 it is a compile error everywhere,
  with the fix in the message. The scan of `beamtalk-symphony` found no code
  to migrate; the stdlib has none by construction (its HOMs are the known
  list). `beamtalk check` surfaces any remaining case in user projects before
  it is run.
- **Docs.** `docs/beamtalk-language-features.md` § Blocks gains the rule and
  the idiom; ADR 0041's §Decision gets an amendment note pointing here.

## Implementation Tracking

**Epic:** BT-3901
**Issues:** BT-3902, BT-3903 (Phase 1); BT-3904, BT-3905, BT-3906, BT-3907,
BT-3908 (Phase 2); BT-3909 (Phase 2/3 boundary); BT-3910, BT-3911, BT-3912,
BT-3913 (Phase 3); BT-3914 (LSP code action)
**Status:** Planned

The ADR 0131 residuals epic BT-3883 is cancelled and replaced by BT-3901:
BT-3862, BT-3858 and BT-3843 are absorbed by BT-3902 and BT-3903; BT-3860,
BT-3833, BT-3841, BT-3861, BT-3875, BT-3876, BT-3878, BT-3891 and BT-3897 are
duplicates of the Phase 3 issue that closes each of them. Three codegen
residuals filed outside BT-3883 (BT-3849, BT-3853, BT-3854) are duplicates of
BT-3911 and BT-3912 for the same reason, and BT-3884 moves under BT-3901 next
to BT-3902.

## References

- Related issues: BT-3899 (this ADR), BT-3901 (implementation Epic), BT-3865
  (ADR 0132 Epic, cancelled), BT-3883 (ADR 0131 residuals, replaced by
  BT-3901; Phase 3 acceptance), BT-3890 and #4372 (the
  review that prompted this), BT-3841, BT-3860, BT-3891, BT-3893, BT-3894,
  BT-3895, BT-3885
- Related ADRs: ADR 0041 (amended), ADR 0128 (superseded), ADR 0132
  (superseded), ADR 0131, ADR 0130, ADR 0120, ADR 0118, ADR 0111, ADR 0122
- Documentation: `docs/beamtalk-principles.md` §4; `docs/agents/expanded.md`
  § State-Threading Codegen; `docs/development/debugging.md` § ThreadedIr
  verifier; `crates/beamtalk-core/src/state_threading_selectors.rs`
- Prior art: Cog VM "remote temp vectors" (Eliot Miranda, *Closures*,
  Cog blog 2008); Pony reference capabilities; Erlang `lists:foldl`
