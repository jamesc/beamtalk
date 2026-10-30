# ADR 0137: Actor Closures See the Actor's Live State

## Status
Accepted (2026-10-20); Proposed 2026-10-20; Amended 2026-10-21 (self-send
exception after Phase 0, BT-4007; snapshot away and simple `call_foreign/3`
after review); Amended 2026-10-26 (stateless loops bracket the
guarded-operator slow path, BT-4121)

> **Amended by [ADR 0142](0142-class-process-owns-only-class-state.md) (2026-10-30), blocks in class methods.** A block passed to a class method is inside the actor whenever that method runs in the caller: every class method of a class whose chain declares no class variables, sealed or not, except the process-bound selectors (`new`, `spawn`, `startLink`, ...). Such a block sees and writes the actor's live state, and the `actor-state-abroad` lint no longer warns at it. A class method of a class that keeps its process (class variables in its chain, or a process-bound selector) still runs the block away. The lint reads `ClassHierarchy::runs_class_method_in_caller`, the rule codegen uses, instead of the retired `direct_class_methods` set.

Amends ADR 0133 (an escaping block may assign `self.field`) and ADR 0041 (an
actor's closures no longer capture `State`). ADR 0130 is the precedent and is
unchanged. ADR 0131, ADR 0118 and the locals half of ADR 0133 are unchanged.

Tracking: BT-3996. Epic: BT-4006. Decision gate: BT-3950. Phase 0 (BT-4007,
#4478) and Phase 1 (BT-4008, #4472) are done. The mechanism, verifier rules,
measurements and phase detail are in the design note
[Actor State Cell](../development/actor-state-cell.md).

## Context

An actor method compiles to a function of `State` that returns a new `State`.
The compiler threads versions (`State`, `State1`, …) through every construct
it lowers in place. A block it does not lower in place is a Tier 1 `fun`
(ADR 0041). The `fun` captures whichever `StateN` was current when it was
created and answers one value, so it cannot hand a new `State` back.

That one fact is behind every actor-closure bug of ADR 0133 Phase 3. Each new
shape (a stored block that writes a field, a block that reads a field after
the method writes it, a self-send inside a block, a send to an alias of
`self`) got its own compile-time rejection or runtime guard, and each of
those also rejects some correct code. Some shapes still answer wrong
silently. The design note lists the eleven issues and the probe results.
PR #4447 tried publishing `State` to the process dictionary on every write
and was closed because every lowering that discards a callee's `NewState`
left the published copy wrong, with nothing to check it.

The maintainer's direction: "I'm all for simplicity of model — if it fits OTP
usage, then let's go." OTP fit decides this ADR.

## Decision

### The rule

A block written in an actor's method uses the actor's fields. When it runs
inside the actor, it sees the fields' current values and its writes stay.
That holds in the method that made it, in a method that method calls, and in
a later message. When it runs anywhere else (in another actor, a `Timer`, a
spawned process), it sees the values the fields had when it was made, and
assigning a field raises `actor_state_unreachable`. A message it sends to
`self` from there is an ordinary message to the actor. Class variables
already follow this rule (ADR 0130).

```beamtalk
Actor subclass: Counter
  state: n = 0
  state: onTick = nil

  twice: aBlock =>
    aBlock value
    aBlock value

  // Inside the actor, now
  addTwice =>
    self twice: [self.n := self.n + 1]
    self.n

  // Inside the actor, in a later message
  arm => self.onTick := [self.n := self.n + 10]
  tick =>
    self.onTick value
    self.n

  // Somewhere else: another actor runs the block
  peekVia: other =>
    b := [self.n]
    self.n := 99
    other run: b
  pokeVia: other => other run: [self.n := 0]   // actor-state-abroad warning

Actor subclass: Runner
  run: aBlock => aBlock value
```

```beamtalk
c := Counter spawn
c addTwice                // => 2
c arm                     // => _
c tick                    // => 12
c peekVia: Runner spawn   // => 12, the value when the block was made
c pokeVia: Runner spawn
// => ERROR: actor_state_unreachable: Counter's state field n cannot be written from this process
```

On `main`, `addTwice`, `arm` and `pokeVia:` are compile errors (BT-3922) and
so is `peekVia:` (BT-3952). The runtime error carries the field and a hint:

```erlang
#beamtalk_error{kind = actor_state_unreachable, class = 'Counter',
                details = #{field => n},
                hint = <<"A block that accesses Counter's state ran outside Counter's own process "
                         "(it was sent to another actor, a Timer or a Future, ...). A block can read "
                         "Counter's fields anywhere, as the values they had when the block was made, "
                         "but can only write them inside Counter's own methods: send Counter a "
                         "message instead.">>}
```

A block passed to a class method is inside the actor only when that method
is direct-called, which is a `class sealed` method of a sealed class without
class variables (ADR 0129 Phase 0b). Any other class method runs in its
class's process, so the block is away.

An `actor-state-abroad` lint, modelled on `class-state-abroad`, warns at a
block literal that reads or writes a field, or sends to `self`, and is passed
to `Timer`, written as the receiver of `fork` (`[...] fork`), to a send whose receiver is statically another actor, or to a
class method that is not direct-called (the lint reads the same
`direct_class_methods` set codegen uses). A write there raises, a read sees a
stale value, and a synchronous self-send can deadlock if the actor is
waiting on the callee:

```beamtalk
Actor subclass: Ticker
  state: n = 0
  bump => self.n := self.n + 1
  startLate => Timer after: 100 do: [self.n := self.n + 1]   // warning; raises in the Timer
  startPeek => Timer after: 100 do: [self.n printNl]         // warning; prints the value at creation
  startWell => Timer after: 100 do: [self bump!]
```

An error that escapes a message discards that message's writes, and `on:do:`
continues with the fields as they were before its protected block, as today.
An `ensure:` cleanup sees every write made before it ran when the protected
block exits normally or with `^`. After an error it sees at least the fields
as they were when the `ensure:` started plus the protected block's own
writes.

### How it works

Method bodies keep threading `State` in locals as today, so a field access in
a method costs what it costs now. The process-dictionary cell
`'$bt_actor_state'`, installed by every callback, carries the state only
across a call that may run user code: the compiler stores the current
version before such a call and loads it after. The cell is tagged with its
owner as `{Owner, Map}`: a block is at home only with the cell present *and*
owned by its own actor, so a block of the host actor running inside a nested
away `super_send/5` reads its creation-time capture instead of the other
actor's fields. A block that runs inside the
actor reads and writes the cell; one that runs elsewhere uses its
creation-time capture. A self-send to a compiled method of this actor stays
threaded with no store or load, and methods keep answering
`{reply, R, NewState}`. Any other path through `dispatch/4` (native methods,
extension funs, inherited runtime methods, `doesNotUnderstand:`) goes through
one runtime helper, `beamtalk_actor_state:call_foreign/3`, which stores the
cell and returns the native code's own answer. A `ThreadedIr` node,
`SyncedCall`, emits each store and load, and a `VerifyError`,
`UnbracketedUserCodeSend`, fails the build where one is missing. Between
callbacks the `gen_server` state is the only copy.

The design note gives the invariant, the full list of calls that need a
store and load, the `dispatch/4` exits, the verifier rules, capture,
`ensure:`, `on:do:` and `^` mechanics, the per-callback contract, OTP checks,
costs and the deletion inventory.

## Prior Art

**ADR 0130.** It made the same move for class variables: one home in the
class process during an invocation, live at home, captured away, rollback at
the invocation and at `on:do:`. It deleted a threading family and four
diagnostics, flipped 21 pinned wrong answers, and cost 1.6x to 1.75x end to
end on a field-heavy loop, because it moved every access to the dictionary.
This ADR keeps the access threaded and pays only at calls.

**Pharo and Squeak.** A block reads and writes instance variables through its
receiver wherever it runs, and a write is immediate. This ADR gives that
inside the actor. It does not give it elsewhere, because a Beamtalk actor's
fields live in one process and Pharo has no such boundary.

**Erlang/OTP and Elixir.** A `gen_server` callback takes state and returns
it, and how it computes it is invisible. A `fun` or Elixir `fn` captures
values: one created in `handle_call` and run in a later callback sees the old
state, and one sent to a `Task` cannot change the server's state at all; the
idiom is to send the server a message (`GenServer.cast`, `Agent.update`). This
ADR adopts the second half, which is the away rule, and departs from the
first, because a Beamtalk block reads fields, not a snapshot of them. The
process dictionary as callback-scoped context is OTP's own practice
(`'$ancestors'`, logger metadata).

**Akka.** Closures run on the actor's thread see its fields live. A closure
over actor fields run on another thread (a `Future` callback) is a documented
race that Akka warns against and does not detect. Here it reads the capture
and a write raises.

**Pony.** Reference capabilities reject, by type, sending another actor a
closure that could write this actor's fields. Beamtalk has no capability
system; the `actor-state-abroad` lint is the static half and the runtime
error the rest.

**Register allocation.** A compiler keeps a variable in a register inside a
function, spills it to its memory home before a call that might read it and
reloads it after. The mechanism is that, with `State` as the variable and the
cell as its home.

## User Impact

What users stop hitting, all for actor fields (and, for BT-3922, class
variables in class methods):

| Today | Under this ADR |
|---|---|
| A block that assigns `self.field` and is stored, returned or passed on is a compile error (BT-3922) | Compiles. Works inside the actor; raises `actor_state_unreachable` elsewhere |
| A block that reads `self.field` and is passed to a self-send or stored is a compile error (BT-3952) | Compiles and reads the current value |
| A self-send in a block that changes state raises `stateful_block_dispatch` (BT-3936, BT-3985) | The change is kept |
| A send to a local bound to `self` is a compile error (BT-3998); `self` passed as an argument loses the write | Works like a self-send on the live state |
| An `ensure:` cleanup that reads a field after a `^` sees the entry value | Sees the writes made before it ran |
| BT-3988 (`{Result, State}` from `on:do:` in a closure), BT-3990 (stale getter in a block) | Correct |

What they can newly hit: `actor_state_unreachable` when a block writes a field
from another process, creation-time values when a block reads a field from
another process, and `deadlock_detected` or a timeout when a block running in
another process sends `self foo` while the actor waits on that process (today
that silently runs against a snapshot). The lint warns on all three where it
can see the block leave. The rules for captured locals (ADR 0133) and the
BT-3948, BT-3971, BT-3939 and BT-3981 errors are unchanged.

**Newcomer.** Blocks in an actor behave like the fields they mention: an
event handler stored in a field, a sort block, a block passed to a helper
method all see and change the actor's current state. The rules about which
block shapes may touch fields disappear. The one new error appears when a
block that writes a field leaves the actor, and its hint says to send the
actor a message instead. The lint warns before that, and also when a block
that leaves reads a field or sends to `self`.

**Smalltalk developer.** Inside the actor, blocks are Pharo blocks: they read
and write instance variables through `self`, in any position, stored or not,
and refactoring a write into a helper method or a block no longer changes
whether it survives. The difference from Pharo is that an actor is a process:
a block that leaves it sees a copy and cannot write.

**Erlang/BEAM developer.** The generated method is still a function of
`State` that returns `{reply, R, NewState}`, and the `gen_server` contract is
exact. The process dictionary holds the state only while a callback runs and
only across calls into code that may run a block. A native actor method or
extension fun that runs user blocks must read the block's writes from the cell
before answering. `call_foreign/3` checks it in every build: if the fun
answers the exact state it was given while the cell changed (a stored block it
ran wrote a field, as for `perform: #ifNotNil: withArguments: #(b)`), the answer
carries the cell's state; if it answered a different state and the cell changed
to something else, it raises `actor_state_conflict` instead of losing either
set of writes. A native that runs no blocks needs no change. A block reading fields in a later
message is a departure from `fun` capture semantics.

**Production operator.** `sys:get_state/1`, `sys:replace_state/2`, hot code
change and supervision behave as today, because the cell exists only inside a
callback. A send that may run user code pays about 10 ns. Generated actor
code changes, so the module ABI marker is bumped and crossing the release
needs a node restart (ADR 0125).

**Tooling developer.** Several compile-time diagnostics disappear and one lint
appears, on every surface equally (`docs/development/surface-parity.md`). The
type checker is unaffected.

## Steelman Analysis

**Status quo: keep the rejections.** Best case: a compile error is better
than a runtime error, threaded state hides nothing, and the rejections are
shipped and tested. Why not: they also refuse correct code (BT-3952's check
cannot tell a callee that runs the block before a write from one that runs it
after), the list grows with block shape times send kind times position, and
silent wrong answers reached `main` anyway. Both of these are correct
programs the status quo rejects:

```beamtalk
Actor subclass: Feed
  state: seen = 0
  state: handler = nil
  filter: aBlock over: xs => xs select: aBlock
  arm => self.handler := [:e | self.seen := self.seen + 1]     // BT-3922: compile error
  newer: xs => self filter: [:e | e > self.seen] over: xs     // BT-3952: compile error
```

**A cell for every field access.** Best case: one lowering, no sync points,
one copy of the state, and the `State` family deleted. This was the first
draft. Why not: 3.7x to 6.9x on a field-heavy body in process, and up to 2x
end to end, on every actor method whether it uses a block or not.

**Bracket every self-send.** Best case: one kind of self-send, one calling
convention, and native code never sees the cell. This was the ADR as accepted
on 2026-10-20. Why not: Phase 0 measured 1.35x to 1.60x on the self-send
itself, the most common call in actor code.

**Message away (Alternative A).** Away from home, a field read or write
becomes a synchronous message to the actor. Best case: one rule with no
snapshot to explain, and it is what a Pharo block does. Why not: the common
callback is synchronous. An actor passes a block to another actor and waits;
a field access from that block calls back into the waiting actor and
deadlocks. Snapshot reads are right in that case, because the actor cannot
change while it waits.

**Raise on any field access away.** Best case: no stale value can ever
surprise anyone. Why not: it breaks read-only callbacks, such as a block
handed to a worker that only reads a configuration field. ADR 0130 rejected
it as its Alternative R. The lint's warning on away reads gives most of the
loudness without breaking working code.

The tension is between Erlang developers, who expect a stored block to keep
the values it closed over, and Smalltalk developers, who expect it to read
the fields. The ADR sides with Smalltalk inside the actor and with Erlang
outside it.

## Alternatives Considered

**Reconcile in `call_foreign/3`.** Compare the entry state, the cell and the
native's answer, take whichever changed, and raise `actor_state_conflict` if
both did, so native code can ignore the cell. Rejected: a native that retries
a failed block gets the failed attempt's writes back, and a native that calls
a compiled method of the same actor raises a false conflict.

**A per-class switch.** Thread when no block in the class touches `self`,
use the cell otherwise. Rejected: two lowering modes, and whether a class
qualifies changes with its superclasses, traits and methods added at run
time (ADR 0066, ADR 0082, ADR 0084).

**Publish on write (#4447).** Two copies kept in step at each write and each
discard. Rejected: the discards are open-ended and nothing checks them. This
design moves the copy at calls, and the verifier checks every call.

**Thread `State` through closures.** Make every actor block a Tier 2 block
that takes and returns `State` (ADR 0132 applied to fields). Rejected for the
reasons ADR 0133 superseded ADR 0132, and it does nothing for blocks that run
in another process.

## Consequences

### Positive
- Blocks in an actor read and write its current fields inside the actor, like
  class variables under ADR 0130. One rule covers both.
- The closure-specific rejections and guards of BT-3922, BT-3936, BT-3952,
  BT-3985 and BT-3998 are deleted, BT-3988 and BT-3990 close, and aliased
  self-sends answer correctly.
- Field access inside method bodies and self-sends cost what they cost today.
- A missed sync point fails the build.

### Negative
- Closed (BT-4121): a loop that keeps its variables in parameters and holds
  no whole `State` brackets the slow path of a guarded operator (`acc + m`,
  where `m` is a user object with its own `+`) without threading `State`: the
  loop-entry `State` is stored before the loop, field reads inside the loop
  and the exit packs load from the cell, and a nested stateless loop hands
  its need to the enclosing bracket. Loops whose operators are all statically
  numeric (gate 3) emit no guard and no bracket. A hybrid body with a guarded
  operator is no longer hybrid: it falls back to `StateAcc`, and a new
  `VerifyError`, `UnbracketedGuardedOperator`, fails the build if any guarded
  operator still escapes the bracket. See the design note §4b.
- Every send that may run user code pays a store and a load, about 10 ns
  (10.4 ns with 18 schedulers, just over the gate).
- The protocol has two homes: compiled code and `call_foreign/3`. A native
  method or extension fun that runs a block must read the block's writes from
  the cell; `call_foreign/3` adopts the cell's state when the answer is the
  entry state and raises `actor_state_conflict` when both changed.
- Some errors move from compile time to run time. The lint sees only blocks
  passed directly to `Timer`, another actor or a non-direct class method.
- A synchronous self-send from a block running in another process that is
  serving this actor's call now raises `deadlock_detected` or times out,
  where today it silently runs against a snapshot.
- `self foo!` has been a real cast since BT-3998 (#4463), so an error in
  `foo` now crashes the actor under ADR 0134, where before it ran inline and
  the caller could catch it.
- A stored block reads the fields' current values in a later message, which
  Erlang developers will not expect from a closure.
- Generated actor code changes, so the module ABI marker is bumped and
  crossing the release needs a node restart (ADR 0125).
- The `State` family and the `{reply, R, State}` convention stay, so this
  deletes less codegen than the per-access cell would.
- Each `ensure:` entry, and each field write inside its protected body, adds
  a `put`.

### Neutral
- Locals, value types and class variables are unchanged, including BT-3922's
  rejection for value-type fields.
- Methods, native actor methods and extension funs keep
  `{reply, R, NewState}`.
- `self foo!` stays a cast to the actor's own mailbox.

## Implementation

Detail per phase is in the design note.

- **Phase 0 (S, done: BT-4007, #4478).** Prototype, gates, oracle and OTP
  checks.
- **Phase 1 (M, done: BT-4008, #4472).** `beamtalk_actor_state` and the
  callback wrappers.
- **Phase 2a (L, BT-4009).** `call_foreign/3`, `SyncedCall` and the verifier,
  driven to zero misses over every corpus before 2b starts.
- **Phase 2b (L, done: BT-4009).** Closures on the cell, `ensure:`/`on:do:`/`^`
  stores, `no_user_code` marks, the native audit, and a generated BEAM-run corpus
  as the exit gate. The synced-calls flag is gone. The module ABI marker is
  `class_var_abi` (the one marker the runtime gates on), bumped from 1 to 2.
  The exit gate is `scripts/gen-actor-closure-matrix.py`: 260 at-home cells
  (block shape x send kind x position, including `^`, stored and passed
  blocks) and 6 away cells, each with an expected value from the generator's own
  model, all passing on a BEAM. Findings the corpus forced, each fixed in the
  node that owns it: a read-only `[...] value` that was left unbracketed;
  an `on:do:`/`ensure:` in operand position that leaked its `{Value, State}`
  pair (it is now a producer with an empty local set); and a `super` send in a
  block or loop body, which now runs against the cell, bracketed by the call that
  runs it. Review then found one more bleed: while an away `super` runs, the
  superclass stores another actor's state in the host's cell, and a host
  block running inside the call read it as its own. The cell now carries its
  owner (`{Owner, Map}` in `beamtalk_actor_state`), `at_home/1` checks
  ownership, every store preserves it, and the corpus covers the shape (an
  away super running one of the host's blocks). The shapes the surviving
  validator rows still reject are not in the
  corpus; those rows go in Phase 3.
- **Phase 3 (M).** Delete the rejections and guards; update the language
  guide.
- **Phase 4 (S).** The `actor-state-abroad` lint and the amendment notes in
  ADR 0133 and ADR 0041.

#4452, #4455 and #4463 landed as stopgaps because each removed a silent wrong
answer. No new actor-state rejection should be added before Phase 3.

## Migration Path

Compiled actors need recompiling and a node restart across the ABI change.
Source that compiles today keeps compiling with the same meaning, except that
an aliased self-send now sees and keeps the live state. Shapes rejected today
compile. A block that writes a field from another process raises at run time;
send the actor a message from it instead. A synchronous `self foo` from a
block run in another process that is serving this actor's call raises
`deadlock_detected` or times out; use `self foo!`. The lint flags both.
Native actor methods and extension funs that run user blocks need to read the
block's writes from the cell before answering.

## Resolved Questions

1. Decided 2026-10-20: a self-send from a block running away from home sends
   a message to the actor, as in Erlang. A timer callback such as
   `Timer every: 1000 do: [self tick]` then works; today it runs `tick` in the
   Timer's process and drops its writes.
2. Decided 2026-10-20: ADR 0133's rejection of an escaping block that assigns
   a class variable is lifted with the field half. ADR 0130's runtime already
   handles it, and fields and class variables follow one rule.
3. Decided 2026-10-20: every send that may run user code is bracketed,
   including sends to other actors. The compiler does not try to prove a
   receiver is not `self`; revisit only if profiling shows it matters.
4. Decided 2026-10-21 after Phase 0: a self-send to a compiled method of this
   actor keeps explicit `State` threading whatever its arguments, and methods
   keep `{reply, R, State}`. Anything else in `dispatch/4` goes through
   `call_foreign/3`, which returns the native code's own answer.
5. Decided 2026-10-21 after the adversarial review: away from home a block
   keeps snapshot semantics, rather than messaging the actor (deadlocks the
   common synchronous callback) or raising on every access (breaks read-only
   callbacks). The lint warns on away reads and self-sends as well as writes.

## References

- Design note: `docs/development/actor-state-cell.md`
- Related issues: BT-3996, BT-4006, BT-4007, BT-4008, BT-4009, BT-4010,
  BT-4020, BT-3950, BT-3922, BT-3936, BT-3948, BT-3952, BT-3971, BT-3981,
  BT-3985, BT-3988, BT-3990, BT-3998, BT-3910, BT-3993
- Related PRs: #4447 (closed), #4448, #4452, #4454, #4455, #4463, #4472, #4478
- Related ADRs: 0130, 0133, 0041, 0118, 0111, 0131, 0120, 0125, 0129, 0134
- Code: `runtime/apps/beamtalk_runtime/src/beamtalk_actor_state.erl`,
  `runtime/apps/beamtalk_runtime/src/beamtalk_actor.erl`,
  `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen.rs`
- Documentation: `docs/beamtalk-principles.md` §4, §7, §9;
  `docs/beamtalk-language-features.md`
