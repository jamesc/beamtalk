# ADR 0137: Actor Closures See the Actor's Live State

## Status
Accepted (2026-10-20); Proposed 2026-10-20; Amended 2026-10-21 (self-send
exception after Phase 0, BT-4007)

Amends ADR 0133 (an escaping block may assign `self.field`) and ADR 0041 (an
actor's closures no longer capture `State`). ADR 0130 is the precedent and is
unchanged. ADR 0131, ADR 0118 and the locals half of ADR 0133 are unchanged.
ADR 0111's `ThreadedIr` gains one node and two `VerifyError`s.

Tracking: BT-3996. Epic: BT-4006. Decision gate: BT-3950. Phase 0 (BT-4007,
#4478) and Phase 1 (BT-4008, #4472) are done.

## Context

### Problem statement

An actor method compiles to a function of `State` that returns a new `State`.
The compiler threads versions (`State`, `State1`, …) through every construct
it lowers in place. A block it does not lower in place is a Tier 1 `fun`
(ADR 0041). The `fun` captures whichever `StateN` was current when it was
created and answers one value, so it cannot hand a new `State` back.

ADR 0133 Phase 3 has been meeting the consequences one shape at a time, each
answered with a compile-time rejection or a runtime guard that also rejects
some correct code:

| Issue | Shape | Response |
|---|---|---|
| BT-3922 (#4426) | Block assigns `self.field` and escapes | Compile error (`WriteKind::ObjectState`) |
| BT-3936 (#4450) | Self-send in a closure changes state, change lost | Runtime `stateful_block_dispatch` (`closure_self_dispatch_state_guard`) |
| BT-3948 (#4444) | Inline-block local in an actor loop body, unbound `StateAcc` | `RESIDUAL_DENY_SET` `InlineBlockLocalInLoop` |
| BT-3971 (#4442) | Tier 2 body ending in `[...] value` answers `{Value, State}` | `CodeGenError::UnsupportedFeature` |
| BT-3952 (#4452) | Block reads `self.field`, is stored or passed to a self-send, reads stale | Compile error for every such read |
| BT-3981 (#4448) | Evaluated `[...] value` in an actor leaks `{Value, State}` | `RESIDUAL_DENY_SET` `EvaluatedBlockValue` |
| BT-3985 (#4455) | Binary and `super` sends to `self` drop the callee's change | Extends the BT-3936 guard |
| BT-3998 (#4463) | Send to a local bound to `self` loses its change | Compile error (`check_actor_self_alias_send`) |
| BT-3988 | `on:do:` in a self-sending closure answers `{Result, State}` | Open |
| BT-3990 | Getter self-send in a block passed to a self-send reads creation-time state | Open |
| BT-3910, BT-3993 | `ensure:` cleanup sees entry state after `^` | Compile error for locals (`report_nlr_out_of_ensure`); fields read stale |

PR #4447 tried a narrow fix. It kept `State` threaded and published it to the
process dictionary on every field write so that closures could read it. It was
closed on 2026-10-20 because the publish had to stay complete forever, and
`fieldAt:` was already a gap. Its review found the general problem: each
lowering that discards a callee's `NewState` (a method-level self-cast, an
aliased self-send, an error arm) left the published copy ahead of the real
one.

The maintainer's direction: "I'm all for simplicity of model — if it fits OTP
usage, then let's go." OTP fit decides this ADR.

### Current state

A throwaway probe package, built with a debug build of `main` from
2026-10-20 and run with `beamtalk test`, answered:

| Method body | Expected | `main`, 2026-10-20 | `main` since BT-3998 |
|---|---|---|---|
| `self.x := 5. other := self. other x` | 5 | 0 | compile error |
| `other := self. other setX: 7. self.x` | 7 | 0 | compile error |
| `self bump!. self.x`, then `p x` from outside | 1 | 0 | 1 |
| `b := [self.x]. self.x := 9. b value` | 9 | 0 | 9 |
| `self apply: [self bump]. self.x` | 1 | `stateful_block_dispatch` | `stateful_block_dispatch` |

The first three had one cause. Every generated callback stashes the entry
`State` in `'$bt_actor_state'` (`pdict_stash_preamble` in
`gen_server/callbacks.rs`) so that an aliased self-send can take
`sync_send/3`'s Layer 1 path through `self_dispatch/2` instead of
deadlocking. The method's own writes never reach that stash, and what
`self_dispatch/2` writes into it is never read back. BT-3998 made `self foo!` a
real cast and made a state-changing send to a local bound to `self` a compile
error. `self` passed as an argument, or read back out of a collection, can
still lose a write.

An error that escapes a method returns the pre-call `State`. An actor's
`on:do:` continues with the `State` from before the protected block. Phase 1
(BT-4008) moved the runtime callback wrappers in `beamtalk_actor.erl` onto
`beamtalk_actor_state`, which installs the cell, adapts the method's
`{reply, R, NewState}` into it and takes it back on exit. Nothing hibernates,
and nothing implements `format_status/1`.

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
to `Timer`, to a send whose receiver is statically another actor, or to a
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

The rest of this section is how the compiler and runtime deliver the rule.
None of it changes what a program means.

### 1. Thread inside a method, sync at calls that may run user code

Method bodies keep threading `State` in locals as today. A field read is
`maps:get/2` on the current version and a field write binds the next one,
with no process-dictionary access.

The cell `'$bt_actor_state'`, which every callback already installs, carries
the state across any call that might run one of this actor's blocks. Before
such a call the compiler emits one store of the current version to the cell;
after it returns, one load into the next version:

```erlang
let _ = call 'erlang':'put'('$bt_actor_state', State3) in
let R = call 'beamtalk_message_dispatch':'send'(Recv, 'do:', [Block]) in
let State4 = call 'erlang':'get'('$bt_actor_state') in
...
```

A call needs the bracket when it may run user code in this process: a
`value`-family call on a block that is not inlined, a send with a block
argument, an aliased self-send, a send to an in-process object (a value type,
a collection, any non-actor object), an Erlang FFI call, and any primitive or
runtime helper that receives a block or calls a user method. The last group
includes the `on:do:` handler and `ensure:` cleanup runners, `Result tryDo:`
and `protect/1`, the `beamtalk_collection` fold helpers, sort comparators,
`Timer`, and collection primitives such as `includes:` that send `=` to
elements. A send to another actor is bracketed too, because the compiler can
rarely prove the receiver is not `self`.

Three kinds of call need no bracket. A construct the compiler lowers in place
(`ifTrue:`, an inlined `do:`, `whileTrue:`, …) is not a call. An intrinsic or
primitive that can never dispatch to user code needs none: integer
arithmetic and `size` on a list qualify, and `,` qualifies only where every
argument is known to be a string, because it calls `printString` on anything
else. For a binary operator on a receiver of unknown type, `operators.rs`
already emits a guard whose `'true'` branch is the bare BIF and whose
`'false'` branch is the message send; the bracket goes in the `'false'` branch
only, so `count + 1` on an integer pays nothing. The third is a self-send
(§2).

The invariant is: at every bracketed call the cell equals the current
threaded `State`, and the caller reloads after it. Between callbacks the
`gen_server` state is the only copy, and the cell does not exist.

### 2. Self-sends

A self-send whose selector statically resolves to a compiled Beamtalk
method of this actor class, or of a compiled actor superclass, is not
bracketed. It passes the current `State` as an argument and rebinds from the
callee's answer, as today. This covers unary, binary, keyword, `super` and
cascade sends to `self`, in sealed and open classes, with any arguments,
blocks included. A self-send the compiler cannot resolve that way is
bracketed at the call site like any other send. A block passed into a
threaded self-send runs only when the callee evaluates it, and the callee
brackets that evaluation with its own current
`State`, which is the caller's plus whatever the callee wrote before. In
`fire => self.cb value`, a threaded `self fire` passes the caller's `State`;
`fire` brackets `self.cb value`; the block's writes are loaded into `fire`'s
next version; `fire` answers that version. `self twice: [self add: 5]` is the
same case with the block as an argument. Every block runs inside some
bracket, emitted by the method or helper that holds the current version.
Phase 0 measured a bracketed self-send at 1.35x to 1.60x (see Cost), which is
the reason for the exception.

Methods keep today's calling convention. A compiled method is
`dispatch(Selector, Args, Self, State)` and answers `{reply, R, NewState}`.
The callback wrapper installs the `gen_server` state in the cell so that
closures can find it, runs the method, and returns the state the method
answered. In outline:

```erlang
dispatch_call(Selector, Args, From, State) ->
    Old = beamtalk_actor_state:install(State),
    try Module:dispatch(Selector, Args, Self, State) of
        {reply, Result, NewState} -> {reply, encode(From, Result), NewState}
    catch
        Class:Reason:ST -> {reply, encode(From, {error, ...}), State}
    after
        beamtalk_actor_state:uninstall(Old)
    end.
```

Not every arm of `dispatch/4` leads to a compiled Beamtalk method, even for
a selector the compiler resolved. The other arms each run Erlang code that does not bracket and may run a block, so each goes
through one runtime helper, `beamtalk_actor_state:call_foreign/3`:

- a native method clause (`native:` and `self delegate`, ADR 0056);
- an ADR 0066 instance-side extension fun, found by the default arm's
  `beamtalk_extensions:lookup` (also when installed at run time on a
  selector the compiler saw as compiled);
- the default arm's hierarchy walk, `beamtalk_dispatch:super/5`, which
  reaches inherited runtime methods on `Object` and `ProtoObject`, the
  reflective `fieldAt:put:` among them;
- the `doesNotUnderstand:args:` fallback.

Phase 2a adds a test that drives every arm of a generated `dispatch/4` and
checks that each one either is a compiled clause or calls `call_foreign/3`.

`call_foreign/3` stores `State` to the cell, invokes the fun with it, and
returns the fun's own `{reply, R, NewState}`. A native method or extension
fun that runs a block must read the block's writes back from the cell
(`beamtalk_actor_state:get/2`) before it answers; one that runs no block, or
never writes state, needs no change. Phase 2b audits the few that run blocks.
In debug builds `call_foreign/3` asserts that if the cell changed during the
call, the returned `NewState` reflects the change, so a native that forgets
fails its tests instead of dropping a write.

The helper does not reconcile the cell with the answer; Alternatives
Considered says why.

An aliased self-send (`other := self. other x`, `self` passed as an argument)
is an ordinary send and is bracketed. `sync_send/3` sees its own pid and runs
the method through `self_dispatch/2` on the cell, which is current, and stores
the answer back, so it starts from the live state and its writes persist.
`self foo!` is a cast to the actor's own pid and runs after the current
message (BT-3998); this ADR keeps that.

### 3. Closures at home and away

A closure has no threaded `State`. Its field reads and writes go through
`beamtalk_actor_state` (`get`, `put`, and the reflective `fieldAt:`,
`fieldAt:put:`, `clearField:`, `hasField:`), as `beamtalk_class_vars` does for
class variables. A self-send in a closure reads the cell, calls the method
with it and stores the answer back, as `self_dispatch/2` does. A closure only
runs at home inside a bracketed call, so the cell is current when it starts
and the method that emitted the bracket loads its writes when it returns.

A block is at home when a callback of its own actor is running in the current
process, tested as `Self#beamtalk_object.pid =:= self()` with the cell
present. Away, a field read answers the block's capture, a write raises
`actor_state_unreachable`, and a self-send is a message to the actor's pid.
If the actor is blocked in the call that carried the block away,
`check_call_stack/2` raises `deadlock_detected` or the call times out, as for
any A→B→A cycle. A block passed to a class method is at home only when the
method is direct-called (ADR 0129 Phase 0b; see The rule), because only then
does it run in the actor's process. An actor restarted by its supervisor is a new process, so a
block from the old process is not at home in the new one; ADR 0130 could not
use pid equality for classes because a class keeps its identity across
restarts.

A block literal that reads a field binds a capture when it is created. In a
method body the capture is the current threaded `State` version, which costs
nothing. In a closure it is `beamtalk_actor_state:capture(Self, Outer)`,
which answers the cell at home (current, since the closure runs inside a
bracket) and the enclosing block's capture otherwise. Reading the cell from a
method body would be wrong: the method's writes since its last bracket are
only in its locals; Phase 0's two stale-read regressions were reads of the
cell from code that held a newer version in its locals. The capture is
consulted only away from home.

A Tier 2 block (ADR 0041) can write both a captured local and a field. The
local comes back through its `StateAcc` return channel as today; the field
goes to the cell. After the call the caller takes locals from the returned
`StateAccN` and fields from the cell load, never fields from `StateAccN`. A
new `VerifyError`, `FieldReadFromStateAcc`, fails if a lowering rebinds the
actor `State` from a Tier 2 block's `StateAcc`.

The later-message case differs from an Erlang `fun`, which keeps the values
it closed over. A Beamtalk block reads fields through its actor at home, as a
Pharo block reads instance variables through its receiver. A stored sort
block or event handler that kept a state from an earlier message, and could
not write, would put us back to rejecting shapes. ADR 0130 §5 does the same
for a class-side block run from a later invocation of its class.

### 4. One node, one check

The bracket is emitted in one place, the send emitter, as one `ThreadedIr`
node, `SyncedCall`, that renders the store, the call and the load together,
as `OnDoCatch` does for its snapshot and restore. A new `VerifyError`,
`UnbracketedUserCodeSend`, fails when a call that may run user code is not a
`SyncedCall`, so a missed sync point is a build failure and never a stale
read. `just verify-threaded-ir` runs it over the stdlib and bootstrap corpus.

The default is "may run user code". A direct self-send (receiver `self` or
`super`) whose selector resolves statically to a compiled method is accepted
whatever its arguments, because every non-compiled arm of `dispatch/4` goes
through `call_foreign/3` (§2). Any other call is exempt only if the
emitter built it from one of two existing sources. Inlined constructs come
from `beamtalk-core/src/state_threading_selectors.rs` and `WellKnownSelector`
(`beamtalk-core/src/ast/well_known.rs`) and never produce a call node. Exempt
intrinsics come from the one table codegen already uses to emit them: the
`'true'` branch of the operator guard in `operators.rs`, and the entries of
`PrimitiveBindingTable` (`primitive_bindings.rs`, built from the stdlib's
`@primitive` pragmas) that carry a new `no_user_code` mark. The mark means
the primitive can never dispatch to user code for any argument, and it is set
in the stdlib source next to the pragma. Every parameter of a marked binding must be a primitive type or a sealed value
type; `build-stdlib` rejects any other parameter type, a block or an open
class included, since either could dispatch to user code. A
primitive that is pure only for some argument types (`,` for strings) stays
unmarked and is exempt only where the type checker shows every argument is a
`String`. The emitter and the verifier read the same mark, so there is no
second list to keep in step. An unmarked primitive is bracketed, which costs
about 10 ns and is never wrong.

#4447 needed an un-publish wherever a state change was discarded. Here the
method body is the source of truth between sync points and the cell is the
source of truth across them. Every crossing is either a `SyncedCall` or a
threaded self-send that passes `State` as an argument, and the verifier
checks both.

### 5. Rollback and `ensure:`

The rule is ADR 0130 §4's. An error that escapes a callback discards its
writes: the wrapper returns the pre-call state, as today. `on:do:` already
continues with the `State` from before the protected block; its catch arm now
also stores that version to the cell before the handler runs, so a closure or
callee in the handler sees it. The `OnDoCatch` node owns the store, and the
verifier's `CatchWithoutClassVarRestore` becomes `CatchWithoutStateRestore`.
`Result tryDo:` and the other runtime catchers do the same through
`protect/1`. A `^` stores the current version to the cell before the
`$bt_nlr` throw. The actor NLR 4-tuple `{'$bt_nlr', Tok, Val, State}` keeps its
`State` slot, which is how the method returns that version.

Entering an `ensure:` stores the current `State` to the cell, and inside the
protected body every field write also stores (write-through). The protected
body still threads `State`; the cleanup reads the cell. On a normal exit and
on a `^` the cleanup sees every write the method made. After an error it is
guaranteed to see the state at `ensure:` entry plus the protected body's own
lexical writes made before the error. Writes made by a callee that then
raised are not guaranteed: the callee may have held them only in its locals.
Making every method store on its exception path would need a `try` per
method, and this ADR does not add one. The next catch boundary or the
callback discards those writes anyway. This fixes the actor half of BT-3910
and BT-3993.

Each `gen_server` callback keeps its own contract. A callback returns the
state its method answered only when it exits normally; on an exception it
returns, or stops with, the state it was called with.

| Callback | User code | Cell | Normal exit | Exception |
|---|---|---|---|---|
| `init/1` | none; `initialize` runs in `handle_continue` (ADR 0078) | not installed | `{ok, State, {continue, initialize}}` | unchanged |
| `handle_continue(initialize, _)` | the `initialize` chain, parent first | installed around the chain, snapshotted before each step | `{noreply, S}` with `S` the last step's answer, after the field check | `{stop, Reason, S}` with `S` the state before the failing step, as today; `terminate:` sees `S` |
| `handle_call`, `handle_cast`, `handle_info`, BT-3968 continuations | one method | around the dispatch | `{reply, R, NewState}` / `{noreply, NewState}` from the method's answer | pre-call state; `handle_call` replies with the error, no-reply handlers follow ADR 0134 |
| `terminate/2` | `terminate:` if defined | around the dispatch | `ok`, cell erased | caught and ignored, as today; cell erased |
| `code_change/3` | none; `beamtalk_hot_reload:code_change/3` | not installed | `{ok, NewState}` | unchanged |
| `format_status/1` | not implemented | not installed | would format the `gen_server` state | n/a |

### 6. Unchanged

Class variables, value-type `Self` threading (ADR 0120) and local-variable
threading stay as they are. Actor methods keep their calling convention
(`dispatch/4` answering `{reply, R, State}`) and their frame mode, and locals
keep riding in the actor map as `__local__` keys (`strip_local_temps/1`).
ADR 0133's rule for captured locals stays, and so does BT-3922's rejection of
an escaping block that writes a value-type field; only the actor-field and
class-variable halves are lifted. The wire protocol,
`sync_send`/`cast_send`, supervision and `stopWith:`/`continueWith:` are
unchanged.

## OTP Fit

A throwaway `gen_server` (OTP 28) that installs the cell on entry, mutates it
through closures and returns it on exit was used to check the following;
Phase 0 turned the checks into EUnit tests against `beamtalk_actor_state`.

System messages are handled only between callbacks, so `sys:get_state/1` and
`sys:replace_state/2` see the returned state. A `get_state` sent during a
200 ms callback waited for it and got the committed value. `code_change/3`
runs while the process is suspended between callbacks; there was no cell and
it received the `gen_server` state. A callback that crashed the process took
the cell with it, and the restarted process had `init`'s state and no cell. A
callback whose error is caught leaves no cell because `after` erases it. A
`{reply, R, S, hibernate}` happens after the cell is erased; the hibernated
process (`gen_server:loop_hibernate/4`) woke with its state and no cell.
Supervisors see processes and exit reasons, not state. A block sent to another
node is away. Nested callbacks save and restore the cell through
`restore_dispatch_pdict/1`, as they do today.

Erlang style discourages the process dictionary for data that should be
passed as arguments. Here a method body still passes `State` as a value; the
cell carries it only across calls into code that cannot take it as an
argument, and the `gen_server` contract of state in, state out holds exactly.
OTP keeps process-scoped context in the dictionary (`'$ancestors'`,
`'$initial_call'`, logger metadata), and so does Beamtalk (`'$bt_call_stack'`,
ADR 0130's class variables).

## Cost

Phase 0 (BT-4007, #4478) measured a prototype behind a codegen flag, in the VM
on the generated Core Erlang, with `runtime/perf/bench_adr0137_gates.escript`
(added by #4478): medians over 101 interleaved batches on a quiet 18-core
machine (load average 1.5 to 2.3), with `+S 1` and with 18 schedulers.

| Measure | `+S 1` | 18 schedulers |
|---|---|---|
| Field loop, 10 reads + 3 writes, intrinsics only (identical code) | 1.009x | 1.010x |
| Store + load pair, isolated | 7.8 ns | 9.0 ns |
| Bracketed in-process send, added per send | +9.9 ns | +10.4 ns |
| Self-send (~31 ns), bracketed, open and sealed, top level and in a `do:` arm | 1.35x–1.38x | 1.49x–1.60x |
| Self-send, threaded under §2 (identical code) | 0.99x–1.02x | |

An intrinsic-only field loop and a self-send compile to the same code as
today, so their cost is unchanged by construction. A send that may run user
code pays one store and one load, about 10 ns. Entering an `ensure:` and each
field write inside its protected body add one `put`, which was not measured
separately. End-to-end `gen_server:call` runs vary by 1% to 2% between rounds
on identical builds, about 50 ns over ten sends, so they cannot resolve a
10 ns difference. The in-VM figures are the ones to use.

Gate 2 is accepted at 10.4 ns. The sends it adds to are sends to value types,
collections and other objects that may run user code, each at least a
function call plus a dispatch, and the alternative designs either rejected
those programs or cost more on every field access. Phase 2b may skip the
store when no field write happened since the last sync, which removes the
store from read-only stretches; it is an optimisation, not a requirement.

The gates, as Phase 0 left them:

1. A method looping over ten field reads and three writes with intrinsics
   only: at most 1.05x today. Passed.
2. A method making ten in-process sends that each update a field: at most
   10 ns more per send than today. Measured 9.9 ns with `+S 1` and 10.4 ns
   with 18 schedulers, just over the line.
3. Open and sealed actor self-sends to compiled methods, top level and in a
   `do:` arm: the generated call site is identical to today's. Bracketing them
   failed this gate, which is why §2 threads them. Phase 0 measured
   self-sends without block arguments only; with one, the call site is the
   same code by construction.
4. Zero `UnbracketedUserCodeSend` over the full corpus. This is the exit
   criterion of Phase 2a. The prototype left 187 of 511 may-run-user-code
   sends in BUnit actor methods unbracketed (loop bodies, branch arms,
   mutation-threaded statements) and produced two stale reads there.

## What It Deletes

Classified against `main` at `d887c155a` (2026-10-21).

| Deleted | Where |
|---|---|
| `closure_self_dispatch_state_guard`, `CLOSURE_SELF_SEND_STATE_CHANGE_HINT`, `closure_drops_state` (BT-3936) | `dispatch_codegen.rs`, `blocks.rs`, `generator/body_state.rs` |
| `in_state_dropping_closure` on the `super` path, and the binary-selector state drop (BT-3985) | `dispatch_codegen.rs`, `generate_super_send` |
| The actor-field and class-variable halves of `WriteKind::ObjectState`, `outer_state_writes`, `OuterWriteWalker::state_write`, `is_captured_self_field_write` and `Walker::escape_write_of` (BT-3922); the value-type half stays | `block_facts.rs`, `validators/local_threading.rs` |
| `outer_state_reads`, `outer_self_send`, `may_change_actor_state`, `report_stale_read`, `report_stale_self_send`, `Escape::{LaterUse, ActorSelfCall, SelfCascade, CollectionResult}` (BT-3952) | `block_facts.rs`, `validators/local_threading.rs` |
| `check_actor_self_alias_send` (BT-3998) | `validators/class_validators.rs` |
| "cannot assign to field '{name}' inside a stored closure" | `block_analyzer.rs` |
| The actor-state `stateful_block_dispatch` for a callable forwarded to a list op | `control_flow/list_ops/mod.rs` |
| `detect_tier2_self_send`, `scan_class_for_tier2_blocks`, `tier2_method_info`, `tier2_block_params` (unreachable since BT-3922) | `block_facts.rs`, codegen |
| The closure use of `generate_discarding_self_dispatch` | `dispatch_codegen.rs` |

The `testSelfSendStateChangeThrough*Raises` tests (BT-3936) in
`stdlib/test/actor_self_send_hom_test.bt` flip to the correct answers. The
BT-3922 actor, BT-3952 and BT-3998 `@load-error` cases in
`tests/repl-protocol/cases/adr0133_escaping_block.btscript` become loads.
BT-3988 and BT-3990 close.

The oracle (BT-3950) settles the rows that mix actor state with locals. Phase 0
ran it with the flag on and off and every cell agreed, because the matrix
probes local threading and the prototype did not change it, so they are
decided after Phase 2b: `RESIDUAL_DENY_SET` `EvaluatedBlockValue` (BT-3981,
with its PIN-BUGs in `bt3971test.bt` and `bt3939tier2inlined_test.bt`) and
`GroupByValue`, and the roughly 15 `DENY_SET` rows marked `stateful`, 11 of
them for actors.

These stay because method bodies and plain self-sends still thread: the
`State` family, the actor frame mode, the `{reply, R, State}` method
convention with the state arms of `unwrap_dispatch_result/1`, the `State` slot
of the actor NLR tuple, ADR 0118's self-send preludes, `strip_local_temps/1`,
and `generate_discarding_self_dispatch` in method bodies (ADR 0118 owns it).
`RESIDUAL_DENY_SET` `InlineBlockLocalInLoop` (BT-3948) stays; it is about the
actor loop frame. The locals rules stay: the `WriteKind::Local` half of
`Tier2BlockNoReturnChannel` with `Escape::Ffi` (BT-3917), `Passed` and
`Cascade`; `ALLOW_SET`, the other `DENY_SET` rows, `RIGHT_STATEMENTS` and
`MatchArmWrite`; `report_nlr_out_of_ensure` and `nlr_dropped_local` (BT-3910,
BT-3993); `reject_last_statement_block_value` (BT-3971) and
`reject_loop_rebind_of_body_local` (BT-3939); the Tier 2
`stateful_block_dispatch` sites in `intrinsics.rs`, `while_loops.rs` and
`exception_handling.rs`; the locals half of
`beamtalk_collection:refuse_writing_fold/3`; `ClassStateAbroad`; and the
`Local` and `SelfVt` families.

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
reloads it after. §1 is that, with `State` as the variable and the cell as
its home.

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
from another process, the `actor-state-abroad` warning that predicts it, and
creation-time values when a block reads a field from another process. A
`self foo` from a block run elsewhere is a real message, so it can raise
`deadlock_detected` or time out if the actor is waiting on that process;
today it silently runs against a snapshot. The lint warns on all three where
it can see the block leave. Whether a block passed to a class method has left
depends on the callee: a direct-called `class sealed` method keeps it inside
the actor, any other class method takes it away. The rules for captured locals (ADR 0133) and the BT-3948,
BT-3971, BT-3939 and BT-3981 errors are unchanged.

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
before answering, and a debug-build assertion catches one that does not; one
that runs no blocks needs no change. A block reading fields in a later message is a
departure from `fun` capture semantics that Erlang developers should know.

**Production operator.** `sys:get_state/1`, `sys:replace_state/2`, hot code
change and supervision behave as today, because the cell exists only inside a
callback. A message that sends user blocks around pays about 10 ns per such
send. Generated actor code changes, so the module ABI marker is bumped and
crossing the release needs a node restart (ADR 0125).

**Tooling developer.** Several compile-time diagnostics disappear and one lint
appears, on every surface equally (`docs/development/surface-parity.md`). The
type checker is unaffected.

## Steelman Analysis

### Status quo: keep the rejections

| Cohort | Strongest argument |
|---|---|
| Newcomer | "A compile error is better than a runtime error. I find out before I run." |
| BEAM veteran | "State is threaded and visible. No process dictionary, no hidden copy." |
| Operator | "The rejections are shipped and tested; a new runtime protocol resets that." |

**Why not.** The rejections also refuse correct code (BT-3952's check cannot
tell a callee that runs the block before any write from one that runs it
after), the list
grows with block shape times send kind times position, and three silent wrong
answers reached `main` before BT-3998 caught two of them by rejection. ADR 0130
made the same call for class variables.

Two correct programs that the status quo rejects:

```beamtalk
Actor subclass: Feed
  state: seen = 0
  state: handler = nil
  filter: aBlock over: xs => xs select: aBlock
  arm => self.handler := [:e | self.seen := self.seen + 1]     // BT-3922: compile error
  newer: xs => self filter: [:e | e > self.seen] over: xs     // BT-3952: compile error
```

### A cell for every field access

| Cohort | Strongest argument |
|---|---|
| Language designer | "One lowering, no sync points to get right, and the `State` family is deleted." |
| Newcomer | "There is one copy of the state, so nothing can be stale." |

This was the first draft of this ADR: methods stop threading and every
`self.x` is a `get` and `put` on the cell. **Why not.** It costs 3.7x to 6.9x
on a field-heavy body in process, and up to 2x end to end for a call making
130 field accesses (spikes on a loaded machine), on every actor method whether
it uses a block or not.

### Bracket every self-send

| Cohort | Strongest argument |
|---|---|
| Language designer | "One kind of self-send and one calling convention. No `call_foreign/3`." |
| Erlang developer | "Native methods never need to reason about the cell." |

This was the ADR as accepted on 2026-10-20: methods take `State` from the
cell on entry and store it on exit, every self-send is a `SyncedCall`, and the
`{reply, R, State}` convention, the state arms of `unwrap_dispatch_result/1`
and the NLR `State` slot are deleted. **Why not.** Phase 0 measured 1.35x to
1.38x on a 31 ns self-send with `+S 1` and 1.49x to 1.60x with 18 schedulers,
for open and sealed actors, top level and in a `do:` arm. Plain self-sends are
the most common call in actor code, and the deletions do not pay for that.

### Message away (Alternative A)

Away from home, a field read or write becomes a synchronous message to the
actor (`self.n` is `self n`, `self.n := v` is `self n: v`), so a block reads
the live value and can write from anywhere.

| Cohort | Strongest argument |
|---|---|
| Newcomer | "One rule: a block always sees the actor's current fields. No snapshot to explain." |
| Smalltalk developer | "This is what a Pharo block does: it talks to its receiver." |
| Erlang developer | "It is just a `gen_server:call`; nothing hidden." |

**Why not.** The common callback is synchronous: an actor passes a block to
another actor's method and waits for the answer. A field access from that
block would call back into an actor that is blocked waiting, and deadlock or
time out. Snapshot reads answer correctly in that case, because the actor
cannot have changed while it waits. Writes from a `Timer` are better written
as `self bump!`, which the hint and the lint say.

### Raise on any field access away

| Cohort | Strongest argument |
|---|---|
| Erlang developer | "A block that leaves the actor touching its fields is a bug. Fail loudly." |
| Newcomer | "No stale value can ever surprise me." |

**Why not.** It is loud, but it breaks read-only callbacks: a block passed to
another actor's method while this actor waits for the answer reads a capture
that is right, and a block handed to a worker that only reads a
configuration field would raise. ADR 0130 rejected the same option as its
Alternative R. The lint now warns on away reads, which gives most of the
loudness without breaking the working cases.

### Tension points

Erlang developers will expect a stored block to keep the values it closed
over; Smalltalk developers will expect it to read the fields. The ADR sides
with Smalltalk inside the actor and with Erlang outside it. Newcomers lose
some compile-time errors in exchange for one runtime error and a stale read,
both of which the lint recovers as warnings where the block visibly leaves.
Snapshot reads were chosen over messaging away (deadlock in synchronous
callbacks) and over raising on reads (breaks read-only callbacks).

## Alternatives Considered

The status quo, a cell for every field access, bracketing every self-send,
messaging the actor away (Alternative A) and raising on any access away are
argued in Steelman Analysis.

**Reconcile in `call_foreign/3`.** Compare the entry state, the cell and the
native's answer, take whichever changed, and raise `actor_state_conflict` if
both did. It lets native code ignore the cell. It was drafted and rejected:
a native that retries a failed block gets the failed attempt's writes back,
and a native that calls a compiled method of the same actor raises a false
conflict.

**A per-class switch: thread when no block touches `self`.** Use today's
lowering for classes whose blocks never read fields or self-send, and the
cell otherwise. That is two lowering modes for actor methods. Whether a class
qualifies depends on its superclasses' methods, on its trait provisions and on
methods added later by `>>`, `compile:source:` or ClassBuilder (ADR 0066,
ADR 0082, ADR 0084), so the switch would have to be recomputed and the class
recompiled when any of those change.

**Publish on write (#4447).** Two copies, kept in step by publishing at each
write and un-publishing at each discard. The discards are open-ended, and
nothing checks them. This design moves the copy at calls instead of at
writes, and the verifier checks every call.

**Thread `State` through closures.** Make every actor block a Tier 2 block
that takes and returns `State`. This is ADR 0132 applied to fields, which
ADR 0133 superseded: a second entry per block-taking method, a runtime
resolution walk, and nothing for blocks that run in another process or after
the method returns.

## Consequences

### Positive
- Blocks in an actor read and write its current fields inside the actor, like
  class variables under ADR 0130. One rule covers both.
- The closure-specific rejections and guards of BT-3922, BT-3936, BT-3952,
  BT-3985 and BT-3998 are deleted, BT-3988 and BT-3990 close, and aliased
  self-sends answer correctly.
- Field access inside method bodies and self-sends cost what they cost today,
  except field writes in `ensure:` protected bodies.
- A missed sync point fails the build through `UnbracketedUserCodeSend`.

### Negative
- Every send that may run user code pays a store and a load, about 10 ns.
- The bracket protocol has two homes: `SyncedCall` in compiled code and
  `call_foreign/3` for native methods and extension funs.
- A native method or extension fun that runs a block must read the block's
  writes from the cell before answering. Only a debug-build assertion checks
  this.
- Some errors move from compile time to run time: a block that writes a field
  from another process raises when it runs, and the lint sees only blocks
  passed directly to `Timer`, another actor or a non-direct class method.
- A synchronous self-send from a block running in another actor's or class's
  process that is serving this actor's call now raises `deadlock_detected` or
  times out; today it silently runs against a snapshot. The lint warns on it.
- `self foo!` has been a real cast since BT-3998 (#4463), so an error in
  `foo` now crashes the actor under ADR 0134, where before it ran inline and
  the caller could catch it.
- A stored block reads the fields' current values in a later message, which
  Erlang developers will not expect from a closure.
- Generated actor code changes, so the module ABI marker is bumped. Older
  modules are refused with `abi_mismatch`, and crossing the release needs a
  node restart (ADR 0125).
- The `State` family, the actor frame mode and the `{reply, R, State}`
  convention stay, so this deletes less codegen than the per-access cell or
  bracketing every self-send would.
- Every `ensure:` entry, and every field write inside an `ensure:` protected
  body, also does a `put`. After an error, the cleanup is guaranteed only
  the state at `ensure:` entry plus the protected body's own writes.
- Away from home, a `Timer` body still reads creation-time values.

### Neutral
- Locals, value types and class variables are unchanged.
- Methods, native Erlang actor methods and ADR 0066 extension funs keep
  `{reply, R, NewState}`.
- Self-sends compile to the same call-site code as today.
- `self foo!` stays a cast to the actor's own mailbox (BT-3998).

## Implementation

**Phase 0 (S, done: BT-4007, #4478).** A prototype behind a codegen flag,
the four gates, the oracle and the OTP checks as EUnit tests. Gate 3 failed
for bracketed self-sends, which led to the self-send exception in §2. The
results are in Cost.

**Phase 1 (M, done: BT-4008, #4472).** `beamtalk_actor_state` (`install`,
`take`, `uninstall`, `installed`, `adapt`, `get`, `put`, `has`, `clear`,
`capture`, `protect`), used by every callback wrapper in `beamtalk_actor.erl`
and by `self_dispatch/2`, and the `actor_state_unreachable` error kind. It
was written for the convention before the amendment, where the callback takes
the cell as its new state (`take/0`) and `adapt/1` stores a native's
`NewState` into the cell.

**Phase 2a (L).** Align `beamtalk_actor_state`'s moduledoc and `adapt/1`
with the amended convention: the method's `{reply, R, NewState}` is the new
state. Add `call_foreign/3` with its debug-build assertion, and route every
non-compiled arm of `dispatch/4` (§2) through it, with the test that drives
every arm. Reword the `actor_state_unreachable` hint, which today says a
block "run after the method returned" is away; a block run in a later
message of its own actor is at home. The `SyncedCall` node in `threaded_ir/`
and bracket emission, behind a flag, from the send emitter in
`dispatch_codegen.rs` and the `'false'` branch of the operator guard in
`operators.rs`, with §2's self-send exception. The `UnbracketedUserCodeSend`
and `FieldReadFromStateAcc` verifiers, and an invariant test that no send
`Document` is emitted outside an IR node in actor context, so a send the
verifier cannot see fails the build too. Drive the `UnbracketedUserCodeSend`
count to zero over the stdlib, BUnit, REPL and generated corpora, fixing each
miss in the `ThreadedIr` node that owns the scope. If the count does not
converge that way, stop and report before Phase 2b.

**Phase 2b (L).** Closure field access and closure self-sends go through the
cell helpers, and field-reading block literals bind the capture as in §3.
Tier 2 blocks that write both a local and a field merge as in §3, with a
corpus case. Entering an `ensure:` stores, and field writes inside its
protected body write through. `OnDoCatch` stores its pre-region version
under `CatchWithoutStateRestore`. `^` stores before the throw. The
`no_user_code` mark goes on `PrimitiveBindingTable` entries, set from the
stdlib pragmas and checked by `build-stdlib`. Audit the native actor methods
and extension funs that run a user block, and make each read the block's
writes from the cell before answering. Optionally skip the store when no
field write happened since the last sync (Cost). The exit gate is a generated
actor-field-closure corpus (block shape × send kind × at home or away ×
position), compiled and run on a BEAM, with every case answering what the
rule in this ADR says. Remove the flag and bump the module ABI marker.

**Phase 3 (M).** Delete the table above and whatever the oracle confirms
after Phase 2b. In `docs/beamtalk-language-features.md`, replace the BT-3922
field sentence and the BT-3952 paragraph under "What Works and What Doesn't",
the BT-3998 alias paragraph under "Explicit Async Cast", and the "MUST use !"
note under "Timer Lifecycle" with the rule in this ADR.

**Phase 4 (S).** The `actor-state-abroad` lint with
`@expect actor_state_abroad`, covering field writes, field reads and
self-sends in blocks passed to `Timer`, another actor or a non-direct class
method (see The rule), the amendment notes in ADR 0133 and ADR 0041,
and `docs/agents/expanded.md` § State-Threading Codegen.

#4452, #4455 and #4463 landed as stopgaps because each removed a silent wrong
answer. No new actor-state rejection should be added before Phase 3.

## Migration Path

Compiled actors need recompiling and a node restart across the ABI change.
Source that compiles today keeps compiling with the same meaning, except that
an aliased self-send now sees and keeps the live state. Shapes rejected today
compile. A block that writes a field from another process raises at run time;
send the actor a message from it instead. A synchronous `self foo` from a
block run in another process that is serving this actor's call raises
`deadlock_detected` or times out instead of silently running against a
snapshot; use `self foo!`. The lint flags both. Native actor methods and
extension funs that run user blocks need to read the block's writes from the
cell before answering.

## Resolved Questions

Decided by the maintainer on 2026-10-20, when the ADR was accepted.

1. A self-send from a block running away from home sends a message to the
   actor, as in Erlang. It does not raise `actor_state_unreachable`. A timer
   callback such as `Timer every: 1000 do: [self tick]` then works: today it
   runs `tick` in the Timer's process and drops its writes, which is why the
   language guide says to use `self tick!`. A synchronous send while the actor
   is blocked waiting on that same process raises `deadlock_detected` or
   times out, which is loud.
2. ADR 0133's compile-time rejection of an escaping block that assigns a class
   variable (the class-variable half of BT-3922) is lifted along with the
   field half. ADR 0130's runtime already reads and writes class variables
   live at home and raises away, so the rejection only refused code that
   works. Fields and class variables follow one rule.
3. Every send that may run user code is bracketed, including sends to other
   actors. The compiler does not try to prove a receiver is not `self`. A
   bracket costs about 10 ns against a 700 ns call; revisit only if profiling
   shows it matters.
4. Decided on 2026-10-21 after Phase 0: a self-send whose target is a
   compiled Beamtalk method keeps explicit `State` threading whatever its
   arguments, and methods keep `{reply, R, State}` (§2). A self-send that
   lands anywhere else in `dispatch/4` goes through `call_foreign/3`, which
   stores the cell and returns the fun's own answer; native code that runs a
   block reads the block's writes from the cell. A three-way reconcile was
   rejected (Alternatives Considered). Bracketing every self-send cost 1.35x
   to 1.60x on the self-send itself.
5. Decided on 2026-10-21 after the adversarial review: away from home a block
   keeps snapshot semantics (reads see creation-time values, writes raise
   `actor_state_unreachable`, `self foo` is an ordinary message). Messaging
   the actor for every away access (Alternative A) deadlocks the common
   synchronous callback, and raising on every away access breaks read-only
   callbacks. The `actor-state-abroad` lint warns on away reads and away
   self-sends as well as writes, so the stale-read and deadlock cases are
   visible at compile time where the block visibly leaves.

## References

- Related issues: BT-3996, BT-4006, BT-4007, BT-4008, BT-3950, BT-3901,
  BT-3922, BT-3936, BT-3948, BT-3952, BT-3971, BT-3981, BT-3985, BT-3988,
  BT-3990, BT-3998, BT-3910, BT-3993, BT-3947, BT-3913
- Related PRs: #4447 (closed), #4448, #4452, #4454, #4455, #4463, #4472, #4478
- Related ADRs: 0130, 0133, 0041, 0118, 0111, 0131, 0120, 0125, 0126, 0129
- Code: `runtime/apps/beamtalk_runtime/src/beamtalk_actor.erl`,
  `runtime/apps/beamtalk_runtime/src/beamtalk_actor_state.erl`,
  `runtime/apps/beamtalk_runtime/src/beamtalk_class_vars.erl`,
  `crates/beamtalk-codegen/src/core_erlang/gen_server/`,
  `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen.rs`,
  `crates/beamtalk-codegen/src/core_erlang/operators.rs`,
  `crates/beamtalk-core/src/state_threading_selectors.rs`,
  `crates/beamtalk-core/src/ast/well_known.rs`
- Documentation: `docs/beamtalk-principles.md` §4, §7, §9;
  `docs/development/benchmarks.md`
