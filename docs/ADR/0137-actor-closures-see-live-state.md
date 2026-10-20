# ADR 0137: Actor Closures See the Actor's Live State

## Status
Proposed (2026-10-20)

Amends ADR 0133 (an escaping block may assign `self.field`) and ADR 0041 (an
actor's closures no longer capture `State`). ADR 0130 is the precedent and is
unchanged. ADR 0131, ADR 0118 and the locals half of ADR 0133 are unchanged.
ADR 0111's `ThreadedIr` gains one node and one `VerifyError`.

Tracking: BT-3996. Decision gate: BT-3950.

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
| BT-3952 (#4452, open) | Block reads `self.field`, is stored or passed to a self-send, reads stale | Compile error for every such read |
| BT-3981 (#4448, open) | Evaluated `[...] value` in an actor leaks `{Value, State}` | `RESIDUAL_DENY_SET` `EvaluatedBlockValue` |
| BT-3985 (#4455, open) | Binary and `super` sends to `self` drop the callee's change | Extends the BT-3936 guard |
| BT-3988 | `on:do:` in a self-sending closure answers `{Result, State}` | Open |
| BT-3990 | Getter self-send in a block passed to a self-send reads creation-time state | Open |
| BT-3910, BT-3993 | `ensure:` cleanup sees entry state after `^` | Compile error (`report_nlr_out_of_ensure`) |

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
2026-10-20 and run with `beamtalk test`, answers:

| Method body | Expected | `main` |
|---|---|---|
| `self.x := 5. other := self. other x` | 5 | 0 |
| `other := self. other setX: 7. self.x` | 7 | 0 |
| `self bump!. self.x`, then `p x` from outside | 1 | 0 |
| `b := [self.x]. self.x := 9. b value` | 9 | 0 |
| `self apply: [self bump]. self.x` | 1 | `stateful_block_dispatch` |

The first three are silent and are on no issue. They have the same cause.
Every generated callback stashes the entry `State` in `'$bt_actor_state'`
(`pdict_stash_preamble` in `gen_server/callbacks.rs`, `dispatch_call/4` in
`beamtalk_actor.erl`) so that an aliased self-send can take `sync_send/3`'s
Layer 1 path through `self_dispatch/2` instead of deadlocking. The method's own
writes never reach that stash, and what `self_dispatch/2` writes into it is
never read back. A method-level `self foo!` (`generate_self_cast_send`) runs
`foo` in-process through `safe_dispatch/3` and drops its state.

The callbacks call `safe_dispatch/3`, strip the `__local__<name>` keys that
actor-method locals ride in (`strip_local_temps/1`), restore the stash in an
`after`, and return `{reply, R, NewState}`. On an error `safe_dispatch/4`
returns the pre-call `State`. An actor's `on:do:` continues with the `State`
from before the protected block. Nothing hibernates, and nothing implements
`format_status/1`.

## Decision

### 1. Thread inside a method, sync at calls that may run user code

Method bodies keep threading `State` in locals as today. A field read is
`maps:get/2` on the current version and a field write binds the next one,
with no process-dictionary access.

The cell `'$bt_actor_state'`, which every callback already installs, becomes
the authoritative copy across any call that might run one of this actor's
blocks or methods. Before such a call the compiler emits one store of the
current version to the cell; after it returns, one load into the next
version:

```erlang
let _ = call 'erlang':'put'('$bt_actor_state', State3) in
let R = call 'beamtalk_message_dispatch':'send'(Recv, 'do:', [Block]) in
let State4 = call 'erlang':'get'('$bt_actor_state') in
...
```

A call needs the bracket when it may run user code in this process: a
`value`-family call on a block that is not inlined, a send with a block
argument, every self-send (unary, binary, keyword, `super`, cascade, aliased),
a send to an in-process object (a value type, a collection, any non-actor
object), an Erlang FFI call, and any primitive or runtime helper that
receives a block or calls a user method. The last group includes the
`on:do:` handler and `ensure:` cleanup runners, `Result tryDo:` and
`protect/1`, the `beamtalk_collection` fold helpers, sort comparators,
`Timer`, and collection primitives such as `includes:` that send `=` to
elements. A call needs no bracket only in two cases. One is a construct the
compiler lowers in place (`ifTrue:`, an inlined `do:`, `whileTrue:`, …), which
is not a call at all. The other is an intrinsic or primitive that can never
dispatch to user code, whatever its arguments are. Integer arithmetic and
`size` on a list qualify. String concatenation with `,` does not in general:
given a non-string argument it calls that argument's `printString`, which can
be a user method. It is exempt only where the compiler knows every argument is
a primitive string, and is bracketed otherwise. For a binary operator on a receiver of
unknown type, `operators.rs` already emits a guard whose `'true'` branch is the
bare BIF and whose `'false'` branch is the message send; the bracket goes in
the `'false'` branch only, so `count + 1` on an integer pays nothing. A send
to another actor is a `gen_server:call` and its blocks run away from home, but
the compiler can rarely prove the receiver is not `self` (an aliased `self` is
an actor reference too), so actor sends are bracketed in practice. The pair
costs 6 to 7 ns against a 700 ns round trip.

The cell is also how `State` crosses a method boundary. A compiled method is
`dispatch(Selector, Args, Self)` and answers a bare value. Its first statement
loads `State` from the cell, its body threads `State`, `State1`, … in locals
with the existing `State` family and actor frame mode, and its exit stores the
last version to the cell. Nothing else in the body touches the cell except
the bracket around a call that may run user code. A self-send is a bracketed
call to `dispatch/3`. The callback wrapper installs the `gen_server` state,
runs the method, and returns the cell:

```erlang
dispatch_call(Selector, Args, From, State) ->
    Old = beamtalk_actor_state:install(State),
    try Module:dispatch(Selector, Args, Self) of
        Result -> {reply, encode(From, Result), beamtalk_actor_state:take()}
    catch
        Class:Reason:ST -> {reply, encode(From, {error, ...}), State}
    after
        beamtalk_actor_state:uninstall(Old)
    end.
```

Between callbacks the `gen_server` state is the only copy. The cell does not
exist outside a callback.

### 2. One node, one check

The bracket is emitted in one place, the send emitter, as one `ThreadedIr`
node, `SyncedCall`, that renders the store, the call and the load together,
as `OnDoCatch` does for its snapshot and restore. A new `VerifyError`,
`UnbracketedUserCodeSend`, fails when a call that may run user code is not a
`SyncedCall`, so a missed sync point is a build failure and never a stale
read. `just verify-threaded-ir` runs it over the stdlib and bootstrap corpus.

The default is "may run user code". A call is exempt only if the emitter
built it from one of two existing sources. Inlined constructs come from
`beamtalk-core/src/state_threading_selectors.rs` and `WellKnownSelector`
(`beamtalk-core/src/ast/well_known.rs`) and never produce a call node. Exempt
intrinsics come from the one table codegen already uses to emit them: the
`'true'` branch of the operator guard in `operators.rs`, and the entries of
`PrimitiveBindingTable` (`primitive_bindings.rs`, built from the stdlib's
`@primitive` pragmas) that carry a new `no_user_code` mark. The mark means
the primitive can never dispatch to user code for any argument, and it is set
in the stdlib source next to the pragma. A primitive that is pure only for
some argument types (`,` for strings) stays unmarked. Codegen exempts such a
call only where the type checker shows every argument is a `String`. A binding with a block-typed parameter
cannot carry it, and `build-stdlib` rejects the combination. The emitter and
the verifier read the same mark, so there is no second list to keep in step.
An unmarked primitive is bracketed, which costs 6 to 7 ns and is never wrong.

### 3. Closures at home and away

A closure has no threaded `State`. Its field reads and writes go through
`beamtalk_actor_state` (`get`, `put`, and the reflective `fieldAt:`,
`fieldAt:put:`, `clearField:`, `hasField:`), as `beamtalk_class_vars` does for
class variables. A closure only runs at home through a bracketed call, so the
cell is current when it starts and the method loads its writes when it
returns.

Where a block's field access goes is decided when the block runs. It is at
home when a callback of its own actor is running in the current process,
tested as `Self#beamtalk_object.pid =:= self()` with the cell present. At
home, `self.x` reads and writes the cell. Away, it reads the value captured
when the block was created, and a write raises. An actor restarted by its
supervisor is a new process with a new pid, so a block from the old process is
not at home in the new one; ADR 0130 could not use pid equality for classes
because a class keeps its identity across restarts.

```beamtalk
Actor subclass: Probe
  state: x = 0
  state: saved = nil

  remember =>
    self.saved := [self.x]
    self.x := 1
    self.saved value          // same callback: 1

  bumpThenAsk =>
    self.x := 2
    self.saved value          // later callback: 2

  handOff: other =>
    other run: self.saved     // runs in other's process: 0, from creation
```

The later-callback case differs from an Erlang `fun`, which keeps the values
it closed over. A Beamtalk block reads fields through its actor at home, as a
Pharo block reads instance variables through its receiver. Between callbacks
the actor has one state, and a stored sort block or event handler that read a
state from an earlier message, and could not write, would put us back to
rejecting shapes. ADR 0130 §5 does the same for a class-side block run from a
later invocation of its class. The capture is only a fallback for running
away: creating a block literal that reads a field binds
`beamtalk_actor_state:capture(Self, Outer)` (one `get`), and the capture is
consulted only away from home.

A write away raises:

```erlang
#beamtalk_error{kind = actor_state_unreachable, class = 'Counter', selector = 'bump',
                hint = <<"A block that writes Counter's state ran outside Counter's own process "
                         "(it was sent to another actor, a Timer or a Future, or run after the "
                         "method returned). Send Counter a message instead: `counter bump!`.">>}
```

A self-send away is an ordinary message to the actor's pid, which is what
`self` means to every other process. If the actor is blocked in the call that
carried the block away, `check_call_stack/2` raises `deadlock_detected` or the
call times out, as for any A→B→A cycle. Today the send runs the actor's method
in the foreign process against the snapshot and drops its writes.

An `actor-state-abroad` lint, modelled on `class-state-abroad`, warns at a
block literal that writes a field and is passed to a send whose receiver is
statically not `self`, or to `Timer` or `Future`:

```beamtalk
Actor subclass: Counter
  state: n = 0
  bump => self.n := self.n + 1
  startLate => Timer after: 100 do: [self.n := self.n + 1]   // warning; raises in the Timer
  startWell => Timer after: 100 do: [self bump!]
```

Today `startLate` is a compile error (BT-3922).

### 4. Rollback

The rule is ADR 0130 §4's. An error that escapes a callback discards its
writes: the wrapper returns the pre-call state, as `safe_dispatch/4` does
today. `on:do:` already continues with the `State` from before the protected
block; its catch arm now also stores that version to the cell before the
handler runs, so a closure or callee in the handler sees it. The `OnDoCatch`
node owns the store, and the verifier's `CatchWithoutClassVarRestore` becomes
`CatchWithoutStateRestore`. `Result tryDo:` and the other runtime catchers do
the same through `protect/1`. A `^` is a sync point: the compiler stores the
current version before the `$bt_nlr` throw and loads after the catch, so the
actor NLR 4-tuple `{'$bt_nlr', Tok, Val, State}` loses its `State` slot.

Inside an `ensure:` protected body, every field write also stores to the
cell (write-through), and the cleanup reads the cell. Entering an `ensure:`
also stores the current `State` to the cell, so writes the method made before
the `ensure:` and never synced are in the cell even when the body writes
nothing. The protected body still threads `State` as usual. The extra cost is
one `put` per `ensure:` entry and one per field write inside an `ensure:`
protected body. What the cleanup sees:

- On a normal exit, every write the method made before the `ensure:` and
  every write the protected body made.
- On a `^`, the same. The `^` also stores before its throw.
- On an exception raised in the protected body itself, every write made
  before the `ensure:` and every write the body made before the raise, because
  each one was stored.
- On an exception raised by a bracketed callee, every write made before the
  `ensure:` and every write the body made, plus the callee's writes up to the
  callee's last store. A callee stores before
  each call it brackets and at its normal exit. Writes it made after its last
  store and before raising were held only in its threaded locals, and the
  cleanup does not see them. The same holds for nested callees.

This fixes the actor half of BT-3910 and BT-3993. Making every method store on
its exception path as well would need a `try` per method, and this ADR does
not add one. On every exception path, the next catch boundary or the callback
discards all of these writes anyway. `on:do:` still rolls back to the state
from before its protected block.

Each `gen_server` callback keeps its own contract. A callback returns the cell
only when it exits normally; on an exception it returns, or stops with, the
state it was called with.

| Callback | User code | Cell | Normal exit | Exception |
|---|---|---|---|---|
| `init/1` | none; `initialize` runs in `handle_continue` (ADR 0078) | not installed | `{ok, State, {continue, initialize}}` | unchanged |
| `handle_continue(initialize, _)` | the `initialize` chain, parent first | installed around the chain, snapshotted before each step | `{noreply, take()}` after the field check | `{stop, Reason, S}` with `S` the state before the failing step, as today; `terminate:` sees `S` |
| `handle_call`, `handle_cast`, `handle_info`, BT-3968 continuations | one method | around the dispatch | `{reply, R, take()}` / `{noreply, take()}` | pre-call state; `handle_call` replies with the error, no-reply handlers follow ADR 0134 |
| `terminate/2` | `terminate:` if defined | around the dispatch (new; today it does not stash) | `ok`, cell erased | caught and ignored, as today; cell erased |
| `code_change/3` | none; `beamtalk_hot_reload:code_change/3` | not installed | `{ok, NewState}` | unchanged |
| `format_status/1` | not implemented | not installed | would format the `gen_server` state | n/a |

#4447 needed an un-publish wherever a state change was discarded. Here the
method body is the source of truth between sync points and the cell is the
source of truth across them, and every crossing is a `SyncedCall` the verifier
checks.

### 5. Self-sends and self-casts

Direct and aliased self-sends both run in-process on the cell, so the aliased
form starts from the live state and its writes persist. A method-level
`self foo!` becomes `gen_server:cast` to the actor's own pid and runs after
the current message, which is what `!` means elsewhere (principles §7) and
already means inside a block.

### 6. Unchanged

Class variables, value-type `Self` threading (ADR 0120) and local-variable
threading stay as they are. Actor methods keep their frame mode, and locals
keep riding in the actor map as `__local__` keys. ADR 0133's rule for captured
locals stays; only its field half is amended. The wire protocol,
`sync_send`/`cast_send`, supervision and `stopWith:`/`continueWith:` are
unchanged.

### Example

```beamtalk
Actor subclass: Tally
  state: total = 0

  add: n => self.total := self.total + n

  twice: aBlock =>
    aBlock value
    aBlock value

  run =>
    reader := [self.total]
    self twice: [self add: 5]
    self.total := self.total + 1
    reader value
```

```text
beamtalk> Tally spawn run
11
```

On `main`, `self twice: [self add: 5]` raises `stateful_block_dispatch`
(BT-3936), and without that line `reader value` answers 0 (BT-3952).

## OTP Fit

A throwaway `gen_server` (OTP 28) that installs the cell on entry, mutates it
through closures and returns it on exit was used to check the following.

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

Throwaway spikes on OTP 28 compare today's threading, this design, and the
per-access cell from the first draft of this ADR (see Alternatives). Figures
are medians over 101 batches on a shared 18-core machine at load average 14
to 77 during the runs, with `+S 1` (three runs) and with 18 schedulers (one
run), so treat differences under about 15% as noise.

| Measure | Schedulers | Threaded (today) | This design | Per-access cell |
|---|---|---|---|---|
| Store + load pair, isolated | `+S 1` / 18 | | 6.2–6.9 ns / 6.1 ns | |
| Field loop, 10 reads + 3 writes, intrinsics only, in process | `+S 1` | 31–41 ns | same code | 171–215 ns |
| Same | 18 | 26 ns | same code | 96 ns |
| `gen_server:call`, that body once | `+S 1` | 2.4–3.2 µs | 0.77x–0.98x | 0.82x–1.16x |
| Same | 18 | 0.70 µs | 1.01x | 1.08x |
| `gen_server:call`, that body 10 times | `+S 1` | 2.9–3.9 µs | 0.96x–0.99x | 1.16x–1.37x |
| Same | 18 | 0.97 µs | 1.16x | 2.03x |

An intrinsic-only field loop compiles to the same code as today, so its cost
is unchanged by construction. A send that may run user code pays one store and
one load, 6 to 7 ns. A field write inside an `ensure:` protected body adds
one `put`, which was not measured separately. A send-heavy benchmark (ten in-process remote calls, each
writing a field) measured this design faster than today's threading in every
run (0.5x to 0.66x). The two versions differ only by the store and load, so
that result is an artifact of the harness and is not reported as a saving;
the isolated pair is the number to use. The end-to-end figures sit within
noise of today's apart from the 18-scheduler 10-body case at 1.16x, a single
run.

Phase 0 gates, measured with `runtime/perf/self_send_bench` and
`runtime/perf/bench_method_threading.erl` as described in
`docs/development/benchmarks.md` (at least 7 interleaved rounds on an idle
machine):

1. A method looping over ten field reads and three writes with intrinsics
   only, through `gen_server:call`: at most 1.05x today.
2. A method making ten in-process sends that each update a field: at most
   10 ns more per send than today.
3. Open and sealed actor self-sends, top level and in a `do:` arm: no
   regression beyond noise.
4. `UnbracketedUserCodeSend` reports zero over the stdlib, bootstrap and BUnit
   corpus.

## What It Deletes

Classified against `main` at `1df39217f` and the open PRs on 2026-10-20.

| Deleted | Where |
|---|---|
| `closure_self_dispatch_state_guard`, `CLOSURE_SELF_SEND_STATE_CHANGE_HINT`, `closure_drops_state` (BT-3936) | `dispatch_codegen.rs`, `generator/mod.rs`, `body_state.rs` |
| `in_state_dropping_closure` on the `super` path, and the binary-selector state drop (BT-3985) | #4455, `generate_super_send` |
| `WriteKind::ObjectState`, `outer_state_writes`, `OuterWriteWalker::state_write`, `is_captured_self_field_write`, the field half of `Walker::escape_write_of` (BT-3922) | `block_facts.rs`, `local_threading.rs` |
| `outer_state_reads`, `outer_self_send`, `may_change_actor_state`, `block_may_change_actor_state`, `report_stale_read`, `report_stale_self_send`, `Escape::{LaterUse, ActorSelfCall, SelfCascade, CollectionResult}` (BT-3952) | #4452 |
| "cannot assign to field '{name}' inside a stored closure" | `block_analyzer.rs` |
| The actor-state `stateful_block_dispatch` for a callable forwarded to a list op | `control_flow/list_ops/mod.rs` |
| `detect_tier2_self_send`, `scan_class_for_tier2_blocks`, `tier2_method_info`, `tier2_block_params` (unreachable since BT-3922) | codegen |
| `generate_discarding_self_dispatch` | `dispatch_codegen.rs` |
| `{reply, R, NewState}` from methods, the state arms of `unwrap_dispatch_result/1`, the `State` slot of the actor NLR tuple | runtime, codegen |
| `generate_self_cast_send` | `dispatch_codegen.rs` |

`testPureIfNoneAfterFoldPinBug` (BT-3952) and the
`testSelfSendStateChangeThrough*Raises` tests (BT-3936) in
`stdlib/test/actor_self_send_hom_test.bt` flip to the correct answers.
BT-3988 and BT-3990 close.

Phase 0 runs the 733-shape oracle (BT-3950) to settle these, which mix actor
state with locals: `RESIDUAL_DENY_SET` `EvaluatedBlockValue` (BT-3981, with
its PIN-BUGs in `bt3971test.bt` and `bt3939tier2inlined_test.bt`) and
`GroupByValue`, and the roughly 15 `DENY_SET` rows marked `stateful`, 11 of
them for actors.

These stay. The `State` family, the actor frame mode, ADR 0118's self-send
preludes (a self-send still rebinds `State`, now from the load), and
`strip_local_temps/1` stay because method bodies still thread. So does
`RESIDUAL_DENY_SET` `InlineBlockLocalInLoop` (BT-3948), which is about the
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

ADR 0130 made the same move for class variables: one home in the class
process during an invocation, live at home, captured away, rollback at the
invocation and at `on:do:`. It deleted a threading family and four
diagnostics, flipped 21 pinned wrong answers, and cost 1.6x to 1.75x end to end
on a field-heavy loop, because it moved every access to the dictionary. This
ADR keeps the access threaded and pays only at calls.

A register-allocating compiler does the same with a variable that lives in a
register inside a function and is spilled to its memory home before a call
that might read it and reloaded after. An Erlang `gen_server` takes state in
and returns it, and how the callback computes it is invisible. In Pharo a
block reads and writes instance variables through its receiver wherever it
runs, which this ADR gives at home. Akka lets closures on the actor's thread
see fields live and leaves a closure on another thread undefined; here it
reads the capture and a write raises. Pony rejects, by type, a closure that
could write actor state being sent to another actor.

## Alternatives Considered

**A cell for every field access.** The first draft of this ADR: methods stop
threading and every `self.x` is a `get` and `put` on the cell. It is one
lowering with no sync points to get right, and it deletes the `State` family.
It costs 3.7x to 6.9x on a field-heavy body in process and up to 2x end to end
for a call that makes 130 field accesses (table above), on every actor method
whether or not it uses a block.

**A per-class switch: thread when no block touches `self`.** Use today's
lowering for classes whose blocks never read fields or self-send, and the
cell otherwise. That is two lowering modes for actor methods. Whether a class
qualifies depends on its superclasses' methods, on its trait provisions and on
methods added later by `>>`, `compile:source:` or ClassBuilder (ADR 0066,
ADR 0082, ADR 0084), so the switch would have to be recomputed and the class
recompiled when any of those change.

**Status quo.** Keep the rejections and close BT-3988 and BT-3990 the same
way. It leaves the three silent answers in the probe, and the list grows with
block shape times send kind times position, which is how ADR 0130 described
class variables before it.

**Publish on write (#4447).** Two copies, kept in step by publishing at each
write and un-publishing at each discard. The discards are open-ended, and
nothing checks them. This design moves the copy at calls instead of at
writes, and the verifier checks every call.

**Thread `State` through closures.** Make every actor block a Tier 2 block
that takes and returns `State`. This is ADR 0132 applied to fields, which
ADR 0133 superseded: a second entry per block-taking method, a runtime
resolution walk, and nothing for blocks that run in another process or after
the method returns.

**Raise on any field access away.** ADR 0130 rejected this as its
Alternative R. A block passed to another actor's method while this actor waits
would raise where the capture is usually right.

**Keep `self foo!` as run-now-and-discard.** It needs a snapshot and restore
per self-cast to keep a behaviour that loses writes today.

## Consequences

### Positive
- Closures in an actor read and write its live state at home, like class
  variables under ADR 0130.
- The closure-specific rejections, guards and pinned wrong answers are
  deleted; BT-3952, BT-3985, BT-3988 and BT-3990 close; the probe's silent
  answers are fixed.
- Field access inside method bodies costs what it costs today, except in
  `ensure:` protected bodies.
- A missed sync point fails the build through `UnbracketedUserCodeSend`.

### Negative
- Every send that may run user code pays a store and a load, 6 to 7 ns.
- Every compiled actor changes calling convention. Older modules are refused
  with `abi_mismatch`, and crossing the release needs a node restart
  (ADR 0125).
- The `State` family and the actor frame mode stay, so this deletes less
  codegen than the per-access cell would.
- Every `ensure:` entry, and every field write inside an `ensure:` protected
  body, also does a `put`.
- If a bracketed callee raises, an `ensure:` cleanup does not see the writes
  that callee made after its last store.
- A method-level `self foo!` now runs after the current message.
- Away from home, a `Timer` or `Future` body still reads creation-time values.
- It undoes parts of BT-3922, BT-3936, #4452 and #4455.

### Neutral
- Locals, value types and class variables are unchanged.
- Native Erlang actor methods keep `{reply, R, NewState}` through an adapter.

## Implementation

**Phase 0 (S).** Behind a codegen flag, emit `SyncedCall` and the cell
calling convention for the stdlib, BUnit and REPL corpora. Run the four
gates and the 733-shape oracle. Turn the spike's OTP checks into EUnit tests.
Proceed only if the gates pass and no shape that answers right today answers
wrong with the flag on.

**Phase 1 (M).** `beamtalk_actor_state` (`install`, `take`, `uninstall`,
`get`, `put`, `has`, `clear`, `capture`, `protect`), used by every callback
wrapper in `beamtalk_actor.erl` and by `self_dispatch/2`. An adapter for native
methods and ADR 0066 instance-side extension funs that return
`{reply, R, NewState}`. The `actor_state_unreachable` error kind.

**Phase 2 (L).** The `SyncedCall` node and `UnbracketedUserCodeSend` in
`threaded_ir/`, emitted by the send emitter in `dispatch_codegen.rs` and the
`'false'` branch of the operator guard in `operators.rs`. Methods become
`dispatch(Selector, Args, Self)`, loading on entry and storing on exit.
Closure field access goes through the helpers, and field-reading block
literals bind the capture. `OnDoCatch` stores its pre-region version under
`CatchWithoutStateRestore`. `^` stores before the throw. Field writes inside
an `ensure:` protected body write through, and entering an `ensure:` stores.
The `no_user_code` mark goes on
`PrimitiveBindingTable` entries, set from the stdlib pragmas and checked by
`build-stdlib`. `self foo!` lowers to a cast. Bump the module ABI marker.

**Phase 3 (M).** Delete the table above and whatever Phase 0 confirmed.
Update the "field mutations require control-flow context" text in
`docs/beamtalk-language-features.md`.

**Phase 4 (S).** The `actor-state-abroad` lint with
`@expect actor_state_abroad`, the amendment notes in ADR 0133 and ADR 0041,
and `docs/agents/expanded.md` § State-Threading Codegen.

While this ADR is Proposed, #4452 and #4455 can land as stopgaps because each
removes a silent wrong answer, but no new actor-state rejection should be
added.

## Migration Path

Compiled actors need recompiling and a node restart across the ABI change.
Source that compiles today keeps compiling. Shapes rejected today compile,
and the probe's silent answers become correct. A block that writes a field
from another process raises at run time. Code that relied on `self foo!`
running before the next statement should use `self foo`.

## Open Questions

1. Should a self-send away raise `actor_state_unreachable` instead of sending
   a message? A message is the Erlang meaning; raising avoids waiting for the
   call timeout when the carry was synchronous.
2. Should ADR 0133's rule for escaping blocks that assign class variables get
   the same amendment? ADR 0130 already makes those writes live at home and
   raising away; BT-3922 rejects them at compile time.
3. Can the compiler prove enough actor-send receivers are not `self` to skip
   their bracket? At 6 to 7 ns against a 700 ns call it is not worth much.

## References

- Related issues: BT-3996, BT-3950, BT-3901, BT-3922, BT-3936, BT-3948,
  BT-3952, BT-3971, BT-3981, BT-3985, BT-3988, BT-3990, BT-3910, BT-3993,
  BT-3947, BT-3913
- Related PRs: #4447 (closed), #4448, #4452, #4454, #4455
- Related ADRs: 0130, 0133, 0041, 0118, 0111, 0131, 0120, 0125, 0126, 0129
- Code: `runtime/apps/beamtalk_runtime/src/beamtalk_actor.erl`,
  `runtime/apps/beamtalk_runtime/src/beamtalk_class_vars.erl`,
  `crates/beamtalk-codegen/src/core_erlang/gen_server/`,
  `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen.rs`,
  `crates/beamtalk-codegen/src/core_erlang/operators.rs`,
  `crates/beamtalk-core/src/state_threading_selectors.rs`,
  `crates/beamtalk-core/src/ast/well_known.rs`
- Documentation: `docs/beamtalk-principles.md` §4, §7, §9;
  `docs/development/benchmarks.md`
