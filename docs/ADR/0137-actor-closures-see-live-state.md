# ADR 0137: Actor Closures See the Actor's Live State

## Status
Proposed (2026-10-20)

Amends ADR 0133 (an escaping block may assign `self.field`), ADR 0041 (actor
blocks no longer carry the actor's fields) and ADR 0118 (an actor self-send
has no state effect to sequence), and removes the `State` family from
ADR 0111's `ThreadedIr`. ADR 0130 is the precedent and is unchanged, as are
ADR 0131 and the locals half of ADR 0133.

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
one. That is the two-copies problem ADR 0130 removed for class variables.

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

### 1. One home during a callback

During a callback the actor's state lives only in the cell
`'$bt_actor_state'`, the slot every callback already installs. Compiled
methods take no `State` and return none. The wrapper installs the cell from
the `gen_server` state, runs the method, and returns the cell as the new
state:

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

### 2. Access

A new module, `beamtalk_actor_state`, owns the key, as `beamtalk_class_vars`
does for class variables.

| Beamtalk | Emitted |
|---|---|
| `self.x` | `beamtalk_actor_state:get(Self, 'x')` |
| `self.x := v` | `beamtalk_actor_state:put(Self, 'x', V)` |
| `fieldAt:`, `fieldAt:put:`, `clearField:`, `hasField:` | the same helpers |
| any self-send (unary, keyword, binary, `super`, sealed, aliased) | `Module:dispatch(Sel, Args, Self)`, a bare value |

The reflective selectors read the same cell, so the `fieldAt:` gap in #4447
has nothing to miss. Phase 0 decides whether to open-code the helpers.

### 3. At home and away

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
today. `on:do:` and the runtime catchers that run blocks (`Result tryDo:`,
`protect/1`) snapshot the cell on entry and restore it when an error crosses
the catch, so a failed try body's writes are dropped, as today. The
`OnDoCatch` node already emits the class-variable snapshot; it takes the actor
cell too, and the verifier's `CatchWithoutClassVarRestore` becomes
`CatchWithoutStateRestore`. A `$bt_nlr` throw restores nothing, and the actor
NLR 4-tuple `{'$bt_nlr', Tok, Val, State}` loses its `State` slot. `ensure:`
is not a boundary: its cleanup sees the writes the protected block made before
a `^` or an error, which fixes the actor half of BT-3910 and BT-3993. A
self-send that raises does not restore on its own; any catch restores to the
start of its region, which is earlier (ADR 0130 rejected per-send rollback as
its Alternative C).

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

#4447 needed an un-publish wherever a state change was discarded, and that set
was open-ended. Here nothing is published. A discard happens only at the
invocation boundary and the catch boundary, each owned by one wrapper or one
`ThreadedIr` node.

### 5. Self-sends and self-casts

Direct and aliased self-sends dispatch in-process on the cell, so the aliased
form starts from the live state and its writes persist. A method-level
`self foo!` becomes `gen_server:cast` to the actor's own pid and runs after
the current message, which is what `!` means elsewhere (principles §7) and
already means inside a block.

### 6. Unchanged

Class variables, value-type `Self` threading (ADR 0120) and local-variable
threading stay as they are. Actor-method locals stop riding in the actor map
as `__local__` keys and thread as they do in class methods, so actor methods
lose their own frame mode. ADR 0133's rule for captured locals stays; only its
field half is amended. The wire protocol, `sync_send`/`cast_send`,
supervision and `stopWith:`/`continueWith:` are unchanged.

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
passed as arguments. The cell is working storage scoped to one callback, and
the `gen_server` contract of state in, state out holds exactly. OTP keeps
process-scoped context there (`'$ancestors'`, `'$initial_call'`, logger
metadata), and so does Beamtalk (`'$bt_call_stack'`, ADR 0130's class
variables). The generated Core Erlang loses its `StateN` chain;
`process_info(Pid, dictionary)` shows the live state mid-callback instead of
today's stale stash.

## Cost

Two throwaway spikes on OTP 28 compare threaded `State` with the cell, in
process (one body of ten field reads and three writes) and end to end (one
`gen_server:call` whose callback runs the body `Reps` times). The figures are
medians over 101 batches; they were taken on a shared 18-core machine at load
average 10 to 18, so the ratios repeat to about ±0.3x across runs and the
nanoseconds swing by up to 1.7x.

| Measure | Schedulers | Threaded | Cell | Ratio |
|---|---|---|---|---|
| One body, in process | `+S 1` (3 runs) | 30–37 ns | 140–200 ns | 3.8x–6.7x |
| One body, in process | 18 | 25 ns | 97 ns | 3.8x |
| Call, `Reps = 1` (13 accesses) | `+S 1` (2 clean runs) | 1.88–1.94 µs | 1.92–1.99 µs | 1.02x |
| Call, `Reps = 1` | 18 | 0.69 µs | 0.73 µs | 1.06x |
| Call, `Reps = 10` (130 accesses) | `+S 1` (3 runs) | 2.12–2.28 µs | 2.78–3.10 µs | 1.23x–1.46x |
| Call, `Reps = 10` | 18 | 0.89 µs | 1.68 µs | 1.88x |

Each access costs 6 to 13 ns more through the cell, because the threaded
form keeps the map in a register. ADR 0130 measured the same for class
variables. A local helper function and a remote helper cost the same, so
Phase 0 has to try open-coded `get`/`put` before counting on inlining. The
per-callback install is already paid today; the new cost is one `get` at exit.
Not credited above: self-sends lose the `{reply, R, NewState}` tuple and the
BT-3936 guard, and callbacks lose `strip_local_temps/1`.

Phase 0 gates, measured with `runtime/perf/self_send_bench` and
`runtime/perf/bench_method_threading.erl` as described in
`docs/development/benchmarks.md` (at least 7 interleaved rounds on an idle
machine):

1. A method looping over ten field reads and three writes, through
   `gen_server:call`: at most 2x today. The spike measured 1.2x to 1.9x.
2. Open and sealed actor self-sends, top level and in a `do:` arm: no
   regression beyond noise.
3. A call to a method with one write: at most 1.05x. The spike measured
   1.02x to 1.08x for thirteen accesses, so this is narrow and needs enough
   rounds to resolve 5%.
4. Creating a field-reading block in a loop: recorded, not gated.

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
| `VersionPrefix::State`, the actor frame mode, `ActorStateInClassMethod`, the `State` rows of `ConstructFamilyDropped`, `FoldFamilyNotCarried`, `StateEffectEscapesExpression` | `threaded_ir/`, `generator/version.rs` |
| ADR 0118's actor self-send preludes, `generate_discarding_self_dispatch` | `dispatch_codegen.rs`, `threaded_expr.rs` |
| `{reply, R, NewState}` from methods, the state arms of `unwrap_dispatch_result/1`, the `State` slot of the actor NLR tuple | runtime, codegen |
| `strip_local_temps/1`, `is_local_temp_key/1`, `__local__` keys in actor state | `beamtalk_actor.erl`, `callbacks.rs` |
| `generate_self_cast_send` | `dispatch_codegen.rs` |

`testPureIfNoneAfterFoldPinBug` (BT-3952) and the
`testSelfSendStateChangeThrough*Raises` tests (BT-3936) in
`stdlib/test/actor_self_send_hom_test.bt` flip to the correct answers.
BT-3988 and BT-3990 close. `CodeGenContext::Actor` has 44 branches in 18
non-test codegen files; Phase 0 reports how many go.

Phase 0 runs the 733-shape oracle (BT-3950) to settle these, which mix actor
state with locals: `RESIDUAL_DENY_SET` `InlineBlockLocalInLoop` (BT-3948) and
`EvaluatedBlockValue` (BT-3981, with its PIN-BUGs in `bt3971test.bt` and
`bt3939tier2inlined_test.bt`), `GroupByValue`, and the roughly 15 `DENY_SET`
rows marked `stateful`, 11 of them for actors.

These stay because they are about locals, Tier 2 or class variables: the
`WriteKind::Local` half of `Tier2BlockNoReturnChannel` with `Escape::Ffi`
(BT-3917), `Passed` and `Cascade`; `ALLOW_SET`, the other `DENY_SET` rows,
`RIGHT_STATEMENTS` and `MatchArmWrite`; `report_nlr_out_of_ensure` and
`nlr_dropped_local` (BT-3910, BT-3993); `reject_last_statement_block_value`
(BT-3971) and `reject_loop_rebind_of_body_local` (BT-3939); the Tier 2
`stateful_block_dispatch` sites in `intrinsics.rs`, `while_loops.rs` and
`exception_handling.rs`; the locals half of
`beamtalk_collection:refuse_writing_fold/3`; `ClassStateAbroad`; and the
`Local` and `SelfVt` families.

## Prior Art

ADR 0130 made the same move for class variables two weeks ago: one home in
the class process during an invocation, live at home, captured away, rollback
at the invocation and at `on:do:`. It deleted a threading family and four
diagnostics, flipped 21 pinned wrong answers, and cost 1.6x to 1.75x end to end
on a field-heavy loop.

An Erlang `gen_server` takes state in and returns it; how the callback body
computes it is invisible. Elixir code keeps callback-scoped context in
`Process.put/2` (Logger metadata, Ecto's dynamic repo). In Pharo a block
reads and writes instance variables through its receiver wherever it runs,
which this ADR gives at home. Akka lets closures on the actor's thread see
fields live and leaves a closure on another thread undefined; here it reads
the capture and a write raises. Pony rejects, by type, a closure that could
write actor state being sent to another actor.

## Alternatives Considered

**Status quo.** Keep the rejections and close BT-3988 and BT-3990 the same
way. It leaves the three silent answers in the probe, and the list grows with
block shape times send kind times position, which is how ADR 0130 described
class variables before it.

**Publish on write (#4447).** Two copies, kept in step by publishing at each
write and un-publishing at each discard. The discards are open-ended, and each
one is a per-call-site reconciliation of the kind CLAUDE.md rules out for
state threading. It also left writes and self-sends in closures broken.

**Thread `State` through closures.** Make every actor block a Tier 2 block
that takes and returns `State`. This is ADR 0132 applied to fields, which
ADR 0133 superseded: a second entry per block-taking method, a runtime
resolution walk, and nothing for blocks that run in another process or after
the method returns.

**A cell for closures only.** Keep threaded `State` in method bodies and sync
it to the cell around closures. This is #4447 with fewer sync points and the
same failure.

**Raise on any field access away.** ADR 0130 rejected this as its
Alternative R. A block passed to another actor's method while this actor waits
would raise where the capture is usually right.

**Keep `self foo!` as run-now-and-discard.** It needs a snapshot and restore
per self-cast to keep a behaviour that loses writes today.

## Consequences

### Positive
- Actors and classes share one model of mutable state.
- The actor-state rejections, guards and pinned wrong answers are deleted;
  BT-3952, BT-3985, BT-3988 and BT-3990 close; the probe's silent answers are
  fixed.
- `ThreadedIr` loses the `State` family, and actor methods lower locals as
  class methods do.
- Self-sends get cheaper.

### Negative
- Field-heavy bodies are about 4x slower in isolation. A typical call costs
  2% to 8% more and a call making 130 field accesses up to 1.9x, gated at 2x.
- Every compiled actor changes calling convention. Older modules are refused
  with `abi_mismatch`, and crossing the release needs a node restart
  (ADR 0125).
- Generated Core Erlang reads the process dictionary.
- A method-level `self foo!` now runs after the current message.
- Away from home, a `Timer` or `Future` body still reads creation-time values.
- It undoes parts of BT-3922, BT-3936, #4452 and #4455.

### Neutral
- Locals, value types and class variables are unchanged.
- Native Erlang actor methods keep `{reply, R, NewState}` through an adapter.

## Implementation

**Phase 0 (S).** Behind a codegen flag, lower actor field access and
self-sends to the cell for the stdlib, BUnit and REPL corpora. Run the four
gates and the 733-shape oracle, and report the line count the deletion
removes. Turn the spike's OTP checks into EUnit tests. Proceed only if the
gates pass and no shape that answers right today answers wrong with the flag
on.

**Phase 1 (M).** `beamtalk_actor_state` (`install`, `take`, `uninstall`,
`get`, `put`, `has`, `clear`, `capture`, `snapshot`, `restore`, `protect`),
used by every callback wrapper in `beamtalk_actor.erl` and by
`self_dispatch/2`. An adapter for native methods and ADR 0066 instance-side
extension funs that return `{reply, R, NewState}`. The `actor_state_unreachable`
error kind.

**Phase 2 (L).** Methods become `dispatch(Selector, Args, Self)`. Field
access, the reflective selectors and every self-send form go through the cell;
generated callbacks return it; field-reading block literals bind the capture;
`OnDoCatch` covers both kinds of state under `CatchWithoutStateRestore`;
`self foo!` lowers to a cast. Bump the module ABI marker.

**Phase 3 (L).** Delete the table above and whatever Phase 0 confirmed. Update
the "field mutations require control-flow context" text in
`docs/beamtalk-language-features.md`.

**Phase 4 (S).** The `actor-state-abroad` lint with
`@expect actor_state_abroad`, the amendment notes in ADR 0133, 0041 and 0118,
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

1. Is a helper call fast enough, or does codegen need to open-code the cell
   access?
2. Should a self-send away raise `actor_state_unreachable` instead of sending
   a message? A message is the Erlang meaning; raising avoids waiting for the
   call timeout when the carry was synchronous.
3. Should ADR 0133's rule for escaping blocks that assign class variables get
   the same amendment? ADR 0130 already makes those writes live at home and
   raising away; BT-3922 rejects them at compile time.

## References

- Related issues: BT-3996, BT-3950, BT-3901, BT-3922, BT-3936, BT-3948,
  BT-3952, BT-3971, BT-3981, BT-3985, BT-3988, BT-3990, BT-3910, BT-3993,
  BT-3947, BT-3913
- Related PRs: #4447 (closed), #4448, #4452, #4454, #4455
- Related ADRs: 0130, 0133, 0041, 0118, 0111, 0131, 0120, 0125, 0126, 0129
- Code: `runtime/apps/beamtalk_runtime/src/beamtalk_actor.erl`,
  `runtime/apps/beamtalk_runtime/src/beamtalk_class_vars.erl`,
  `crates/beamtalk-codegen/src/core_erlang/gen_server/`,
  `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen.rs`
- Documentation: `docs/beamtalk-principles.md` §4, §7, §9;
  `docs/development/benchmarks.md`
