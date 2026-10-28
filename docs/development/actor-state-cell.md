# Actor State Cell: Design Note

How ADR 0137 is implemented. The ADR states the user-level rule (a block
sees the actor's live fields inside the actor and a creation-time snapshot
elsewhere) and the decision; this note holds the mechanism, the verifier
rules, the measurements and the phase detail. Read the ADR first:
[ADR 0137](../ADR/0137-actor-closures-see-live-state.md).

Tracking: BT-4006 (epic), BT-4009 (Phase 2), BT-4010, BT-4020.

## Background

### The shapes ADR 0133 Phase 3 met

ADR 0133 Phase 3 met the cause given in ADR 0137's Context one shape at a
time, each answered with a compile-time rejection or a runtime guard that
also rejects some correct code:

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

### State before ADR 0137

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

## Mechanism

### 1. Thread inside a method, sync at calls that may run user code

Method bodies keep threading `State` in locals as today. A field read is
`maps:get/2` on the current version and a field write binds the next one,
with no process-dictionary access.

The cell `'$bt_actor_state'`, which every callback already installs, carries
the state across any call that might run one of this actor's blocks, tagged
with the owning actor as `{Owner, Map}`. Before such a call the compiler
emits one store of the current version to the cell; after it returns, one
load into the next version:

```erlang
let _ = call 'beamtalk_actor_state':'store'(State3) in
let R = call 'beamtalk_message_dispatch':'send'(Recv, 'do:', [Block]) in
let State4 = call 'beamtalk_actor_state':'take'() in
...
```

`store/1` keeps the cell's current owner, so a store made while another
actor's state is installed (a nested away `super_send/5`) does not claim that
state for the host process.

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
Phase 0 measured a bracketed self-send at 1.35x to 1.60x (§7), which is
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
returns the fun's own `{reply, R, NewState}` unless the cell changed during the
call (always on, every build): if the answer is the exact entry state, the cell
holds the only writes (a stored block run by an inherited `Object` method such as
`ifNotNil:` reached through `perform:withArguments:`) and the answer carries the
cell's state; if the answer is a different state and the cell changed to
something else, `call_foreign/3` raises `actor_state_conflict`; an answer equal
to the cell (a compiled callee that stored and threaded the same version) or an
unchanged cell passes through. A native method or extension fun that runs a block
should still read the block's writes back from the cell
(`beamtalk_actor_state:get/2`) before it answers.

Phase 2b audit of what runs blocks behind a foreign exit: the only inherited
methods that evaluate a caller-supplied block are `Object>>ifNotNil:`,
`ifNil:ifNotNil:` and `ifNotNil:ifNil:` (compiled Beamtalk in `object.bt`; `ifNil:`
never runs its block), plus `perform:withArguments:` reaching them. Both shapes
are covered by `closure_cell_foreign_exit.bt`. No other native or extension fun
in the runtime evaluates a block against the actor's state.

The helper does not reconcile the cell with the answer; ADR 0137's
Alternatives Considered says why.

An aliased self-send (`other := self. other x`, `self` passed as an argument)
is an ordinary send and is bracketed. `sync_send/3` sees its own pid and runs
the method through `self_dispatch/2` on the cell, which is current, and stores
the answer back, so it starts from the live state and its writes persist.
`self foo!` is a cast to the actor's own pid and runs after the current
message (BT-3998); ADR 0137 keeps that.

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
present *and owned by that same actor*. While a nested away `super_send/5`
runs, the cell holds the other actor's state, so a host block running inside
the call is away: its reads answer its capture, its writes raise
`actor_state_unreachable`, and its self-sends raise `deadlock_detected`
(the host's callback is blocked in that call, so the send could never be
served). Otherwise away, a field read answers the block's
capture, a write raises `actor_state_unreachable`, and a self-send is a
message to the actor's pid.
If the actor is blocked in the call that carried the block away,
`check_call_stack/2` raises `deadlock_detected` or the call times out, as for
any A→B→A cycle. A block passed to a class method is at home only when the
method is direct-called (ADR 0129 Phase 0b), because only then
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

### 4b. Stateless loops bracket the guarded-operator slow path

A loop that keeps its variables in parameters (tuple-accumulator folds,
direct-params counted and `while` loops, the dict and list lowerings) holds
no whole `State`, so the slow path of a guarded operator in its body or
condition (`acc + m`, where `m` is a user object with its own `+`) would run
user code against whatever the cell held at method entry — a stale cell
(BT-4121). Threading `State` through every such loop would close it at the
cost of gate 3. The bracket closes it without threading (BT-4121):

- Before the body and condition lower, the loop pushes a marker
  (`push_guarded_op_loop`, `synced_call.rs`). A walk over the same
  `classify_binary` the send emitter uses decides `will_bracket`, so the two
  cannot disagree; loops whose operators are all statically numeric emit no
  guard, push a no-op marker, and compile byte-identical code to before.
- When the marker brackets, the loop-entry `State` is stored before the loop,
  every field read inside the loop body and condition reads through the cell
  (`take()`), and the loop's exit packs and loads seed from the cell too.
  The fast path is untouched; the slow path runs against a current cell and
  the exit pack loads its writes back.
- A loop nested in another stateless loop's `fun` has no `State` in scope to
  store, so it hands its need to the enclosing marker, whose own walk saw
  the same operators and whose bracket keeps the cell current for both. Its
  exit packs still seed from the cell.
- A hybrid loop cannot bracket: its pre-extracted field parameters advance
  past the entry `State` every iteration, so a store of the entry would be
  stale at once. Mode selection routes a hybrid body carrying a guarded
  operator to the `StateAcc` fallback instead, and a hybrid checkout that
  still reaches the bracket reports loud (below).
- An `on:do:` inside a bracketing loop (directly, or in a stateless loop
  nested in one) snapshots the cell on entry to the protected block and
  restores that snapshot on catch (`CellBoundary::Snapshot`, as in an actor
  closure), because the loop-entry `State` is stale for the slow paths'
  writes (BT-4179). Elsewhere it stores the threaded `State`
  (`CellBoundary::PreState`). A hybrid loop keeps `PreState`: its body runs
  no user code (an operator, self-send or user-code send, handler included,
  routes it to `StateAcc`), and it is never nested in a bracketing loop,
  since no loop that brackets admits a field write in its body.
- A `StateAcc` loop or fold nested in a bracketing loop (a fold whose body
  holds a parenthesised conditional writing an outer local keeps the
  enclosing loop's direct parameters) seeds its accumulator from the cell
  (`take()`), like the loop's exit pack, not from the loop-entry `State`:
  every store the nested construct makes (a bracket, an `on:do:` boundary)
  would otherwise put that stale `State` back (BT-4179).
- Each marker records the loop-entry `State`, and each threaded-`State` use
  in its body (an `on:do:` `PreState` boundary, a `StateAcc` seed) is noted
  on the innermost marker. A loop whose checkout brackets reports a use of
  its entry `State` (`CatchWithoutStateRestore` with `PreStateOverLiveCell`,
  or `StaleStateSeedInBracketedLoop`), and a loop that does not bracket
  hands its uses outward. So both rules are checked against the bracket
  decision, not against the gates that chose the shapes.
- An actor closure created inside a bracketing loop captures the cell
  (`take()`) at creation, bound around its `fun`, not the loop-entry
  `State`: run away from home it reads the slow paths' writes of the
  iterations before its creation (BT-4180). A conditional or `on:do:` in a
  plain closure frame (a loop condition) seeds its scratch map from the cell
  for the same reason.

**Read-side backstop (BT-4180).** Inside a *bracketed region* (the body and
condition of a stateless loop whose checkout bracketed, plus everything
lowered inside it, nested loops of any mode included) every actor-state read
source must be the cell. Every site that hands a lowering the state map to
read from — a `self.x` or bare field read, a `^`'s carried state, an actor
closure's creation-time capture, a plain closure's scratch seed or exception
outer state, a nested loop plan's exit rebuild, repack or hybrid
pre-extraction — asks one choke point, `actor_state_read_source`
(`synced_call.rs`), which answers the cell where `guarded_op_take_reads`
predicts a bracket and otherwise records the source it handed out
(`StaleStateUseKind::Read`) on the innermost marker, beside the `on:do:` and
seed uses above. The records travel outward with the marker's other uses and
the checkout that brackets reports each one as `StaleReadInBracketedLoop`,
whatever variable it read: the check compares the emitted source with the
checkout *outcome*, never with the push-time prediction that chose it, so a
drift between the two is caught too. A read in a whole-`State` frame nested
in the region (a `StateAcc` loop or fold) records nothing, since that frame's
state was seeded from the cell and the seed is checked itself. Stores are not
reads: a loop's pre-store, an `on:do:` pre-region `State` and a loop plan's
initial state go through the non-recording `state_var_to_store`, and a source
test (`tests/stale_read_sites.rs`) keeps every other codegen file off the
accessors the choke point is built from. The check is emitter-recorded, like
`UnbracketedGuardedOperator`, rather than a `verify()` tree walk: field reads
are AST-directed `Document`s inside opaque `Statement`/`ValueRef::Doc` nodes
and the bracket itself is a spliced pre-store `Document`, so the IR cannot see
either; typing every read into the IR would be a change of its own.

A new `VerifyError`, `UnbracketedGuardedOperator` (`verify.rs`, kept as a
separate variant for BT-4122), fails the build when a guarded operator
escapes the bracket: a hybrid checkout, an operator the push-time walk
missed (the walk/emitter drift alarm — field reads already lowered against
the entry `State` cannot be covered after the fact), or a nested need with
no enclosing marker. A missed sync point stays a build failure, never a
stale read.

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
method, and ADR 0137 does not add one. The next catch boundary or the
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

## 6. OTP fit

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

## 7. Cost and gates

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

## 8. Deletion inventory

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
with its PIN-BUGs in `bt3971test.bt` and `bt3939tier2inlined_test.bt`) and the
roughly 15 `DENY_SET` rows marked `stateful`, 11 of them for actors.
(`GroupByValue` is gone: ADR 0133 Phase 3, BT-3912, deleted that residual.)

Phase 2b (BT-4009) result: the generated corpus
(`scripts/gen-actor-closure-matrix.py`, 260 at-home and 6 away cells) agrees with
the rule for every shape it generates, on a BEAM. The corpus leaves out the
shapes the surviving validator rows still reject (an inlined `[...] value` that
writes a field or sends to `self` as a message argument; the local-threading
rows), so those rows stay until Phase 3 (BT-4010) deletes them.

Phase 3 (BT-4010) result: the §8 table is deleted (the BT-3936 guard with its
`closure_drops_state` plumbing, the BT-3985 super guard, the Tier 2 self-send
promotion, the list-op forwarded-state-change error, the actor and class-var
halves of BT-3922, the BT-3952 stale-read rejection, the BT-3998 alias check).
Of the oracle rows: the five `stateful` `DENY_SET` rows are deleted (dead: every
candidate shape is a local-threading producer, accepted before `DENY_SET`);
`EvaluatedBlockValue` and `InlineBlockLocalInLoop` stay (re-measured on a BEAM
with the model on: the tuple still leaks, the loop shape still fails `erlc`
with unbound `StateAcc`). BT-3990's residuals (getter self-send in a block
passed to a self-send, stored, and nested) are fixed, with `BUnit` cells in
`stdlib/test/bt3990stale_getter_test.bt`; BT-1325's alias tests are restored.

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

## 9. Phase detail

**Phase 0 (S, done: BT-4007, #4478).** A prototype behind a codegen flag,
the four gates, the oracle and the OTP checks as EUnit tests. Gate 3 failed
for bracketed self-sends, which led to the self-send exception in §2. The
results are in §7.

**Phase 1 (M, done: BT-4008, #4472).** `beamtalk_actor_state` (`install`,
`take`, `uninstall`, `installed`, `adapt`, `get`, `put`, `has`, `clear`,
`capture`, `protect`), used by every callback wrapper in `beamtalk_actor.erl`
and by `self_dispatch/2`, and the `actor_state_unreachable` error kind. It
was written for the convention before the amendment, where the callback takes
the cell as its new state (`take/0`) and `adapt/1` stores a native's
`NewState` into the cell.

**Phase 2a (L).** Align `beamtalk_actor_state`'s moduledoc and `adapt/1`
with the amended convention: the method's `{reply, R, NewState}` is the new
state. Add `call_foreign/3` (always on: it adopts the cell or raises `actor_state_conflict`), and route every
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
field write happened since the last sync (§7). The exit gate is a generated
actor-field-closure corpus (block shape × send kind × at home or away ×
position), compiled and run on a BEAM, with every case answering what the
rule in ADR 0137 says. Remove the flag and bump the module ABI marker (done in BT-4009: `class_var_abi` is the one marker, now 2).

**Phase 3 (M).** Delete the §8 table and whatever the oracle confirms
after Phase 2b. In `docs/beamtalk-language-features.md`, replace the BT-3922
field sentence and the BT-3952 paragraph under "What Works and What Doesn't",
the BT-3998 alias paragraph under "Explicit Async Cast", and the "MUST use !"
note under "Timer Lifecycle" with the rule in ADR 0137.

**Phase 4 (S).** The `actor-state-abroad` lint with
`@expect actor_state_abroad`, covering field writes, field reads and
self-sends in blocks passed to `Timer`, another actor or a non-direct class
method (ADR 0137, The rule), the amendment notes in ADR 0133 and ADR 0041,
and `docs/agents/expanded.md` § State-Threading Codegen.

#4452, #4455 and #4463 landed as stopgaps because each removed a silent wrong
answer. No new actor-state rejection should be added before Phase 3.
