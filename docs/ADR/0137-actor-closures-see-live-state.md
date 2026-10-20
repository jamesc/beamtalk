# ADR 0137: Actor Closures See the Actor's Live State

## Status
Proposed (2026-10-20)

Asks one question at the model level: during an actor's callback, should the
actor's state live in one place, a process-local cell owned by the dispatch
wrapper, instead of being threaded as a `State` value through the compiled
method and captured as a snapshot by every closure? If accepted, this ADR
**amends ADR 0133** (an escaping block may assign `self.field`; the write takes
effect at home and raises away), **amends ADR 0041** (an actor's Tier 1 and
Tier 2 blocks no longer carry the actor's fields), **amends ADR 0118** (an
actor self-send has no state effect to sequence), and removes the `State`
family from ADR 0111's `ThreadedIr`. ADR 0130 (class variables) is the
precedent and is unchanged. ADR 0131 (outer-local rebinds) is unchanged:
locals are not actor state.

Tracking: BT-3996. Decision gate: BT-3950.

## Context

### Problem statement

An actor method is compiled as a function of `State` that returns a new
`State`. Inside the method body the compiler threads versions (`State`,
`State1`, …) through every construct it lowers in place: loops, folds,
conditionals, `on:do:`. A block that the compiler does not lower in place is
a real closure (a Tier 1 `fun`, ADR 0041). It captures whichever `StateN` was
current when it was created, and a `fun` answers one value, so it has no way
to hand a new `State` back.

ADR 0133 Phase 3 has been finding the consequences one shape at a time. Each
fix is a compile-time rejection or a runtime guard, and each rejects some
correct code:

| Issue | Shape | Response |
|---|---|---|
| BT-3922 (#4426, merged) | A block that assigns `self.field` and escapes | Compile error, `Tier2BlockNoReturnChannel` (`WriteKind::ObjectState`) |
| BT-3936 (#4450, merged) | A self-send in a closure changes state; the change is lost | Runtime `stateful_block_dispatch` (`closure_self_dispatch_state_guard`) |
| BT-3948 (#4444, merged) | Inline-block local in a direct loop body in an actor; unbound `StateAcc` | `RESIDUAL_DENY_SET` row `InlineBlockLocalInLoop` (Actor only) |
| BT-3971 (#4442, merged) | Tier 2 body ending in `[...] value` answers the raw `{Value, State}` | `CodeGenError::UnsupportedFeature` |
| BT-3952 (#4452, open) | A block that reads `self.field` and is passed to a self-send or stored reads a stale value | Compile error (rejects every escaping read, right or wrong) |
| BT-3981 (#4448, open) | An evaluated `[...] value` in an actor leaks `{Value, State}` | `RESIDUAL_DENY_SET` row `EvaluatedBlockValue` (Actor only) |
| BT-3985 (#4455, open) | Binary-selector and `super` sends to `self` drop the callee's state change | Extends the BT-3936 guard |
| BT-3988 | `on:do:` in a closure whose body self-sends answers `{Result, State}` | Open |
| BT-3990 | A getter self-send in a block passed to a self-send reads the creation-time state | Open, three residual entries |
| BT-3910, BT-3993 | `ensure:` cleanups see the entry state after a `^` | Compile errors (`report_nlr_out_of_ensure`) |

Most of these trace to the one fact above. Reads go stale (BT-3952, BT-3990).
Writes and self-sends are lost (BT-3936, BT-3985, BT-3988). Results leak the
`{Value, State}` carrier (BT-3971, BT-3981). The `ensure:` rules mix an
actor-state half with a locals half.

PR #4447 tried a narrow fix: keep `State` threaded, and also publish it to the
process dictionary on every field write so closures could read it live. The
maintainer closed it on 2026-10-20 as "a publish on every actor field write
that must stay complete forever (`fieldAt:` was already a gap)". Its review
found the general failure: every lowering that *discards* a callee's
`NewState` (a method-level self-cast, an aliased self-send, an error arm) left
the published copy ahead of the real one. That is the two-representations
problem ADR 0130 removed for class variables, rebuilt for actors.

The maintainer's direction for this ADR (2026-10-20): "I'm all for simplicity
of model — if it fits OTP usage, then let's go." OTP fit is the deciding
criterion.

### Current state, measured

A probe package compiled with a debug build of `main` from 2026-10-20
(`LiveProbe`, six BUnit tests, run with `beamtalk test`; throwaway, not
committed) gives:

| Method | Correct answer | `main` answers |
|---|---|---|
| `self.x := 5. other := self. other x` | 5 | **0** (silent) |
| `other := self. other setX: 7. self.x` | 7 | **0** (silent; the write is lost when the method returns) |
| `self bump!. self.x`, then `p x` from outside | 1 | **0** forever (silent; `generate_self_cast_send` runs `bump` in-process and discards its state) |
| `b := [self.x]. self.x := 9. b value` | 9 | **0** (silent; BT-3952, #4452 makes it a compile error) |
| `self.x := 3. self apply: [self x]` | 3 | 3 |
| `self apply: [self bump]. self.x` | 1 | raises `stateful_block_dispatch` (BT-3936) |

The first three rows are not on any list above. They come from the same
root: there is already a second copy of the state. Every generated callback
stashes the entry `State` in `'$bt_actor_state'` (`pdict_stash_preamble` in
`gen_server/callbacks.rs`; `dispatch_call/4` and friends in
`beamtalk_actor.erl`) so that an aliased self-send (`sync_send/3`'s Layer 1
fast path, `self_dispatch/2`) does not deadlock. That copy is never updated by
the method's own writes, and what `self_dispatch/2` writes into it is never
read back by the method. So today there are two representations of actor
state during a callback, the lexical `StateN` and the stash, and neither is
complete.

### How actor state flows today

- `handle_call`/`handle_cast`/`handle_info`/`handle_continue`/`init`, generated
  per actor in `gen_server/callbacks.rs` and shared in `beamtalk_actor.erl`,
  stash `State` in `'$bt_actor_state'`, call `safe_dispatch/3`, strip the
  codegen-internal `__local__<name>` keys (`strip_local_temps/1`) from the
  returned `NewState`, restore the stash in an `after`, and return
  `{reply, R, NewState}`.
- `safe_dispatch/4` catches every error and returns the **pre-call** `State`,
  so an escaping error discards the invocation's writes.
- A compiled method is `dispatch(Selector, Args, Self, State)` answering
  `{reply, Result, NewState}`. A self-send calls it and rebinds `StateN+1`
  from the reply; ADR 0118's preludes exist to sequence that rebind inside
  expressions.
- Actor-method locals threaded through constructs ride in the same map as
  `__local__<name>` keys, which is why the boundary strips them and why the
  BT-3936 guard compares twice.
- An actor's `on:do:` continues with the `State` from before the protected
  block when its handler runs.
- Nothing hibernates (`hibernate` and `format_status` occur nowhere in
  `runtime/` or `crates/beamtalk-codegen/`). `code_change/3` delegates to
  `beamtalk_hot_reload:code_change/3`.

### Constraints

- **OTP fit.** Between callbacks the `gen_server` state is the actor's state,
  and the only one. `sys:get_state/1`, `sys:replace_state/2`, `code_change/3`,
  crash and restart, and supervisor semantics must be exactly what an Erlang
  programmer expects.
- No silent wrong answer (ADR 0132's first goal, inherited by ADR 0133):
  every shape is correct, a compile error, or a structured runtime error.
- An identifier means the same thing in every boot context (principles §4).
  A block's behaviour may depend on *which actor's invocation* it runs in, the
  way a class variable's does under ADR 0130, but not on REPL versus release.
- `ThreadedIr` and its verifier stay the home of state invariants; no new
  ad-hoc checks at call sites (CLAUDE.md).
- Hot upgrade (ADR 0125) must not silently mix calling conventions.

## Decision

### 1. One home for actor state during a callback

During any callback of a compiled actor (`init`, `handle_call`, `handle_cast`,
`handle_info`, `handle_continue`, `terminate`), the actor's state lives in
exactly one place: the process-dictionary cell `'$bt_actor_state'`, the slot
every callback already installs. There is no lexical `StateN`, no published
copy, and no `State` in a block's environment.

The callback wrapper installs the cell from the `gen_server` state on entry,
runs the method, and **reads the cell once on exit and returns it** as the new
`gen_server` state:

```erlang
dispatch_call(Selector, Args, From, State) ->
    Old = beamtalk_actor_state:install(State),       %% put, returning the previous value
    try Module:dispatch(Selector, Args, Self) of     %% methods take no State, return no State
        Result -> {reply, encode(From, Result), beamtalk_actor_state:take()}
    catch
        Class:Reason:ST -> {reply, encode(From, {error, ...}), State}  %% escaping error: pre-call state
    after
        beamtalk_actor_state:uninstall(Old)          %% restore or erase, as restore_dispatch_pdict/1 does today
    end.
```

That is the whole OTP story. The cell is a register that exists only while a
callback runs. The value the callback returns is the state, as in any
`gen_server`, and nothing outside the callback can observe the cell.

### 2. Access goes through one runtime module

A new runtime module, `beamtalk_actor_state`, owns the key and every access,
as `beamtalk_class_vars` does for class variables. Codegen never builds the
key.

| Beamtalk (actor instance method) | Emitted |
|---|---|
| `self.x` | `beamtalk_actor_state:get(Self, 'x')` |
| `self.x := v` | `beamtalk_actor_state:put(Self, 'x', V)` |
| `self fieldAt: #x`, `fieldAt:put:`, `clearField:`, `hasField:` | the same helpers |
| `self foo: a` (any form: unary, keyword, binary, `super`, sealed, aliased) | `Module:dispatch('foo:', [A], Self)`, answering a bare value |

The reflective selectors use the same cell, so the `fieldAt:` gap that sank
#4447 cannot exist: there is no second copy for it to miss. Phase 0 decides
whether hot paths inline the helpers as `erlang:get/1` plus `maps:get/2` (the
spike below says reads cost about the same either way).

### 3. At home and away

**At home** means: the block's `Self` is the actor whose callback is running
in this process. The test is `Self#beamtalk_object.pid =:= self()` with the
cell present. ADR 0130 rejected pid equality for classes because a class
process is restarted under the same identity; an actor restarted by its
supervisor is a new process with a new pid and a fresh state, so a closure
from the old incarnation is rightly not at home in the new one.

- **At home, a closure reads and writes the live state.** This covers every
  block the actor creates and runs in its own process: blocks passed to its own
  methods, stored closures invoked later in the same callback or in a later
  callback, blocks passed to direct-called stdlib code, sort blocks, `ensure:`
  cleanups and `on:do:` handlers. A self-send inside such a block is a plain
  in-process dispatch on the cell, so its writes stay.
- **Away, a closure reads the state it captured at creation and cannot write.**
  A block carried to another actor, a `Future`, a `Timer`, another node, or run
  after the callback ended outside any callback of its actor, sees the field
  values from when it was created, which is Erlang's rule for a `fun` and
  ADR 0130 §5's rule for class variables. Creation of a block literal that
  reads a field binds `beamtalk_actor_state:capture(Self, Outer)` (one `get`),
  consulted only away from home. A field write away raises:

```erlang
#beamtalk_error{kind = actor_state_unreachable, class = 'Counter', selector = 'bump',
                hint = <<"A block that writes Counter's state ran outside Counter's own process "
                         "(it was sent to another actor, a Timer or a Future, or run after the "
                         "method returned). Send Counter a message instead: `counter bump!`.">>}
```

- **Away, a self-send is an ordinary message send** to the actor's pid, which
  is what `self` is to every other process. If the actor is blocked in a
  synchronous call that carried the block away, the cycle is the existing
  A→B→A case, reported by `check_call_stack/2` (`deadlock_detected`) or by the
  call timeout. Today such a send instead runs the actor's method in the
  foreign process against the captured snapshot and drops its writes.

A compiler lint, `actor-state-abroad`, mirrors `class-state-abroad`: a block
literal that writes a field and is passed to a send whose receiver is
statically not `self`, or to `Timer`/`Future` constructors, gets a warning at
the block. It is a lint, not a guarantee; the runtime error is the guarantee.

### 4. Rollback: boundaries discard, nothing else does

This keeps today's observable semantics and states them as one rule, the same
rule as ADR 0130 §4:

- **The invocation boundary.** An error that escapes the callback discards
  every write it made: the wrapper returns the pre-call `State` (unchanged from
  `safe_dispatch/4` today).
- **The catch boundary.** `on:do:` and every runtime catcher that runs a
  Beamtalk block (`Result tryDo:`, `protect/1`) snapshot the cell on entry and
  restore it when an error crosses the catch, before the handler runs. So a
  failed try body's writes do not persist, as today. The `OnDoCatch` node
  already emits ADR 0130's class-variable snapshot and restore; it gains the
  actor half in the same node, and the verifier's
  `CatchWithoutClassVarRestore` becomes `CatchWithoutStateRestore`, covering
  both. A `$bt_nlr` throw is not an error and restores nothing.
- **`ensure:` is not a boundary.** The cleanup sees the live state, including
  writes the protected block made before a `^` or an error. That is the actor
  half of BT-3910/BT-3993, fixed by construction. The locals half is
  unaffected.
- **A non-local return** keeps the writes made before it. The actor-shaped NLR
  4-tuple `{'$bt_nlr', Tok, Val, State}` loses its `State` slot.
- **A raising self-send** does not restore on its own. Today a self-send that
  raises returns its pre-call `State`, but that is only observable when the
  error is caught, and every catch already restores to the start of its
  protected region, which is earlier. ADR 0130 rejected per-send rollback
  (its Alternative C) for the same reason.

The difference from #4447 is structural. #4447 needed a matching "un-publish"
at every place that discards a state change, and the places were open-ended.
Here nothing is published. A discard happens only at the two boundary kinds,
each owned by one node or wrapper, and the verifier checks the catch boundary.

### 5. Re-entrant and self-cast sends

- **Direct and aliased self-sends** (`self foo`, `other := self. other foo`)
  both dispatch in-process on the cell. The aliased form starts from the live
  state, not the callback's entry state, and its writes persist. Probe rows 1
  and 2 flip to 5 and 7.
- **`self foo!` at method level becomes a real cast to the actor's own
  mailbox**, handled after the current message, as `!` means everywhere else
  (principles §7) and as it already means inside a block. Today it runs `foo`
  synchronously and throws its state change away (probe row 3). Keeping
  "run now, discard" would need a snapshot and restore around every self-cast
  for a behaviour nobody wants.

### 6. What is not changed

- Class variables (ADR 0130), value-type `Self` threading (ADR 0120) and
  local-variable threading (ADR 0131, ADR 0133) are unchanged. Locals in actor
  methods stop riding in the actor map as `__local__` keys and thread as they
  do in class methods, so actor methods lose their own frame mode.
- ADR 0133's rule for **captured locals** is unchanged. Only the field half is
  amended: an escaping block may assign `self.field`, effective at home and
  raising away.
- The `gen_server` wire protocol, `sync_send`/`cast_send`, supervision,
  `stopWith:`/`continueWith:` and observability events are unchanged.

### REPL session

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

On `main` today `self twice: [self add: 5]` raises `stateful_block_dispatch`
(BT-3936), and with that line removed `reader value` answers 0 (BT-3952).

### Error example

```beamtalk
Actor subclass: Counter
  state: n = 0
  bump => self.n := self.n + 1
  startLate => Timer after: 100 do: [self.n := self.n + 1]   // runs in the Timer's process
  startWell => Timer after: 100 do: [self bump!]              // a message: runs in Counter
```

`startLate` compiles with an `actor-state-abroad` warning at the block. When
the timer fires, the write raises in the timer's process:

```text
actor_state_unreachable: Counter's state cannot be written from this process
  hint: A block that writes Counter's state ran outside Counter's own process (it was sent
        to another actor, a Timer or a Future, or run after the method returned). Send
        Counter a message instead: `counter bump!`.
```

Today the same block is a compile error (BT-3922, "`self.n` is object state").

## OTP fit

This section takes each piece of OTP machinery in turn. The claims marked
*spike* were checked with a throwaway `gen_server` (`bt3996_otp.erl`, OTP 28,
not committed) that installs the cell on entry, mutates it through closures,
and returns it on exit.

| Concern | Behaviour | Evidence |
|---|---|---|
| Source of truth | The value the callback returns. The cell is read once at exit and returned; it is erased or restored in `after`. | By construction |
| `sys:get_state/1`, `sys:replace_state/2` | System messages are handled only between callbacks, so they always see the returned state, never the cell. A `get_state` sent during a 200 ms callback waited for it and saw the committed value. | *spike* |
| `format_status/1` | Not implemented today. Formats the `gen_server` state, which is complete between callbacks. A crash report produced inside a callback already shows the pre-call state. | By construction |
| `code_change/3` | Runs while the process is suspended between callbacks, so no cell exists; it receives the `gen_server` state as today. | *spike* (`sys:suspend`, `change_code`, `resume`) |
| Crash | The cell dies with the process. A supervisor restart is a new process with `init`'s state and no cell. A callback that crashes without killing the process (the normal `safe_dispatch` path) leaves no cell, because `after` erases it. | *spike* |
| `hibernate` | Unused today. A `{reply, R, S, hibernate}` happens after the callback has returned and erased the cell; the hibernated process (`gen_server:loop_hibernate/4` on OTP 28) kept its state and had no cell on waking. `erlang:hibernate/3` keeps the process dictionary in any case. | *spike* |
| Supervisors | No change: they see processes and exit reasons, not state. | |
| Distribution (ADR 0126) | A block sent to another node is away by definition: reads are the capture, writes raise. | §3 |
| Re-entrancy | Already the case today for aliased self-sends; the cell is saved and restored around nested callbacks (`restore_dispatch_pdict/1`), unchanged. | Existing |
| Observability | Mid-callback, `process_info(Pid, dictionary)` shows the live state; today it shows the stale entry state. | |

Is it idiomatic? Erlang style discourages the process dictionary for data that
should be passed as arguments, and this ADR does put state there. Two things
make it acceptable. The cell is callback-scoped working storage, not hidden
state: outside a callback it does not exist, and the `gen_server` contract
(state in, state out) holds exactly. OTP itself keeps process-scoped context
there (`'$ancestors'`, `'$initial_call'`, logger metadata, the `rand` seed),
and Beamtalk already does (`'$bt_actor_state'`, `'$bt_call_stack'`, ADR 0130's
class-variable key). What an Erlang reader loses is the lexical `StateN` chain
in generated Core Erlang. A method becomes "read the field, write the field",
which is what the Beamtalk source says.

## Cost

### Spike numbers

`bt3996_bench.erl` (throwaway, not committed) compares a tail-recursive loop
over a threaded map with the same loop over the cell, 2 million iterations,
11 rounds, OTP 28 on an 18-core Mac. **The machine was heavily loaded** (load
average 67 to 88 from other sessions), so treat these as order of magnitude.
Medians in ns per iteration, with the spread across two 8-field runs and one
40-field run:

| Operation | Threaded `State` | Cell, inline `get` | Cell, remote helper |
|---|---|---|---|
| One field read | 3.4 to 3.5 | 4.5 to 5.3 | 5.5 to 6.6 |
| `self.f := self.f + 1` | 45 to 69 | 20 to 42 | 41 to 54 |
| Ten reads and three writes (ADR 0130's gate-3 body) | 21 to 75 | 104 to 278 | 119 to 318 |
| `on:do:` around one write, no raise | 45 to 89 | 20 to 41 (snapshot) | |
| Per-callback install/try/after vs plain reply | 5 to 11 | 30 to 37 | |
| Reference: one `gen_server:call` round trip | | | 695 to 727 |

Reading this honestly:

- A read costs one to two nanoseconds more.
- A single write in a loop is no slower. Both forms allocate a new map per
  write, and that dominates.
- A field-heavy straight-line body is **2 to 5 times slower**, because the
  threaded form keeps the map in a register and the cell form goes to the
  process dictionary for every access. This is the same shape ADR 0130
  measured for class variables (5 to 7 times per write in isolation, 1.6 to
  1.75 times end to end, inside its 2x gate).
- The per-callback cost is already paid. Every callback installs and restores
  `'$bt_actor_state'` today; the only addition is one `get` at exit.
- Some costs go away: the `{reply, R, NewState}` tuple and its `case` on every
  self-send, the BT-3936 guard's `=:=` (and second `strip_local_temps/1`
  compare when it fails), `strip_local_temps/1` at every callback exit, the
  `__local__` keys in the actor map, and `on:do:`'s state threading.

### Measurement plan (Phase 0 gates)

Use `runtime/perf/self_send_bench` and `runtime/perf/bench_method_threading.erl`
with the method in `docs/development/benchmarks.md`: at least 7 interleaved
rounds per side on an idle machine, medians with min and max.

1. **Field loop.** An actor method looping over ten field reads and three
   writes per iteration, end to end through a `gen_server:call`:
   `median_after <= 2 * median_today`. If it fails, inline the helpers and
   measure again; if that fails, record the number and decide at the gate.
2. **Self-send.** Open and sealed actor self-sends, at top level and inside a
   `do:` arm: no regression beyond noise. Expected to improve.
3. **Round trip.** A `gen_server:call` to a method with one write:
   `median_after <= 1.05 * median_today`.
4. **Block creation.** A loop that creates a field-reading block per iteration
   (the capture `get`): recorded, not gated.

## What it deletes and what stays

Classified from `main` at `1df39217f` and the open PRs as of 2026-10-20.
"Deleted" means the shape is wrong only because a closure's `State` is a stale
snapshot, or the code exists only to carry `State`. "Verify" means the trigger
mixes actor state and locals; Phase 0 runs the 733-shape oracle (BT-3950) with
the cell to settle it.

### Deleted

| Item | Where |
|---|---|
| BT-3936 closure self-send guard: `closure_self_dispatch_state_guard`, `CLOSURE_SELF_SEND_STATE_CHANGE_HINT`, the `closure_drops_state` flag | `dispatch_codegen.rs`; `generator/mod.rs`, `body_state.rs` |
| BT-3985's extension of it (`in_state_dropping_closure` on the `super` path) and the binary-selector state drop | #4455 (open); `generate_super_send` |
| BT-3922 field half: `WriteKind::ObjectState`, `outer_state_writes`, `OuterWriteWalker::state_write` / `is_captured_self_field_write`, the field half of `Walker::escape_write_of` and the object-state hint in `report_escape` | `semantic_analysis/block_facts.rs`, `validators/local_threading.rs` |
| BT-3952's stale-read rejection: `outer_state_reads`, `outer_self_send`, `may_change_actor_state`, `block_may_change_actor_state`, `Walker::report_stale_read`, `report_stale_self_send`, `Escape::{LaterUse, ActorSelfCall, SelfCascade, CollectionResult}` | #4452 (open) |
| "cannot assign to field '{name}' inside a stored closure" | `semantic_analysis/block_analyzer.rs` |
| Actor-state `stateful_block_dispatch` for a non-literal callable forwarded to a list op ("block changed actor state") | `control_flow/list_ops/mod.rs` |
| Unreachable Tier 2 promotion left by BT-3922: `detect_tier2_self_send`, `scan_class_for_tier2_blocks`, `tier2_method_info`, `tier2_block_params` | codegen |
| The `State` family: `VersionPrefix::State`, its `StateAcc` rendering in actor frames, the actor frame mode, `ActorStateInClassMethod`, the `State` rows of `ConstructFamilyDropped`, `FoldFamilyNotCarried` and `StateEffectEscapesExpression` | `threaded_ir/`, `generator/version.rs` |
| ADR 0118's actor self-send preludes and the discarding self-dispatch fallback (`generate_discarding_self_dispatch`) | `dispatch_codegen.rs`, `threaded_expr.rs` |
| `{reply, R, NewState}` from compiled methods; `unwrap_dispatch_result/1`'s state arms; the actor NLR 4-tuple's `State` slot and its arms in `safe_dispatch/4` and `on_do_catch_preamble` | runtime and codegen |
| `strip_local_temps/1`, `is_local_temp_key/1` and the `__local__` keys in actor state | `beamtalk_actor.erl`, `callbacks.rs` |
| The method-level self-cast that discards state (`generate_self_cast_send`) | `dispatch_codegen.rs` |
| PIN-BUG `testPureIfNoneAfterFoldPinBug` (BT-3952) flips to 4; `testSelfSendStateChangeThrough*Raises` (BT-3936) flip to the correct values | `stdlib/test/actor_self_send_hom_test.bt` |
| BT-3988 and BT-3990 close as fixed by construction | Linear |

`CodeGenContext::Actor` appears in 44 branches across 18 non-test codegen
files. Phase 3 deletes or merges most of them into the class-method path; the
exact line count is a Phase 0 deliverable rather than a guess here.

### Verify in Phase 0 (expected to go)

| Item | Why it may go |
|---|---|
| `RESIDUAL_DENY_SET` `InlineBlockLocalInLoop` (BT-3948, Actor only) | The failure is an actor loop frame with no `StateAcc`; actor frames go away |
| `RESIDUAL_DENY_SET` `EvaluatedBlockValue` (BT-3981, #4448, Actor only) | Its field-write and self-send triggers go; its local-bind trigger depends on the locals lowering |
| The `stateful: true` rows of `DENY_SET` (about 15 of 58, 11 of them Actor) | "A self-send or field write in the outermost block" changes how locals are lost only because of the shared map |
| `RESIDUAL_DENY_SET` `GroupByValue` | Its triggers include field writes and self-sends; the leak is the `{Map, StateAcc}` carrier |
| PIN-BUG BT-3981 in `stdlib/test/bt3971test.bt` and `bt3939tier2inlined_test.bt` | As for `EvaluatedBlockValue` |

### Stays

| Item | Why |
|---|---|
| ADR 0133's captured-local rule: `Tier2BlockNoReturnChannel` for `WriteKind::Local`, `Escape::Ffi` (BT-3917), `Passed`, `Cascade` | Locals are not actor state |
| `ALLOW_SET`, `DENY_SET` (non-`stateful` rows), `RIGHT_STATEMENTS`, `RESIDUAL_DENY_SET` `MatchArmWrite` | Outer-local threading, retired by ADR 0133 Phase 3 on its own schedule |
| `report_nlr_out_of_ensure`, `nlr_dropped_local` (BT-3910, BT-3993, #4454) | The locals half of the `ensure:` rule |
| `reject_last_statement_block_value` (BT-3971), `reject_loop_rebind_of_body_local` (BT-3939) | Tier 2 local carrier |
| Tier 2 `stateful_block_dispatch` sites in `intrinsics.rs`, `while_loops.rs`, `exception_handling.rs`; `beamtalk_collection:refuse_writing_fold/3` | Tier 2 locals protocol; the field-write half of `refuse_writing_fold` goes |
| `ClassStateAbroad` | Class variables; gains the `actor-state-abroad` sibling |
| BT-3956 value-type `do:` pins | Value types |
| The `Local` and `SelfVt` families, `NlrThrowWithoutSlot`, `UnboundVersion`, `NonLinearVersion`, `HeldRebindNotIdentity` | Not actor state |

## Prior Art

**Beamtalk class variables (ADR 0130).** The same design, accepted and
implemented two weeks ago: one home in the class process during an
invocation, live reads and writes at home, a creation-time capture away,
boundary rollback at the invocation and at `on:do:`. It deleted a threading
family, the commit map and four diagnostics, and flipped 21 known-wrong pinned
tests. The end-to-end cost was 1.6 to 1.75 times on a field-heavy loop.

**Erlang `gen_server`.** State in, state out per callback. This ADR keeps that
contract exactly and changes only how a callback's body is compiled.
Hand-written Erlang often uses a record in a local and a `#state{}` update per
step; nothing outside the callback can tell the two apart.

**Elixir `Agent` and `GenServer`.** Same contract. Elixir code that wants
callback-scoped context uses `Process.put/2` (Ecto's repository context,
Logger metadata), which is the pattern here.

**Pharo and Squeak.** Instance variables live in the object; a block reads and
writes them through its home receiver wherever it runs. This ADR gives that
meaning at home, which is where nearly every block runs, and Erlang's meaning
away.

**Akka (classic actors).** Actor fields are plain mutable fields; closures that
run on the actor's thread see them live, and the documentation warns that a
closure run on another thread (a `Future` callback) must not touch them. Akka
leaves the off-thread case undefined. Here it reads the capture and a write
raises.

**Pony.** Reference capabilities forbid sending a closure that can write actor
state to another actor. Beamtalk enforces the same at run time, with a lint.

## User Impact

**Newcomer.** Blocks inside an actor behave like blocks: a field read sees the
latest value, a write sticks, a self-send works. Most of the "this block cannot
…" errors in actors go away. One error remains, for a block that writes a field
from another process, and its hint names the fix (send a message).

**Smalltalk developer.** Instance variables behave as in Pharo inside the
actor, including through stored blocks and the template-method pattern
through blocks. The remaining difference, writes from another process, is the
actor model's, not the compiler's.

**Erlang/BEAM developer.** Between callbacks nothing changes: the state is the
`gen_server` state, inspectable with `sys:get_state/1`, migrated by
`code_change/3`. Inside a callback the state is in the process dictionary,
which they will not like on sight and will recognise from ADR 0130. Generated
Core Erlang for an actor method gets shorter: no `StateN` chain, no
`{reply, R, State}` per self-send. A hand-written native actor method keeps
its `{reply, R, NewState}` shape through an adapter in the runtime (Phase 1).

**Production operator.** Mid-callback, the process dictionary shows the live
state rather than a stale copy. A field-heavy actor method may run up to 2x
slower in its body, gated in Phase 0; a self-send gets cheaper. Upgrading
across the release that changes the calling convention needs a node restart,
as ADR 0130's did.

**Tooling developer.** The LSP loses a family of actor-specific diagnostics and
gains one lint. Static analysis no longer needs to know whether a block is
inlined to know whether a field read is current.

## Steelman Analysis

### A. Status quo: rejections plus residuals (rejected)

- 🏭 **Operator**: "Every shape that would be wrong is now a compile error or a
  structured runtime error. That is the safety property. Rejections are
  cheap."
- ⚙️ **BEAM veteran**: "Threaded `State` is pure, local, debuggable Core
  Erlang. Don't trade that for the process dictionary."
- 🎨 **Language designer**: "The rejected shapes are rare. #4447's closing note
  says the BT-3952 shape has no occurrence in `stdlib/src` or the examples."

The safety claim is not true today: the probe finds three silent wrong answers
on `main` (aliased read, aliased write, method-level self-cast) that no
rejection covers, and each new phase of ADR 0133 has found more. The list of
rejections grows with the product of block shape, send kind and position, the
same non-convergence ADR 0130 documented for class variables. Rarity of each
shape is the argument for each rejection, not for the set.

### B. Publish on write (#4447) (rejected)

- 🧑‍💻 **Newcomer**: "It fixed the reads I hit, and it was small."
- ⚙️ **BEAM veteran**: "The method body stays pure threading; only closures
  pay."

It keeps two representations and makes them agree by publishing at every
write and un-publishing at every discard. The discards are open-ended (the
review found the self-cast and the aliased self-send; `fieldAt:` was already
missing), and each one is a per-call-site reconciliation, the pattern CLAUDE.md
forbids for state-threading fixes. It also leaves writes and self-sends in
closures broken.

### C. Full threading through closures (rejected)

- 🎩 **Smalltalk purist**: "Make every actor block a Tier 2 block that takes and
  returns `State`; then nothing is a snapshot."
- 🎨 **Language designer**: "One mechanism, already in the compiler, extended to
  its limit."

This is ADR 0132's design applied to `State`, which ADR 0133 superseded: a
second entry per block-taking method, a runtime resolution walk, and no answer
for blocks that run in another process or after the method returned. It costs
more than the cell for every user higher-order method and fixes less.

### D. The live cell (chosen)

- 🧑‍💻 **Newcomer**: "Fields just work in blocks."
- 🎩 **Smalltalk purist**: "Instance variables mean what they mean in Pharo,
  inside the actor."
- ⚙️ **BEAM veteran**: "Between callbacks it is a textbook `gen_server`. Inside,
  it is the same trick the class process already uses."
- 🏭 **Operator**: "One representation; the rollback points are the ones I
  already know: escaping error and `on:do:`."
- 🎨 **Language designer**: "Deletes a threading family and a list of
  per-shape rules instead of adding to it."

### E. Cell for closures only, threaded `State` in method bodies (rejected)

A hybrid: method bodies keep the fast threaded form and sync into the cell
before creating or calling a closure, reading back after. It is B with
coarser publish points, and it has B's problem: every boundary between the two
forms is a reconciliation site.

### Tension points

- BEAM veterans weigh the process dictionary heavily; operators and
  newcomers weigh "no silent wrong answers, fewer errors". The OTP-fit table
  above is the answer to the first group: the `gen_server` contract is intact.
- Performance-minded users will see a 2 to 5x slowdown on field-heavy
  straight-line bodies in isolation. The Phase 0 gate bounds it end to end.
- Language designers who count mechanisms prefer D; those who count
  process-dictionary keys prefer A. ADR 0130 already chose D's side for class
  variables, and two models of mutable state would be harder to teach than one.

## Alternatives Considered

### A. Status quo with residuals
Described above. Rejected: it does not converge and it has silent wrong
answers today.

### B. Publish on write (#4447)
Described above. Rejected: two representations kept in step by per-site
publishing.

### C. Full threading through closures
Described above. Rejected: superseded by ADR 0133 for the same reasons.

### E. Hybrid cell
Described above. Rejected: B with fewer sync points, same failure mode.

### F. Raise on any field access away from home
Like ADR 0130's Alternative R: a block that reads a field away raises instead
of reading the capture. Rejected for the same reason ADR 0130 gave: the
common synchronous case (a block passed to another actor's method while this
actor waits) would raise where the captured value is usually right, and
Erlang's own rule for a `fun` is the capture.

### G. Keep `self foo!` as "run now, discard"
Rejected: it needs a snapshot and restore per self-cast to keep a behaviour
that loses writes silently today. A real cast is what `!` means.

## Consequences

### Positive
- One model of mutable state for actors and classes: live at home, captured
  away, discarded at boundaries.
- The actor-state family of rejections, guards and PIN-BUGs is deleted rather
  than extended, and BT-3952, BT-3985, BT-3988 and BT-3990 close by
  construction. The three silent wrong answers found by the probe are fixed.
- `ThreadedIr` loses the `State` family; actor methods lower like class
  methods for locals, so the per-context frame modes shrink by one.
- Self-sends get cheaper: no state tuple, no guard.
- The reflective selectors cannot fall out of step, because there is nothing to
  keep in step.

### Negative
- Field-heavy straight-line method bodies get slower, 2 to 5x in isolation on
  a loaded machine; gated at 2x end to end.
- A calling-convention change across every compiled actor. Modules compiled
  before it are refused at load with `abi_mismatch`; crossing the release
  needs a node restart (ADR 0125).
- Generated Core Erlang reads the process dictionary; reasoning about a method
  body as a pure function of `State` is gone.
- `self foo!` at method level changes from "run now, discard" to "run after
  this message". Code that relied on the synchronous side effects of a
  self-cast (other than state, which was discarded) sees them later.
- Away-from-home field reads are still creation-time captures, so a `Timer` or
  `Future` body reads old values. This is Erlang's rule and today's behaviour,
  and the lint names it, but it is not "live everywhere".
- It reverses part of four merged or in-flight ADR 0133 Phase 3 changes
  (BT-3922, BT-3936, #4452, #4455).

### Neutral
- Locals, value types and class variables are unchanged.
- The wire protocol, supervision and `stopWith:`/`continueWith:` are
  unchanged.
- Native (hand-written Erlang) actors keep their shape through an adapter.

## Implementation

**Phase 0: prove it (S).** Behind a codegen flag, lower actor field access and
self-sends to the cell for the stdlib, BUnit and REPL corpora. Run the four
cost gates above. Run the 733-shape oracle (BT-3950) with the flag on and
report which rows in "Verify in Phase 0" go, and the line count the deletion
would remove. Turn the OTP checks from the spike (sys, `code_change`, crash,
hibernate, closures) into permanent EUnit tests. Gate: all four cost gates,
and no shape that answers right today answers wrong with the flag on.

**Phase 1: runtime (M).** `beamtalk_actor_state` (`install`, `take`,
`uninstall`, `get`, `put`, `has`, `clear`, `capture`, `snapshot`, `restore`,
`protect`, and the home test), used by every callback wrapper in
`beamtalk_actor.erl` and by `self_dispatch/2`. `protect/1` joins ADR 0130's
for runtime catchers. An adapter keeps native actor methods and instance-side
extension funs (ADR 0066) that answer `{reply, R, NewState}` working by writing
`NewState` into the cell. New error kind `actor_state_unreachable` in
`beamtalk_error`.

**Phase 2: codegen (L).** Methods become `dispatch(Selector, Args, Self)`
returning a bare value; field reads and writes and the reflective selectors call
the helpers; every self-send form calls `dispatch/3` directly; generated
callbacks read the cell on exit; field-reading block literals bind the
capture; `OnDoCatch` snapshots and restores both kinds of state, checked by
`CatchWithoutStateRestore`; `self foo!` lowers to a real cast. Bump the module
ABI marker so mixed modules are refused. Snapshot tests are regenerated.

**Phase 3: delete (L).** Everything in the "Deleted" table and whatever Phase 0
confirmed in "Verify". PIN-BUGs flip. `docs/beamtalk-language-features.md`
loses the "field mutations require control-flow context" limits (lines near
828 and 2802 today) and gains the at-home/away rule.

**Phase 4: lint and docs (S).** The `actor-state-abroad` lint with
`@expect actor_state_abroad`; amendments recorded in ADR 0133, ADR 0041 and
ADR 0118; `docs/agents/expanded.md` § State-Threading Codegen updated to three
families (`Local`, `SelfVt`, and the in-place catch boundary).

**Sequencing with ADR 0133 Phase 3.** While this ADR is Proposed, the open
actor-state PRs (#4452 BT-3952, #4455 BT-3985) can land as stopgaps, since
each removes a silent wrong answer, but no new actor-state rejection should be
added. If this ADR is accepted, BT-3988 and BT-3990 are closed as superseded
and BT-3947's actor-only residual work is re-scoped after Phase 0.

Affected components: runtime (`beamtalk_actor`, new `beamtalk_actor_state`,
`beamtalk_error`, `beamtalk_hot_reload` ABI check), codegen (`gen_server/`,
`dispatch_codegen.rs`, `expressions.rs`, `threaded_ir/`, `control_flow/`),
semantic analysis (`block_facts.rs`, `local_threading.rs`,
`block_analyzer.rs`), lint, docs. Parser and REPL protocol are unchanged.

## Migration Path

- **Compiled code.** One ABI break for compiled actors; modules from before it
  are refused at load. Recompile and restart the node.
- **Source code.** Nothing that compiles today stops compiling. Shapes that are
  rejected today start compiling and answer the Pharo answer. Shapes that
  answer silently wrong today (the probe's rows 1 to 4) start answering right.
  A block that writes a field from another process raises instead of being
  rejected at compile time or dropping the write.
- **Self-casts.** A method that relied on `self foo!` running before the next
  statement gets the cast semantics. The fix is `self foo`.

## Open Questions

1. Does Phase 0 show the inline lowering is needed, or is the helper call fast
   enough? The spike says reads are equal either way and writes are close.
2. Away from home, should a self-send raise `actor_state_unreachable` instead of
   becoming a message send? The message send is the Erlang meaning; raising is
   stricter and avoids the synchronous-carry deadlock reaching the timeout.
3. Should ADR 0133's rule for escaping blocks that assign **class variables**
   get the same amendment? ADR 0130 already makes such writes live at home and
   raising away at run time; BT-3922 rejects them at compile time.

## References

- Related issues: BT-3996 (this ADR), BT-3950 (gate), BT-3901 (ADR 0133 Epic),
  BT-3922, BT-3936, BT-3948, BT-3952, BT-3971, BT-3981, BT-3985, BT-3988,
  BT-3990, BT-3910, BT-3993, BT-3947, BT-3913
- Related PRs: #4447 (closed, publish on write), #4452, #4448, #4454, #4455
- Related ADRs: ADR 0130 (precedent), ADR 0133 (amended), ADR 0041 (amended),
  ADR 0118 (amended), ADR 0111, ADR 0131, ADR 0120, ADR 0125, ADR 0126,
  ADR 0129
- Documentation: `docs/beamtalk-principles.md` §4, §7, §9;
  `docs/development/benchmarks.md`; `docs/agents/expanded.md` § State-Threading
  Codegen
- Code: `runtime/apps/beamtalk_runtime/src/beamtalk_actor.erl`
  (`self_dispatch/2`, `restore_dispatch_pdict/1`, `strip_local_temps/1`,
  `dispatch_call/4`), `crates/beamtalk-codegen/src/core_erlang/gen_server/`,
  `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen.rs`
  (`closure_self_dispatch_state_guard`, `generate_self_cast_send`),
  `runtime/apps/beamtalk_runtime/src/beamtalk_class_vars.erl`
