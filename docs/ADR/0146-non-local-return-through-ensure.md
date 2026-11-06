# ADR 0146: A `^` Through `ensure:` — Actor Fields Are Read From the Cell, Only Threaded State Is Carried

## Status
Proposed (2026-11-06). Tracking: BT-4505.

Builds on ADR 0041 (state-carrying `^`), ADR 0111 (the `ThreadedIr`
verifier), ADR 0130 (class variables in place), ADR 0137 (the actor-state
cell) and ADR 0142 (where class methods run). BT-4430 is decided by a
separate amendment to ADR 0133 (§4). If accepted it replaces the BT-4429 foreign relay and the BT-4448 owner stamp,
and decides BT-4459. It depends on three existing bugs being fixed
(BT-4523, BT-4524, BT-4525; §1 P0 and R3).

## Context

### The chain

How `^` interacts with `ensure:` needed six fixes in about a week:

| Issue | PR | Change |
|---|---|---|
| BT-4312 | #4811 | `ensure:` catch relays its scope's own `^`: runs the cleanup from the carried state, re-throws the cleanup's result (`ThreadedStmt::EnsureCatch`, `VerifyError::EnsureCleanupLostOnNlr`). |
| BT-4424 | #4833 | A loop-body cleanup's re-reads lowered by the frame the arm really is (REPL `LocalRebindModeMismatch`). |
| BT-4429 | #4841 | `Block>>ensure:` reached by `perform:` relays every throw (`NlrCarry::Foreign`), resuming with the cell over the carried state "when the carried state is the cell's" (`refresh_carried/2`). |
| BT-4430 | #4844 | A local write in a half-literal `on:do:`/`ensure:` is rejected by the analyser instead of threaded. |
| BT-4433 | #4846 | Typed `private_scratch`, `NlrCarry::writable_by_cleanup`, indirect-write corpus cells. |
| BT-4448 | #4848 | BT-4429 decided "the cell's" by value; a same-class peer's equal state was taken. Added the `'__local__$nlr_away_owner'` stamp (`nlr_state/2`). |
| BT-4459 | open | An away closure's `^` makes its own actor commit the block's creation-time capture. |

BT-4448 and BT-4459 have one cause. An actor's fields exist in **two** places
while a `^` is in flight: in the actor-state cell, which every writer updates
in place, and in the throw's 4th element, which is a copy taken at the `^`.
The method boundary commits the copy
(`crates/beamtalk-codegen/src/core_erlang/nlr.rs:114-120`, `{'reply', Val,
State}`), so every relay on the way must patch the copy back to the cell. A
patcher that cannot tell whose copy it holds compares values (BT-4448). A
copy that was stale from the start, an away closure's capture, is committed
as it is (BT-4459).

Paths below are relative to `crates/beamtalk-codegen/src/core_erlang/` for
`.rs` files and `runtime/apps/beamtalk_runtime/src/` for `.erl` files unless
given in full.

### Current state

**What a `^` carries** is decided in one place, `NlrSlot`
(`nlr.rs:232-290`), rendered by `nlr_throw_state_doc` (`nlr.rs:295-334`):

| `NlrSlot` | Scope | 4th element | `NlrSlot::carry` (`nlr.rs:263-271`) |
|---|---|---|---|
| `Actor` | actor instance method | threaded `State`, or `take()` in a bracketed region | `ActorState` |
| `CellClosure` | actor closure | `nlr_state(Self, Capture)`: cell at home, capture stamped with the owner pid away (`beamtalk_actor_state.erl:244-248`) | `Live` |
| `Tier2Merged` | merged Tier 2 fun (BT-4122) | cell with captured locals flushed as `__local__` keys | `Live` |
| `Repl` | REPL eval | bindings map | `Bindings` |
| `ValueType` | value-type method | latest `Self{N}` | `SelfVt` |
| `ClassMethod` | class method | `nil` | none |

An actor method's `^` stores its state to the cell before the throw when
`synced_calls_active()` (every actor instance method body, `synced_call.rs:496-514`)
or in a merged Tier 2 fun (`mod.rs:418-431`). A closure's `^` reads the cell.
ADR 0137 §5 (`docs/development/actor-state-cell.md` § 5) intends the cell to
hold every field write when a `^` is in flight. **Two known paths break that
today** (found while reviewing this ADR; both are existing bugs, independent
of `ensure:`):

- **Hybrid-params loops (BT-4523).** A field write in a hybrid loop lands in a
  loop parameter (`lower_hybrid_field_rebind`,
  `control_flow/conditionals.rs:2300-2344`). A `^` in the body stores and
  throws the loop-*entry* `State`: `1 to: 5 do: [:i | self.n := i. i == 3
  ifTrue: [^i]]` answers 3 and leaves `n` at 0.
- **A `^`-bearing block passed to an unbracketed call (BT-4524).**
  `classify_loop` (`synced_call.rs:753-765`) and
  `analysis_touches_actor_state` (`threading_analysis.rs:239-253`) do not
  count a `^`, so `self.n := 6. 3 timesRepeat: [^7]` (and `[^2] ensure: [nil]`,
  `to:do:`, `whileTrue:`) runs the closure with no store before it; its `^`
  reads a stale cell and `n` stays 0.

**The catch** of every compiled `ensure:` is one `ThreadedStmt::EnsureCatch`
(`threaded_ir/ir.rs:942-993`), built and verified by `ensure_catch`
(`ensure_nlr_relay.rs:95-166`). Its relay clause matches only the scope's own
token (`scope_nlr_token`, `nlr.rs:388-392`), except the `Block>>ensure:`
fallback (`NlrCarry::Foreign`), whose guard matches every `$bt_nlr`
(`threaded_ir/emit.rs:750-751`, `ir.rs:968-972`). `check_ensure_relay`
(`threaded_ir/verify.rs:2040-2085`) requires a relay when the cleanup can
write the carried state, with the rethrow slot the pair (carry, cleanup) calls
for. `writable_by_cleanup` (`ir.rs:1684-1686`) exempts a private scratch map
for the `Bindings` and `ActorState` carries. A throw with another token takes
the error path `other`, an opaque `Document`: cleanup, then re-raise unchanged
(`ensure_nlr_relay.rs:35-37`, `control_flow/exception_handling.rs:1541-1557`).

**Process hops** relay a `$bt_nlr`. A callee actor's `safe_dispatch` answers
`{'error', Nlr, State}` with its **entry** `State`
(`gen_server/dispatch.rs:317-327`; `beamtalk_actor.erl:4220-4229` for the
runtime dispatcher), so the callee keeps its pre-call state. The caller's
`sync_call` re-throws (`beamtalk_actor.erl:1819-1828`). The class-process hops
do the same: compiled class methods (`beamtalk_class_dispatch.erl:1773-1774`,
`1821-1822`), class-side extension funs (`1554-1555`), and the relay back to
the caller (`1277-1284`, `1361-1374`; in-process self-dispatch `1075-1080`). A
compiled self-send re-throws too (`dispatch_codegen.rs:400-445`). **One hop
does not:** an aliased self-send (`other := self. other run: [^3]`) goes
through `self_dispatch/2`, whose `unwrap_dispatch_result` stores the entry
state to the cell (`adapt/1`, `beamtalk_actor_state.erl:196-198`) and turns
the throw into an error (`beamtalk_exception_handler.erl:707`), so the method
never returns (BT-4525). Class variables keep their writes on a `^`:
`beamtalk_class_vars:protect/1` re-raises an NLR without restoring
(`beamtalk_class_vars.erl:476-491`).

**Ownership** is decided three ways today:

1. Compiled relays: by token (the throw is this scope's) and by
   `at_home(Self)` inside `refresh/2` (`beamtalk_actor_state.erl:342-347`,
   `483-490`): pid of `Self` is `self()` and the cell's owner is that pid.
2. The `Block>>ensure:` relay: `refresh_carried/2`
   (`beamtalk_actor_state.erl:368-383`): the snapshot's owner is `self()`,
   the stamp (if any) is that owner, **and** the carried fields equal the
   snapshot's. The last test is by value.
3. The method boundary: none. It commits whatever the throw carries.

### Constraints

- CLAUDE.md: a state-threading fix states its invariant for every scope kind,
  lives in the node lowering that owns the scope, and comes with a
  `VerifyError` that fails before the fix.
- ADR 0137's callback contract: a callback that exits by an exception returns
  the state it was called with.
- The `{'$bt_nlr', Tok, Val, State}` shape is matched across runtime and
  codegen (`?IS_NLR`, `include/beamtalk.hrl:207-216`, `just
  lint-nlr-tuple-matches`); changing it is costly.
- Compiled code calls `nlr_state/2` and `refresh_carried/2` today; a
  dependency built by an older compiler, or code running across a hot
  upgrade, still does after they change.

## Decision

### 1. The model: stored state is read where it lives; only threaded state is carried

There are two kinds of state a cleanup run by a `^` can write.

- **Stored state** lives in a store owned by one process: an actor's fields
  in the cell `{OwnerPid, Map}` (ADR 0137), class variables in the class's
  store (ADR 0130). Every writer, a cleanup included, writes the store in
  place. The store is the only copy that counts while a `^` is in flight.
  The boundary that commits it **reads the store**, and only if it is the
  store's owner. Nothing on the way patches a copy.
- **Threaded state** has no store: the REPL's bindings, a value-type method's
  `Self`, and the `__local__` keys of an actor scope (construct scratch,
  a merged Tier 2 fun's flushed captured locals). It rides in the throw's 4th
  element. Every `ensure:` whose cleanup can write it must re-throw the
  cleanup's result (the BT-4312 rule, unchanged).

So **at home** the fields inside an actor throw's 4th element are dead: no one
reads them. Its `__local__` keys stay live between relays. **Away** (an
`super_send/5` boundary in another process, `beamtalk_actor_state.erl:392-420`)
`refresh/2` answers the carried state, and `super_send` compares it with the
reply to detect an illegal write (`412-414`). That stays correct because away
no cleanup can write fields: a write raises `actor_state_unreachable`
(`423-426`, `501-509`).

The rule for the actor-method boundary (`NlrBoundary::ActorReply`) becomes
`{'reply', Val, beamtalk_actor_state:refresh(Self, State)}`: the cell's fields
over the carried `__local__` keys, at home; the carried state unchanged when
not at home. `strip_local_temps` still drops the `__local__` keys at commit
(`gen_server/callbacks.rs:88-110`).

The model rests on one premise and three rules, each stated for every scope
kind:

- **P0. The cell is current whenever a `^` of an actor scope is in flight.**
  Every `^` in an actor method body stores the state that includes every field
  written so far, loop parameters included (fixes BT-4523), and every call that
  may run a block containing a `^` of this scope is bracketed
  (`UserCodeCall::Yes`), so the cell is stored before the block runs (fixes
  BT-4524). Value types, class methods and the REPL have no cell; P0 is
  vacuous there.
- **R1. A `^` is never an error for its owner.** Stated generally: every
  unwinding exit of a scope that threads actor `State` leaves the cell holding
  that scope's final version. The only node that runs user code while
  unwinding is `EnsureCatch` (an `on:do:` handler does not run on a `^`; loops
  and branches run no user code while unwinding), so R1 is `EnsureCatch`'s
  rule: its foreign-`$bt_nlr` clause stores the cleanup's final `StateAcc` to
  the cell when the cell is live. Today a foreign `^` discards the inlined
  cleanup's field writes (`exception_handling.rs:1457-1466`: the error-path
  cleanup is compiled after write-through is switched off), unless that
  `ensure:` sits inside another protected body, where write-through is on.
- **R2. A `^` that leaves another actor's callback is an abnormal exit for
  that actor.** The intermediate actor keeps its entry state, as ADR 0137's
  callback contract says; its writes, a cleanup's included, are discarded.
  This is today's behaviour, pinned by `testPerformedEnsureInAPeerDoesNotTakeThePeersState`
  (`stdlib/test/actor_closure_cell_test.bt:142-148`, `b n` = 0). A class
  process in the same position keeps its class-variable writes (`protect/1`
  does not restore); see Tension under Steelman.
- **R3. Hops neither rewrite the term nor store to the cell on a `$bt_nlr`.**
  `sync_call`, the class-dispatch hops and self-dispatch re-throw the term they
  received. No state is resolved at a hop; it is resolved at the owner's
  boundary. `self_dispatch/2`'s `unwrap_dispatch_result` breaks this today and
  gets an NLR clause that re-throws with no `adapt` (BT-4525).

Placing `refresh/2` in `nlr_arm_result` and R1's store in `EnsureCatch` is the
lowering of the node that owns the scope, not a per-call-site refresh, so it
meets CLAUDE.md's state-threading rule.

### 2. Routes

Every way a `^` reaches an `ensure:` cleanup. "Sees" is the state the cleanup
reads; "carries" is the 4th element afterwards; "commits" is who keeps the
writes. *Today* differs from *Decided* only where shown. Every actor row
assumes P0; until BT-4523 and BT-4524 are fixed, a route reached through a
hybrid loop or an unbracketed `^` closure sees a stale cell.

| # | Route | Code | Cleanup sees | Throw carries afterwards | Commits |
|---|---|---|---|---|---|
| 1a | Inline `ensure:`, REPL | `ensure_with_mutations_catch` / `ensure_relay` (`exception_handling.rs:1532-1619`, `1675-1693`) | carried bindings (`StateAcc = Carried`) | cleanup's final `StateAcc` | REPL eval boundary (`nlr.rs:122-140`), eval process |
| 1b | Inline, actor method | same; seed `refresh(Self, StateAcc)` when the cell is live (`1626-1636`) | cell | final `StateAcc`, stored to the cell first (`cell_store`, `1691`) | Today: method boundary commits the carried copy. Decided: commits `refresh(Self, Carried)` |
| 1c | Inline, value-type method | `ensure_relay` `SelfVt` arm (`1695-1722`) | carried `Self` | final `Self{N}` | Nobody: the boundary answers the bare value (`nlr.rs:121`, BT-4503). Observable only by an outer cleanup |
| 1d | Inline, actor closure at home or merged Tier 2 fun | `ensure_relay` `Live` arm (`1723-1737`) | cell | `refresh(Self, Carried)` | home method's boundary |
| 1e | Inline, class method | carry `None` | class variables in place | `nil` | class store, in place (ADR 0130) |
| 1f | Inline, private scratch (direct-params loop body) | `writable_by_cleanup` false for `Bindings`/`ActorState` carries (`ir.rs:1684-1686`) | scratch | unchanged | nothing to keep. A value type's `Self` is still relayed (1c); only its scratch locals are exempt |
| 2 | Closure `ensure:` (an operand is not a block literal, or no threading) | `generate_ensure` (`exception_handling.rs:1150-1289`; relay `1202-1220`) | cell (closure) | actor: `refresh(Self, Carried)`; REPL/value type: unchanged (a closure cannot write them, ADR 0133, BT-4430) | as 1a-1e. Today `[^2] ensure: [nil]` runs unbracketed and its `^` reads a stale cell (BT-4524) |
| 3 | `Block>>ensure:` by `perform:` (structural fallback) | `stdlib/src/block.bt:94` → `primitives/intrinsic_bodies.rs:438` → `generate_ensure_structural_fallback` (`exception_handling.rs:1955-2041`), catch `1901-1944` | cell (closure) | Today: `refresh_carried(Snap, Carried)`. Decided: unchanged | Today: boundary commits the patched copy. Decided: owner's boundary reads the cell |
| 3' | same, with a Tier 2 (arity-1) block | `exception_handling.rs:2005-2030` | — | raises `stateful_block_dispatch` | — |
| 4 | Foreign `^` (other method, same actor) through a compiled `ensure:` | error path `other` (`exception_handling.rs:1541-1557`) | cell / entry scratch | unchanged | Today: the inlined cleanup's field writes are lost, unless the `ensure:` sits inside another protected body (write-through) or the cleanup makes a bracketed call (writes before it survive). Decided (R1): stored to the cell, owner's boundary reads it |
| 5 | Away closure: actor A's block with a `^` run in process P | throw `nlr_state` (`beamtalk_actor_state.erl:244-248`); P's compiled `ensure:` is route 4, P's `Block>>ensure:` route 3; hop `beamtalk_actor.erl:1819-1828` or `beamtalk_class_dispatch.erl:1277-1284` | in P: P's cell for P's blocks; A's capture for A's (a write raises `actor_state_unreachable`, `beamtalk_actor_state.erl:423-426`, `501-509`) | A's capture (today stamped) | P keeps its entry state (R2). A: today the capture (BT-4459); decided: A's cell |
| 6 | Class-side method (ADR 0142) | in the caller: as routes 1e/4 in the caller's process; in the class process: `beamtalk_class_dispatch.erl:1773-1774`, `1821-1822` (extension funs `1554-1555`), re-thrown in the caller | class variables in place | unchanged | class store in place; the `^`'s owner as routes 1-5 |
| 7 | REPL | routes 1a, 2; `perform:` route 3 leaves the bindings (no cell in the eval process) | bindings | as 1a / unchanged | REPL eval boundary |
| 8 | Aliased self-send (`other := self. other run: [^3]`) | `sync_send_pid` → `self_dispatch/2` → `unwrap_dispatch_result` (`beamtalk_actor.erl:1745-1747`) | as route 4 in the callee | Today: rewinds the cell to entry state and raises an error (BT-4525). Decided (R3): re-thrown unchanged | owner's boundary |
| 9 | `terminate:` callback | `gen_server/callbacks.rs:2389+`, no cell installed before dispatch | as 1b once a store creates a cell owned by `self()` | as 1b | the state is discarded after `terminate:`; harmless |

Out of scope, checked: runtime `try ... after` clauses run only runtime
cleanup (file close, the internal `Stream` finaliser,
`runtime/apps/beamtalk_stdlib/src/beamtalk_stream.erl:211-217`), never a user
block. A user `Stream` finaliser is not exposed by the stdlib.

### 3. Ownership

Ownership is decided by identity, in one place: `beamtalk_actor_state:at_home/1`
(`beamtalk_actor_state.erl:483-490`) — `Self`'s pid is `self()` and the
installed cell is owned by that pid. Codegen reaches it through
`refresh/2` at the actor-method boundary (rendered by `nlr_arm_result`,
`nlr.rs:112-141`, the one place every boundary arm is built) and in the
existing `Live`/closure relays. The token decides whose `^` a compiled relay
handles (`scope_nlr_token`). No code compares two state maps to decide whose
they are.

That deletes, once the boundary reads the cell:

- `NlrCarry::Foreign`, `RethrowSlot::CellIfCarried`, the fallback's relay
  clause and its snapshot (`ir.rs:1665-1673`, `1723-1730`; `emit.rs:775-797`).
  The fallback keeps a plain catch: cleanup, re-raise.
- The stamp: `?AWAY_OWNER` and `reflect_state/2`'s `unstamped/1` go
  (`beamtalk_actor_state.erl:89`, `294-309`). Codegen stops calling
  `nlr_state/2` and `refresh_carried/2`, but the runtime keeps both for one
  release for already-compiled code: `nlr_state/2` as an alias of `view/2`,
  `refresh_carried/2` answering the carried state unchanged.

`call_foreign/3`'s value comparison (`beamtalk_actor_state.erl:276-305`)
detects a conflict between two writers of the same actor; it is not an
ownership test and stays.

### 4. BT-4430: decided separately

BT-4430's rejection of a local write in a half-literal `on:do:`/`ensure:`
(`b ensure: [t := 9]`) is made the rule by an amendment to ADR 0133 (`on:do:`
and `ensure:` are in place only when both operands are block literals), in its
own PR so it can be accepted independently of this ADR. A `^` and reads in such
a block are unaffected; actor field writes are accepted (stored state, §1).

### 5. BT-4459: covered by the model

BT-4459 is the case where the carried copy was stale from the start. Under §1
the copy's fields are never committed at home, so A's boundary commits A's
cell, which holds `n = 5`: the send to the peer is bracketed
(`UserCodeCall::Yes`, `synced_call.rs:634`), so A stored its state before the
call. No hop-specific fix is needed (R3), and the `perform:`ed `ensure:` in A
on the way back needs no relay. The fix is the boundary change, checked by a
BT-4459 BUnit cell (§6). BT-4459's "also in scope" item
(`beamtalk_tagged_map:user_field_keys/1` showing `__local__` keys) is still its
own fix; deleting the stamp removes one source of such keys, not all.

### 6. Checks

| Check | Kind | Fails when | Routes |
|---|---|---|---|
| `EnsureCleanupLostOnNlr` (exists, `verify.rs:543-571`) | `VerifyError` on `EnsureCatch` | a cleanup that can write **threaded** state has no relay, or re-throws a stale version | 1a, 1b (`__local__` keys), 1c, 1d, 2 |
| `EnsureForeignCleanupNotStored` (new) | `VerifyError` on `EnsureCatch`; `other` is split into an NLR clause and an error clause | where the cell is live, the clause a foreign `$bt_nlr` takes does not store the cleanup's final `StateAcc` to the cell (R1) | 4, 5 (in A's process), 8 |
| Bracket every `^`-bearing closure call (new, P0) | `SyncedCall` verifier rule (ADR 0137 verifier) | a closure argument whose body holds a `^` of this scope is called with `UserCodeCall::No` (BT-4524) | 2, and every actor route reached through a closure |
| Hybrid-loop `^` store (new, P0) | `VerifyError` on the loop node | a `^` in a hybrid-params loop stores a state that omits a live field param (BT-4523) | 1b, 4 through a hybrid loop |
| `ActorNlrCommitBypassesCell` (new) | `VerifyError` on `NlrCatch` with `NlrBoundary::ActorReply` (`NlrArmShape` gains a resolved reply; next to `MethodNlrAnswersPair`, `verify.rs:1831-1839`) | the arm commits the carried state instead of `refresh(Self, State)`. A shape guard: `nlr_arm_result` is a pure function of the boundary, so it catches a regression in that function, not a run-time stale cell. It lands in the same change as the boundary (Phase 2) and before that is shown failing only through its `fault` switch | 1b, 1d, 2, 3, 4, 5, 6 |
| BT-4459 and route-4 run-time cells | BUnit, `PIN-BUG` before the fix | A's commit after a block run by a peer; a foreign `^` through an inlined cleanup | 4, 5 |
| Ownership by identity | runtime, EUnit | `beamtalk_actor_state_tests`: the boundary's `refresh/2` answers the cell for (own pid, own cell), the carried state for (own pid, other owner's cell) and (other pid, any cell) | 3, 5, 6 |
| Hops re-throw unchanged | runtime, existing lint + EUnit | a hop builds a new `$bt_nlr` term or stores to the cell on one (`just lint-nlr-tuple-matches` already rejects hand-rolled patterns; extend it to constructions outside codegen and `beamtalk_guard_return`; EUnit for `unwrap_dispatch_result`) | 5, 6, 8 |

`EnsureCleanupLostOnNlr`'s `CellOverCarried`/`CellIfCarried` expectations
become redundant at home once the boundary reads the cell and are removed with
the variants; a corpus cell for `^` plus `ensure:` inside an away
`super_send/5` keeps the away case covered. Each new `VerifyError` gets a
`fault` switch (as `ensure_nlr_relay::fault`) and is proved over
`ensure_relay_corpus.rs`.

Making `ThreadedStmt::NlrThrow` a live IR node (it has no production caller,
`ir.rs:1105-1119`; `^` is an opaque `Statement` built at `mod.rs:407-432`) was
considered for checking P0 at the throw. It is deferred: the one emitter
already stores before every method-body `^`, and both known P0 failures are
caught by the two P0 checks above.

### 7. Coverage today

| # | Covered by | Gap |
|---|---|---|
| 1a | `crates/beamtalk-repl/tests/repl_loop_body_return.rs` (rows at 130-188, 384-454, run with `without_relay`); `tests/repl-protocol/cases/bt4003_repl_returns.btscript:112-126`; `stdlib/test/bt4424ensure_loop_rebind_test.bt` | — |
| 1b | `crates/beamtalk-codegen/src/core_erlang/tests/ensure_relay_corpus.rs` (`ActorMethod`); `stdlib/test/actor_closure_cell_test.bt:53-83` (`stdlib/test/fixtures/closure_cell_ensure.bt:75-102`) | through a hybrid loop (BT-4523) |
| 1c | `ensure_relay_corpus.rs` (`ValueTypeMethod`), compile and verify only | **untested at run time** (an outer cleanup reading an inner cleanup's `Self` write) |
| 1d | `ensure_relay_corpus.rs` (`ActorClosure`, `MergedTier2Fun`); `actor_closure_cell_test.bt:85-88` | merged Tier 2 fun **untested at run time**; likely unreachable from a validated program (`ensure_relay_corpus.rs:29-33`; open question 2) |
| 1e | `ensure_relay_corpus.rs` (`ClassMethod`); `class_var_semantics_{open,override,sealed}_test.bt` via the abstract `fixtures/class_var_semantics_matrix_test.bt:203-205` (`rowOwnNlrEnsure`) | — |
| 1f | `threaded_ir/tests/ensure_catch.rs:112`; `bt4424ensure_loop_rebind_test.bt` | — |
| 2 | `ensure_relay_corpus.rs` (closure shapes); `actor_closure_cell_test.bt:90-127`; `ensure_catch.rs:205`; `tests/repl-protocol/cases/bt4430_ensure_stored_receiver.btscript` | **unbracketed `^` closure after a field write** (BT-4524) |
| 3 | `ensure_relay_corpus.rs:446` (`block_ensure_fallback_relays_a_foreign_throw`); `ensure_catch.rs:274`; `actor_closure_cell_test.bt:129-168`; `stdlib/test/blocks_test.bt:284-292` (a `TestCase`, a value type, carries `Self`); `runtime/apps/beamtalk_runtime/test/beamtalk_actor_state_tests.erl:306-385` | REPL `^` through `perform: #ensure:` **untested**; class method **untested** |
| 3' | `blocks_test.bt:228-256` | — |
| 4 | — | **untested** (behaviour measured while reviewing this ADR: writes lost, as in §2) |
| 5 | `actor_closure_cell_test.bt:142-148` (peer side, BT-4448) | A's commit **untested**, no `PIN-BUG` (BT-4459); hop through a class process **untested** |
| 6 | `class_var_semantics_matrix_test.bt` foreign rows (via `on:do:`, not `ensure:`) | foreign `^` through a class-method `ensure:` **untested** |
| 7 | as 1a, 2 | as 3 |
| 8 | — | **untested** (fails today, BT-4525) |

`ensure_relay_corpus.rs` has no REPL scope, no foreign-token shape and no
cross-process shape; the REPL corpus covers only own-token `^`. The new tests
belong there: a scope axis for REPL, a "who throws" axis (own scope, another
method of the same actor, a closure of another actor), and a "how the `^`
closure is called" axis (bracketed send, intrinsic loop, closure `ensure:`,
`timesRepeat:`/`to:do:`/`whileTrue:`).

## Prior Art

- **Smalltalk (Pharo, Squeak).** `ensure:` runs during unwinding and objects
  live on one heap, so a cleanup's write is simply there afterwards. There is
  nothing to carry. Beamtalk's stored state recovers this: the cell and the
  class store are that heap for one actor or class. Smalltalk has no
  cross-process `^`: Pharo raises `BlockCannotReturn` when the home context is
  not on the current process's stack. So routes 1-4 have precedent; route 5
  (an away closure's `^`) does not.
- **Newspeak** has the same non-local return and unwind semantics as
  Smalltalk; the object model is again one heap.
- **Ruby.** `return` inside a `proc` is a non-local return from the defining
  method, and `ensure` clauses run as it unwinds; instance variables are
  written in place, so a cleanup's write survives. A `proc` whose method has
  already returned raises `LocalJumpError`, Ruby's version of Pharo's rule.
  Ruby threads share one heap, so it has no route-5 analogue either.
- **Erlang/Elixir.** `try ... after` runs cleanup for side effects only; its
  value is discarded and nothing is threaded back. Process state changes only
  through a callback's return. R2 is that rule: a callback left by a throw
  keeps its old state.
- **Python, Swift.** `finally` / `defer` run on every exit, including `return`;
  state is on the heap, so there is nothing to carry. A `return` in `finally`
  overrides the in-flight one in Python, a hazard Beamtalk avoids by
  re-throwing the original `^` after the cleanup.

We adapt the Smalltalk/Ruby view for state that has a store, and the Erlang
view for the process boundary. Route 5 is Beamtalk's own (BT-3582 deliberately
relays a `^` across processes); its semantics here are a choice, not prior art.

## User Impact

- **Newcomer / Smalltalk developer:** `[^1] ensure: [self.n := 9]` keeps `n`
  wherever the block runs from, as in Pharo or Ruby. In the REPL:

  ```
  > c := Counter spawn.
  > c runAndReturn        // self.n := 5. [^1] ensure: [self.n := self.n + 1]
  // => 1
  > c n
  // => 6
  ```

  Two rules change visibly. An actor's `^` that comes back from a block run by
  another actor now keeps the writes the actor made after creating the block
  (BT-4459). A foreign `^` (a block from another method of the same actor)
  now keeps the writes of an inlined cleanup it passes through (R1); today they
  are lost.
- **Smalltalk developer, R2:** a `^` that passes through another actor's
  callback discards that actor's writes. This is Erlang's rule, not Pharo's,
  and is documented in `docs/beamtalk-language-features.md`.
- **Erlang developer:** the generated reply calls
  `beamtalk_actor_state:refresh/2` on the NLR path; no new message shapes.
- **Operator:** one process-dictionary read plus a strip-and-merge per `^`
  that reaches an actor method's boundary, below the cost of the throw itself.
  Early-exit idioms (`detect:`, guard `^`s) pay it; BT-4524's bracketing adds a
  store/take around calls that run a `^` closure, measured in that issue.
- **Tooling:** none.

## Steelman Analysis

**Keep patching the copy (status quo plus BT-4459 at the hops).**
- *Operator:* "Each fix is small and local; the boundary stays a plain tuple."
- *BEAM veteran:* "The throw is a value; resolving it at the hop is explicit."
- Against it: every new route adds a patcher, and each patcher needs an
  ownership test. Two of the six fixes were ownership bugs.

**Stamp every carried actor state with its owner, keep relays.**
- *Language designer:* "Ownership becomes explicit data in the throw."
- Against it: it keeps two copies of the fields and still needs every relay
  to patch the copy; the stamp leaks into anything that enumerates fields
  (BT-4459's also-in-scope item).

**Make an away closure's `^` an error (`non_local_return_abroad`).**
- *Smalltalk developer:* "That is what Pharo does; a `^` whose home is not on
  my stack is a bug."
- *Language designer:* "It deletes route 5, R2, the stamp and the whole
  BT-4459 class, and matches `actor_state_unreachable` for writes."
- *Measured (2026-11-06, BT-4505).* A run-time trace of every
  cross-process `$bt_nlr` relay (`sync_call` re-throw and the class-process
  `nlr_relay` points) over the beamtalk BUnit suite (10,219 tests), the stdlib
  tests, Buccaneer (826 tests plus a bench smoke run over h1, h2 and
  WebSocket), Catalina, beamtalk-http and hpack found **20 relays, all in tests
  written for this feature** (BT-3022, BT-3032/ADR 0110, BT-4448, the
  class-variable semantics matrix) and **none in the packages**. A static scan
  of all 497 `^`-in-block sites found no application code that passes a
  `^`-block to an actor or to a class method that runs in its class process.
- Against it: the one real dependency is the stdlib `Collection` protocol.
  `includes:`, `detect:`, `detect:ifNone:`, `anySatisfy:`, `allSatisfy:` and
  `anyOne` (`stdlib/src/collection.bt`) are `self do: [... ^ ...]`, and List
  `indexOf:` is similar. For a user collection whose `do:` runs in an actor or
  a class process (BT-3022's class-driven collection), they work only because a
  `^` crosses the process. Making an away `^` an error would break them and
  need an early-exit protocol for user collections that does not use `^`,
  plus changes to about 20 BUnit tests and the runtime EUnit relay tests. This
  ADR's model makes route 5 correct at the cost of one `refresh/2` per `^`, so
  it keeps the feature. The error stays available as the fallback if route 5
  keeps producing bugs.

**Thread the half-literal `ensure:` (BT-4430 as a gap).**
- *Newcomer:* "`b ensure: [t := 9]` looks like it should work."
- Against it: it needs one-arm variants of every exception producer; the
  wrapper is one token away. Decided in the ADR 0133 amendment (§4).

**Tension.** R2 (an intermediate actor drops its cleanup's writes) is
Erlang-correct but surprises a Smalltalker, and it is the opposite of class
variables: a class process a foreign `^` passes through keeps its writes,
because `protect/1` does not restore. Both are "stored state" in §1. The
difference is deliberate: an actor's state is a gen_server callback result,
under ADR 0137's callback contract, while class variables are a store written
in place with no callback return (ADR 0130). Making them uniform would mean
either rolling back class-variable writes on a `^` (a new mechanism) or
letting a `^` commit two actors. Open question 4.

## Alternatives Considered

### Resolve at each hop (`sync_call`, class dispatch) for BT-4459
Re-home a stamped capture to the cell when the throw re-enters its owner's
process. Rejected: a per-call-site reconciliation, which CLAUDE.md rules out,
and it leaves `refresh_carried/2`'s value test in place.

### Put the owner in the throw tuple
A fifth element `{'$bt_nlr', Tok, Val, State, Owner}`. Rejected: changes
`?IS_NLR` and every pattern the lint guards, and the owner is already known
at the boundary (`Self`).

### Carry only `__local__` keys from the throw site
Strip actor fields at the `^` instead of ignoring them at the boundary.
Rejected for now: same result at home, but every throw site changes, and the
away `super_send/5` check still needs the fields. Could follow once the stamp
is gone.

### Make an away closure's `^` an error
See Steelman. No real code uses route 5 today (measured), but the stdlib
`Collection` protocol depends on it for user collections whose `do:` hops
processes. Rejected; kept as the fallback.

### Make the cell authoritative for value types and the REPL too
Rejected: they have no store; adding one is a runtime mechanism for state
that threads correctly today.

## Consequences

### Positive
- One ownership test, by identity, in one module.
- BT-4459 is fixed by the general rule; BT-4448's mechanism is deleted.
- The `Block>>ensure:` fallback loses its special relay.
- The relay obligation narrows to threaded state, which `verify()` can see.
- Writing P0 down exposed two existing field-loss bugs (BT-4523, BT-4524) and
  a broken hop (BT-4525), now filed with checks.

### Negative
- A `refresh/2` call on every actor-method NLR commit, and BT-4524's brackets
  around calls that run a `^` closure.
- The model rests on P0. Its two known failures are checked by new
  `VerifyError`s; a third path that loses the cell would not be caught by the
  boundary rule.
- R1 needs `EnsureCatch`'s opaque `other` split into typed clauses.
- Two runtime functions live on as compatibility shims for one release.
- Observable behaviour changes on routes 4 and 5 (see Migration Path).

### Neutral
- Value-type, class-method and REPL behaviour is unchanged.
- R2 is documented, not changed.

## Implementation

Phases land in order. Each phase's PR includes its tests and keeps CI green;
no check lands failing.

0. **Prerequisite bugs** (independent, any order): BT-4523 (hybrid-loop `^`),
   BT-4524 (bracket `^` closures, with the `SyncedCall` rule), BT-4525
   (`self_dispatch/2` NLR clause). Each brings its `VerifyError` or EUnit and
   BUnit repro. ~M, M, S. Suites: codegen unit + `just test-corpus`, BUnit,
   runtime EUnit.
1. **Boundary + R1, together.** `nlr_arm_result`'s `ActorReply` arm renders
   `refresh(Self, State)`; split `EnsureCatch::other` and store the cleanup's
   result on a foreign `$bt_nlr`. Landing the boundary alone would turn
   today's consistent "foreign `^` loses the cleanup's writes" into "loses the
   writes made after the cleanup's last bracketed call". Add
   `ActorNlrCommitBypassesCell` and `EnsureForeignCleanupNotStored` with
   `fault` switches, the BT-4459 and route-4 BUnit cells (`PIN-BUG`s flipped
   here), the route-8 cell, and the class-process hop cell. ~L. Suites: codegen
   unit, `just verify-threaded-ir`, `just test-corpus`, BUnit,
   `just test-repl-protocol`.
2. **Delete** `NlrCarry::Foreign`, `RethrowSlot::CellIfCarried`, the
   fallback's relay and the stamp; codegen stops calling `nlr_state/2` and
   `refresh_carried/2`, which stay as shims for one release (§3). Update
   `ensure_relay_corpus.rs:446` and the EUnit cases; add the away
   `super_send/5` corpus cell. ~M.
3. **Corpus.** Add the REPL scope, the "who throws" axis and the "how the
   closure is called" axis to `ensure_relay_corpus.rs`; add the untested rows
   of §7. ~M.

Components: codegen (`nlr.rs`, `threaded_ir/{ir,verify,emit}.rs`,
`exception_handling.rs`, `synced_call.rs`, `threading_analysis.rs`, loop
lowering), runtime (`beamtalk_actor_state.erl`, `beamtalk_actor.erl`), tests.
No parser or REPL change.

## Migration Path

No syntax changes and no source edits are required. Two run-time behaviours
change, both from losing writes to keeping them:

- **Route 4 (R1):** a foreign `^` through an inlined `ensure:` cleanup now keeps
  the cleanup's field writes.
- **Route 5 (BT-4459):** an actor whose block is run by another actor and
  returns with `^` now keeps the writes it made after creating the block.

The prerequisite bugs (BT-4523, BT-4524) likewise change "field write lost on
`^`" to "kept". Code that relied on the loss would be relying on a bug; none is
known in the stdlib or the Buccaneer/Catalina/beamtalk-http packages. The
release notes list both. Dependencies compiled by an older compiler keep
working through the one-release runtime shims (§3); after that, they must be
recompiled.

## Open Questions

1. **Route 1b seed.** With a live cell the relay seeds from
   `refresh(Self, StateAcc)` over the construct's entry scratch, not over the
   carried state (`exception_handling.rs:1675-1679`). A reading of the code
   says carried `__local__` keys missing from the entry scratch survive via the
   cell, keys present in both are reset to entry values, and in an actor
   method nothing observes the difference because commit strips them. Confirm
   with a corpus cell before Phase 2 removes `CellOverCarried`.
2. **Merged Tier 2 reachability.** A review found no validated program that
   lowers a merged Tier 2 fun (`blocks.rs:190-198`,
   `gen_server/methods.rs:1548-1564`; ADR 0133 rejects both triggers). If none
   can, route 1d's Tier 2 half can be deleted rather than tested.
3. **R2.** Should an intermediate actor whose callback a foreign `^` leaves
   keep its writes? This ADR keeps today's answer (no).
4. **Uniform abort semantics.** Should class variables also roll back when a
   foreign `^` passes through the class process, to match R2? This ADR keeps
   them different (Tension).

## References
- Related issues: BT-4505, BT-4312, BT-4424, BT-4429, BT-4430, BT-4433, BT-4448, BT-4459, BT-4122, BT-4503, BT-3582, BT-4523, BT-4524, BT-4525
- Related ADRs: ADR 0041, ADR 0111, ADR 0130, ADR 0131, ADR 0133 (amended separately for BT-4430), ADR 0137, ADR 0142
- Documentation: `docs/development/actor-state-cell.md` § 5, `docs/development/debugging.md` § ThreadedIr verifier (`EnsureCleanupLostOnNlr`), `docs/beamtalk-language-features.md` § What Works and What Doesn't
- External: Pharo `BlockCannotReturn`; Ruby `LocalJumpError` and `ensure`
