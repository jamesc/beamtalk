# ADR 0145: A `^` Through `ensure:` — Actor Fields Are Read From the Cell, Only Threaded State Is Carried

## Status
Proposed (2026-11-06). Tracking: BT-4505.

Builds on ADR 0041 (state-carrying `^`), ADR 0111 (the `ThreadedIr`
verifier), ADR 0130 (class variables in place), ADR 0133 (escaping blocks),
ADR 0137 (the actor-state cell) and ADR 0142 (where class methods run). If
accepted it replaces the BT-4429 foreign relay and the BT-4448 owner stamp,
and decides BT-4430 and BT-4459.

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
`synced_calls_active()` (every actor instance method body, `synced_call.rs:496-498`)
or in a merged Tier 2 fun (`mod.rs:418-431`). A closure's `^` reads the cell.
So at home the cell already holds every field write when a `^` is in flight
(ADR 0137 §5, `docs/development/actor-state-cell.md` § 5).

**The catch** of every compiled `ensure:` is one `ThreadedStmt::EnsureCatch`
(`threaded_ir/ir.rs:942-993`), built and verified by `ensure_catch`
(`ensure_nlr_relay.rs:95-166`). Its relay clause matches only the scope's own
token (`scope_nlr_token`, `nlr.rs:388-392`). `check_ensure_relay`
(`threaded_ir/verify.rs:2040-2085`) requires a relay when the cleanup can
write the carried state, with the rethrow slot the pair (carry, cleanup) calls
for. `writable_by_cleanup` (`ir.rs:1684-1686`) exempts a private scratch map.
A throw with another token takes the error path `other`, an opaque `Document`:
cleanup, then re-raise unchanged (`ensure_nlr_relay.rs:35-37`,
`control_flow/exception_handling.rs:1541-1557`).

**Process hops** relay a `$bt_nlr` without touching it. A callee actor's
`safe_dispatch` answers `{'error', Nlr, State}` with its **entry** `State`
(`gen_server/dispatch.rs:317-327`; `beamtalk_actor.erl:4220-4229` for the
runtime dispatcher), so the callee keeps its pre-call state. The caller's
`sync_call` re-throws (`beamtalk_actor.erl:1819-1828`). The class-process hops do
the same (`beamtalk_class_dispatch.erl:1277-1284`, `1361-1374`; in-process
self-dispatch `1075-1080`). A compiled self-send re-throws too
(`dispatch_codegen.rs:400-445`). Class variables keep their writes on a `^`:
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
  codegen (`?IS_NLR`, `beamtalk.hrl:207-216`, `just lint-nlr-tuple-matches`);
  changing it is costly.

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

So the fields inside an actor throw's 4th element are dead: no one reads
them. Its `__local__` keys stay live between relays.

The rule for the actor-method boundary (`NlrBoundary::ActorReply`) becomes
`{'reply', Val, beamtalk_actor_state:refresh(Self, State)}`: the cell's fields
over the carried `__local__` keys, at home; the carried state unchanged when
not at home (an away `super_send/5`, `beamtalk_actor_state.erl:392-420`).
`strip_local_temps` still drops the `__local__` keys at commit.

Three more rules complete it, for every scope kind:

- **R1. A `^` is never an error for its owner.** A cleanup that a `^` runs
  keeps its writes in the owner's process whether the `^` is the scope's own
  or foreign (another method of the same actor). Today a foreign `^` discards
  an inlined cleanup's threaded writes (`exception_handling.rs:1551-1557`).
  The `other` path must store the cleanup's final `StateAcc` to the cell when
  the exception is a `$bt_nlr` and the cell is live.
- **R2. A `^` that leaves another actor's callback is an abnormal exit for
  that actor.** The intermediate actor keeps its entry state, as ADR 0137's
  callback contract says; its writes, a cleanup's included, are discarded.
  This is today's behaviour, pinned by `testPerformedEnsureInAPeerDoesNotTakeThePeersState`
  (`stdlib/test/actor_closure_cell_test.bt:142-148`, `b n` = 0).
- **R3. Hops never rewrite.** `sync_call`, the class-dispatch hops and
  self-dispatch re-throw the term they received. No state is resolved at a
  hop; it is resolved at the owner's boundary.

### 2. Routes

Every way a `^` reaches an `ensure:` cleanup. "Sees" is the state the cleanup
reads; "carries" is the 4th element afterwards; "commits" is who keeps the
writes. *Today* differs from *Decided* only where shown.

| # | Route | Code | Cleanup sees | Throw carries afterwards | Commits |
|---|---|---|---|---|---|
| 1a | Inline `ensure:`, REPL | `ensure_with_mutations_catch` / `ensure_relay` (`exception_handling.rs:1532-1619`, `1675-1693`) | carried bindings (`StateAcc = Carried`) | cleanup's final `StateAcc` | REPL eval boundary (`nlr.rs:133-139`), eval process |
| 1b | Inline, actor method | same; seed `refresh(Self, StateAcc)` when the cell is live (`1626-1636`) | cell | final `StateAcc`, stored to the cell first (`cell_store`, `1691`) | Today: method boundary commits the carried copy. Decided: commits `refresh(Self, Carried)` |
| 1c | Inline, value-type method | `ensure_relay` `SelfVt` arm (`1695-1722`) | carried `Self` | final `Self{N}` | Nobody: the boundary answers the bare value (`nlr.rs:121`, BT-4503). Observable only by an outer cleanup |
| 1d | Inline, actor closure at home or merged Tier 2 fun | `ensure_relay` `Live` arm (`1723-1737`) | cell | `refresh(Self, Carried)` | home method's boundary |
| 1e | Inline, class method | carry `None` | class variables in place | `nil` | class store, in place (ADR 0130) |
| 1f | Inline, private scratch (direct-params loop body, class or value-type method) | `writable_by_cleanup` false (`ir.rs:1684-1686`) | scratch | unchanged | nothing to keep |
| 2 | Closure `ensure:` (an operand is not a block literal, or no threading) | `generate_ensure` (`exception_handling.rs:1150-1289`; relay `1202-1220`) | cell (closure) | actor: `refresh(Self, Carried)`; REPL/value type: unchanged (a closure cannot write them, ADR 0133, BT-4430) | as 1a-1e |
| 3 | `Block>>ensure:` by `perform:` (structural fallback) | `stdlib/src/block.bt:94` → `intrinsic_bodies.rs:438` → `generate_ensure_structural_fallback` (`exception_handling.rs:1955-2041`), catch `1901-1944` | cell (closure) | Today: `refresh_carried(Snap, Carried)`. Decided: unchanged | Today: boundary commits the patched copy. Decided: owner's boundary reads the cell |
| 3' | same, with a Tier 2 (arity-1) block | `exception_handling.rs:2005-2030` | — | raises `stateful_block_dispatch` | — |
| 4 | Foreign `^` (other method, same actor) through a compiled `ensure:` | error path `other` (`exception_handling.rs:1541-1557`) | cell / entry scratch | unchanged | Today: inlined cleanup's writes discarded (see Open Questions). Decided (R1): stored to the cell, owner's boundary reads it |
| 5 | Away closure: actor A's block with a `^` run in process P | throw `nlr_state` (`beamtalk_actor_state.erl:244-248`); P's compiled `ensure:` is route 4, P's `Block>>ensure:` route 3; hop `beamtalk_actor.erl:1819-1828` or `beamtalk_class_dispatch.erl:1277-1284` | in P: P's cell for P's blocks; A's capture for A's (a write raises `actor_state_unreachable`, `beamtalk_actor_state.erl:423-426`, `501-509`) | A's capture (today stamped) | P keeps its entry state (R2). A: today the capture (BT-4459); decided: A's cell |
| 6 | Class-side method (ADR 0142) | in the caller: as routes 1e/4 in the caller's process; in the class process: `beamtalk_class_dispatch.erl:1554-1555`, re-thrown in the caller | class variables in place | unchanged | class store in place; the `^`'s owner as routes 1-5 |
| 7 | REPL | routes 1a, 2; `perform:` route 3 leaves the bindings (no cell in the eval process) | bindings | as 1a / unchanged | REPL eval boundary |

Out of scope, checked: runtime `try ... after` clauses run only runtime
cleanup (file close, the internal `Stream` finaliser, `beamtalk_stream.erl:211-217`),
never a user block. A user `Stream` finaliser is not exposed by the stdlib.

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
- `refresh_carried/2` and the stamp: `nlr_state/2` becomes `view/2` again,
  `?AWAY_OWNER` and `reflect_state/2`'s `unstamped/1` go
  (`beamtalk_actor_state.erl:89`, `244-248`, `294-309`, `368-383`).

`call_foreign/3`'s value comparison (`beamtalk_actor_state.erl:276-305`)
detects a conflict between two writers of the same actor; it is not an
ownership test and stays.

### 4. BT-4430: permanent semantics

The rejection stays. ADR 0133 says a block that is not written in place
escapes. In `b ensure: [t := 9]` the receiver is a block value, so codegen runs
the whole send as a closure and the literal half with it (`generate_ensure`,
`exception_handling.rs:1166-1178`, `block_facts::inlined_exception_blocks`).
Threading the literal half anyway would make "in place" depend on the other
operand and would need every exception-producer predicate to handle one
literal arm. The rewrite is one wrapper (`[b value] ensure: [t := 9]`), and
the error says so. A `^` and reads in such a block are unaffected; actor
field writes are accepted (stored state, §1). `docs/beamtalk-language-features.md`
§ What Works and What Doesn't now says this is permanent.

### 5. BT-4459: covered by the model

BT-4459 is the case where the carried copy was stale from the start. Under §1
the copy's fields are never committed, so A's boundary commits A's cell, which
holds `n = 5`: the send to the peer is bracketed (`UserCodeCall::Yes`,
`synced_call.rs:634`), so A stored its state before the call. No hop-specific
fix is needed (R3), and the `perform:`ed `ensure:` in A on the way back needs
no relay. The fix is the boundary change, checked by `ActorNlrCommitBypassesCell`
(§6). BT-4459's "also in scope" item (`beamtalk_tagged_map:user_field_keys/1`
showing `__local__` keys) is still its own fix; deleting the stamp removes one
source of such keys, not all.

### 6. Checks

| Check | Kind | Fails when | Routes |
|---|---|---|---|
| `EnsureCleanupLostOnNlr` (exists, `verify.rs:543-571`) | `VerifyError` on `EnsureCatch` | a cleanup that can write **threaded** state has no relay, or re-throws a stale version | 1a, 1b (`__local__` keys), 1c, 1d, 2 |
| `ActorNlrCommitBypassesCell` (new) | `VerifyError` on `NlrCatch` with `NlrBoundary::ActorReply` (`NlrArmShape` gains a resolved reply; checked next to `MethodNlrAnswersPair`, `verify.rs:1831-1839`) | the arm commits the carried state instead of `refresh(Self, State)`. Fails on every actor method today: that is the BT-4459 repro | 1b, 1d, 2, 3, 4, 5, 6 |
| `NlrThrowWithoutCellStore` (new) | `VerifyError` on `ThreadedStmt::NlrThrow`, made live (it has no production caller, `ir.rs:1105-1119`; `^` is an opaque `Statement` built at `mod.rs:407-432`) | a `^` in an actor method body throws without storing its state to the cell first. The premise of the boundary rule | every actor route |
| `EnsureForeignCleanupNotStored` (new) | `VerifyError` on `EnsureCatch`; `other` is split into an NLR clause and an error clause | where the cell is live, the clause a foreign `$bt_nlr` takes does not store the cleanup's final `StateAcc` to the cell (R1) | 4, 5 (in A's process) |
| Ownership by identity | runtime, EUnit | `beamtalk_actor_state_tests`: the boundary's `refresh/2` answers the cell for (own pid, own cell), the carried state for (own pid, other owner's cell) and (other pid, any cell) | 3, 5, 6 |
| Hops re-throw unchanged | runtime, existing lint | a hop builds a new `$bt_nlr` term (`just lint-nlr-tuple-matches` already rejects hand-rolled patterns; extend it to constructions outside codegen and `beamtalk_guard_return`) | 5, 6 |

`EnsureCleanupLostOnNlr`'s `CellOverCarried`/`CellIfCarried` expectations
become redundant once the boundary reads the cell and are removed with the
variants. Each new `VerifyError` gets a `fault` switch (as
`ensure_nlr_relay::fault`) and is proved over `ensure_relay_corpus.rs`.

### 7. Coverage today

| # | Covered by | Gap |
|---|---|---|
| 1a | `crates/beamtalk-repl/tests/repl_loop_body_return.rs` (rows at 130-188, 384-397, run with `without_relay`); `tests/repl-protocol/cases/bt4003_repl_returns.btscript:112-126`; `stdlib/test/bt4424ensure_loop_rebind_test.bt` | — |
| 1b | `crates/beamtalk-codegen/src/core_erlang/tests/ensure_relay_corpus.rs` (`ActorMethod`); `stdlib/test/actor_closure_cell_test.bt:53-83` (`stdlib/test/fixtures/closure_cell_ensure.bt:75-102`) | — |
| 1c | `ensure_relay_corpus.rs` (`ValueTypeMethod`), compile and verify only | **untested at run time** (an outer cleanup reading an inner cleanup's `Self` write) |
| 1d | `ensure_relay_corpus.rs` (`ActorClosure`, `MergedTier2Fun`); `actor_closure_cell_test.bt:85-88` | merged Tier 2 fun **untested at run time** (reachable only in an unvalidated compile, `ensure_relay_corpus.rs:29-33`) |
| 1e | `ensure_relay_corpus.rs` (`ClassMethod`); `stdlib/test/fixtures/class_var_semantics_matrix_test.bt:203-205` (`rowOwnNlrEnsure`) | — |
| 1f | `crates/beamtalk-codegen/src/core_erlang/threaded_ir/tests/ensure_catch.rs:112`; `bt4424ensure_loop_rebind_test.bt` | — |
| 2 | `ensure_relay_corpus.rs` (closure shapes); `actor_closure_cell_test.bt:90-127`; `ensure_catch.rs:205`; `tests/repl-protocol/cases/bt4430_ensure_stored_receiver.btscript` | — |
| 3 | `ensure_relay_corpus.rs:446` (`block_ensure_fallback_relays_a_foreign_throw`); `ensure_catch.rs:274`; `actor_closure_cell_test.bt:129-168`; `stdlib/test/blocks_test.bt:284-292` (a `TestCase`, a value type, carries `Self`); `runtime/apps/beamtalk_runtime/test/beamtalk_actor_state_tests.erl:306-385` | REPL `^` through `perform: #ensure:` **untested**; class method **untested** |
| 3' | `blocks_test.bt:228-256` | — |
| 4 | — | **untested** |
| 5 | `actor_closure_cell_test.bt:142-148` (peer side, BT-4448) | A's commit **untested**, no `PIN-BUG` (BT-4459); hop through a class process **untested** |
| 6 | `class_var_semantics_matrix_test.bt` foreign rows (via `on:do:`, not `ensure:`) | foreign `^` through a class-method `ensure:` **untested** |
| 7 | as 1a, 2 | as 3 |

`ensure_relay_corpus.rs` has no REPL scope, no foreign-token shape and no
cross-process shape; the REPL corpus covers only own-token `^`. The new tests
belong there: a scope axis for REPL, and a "who throws" axis (own scope,
another method of the same actor, a closure of another actor).

## Prior Art

- **Smalltalk (Pharo, Squeak).** `ensure:` runs during unwinding and objects
  live on one heap, so a cleanup's write is simply there afterwards. There is
  nothing to carry. Beamtalk's stored state recovers this: the cell and the
  class store are that heap for one actor or class.
- **Erlang/Elixir.** `try ... after` runs cleanup for side effects only; its
  value is discarded and nothing is threaded back. Process state changes only
  through a callback's return. R2 is that rule: a callback left by a throw
  keeps its old state.
- **Newspeak** has the same non-local return and unwind semantics as
  Smalltalk; the object model is again one heap.

We adapt the Smalltalk view for state that has a store, and the Erlang view
for the process boundary.

## User Impact

- **Newcomer / Smalltalk developer:** `[^1] ensure: [self.n := 9]` keeps `n`
  wherever the block runs from, as in Pharo. One rule changes in a visible
  way: an actor's `^` that comes back from a block run by another actor now
  keeps the writes the actor made after creating the block (BT-4459).
- **Erlang developer:** the generated reply calls `beamtalk_actor_state:refresh/2`
  on the NLR path; no new message shapes.
- **Operator:** one map merge per `^` that reaches an actor method's
  boundary. A `^` is rare on hot paths.
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

**Thread the half-literal `ensure:` (BT-4430 as a gap).**
- *Newcomer:* "`b ensure: [t := 9]` looks like it should work."
- Against it: it contradicts ADR 0133's in-place rule and needs one-arm
  variants of every exception producer; the wrapper is one token away.

Tension: R2 (an intermediate actor drops its cleanup's writes) is
Erlang-correct but surprises a Smalltalker. It is today's tested behaviour;
changing it would make a `^` commit two actors.

## Alternatives Considered

### Resolve at each hop (`sync_call`, class dispatch) for BT-4459
Re-home a stamped capture to the cell when the throw re-enters its owner's
process. Rejected: a per-call-site reconciliation, which CLAUDE.md rules out,
and it leaves `refresh_carried/2`'s value test in place.

### Put the owner in the throw tuple
A fifth element `{'$bt_nlr', Tok, Val, State, Owner}`. Rejected: changes
`?IS_NLR` and every pattern the lint guards, and the owner is already known
at the boundary (`Self`).

### Make the cell authoritative for value types and the REPL too
Rejected: they have no store; adding one is a runtime mechanism for state
that threads correctly today.

## Consequences

### Positive
- One ownership test, by identity, in one module.
- BT-4459 is fixed by the general rule; BT-4448's mechanism is deleted.
- The `Block>>ensure:` fallback loses its special relay.
- The relay obligation narrows to threaded state, which `verify()` can see.

### Negative
- A `refresh/2` call on every actor-method NLR commit.
- The model rests on "the cell is current when a `^` is in flight", which is
  checked only once `NlrThrow` is a real IR node (§6).
- R1 needs `EnsureCatch`'s opaque `other` split into typed clauses.

### Neutral
- Value-type, class-method and REPL behaviour is unchanged.
- R2 is documented, not changed.

## Implementation

1. **Checks first.** Make `ThreadedStmt::NlrThrow` live for actor scopes and
   add `NlrThrowWithoutCellStore`; add `ActorNlrCommitBypassesCell`
   (fails on every actor method) with a `fault` switch.
2. **Boundary.** `nlr_arm_result`'s `ActorReply` arm renders
   `refresh(Self, State)`. Flips `ActorNlrCommitBypassesCell`. Add the
   BT-4459 BUnit cells (owner writes after creating the block; the same with a
   `perform:`ed `ensure:` in the owner) and the class-process hop.
3. **Delete** `NlrCarry::Foreign`, `RethrowSlot::CellIfCarried`,
   `refresh_carried/2`, the stamp and `unstamped/1`; the fallback keeps a plain
   catch. Update `ensure_relay_corpus.rs:446` and the EUnit cases.
4. **R1.** Split `EnsureCatch::other`; store the cleanup's result on a foreign
   `$bt_nlr`; add `EnsureForeignCleanupNotStored` and route-4 tests.
5. **Corpus.** Add the REPL scope and the "who throws" axis to
   `ensure_relay_corpus.rs`; add the untested rows of §7.

Components: codegen (`nlr.rs`, `threaded_ir/{ir,verify,emit}.rs`,
`exception_handling.rs`, `mod.rs`), runtime (`beamtalk_actor_state.erl`),
tests. No parser or REPL change.

## Open Questions

1. **Route 4 today.** In an actor method N, does a field write in an inlined
   cleanup on the error path reach the cell, or only the threaded `StateAcc`
   that the re-raise discards (`exception_handling.rs:1551-1557`)? ADR 0137
   §5 promises write-through only inside the protected body. A test decides.
2. **Route 1b seed.** With a live cell the relay seeds from
   `refresh(Self, StateAcc)` over the construct's entry scratch, not over the
   carried state (`exception_handling.rs:1675-1679`). Does that drop
   `__local__` keys a deeper `^` carried (for example a merged Tier 2 fun's
   flushed locals)? Not determined from the code.
3. **Merged Tier 2 reachability.** Can any validated program still lower a
   merged Tier 2 fun? `ensure_relay_corpus.rs:29-33` says only unvalidated
   compiles do. If none can, route 1d's Tier 2 half can be deleted rather than
   tested.
4. **R2.** Should an intermediate actor whose callback a foreign `^` leaves
   keep its writes? This ADR keeps today's answer (no).

## References
- Related issues: BT-4505, BT-4312, BT-4424, BT-4429, BT-4430, BT-4433, BT-4448, BT-4459, BT-4122, BT-4503
- Related ADRs: ADR 0041, ADR 0111, ADR 0130, ADR 0131, ADR 0133, ADR 0137, ADR 0142
- Documentation: `docs/development/actor-state-cell.md` § 5, `docs/development/debugging.md` § ThreadedIr verifier (`EnsureCleanupLostOnNlr`), `docs/beamtalk-language-features.md` § What Works and What Doesn't
