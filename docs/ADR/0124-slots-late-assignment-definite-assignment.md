# ADR 0124: Slots — Late Assignment and Definite Assignment

## Status
Proposed (2026-09-18)

**Two parts, both proposable.** This ADR decides how a slot declares *how it
becomes valid*. The driving issue framed the first half as lazy
initialisation; surveying the two real applications built on Beamtalk
(Exdura and Symphony) showed that is the wrong abstraction for what they do,
so Part B decides **late assignment** instead and records `lazy` as a
rejected alternative.

- **Part A — definite assignment (§6, §7).** A compile-time diagnostic at the
  construction site for a slot that is declared, non-nilable, defaulted
  nowhere, and assigned by no `initialize` in the chain. Small, low-risk,
  entirely in `beamtalk-core`.
- **Part B — `late` slots (§1–§5, §8, §9).** A slot that is **exempt from ADR
  0078's post-`initialize` check**, absent until assigned, and raises the
  existing `UninitializedStateError` if read before assignment. Its declared
  type is usually non-nilable — that is the point, since the `| Nil` such
  slots carry today is an escape from the check rather than a claim about the
  data — but `late … | Nil` is permitted and gives three states
  (absent / `nil` / a value; §1). No initialiser expression, so **reading
  never writes**, which is what keeps it small.

  **Part B's evidence is two slots**, and that is the main thing a reviewer
  should weigh. Successive passes over Exdura and Symphony narrowed it from
  "many nilable slots" to two genuine candidates (§Context (a)); most nilable
  slots in that corpus encode a *resolution strategy* in their `nil` and
  should not change. The pattern behind those two is real and general — a
  resource acquired by an explicit lifecycle call, whose declared type is
  non-nilable in truth — and the cost is now low enough (one guarded read,
  two reflective selectors, no lowering) that it is worth doing on two
  examples. But it is a judgement call, not a demand from the corpus, and
  **Part A is where the clear value is.**

The two are the same idea at two ends: Part A checks the slots that must be
valid after `initialize`, Part B declares the ones that legitimately are not
yet. A slot is `late` exactly when Part A's diagnostic would otherwise be
wrong.

**What changed from the first draft, and why.** The draft decided `lazy`
(memoise on first read). Review of `jamesc/beamtalk-exdura` and
`jamesc/beamtalk-symphony` found **zero memoise-on-first-read sites in 148
`.bt` files** — the `ifNil: [self.x := …]` idiom appears nowhere — while
every nilable resource slot is assigned by an explicit lifecycle method and
several are unset again. The `| Nil` on those slots is an escape hatch from
the post-`initialize` check, not a claim about optionality, and it costs every
downstream reader ADR 0107's nil-narrowing. `lazy` answered a question
nobody in the corpus was asking, and answering it required an invalidation
primitive and a `Result` channel it did not have (§Alternatives). `late`
answers the question they are asking, and because it has no initialiser it
avoids the entire state-threading lowering `lazy` needed (§4).

## Context

### Problem statement

A Beamtalk slot declaration (ADR 0067: `state:` on an Actor, `field:` on a
Value, `classState:` on any kind) says what data a class holds and,
optionally, what it starts as. What it does *not* say is **how the slot
becomes valid**. Three gaps follow from that omission:

1. **No way to declare a slot that is assigned after `initialize`.** A slot
   whose value is acquired by an explicit lifecycle call — open a subprocess,
   start a listener, attach a supervisor — has to be declared nilable and
   nil-checked at every read, because ADR 0078's post-`initialize` check
   raises `UninitializedStateError` on any non-nilable typed slot still unset
   when `initialize` returns.

   The stdlib is poor evidence here: its three nilable singletons
   (`transcript_stream.bt`, `beamtalk_interface.bt`,
   `workspace_interface.bt`) are injected by
   `beamtalk_workspace_bootstrap:bootstrap_singleton/3` (`:149-186`) with
   `erlang:monitor` and rebootstrap-on-death, and `retry_policy.bt`'s nilable
   field is a genuinely optional user-supplied value. **The evidence is in
   the two real applications built on Beamtalk** — Exdura (a workflow engine)
   and Symphony (an agent orchestrator), both cited as real-world corpora by
   ADR 0067. Surveyed at `exdura@d6b350e` / `symphony@d95ce02`.

   **(a) Every nilable resource slot is assigned by an explicit lifecycle
   method, and several are unset again.**

   | Slot | Assigned | Unset | A `late` candidate? |
   |---|---|---|---|
   | `symphony/codex_client.bt` `proc :: Subprocess \| Nil` | `launch` (`:97`) | `stopProcess` (`:71`) | **Yes** — a running client always has one; `nil` is purely "not launched" |
   | `exdura/exdura_http_server.bt` `httpServer` | in `initialize` (`:256`) | `destroy` | **Yes** — `nil` is purely absence |
   | `exdura/exdura_client.bt` `supervisor` | `setSupervisor:` (`:170`) | — | **No** — `nil` means "standalone mode" and selects a resolution strategy (`:144-155`); see (b) |
   | `exdura/exdura_client.bt` `httpServer` | lifecycle (`:190`) | — | **No** — genuinely optional per its own doc ("nil unless started with an `#http` config") |
   | `exdura/workflow_engine.bt` `eventStore`, `activityPool` | — | — | **No** — `nil` means "resolve by name"; see (b) and (d) |

   **The honest size of the evidence: two slots.** Three passes over this
   corpus each narrowed it — first from "none" to "many", then from "many" to
   "some", and now, after PR review pushed on the `supervised` claim, to two
   slots across two classes. They are genuine, and the pattern they share is
   real and general (a resource acquired by an explicit lifecycle call, whose
   declared type is non-nilable in truth), but Part B should be judged on two
   examples, not on the slot count in the survey.

   ```beamtalk
   typed Actor subclass: CodexClient
     state: proc :: Subprocess | Nil = nil
     ...
     launch -> Result(Nil, CodexError) =>
       procResult := [
         Subprocess open: "/bin/bash" args: #("-lc", self.config codexCommand)
           dir: self.workspacePath
       ] on: Error do: [:e | ^Result error: (CodexError notFound: e message)]
       ...
       self.proc := procResult unwrap
   ```

   `proc` is not optional — a running `CodexClient` always has one. The
   `| Nil` exists solely to survive the window before `launch`, and it makes
   every later `self.proc readLine:` / `writeLine:` / `close` a read of a
   nilable type.

   **(b) `nil` is overloaded — but in this corpus it is overloaded with
   *mode*, which `late` does not fix.** Exdura's engine
   (`exdura/src/workflow/workflow_engine.bt:20-26`) carries an extra slot
   that disambiguates `nil`:

   ```beamtalk
   typed Actor subclass: WorkflowEngine
     state: eventStore :: EventStore | Nil = nil
     state: activityPool :: ActivityWorkerPool | Nil = nil
     /// Set via `withArgs: #{#supervised => true}` on the ExduraSupervisor
     /// child spec. Distinguishes a nil `activityPool` that means "look it
     /// up by name, the supervisor's ActivityWorkerPool is up somewhere"
     /// from the legitimate standalone case (`withStore:`, no pool at all).
     state: supervised :: Boolean = false
   ```

   An earlier revision of this ADR called that the strongest evidence in the
   document and claimed `late` would retire the `supervised` flag. **Reading
   the resolvers shows that was wrong**, and PR review was right to push on
   it. `eventStore` (`:983`) is:

   ```beamtalk
     eventStore -> EventStore =>
       self.eventStore match: [
         nil -> (EventStore named: #eventStore) unwrap;
         s -> s
       ]
   ```

   `nil` there means **"resolve by name, freshly, every call"** — the comment
   says so explicitly, "rather than caching the lookup, means a
   `rest_for_one` restart of EventStore is picked up automatically" — and it
   never consults `supervised` at all. `currentActivityPool` (`:1005`) does
   consult it, but only to choose between *two* resolve-time behaviours
   ("resolve by name" vs "genuinely no pool"). So `supervised` is not a
   workaround for a missing representation; it is a flag selecting a
   **resolution strategy**, and `late` does not retire it: a `late … | Nil`
   slot offers absent / `nil` / value, while `activityPool` needs to
   distinguish two different meanings *of absence*.

   The same is true of `exdura_client.bt`'s `supervisor` (`:144-155`): nil
   means "standalone, use the direct refs", a set value means "supervised,
   resolve by name". The nil is load-bearing logic, not a gap.

   What survives from this example is narrower and still worth having:
   `nil` demonstrably ends up carrying structural meaning when authors have
   nowhere else to put it. That supports §2's rule — `nil` should not be the
   representation for "unassigned" — but §2 stands on its own ground (`nil`
   is *already* the compiler's "no value supplied", twice over), not on this
   example. **The corpus does not supply independent validation for §2, and
   this ADR no longer claims it does.**

   **(c) There is no memoise-on-first-read anywhere.** Across 148 `.bt` files
   in both applications, the classic `ifNil: [self.x := …]` lazy idiom occurs
   **zero times**. The only `isNil`-adjacent writes are
   `codex_client.bt:71`'s `self.proc := nil` (unsetting) and
   `workflow_watcher.bt:36`'s first-observation baseline for change
   detection. Deferred computation is not a need these codebases have.

   **(d) Some slots must never be memoised.** Exdura's timer manager
   (`exdura/src/timer/timer_manager.bt:15-20`) resolves by name on *every*
   read, deliberately:

   ```beamtalk
     /// (ADR 0079) instead of passing live refs, since neither exists yet
     /// when the child spec is built. `engine`/`eventStore` below resolve
     /// fresh by name in that case, so a `rest_for_one` restart of either is
     /// picked up automatically instead of leaving a nil/stale field.
     state: engine :: WorkflowEngine | Nil = nil
     state: eventStore :: EventStore | Nil = nil
   ```

   `exdura_client.bt:144-155` does the same via `currentEngine` /
   `currentEventStore`. These stay ordinary methods, which is already what
   their authors wrote — and they are the reason this ADR does not introduce
   a memoising slot kind that would invite converting them.

   Note that `state: x :: T = <expr>` already permits an arbitrary **eager**
   initialiser (`gen_server/state.rs:36`, `:98`). The gap is not computing a
   value late; it is *declaring that a slot is legitimately unset for a
   while* without lying about its type.

2. **Definite assignment is enforced only at runtime.** ADR 0078 added a
   post-`initialize` check: after the auto-chained `initialize` sequence, any
   slot that is annotated, has no default, and whose type does not admit
   `Nil` must be non-`nil`, or the actor fails to start with
   `UninitializedStateError`. It works, it is not gated on the `typed`
   keyword (`stdlib/test/fixtures/non_typed_uninitialized_state_actor.bt`
   pins that), and it is a *spawn-time crash* for a mistake that is usually
   visible in the source:

   ```beamtalk
   // stdlib/test/fixtures/uninitialized_state_actor.bt
   typed Actor subclass: UninitializedStateActor
     state: connection :: String
     state: count :: Integer = 0

     initialize -> Integer =>
       // Deliberately does NOT set self.connection
       self.count := 1
   ```

   Nothing tells the author at compile time. The LSP is silent, `beamtalk
   build` is silent, and the first signal is a failed `spawn`.

3. **Slot kind is invisible to reflection and tooling.** ADR 0035's
   field-based reflection answers *which* slots exist and *what* they hold.
   It cannot answer *how* a slot is initialised, so ADR 0095's inspector has
   no way to distinguish "this slot holds `nil`" from "this slot has not
   been assigned yet" — a distinction that only exists once `late` slots do.

### Current state

**The runtime predicate.** `generate_post_initialize_check`
(`crates/beamtalk-codegen/src/core_erlang/gen_server/callbacks.rs:474`)
emits, per qualifying slot, a `maps:get(Slot, InitNewState)` and a `case`
on `'nil'`. Qualifying is decided by `inherited_typed_no_default_fields`
(`:709`) walking the flattened superclass chain, with the per-slot test
spelled out at `:723`:

```rust
s.type_annotation.is_some()
    && s.default_value.is_none()
    && !Self::is_nilable_type(s.type_annotation.as_ref())
```

`is_nilable_type` (`:619`) and `is_nilable_type_name` (`:806`) are
codegen-local. That predicate is a *semantic* property of a declaration, and
a compile-time check needs the identical answer — so its current home is the
wrong layer (see Decision §7).

**Slot defaults.** An eager initialiser is an arbitrary expression compiled
into `init/1`'s state map literal (`gen_server/state.rs:36`, `:98`); a slot
with no initialiser is emitted as `'nil'`. So `nil` is *already* the
compiler's stand-in for "no value was supplied", which is exactly why it
cannot also mean "not computed yet".

**Reserved state-map keys.** Actor state carries `'__methods__'`,
`beamtalk_tagged_map:class_key()`, `'__class_mod__'`, ADR 0123's
`'__shape_version__'`, and transient `'__local__'`-prefixed threading temps
(`beamtalk_actor.erl`, `strip_local_temps/1`). `init/1` validates only
`'$beamtalk_class'` and `'__methods__'` (`beamtalk_actor.erl:1582`), so a
missing *user* slot key breaks no existing invariant.

It is worth being precise about what this does **not** establish.
`beamtalk_actor:changed_state_keys/2` (`beamtalk_actor.erl:1963`) uses
`'__absent__'` as a local `maps:get/3` default to compare two maps. It is
never stored, returned, or observed. So an absent key in a *live* actor state
map would be genuinely new, and §2 has to carry that cost rather than borrow
credibility from a comparison placeholder.

**Reads that mutate state already exist.** ADR 0118 made every expression
in a state-threading context return `ThreadedValue { prelude, value }`, where
the prelude may advance any versioned prefix. ADR 0122 unified the three
storage families (`VersionPrefix::{State, ClassVars, SelfVt}`), and
`ThreadedIr::verify()` checks produced/consumed balance for all of them.
A slot read that writes back a computed value is therefore a shape the IR
already models — it is a prelude with one `State` (or `ClassVars`) step.

**ADR 0123 deferred part of this to here, by name.** Its References
section records BT-3525 as owning *"the 'absent → declared initialiser
policy' generalisation of reconcile step 3"*. Reconcile step 3 currently
says: a declared slot present in the migrated dictionary is kept; absent
takes its declared default; absent with no default is `nil` on an untyped
class and a failure on a `typed` one.

### Constraints

- **The runtime check cannot be deleted.** Slots are writable through
  `fieldAt:put:`, `perform:`, `spawnWith:`, `ClassBuilder` (ADR 0038), and a
  hot-patched `initialize` (ADR 0082/0084). Static analysis in an open world
  (ADR 0100) is advisory; `UninitializedStateError` stays as the backstop.
- **Value instances are plain maps with no owning process** (ADR 0042).
  There is nothing to memoise *into* without returning a new instance.
- **State-threading codegen must go through `ThreadedIr`** and be covered by
  `verify()` — no ad-hoc `debug_assert!` at a new call site (ADR 0111).
- **An initialiser runs inside the owning gen_server.** Note what this does
  *not* buy: ADR 0043 (`:104`) records that "self-sends within actor methods
  already bypass gen_server (calling `Module:dispatch` directly to avoid
  deadlock)", so a self-send in an initialiser is a direct call on the same
  stack, not a serialised message. What deadlocks is a synchronous message to
  the actor's *own pid* via a captured reference or `Actor named:`.
- **ADR 0078 left room for this deliberately**: "Any solution should not
  preclude future evolution toward declarative slot initializers
  (Newspeak-style)."

## Decision

Give every slot a **declared story for how it becomes valid**, and check the
consequences at compile time where they are provable.

1. `late` as a declaration-level modifier on `state:` and `classState:`.
2. **Key absence**, not `nil` or a sentinel value, as the representation of
   an unassigned `late` slot.
3. A **definite-assignment analysis** in `beamtalk-core` that reports
   statically, at the construction site, what ADR 0078's check reports at
   spawn time — with `late` slots exempt.

**Why these are one ADR.** They are the same rule from both sides. Part A
says "a non-nilable, defaulted-nowhere slot must be assigned by the time
`initialize` returns"; `late` is the declaration that says "not this one, and
here is what happens if you read it early". Neither is complete without the
other: without `late`, Part A's diagnostic is wrong for every
lifecycle-assigned slot in Exdura and Symphony; without Part A, `late` is a
keyword with no check behind it.

### 1. `late` — a declaration-level modifier

```beamtalk
typed Actor subclass: CodexClient
  late state: proc :: Subprocess
  state: nextId :: Integer
  state: workspacePath :: String

  launch -> Result(Nil, CodexError) =>
    procResult := [
      Subprocess open: "/bin/bash" args: #("-lc", self.config codexCommand)
        dir: self.workspacePath
    ] on: Error do: [:e | ^Result error: (CodexError notFound: e message)]
    procResult isError ifTrue: [^Result error: (CodexError notFound: procResult error displayString)]
    self.proc := procResult unwrap
    Result ok: nil

  sendLine: line :: String -> Nil => self.proc writeLine: line
```

`launch` is **unchanged from the real code** — it keeps its
`Result(Nil, CodexError)` contract, because `late` adds no initialiser
expression and takes nothing over. What changes is the declaration:
`proc :: Subprocess` instead of `proc :: Subprocess | Nil = nil`, so
`self.proc writeLine:` reads a `Subprocess`, not a nilable, and every reader
downstream keeps ADR 0107's narrowing.

The modifier precedes the declaration keyword, matching the existing
class-level modifier position (`sealed typed Collection subclass: Array`,
parsed by the modifier loop at `declarations.rs:192-208`). `late` is
currently unused as a word anywhere in the stdlib's `.bt` sources, so it can
be a contextual keyword in this position without colliding with a selector
or identifier.

**Where `late` is permitted:**

| Declaration | `late` | Rationale |
|---|---|---|
| `late state:` (Actor) | **Yes** | The instance gen_server holds the slot |
| `late classState:` (any class kind) | **Yes** | The class gen_server holds it (ADR 0036); ADR 0056 permits `classState:` on `native:` actors too |
| `late field:` (Value) | **Error** | A Value is fully constructed by `new`/`new:`/the keyword constructor and never mutated (ADR 0042) — "assigned later" has no meaning; see §5 |
| `late state:`/`field:` on `Object` | **Error** | Already an error — Object holds no instance data (ADR 0067) |
| `late state:` on a `native:` Actor | **Error** | Already an error — ADR 0056 prohibits `state:` there outright |

**A type annotation is required.** `late state: proc` with no `::` is an
Error: the entire purpose is to declare a non-nilable type for a slot that is
temporarily unset, and an untyped slot already defaults to `nil` with no
check. This is the one place `late` is stricter than the plain form.

**A default is forbidden.** `late state: x :: Integer = 0` is an Error — a
slot with a default is never unset, so `late` is meaningless on it.

**`| Nil` is still allowed, and means something distinct.** A
`late state: x :: T | Nil` slot has three states rather than two:

| State | Meaning | Test |
|---|---|---|
| absent | not assigned yet | `(self hasField: #x) not` |
| `nil` | assigned, and legitimately empty | `self.x isNil` |
| a value | assigned | — |

This is useful where "never set" and "set to nothing" are different facts —
a cache that has been explicitly emptied versus never populated. It is *not*
a general replacement for a mode flag: if absence itself needs to carry two
meanings, as in Exdura's `activityPool` (§Context (b)), three states are not
enough and an explicit flag remains the right answer.

### 2. Representation: the key is absent until assigned

A `late` slot's key is **not present** in the state map until it is assigned.
It is not `nil`, and not a reserved sentinel value.

- The presence test is `maps:is_key/2`. No assigned value — including `nil`,
  `false`, or `'__absent__'` itself — can be mistaken for "unassigned".
- **The ground for this is compiler-internal, not corpus evidence.** `nil` is
  *already* the representation for "no value supplied" in two places —
  `state.rs:36` emits `'nil'` for a defaultless slot, and ADR 0078's check
  reads `'nil'` as unassigned — so a third structural meaning could not be
  told apart from either. An earlier revision cited Exdura's
  `supervised :: Boolean` flag as independent real-world support for this;
  §Context (b) retracts that, because the flag selects a *resolution
  strategy* rather than working around a missing representation. The
  argument here does not depend on it.
- `beamtalk_actor:init/1` validates only `'$beamtalk_class'` and
  `'__methods__'` (`beamtalk_actor.erl:1582`), so a missing *user* slot key
  breaks no existing invariant.
- **`late` slots are excluded from ADR 0078's post-`initialize` check.** That
  is the point of the feature, and it is also a required change rather than a
  nicety: the check emits 2-arity `maps:get(Slot, InitNewState)`, which would
  `badkey`-crash on an absent key.

Being precise about what this does *not* establish:
`beamtalk_actor:changed_state_keys/2` (`beamtalk_actor.erl:1963`) uses
`'__absent__'` as a local `maps:get/3` default to compare two maps. It is
never stored, returned, or observed, so an absent key in a *live* state map
is genuinely new and this ADR carries that cost rather than borrowing
credibility from a comparison placeholder.

`nil` keeps its present meaning — the value of an unannotated, undefaulted
slot — and is now free to mean "legitimately none" on a `late … | Nil` slot.

### 3. Read and write semantics

**Reading never writes.** This is the structural difference from a memoising
design and the reason Part B is small.

A read of a `late` slot compiles to a guarded map read:

```text
case maps:find(Slot, State) of
  {ok, V}  -> V
  'error'  -> <raise uninitialized_state_error, naming the slot and class>
end
```

`generate_field_access` (`expressions.rs:519`) stays **pure** — it still
returns `Result<Document<'static>>` with no prelude channel — because the
guard produces a value and never a new state. Consequently **none** of the
following applies to `late` slots: ADR 0118 preludes, `ThreadedIr` state
threading, the `ClassVars` mutation detector (`analysis.rs:253`), ADR 0110's
shadow write, loop-hoisting interference (`loop_mode.rs:118`), or a
`terminate:`/`handle_info` returned-state problem. Those were all
consequences of *reading* being a mutation, which `late` does not make it.

**Writing is the existing syntax, unchanged.** `self.proc := v` assigns and
so marks the slot present. No new assignment form.

**The error is the existing one.** Reading an unassigned `late` slot raises
`uninitialized_state_error` (`beamtalk_error.erl:433`), the same error ADR
0078 already raises for the same underlying mistake — read-before-assign —
just detected at the read instead of after `initialize`. Its hint names the
slot, its declared type, and the owning class:

```beamtalk
c := CodexClient start: "/tmp/ws" config: cfg onEvent: nil
c sendLine: "hello"
// => UninitializedStateError in 'sendLine:' on CodexClient
//    hint: CodexClient field 'proc' (:: Subprocess) is declared `late` and
//          has not been assigned yet
```

That is a strict improvement on the status quo, where reading too early
answers `nil` and the failure surfaces as a `does_not_understand` for
`writeLine:` on `nil`, several frames from the real cause.

**Two new surfaces, both small and both demanded by the real code.**

`hasField:` — a presence test that does not raise. Symphony's cleanup guard
needs exactly this, and it is the one line of the real code that must change:

```beamtalk
  // before: self.proc isNil ifFalse: [...]
  stopProcess -> Nil =>
    (self hasField: #proc)
      ifTrue: [
        self.proc close
        self.proc stop
        self clearField: #proc
      ]
    nil
```

`clearField:` — returns a `late` slot to the unassigned state, so the
resource can be re-acquired. Symphony clears `proc` so `launch` can run
again; Exdura's `ExduraHttpServer destroy` does the same for `httpServer`.
Both are `field`-prefixed, joining ADR 0035's existing family (`fieldNames`,
`fieldAt:`, `fieldAt:put:`, `allFieldNames`) rather than inventing new
vocabulary, and neither currently exists.

**Why not let `self.proc := nil` mean "unassign"?** It is tempting — on a
non-nilable `late` slot, `nil` cannot be a legitimate value, so there would
be no ambiguity, and Symphony's `stopProcess` would need no edit at all.
Rejected because this ADR's central argument is that `nil` should stop
carrying structural meaning; re-introducing it as a magic unassign, even
unambiguously, trades the thesis for one saved line. Assigning `nil` to a
non-nilable slot stays the type error it is today, and its message names
`clearField:` as the fix.

### REPL session

```beamtalk
c := CodexClient start: "/tmp/ws" config: cfg onEvent: nil
c hasField: #proc                      // => false — declared, not yet assigned
c launch                               // => Result ok: nil
c hasField: #proc                      // => true
CodexClient fieldKinds                 // => #{#proc => #late, #nextId => #eager, ...}
c stopProcess                          // => nil
c hasField: #proc                      // => false — cleared, relaunchable
```

### 4. What it costs

Far less than the memoising design this replaces, because reading is not a
mutation — but not nothing.

**a. `generate_field_access` needs a guarded form for `late` slots.** Today
it emits 2-arity `maps:get` for the Actor, class-method (`:526`) and Repl
contexts, and an absent key there is a `badkey` crash. The `late` read must
emit `maps:find` plus the error arm. The function stays pure, so its ~15
sibling state `maps:get` sites (`blocks.rs:494,504,613,745,769,788`;
`control_flow/body.rs:1389,1424`; `control_flow/util.rs:53`;
`conditionals.rs:2172`; `while_loops.rs:896`; `dispatch_codegen.rs:3309`;
`threaded_expr.rs:533`; `threaded_ir/emit.rs:730`) need auditing for the
same badkey exposure, but **not** re-architecting for a prelude channel.

**b. Loop hoisting must not pre-extract a `late` slot.**
`hybrid_readonly_field_params` (`loop_mode.rs:118`) substitutes a field's
value before the letrec instead of emitting a read. For a `late` slot that
converts "raises when read inside the body" into "raises before the loop,
even if the body never runs". DirectParams/Hybrid hoisting must fall back to
the guarded read for `late` slots. This is the one place the feature touches
the loop optimiser, and it is a fallback, not a new threading mode.

**c. Reads of a `late` slot cost a `maps:find` rather than a `maps:get`.**
Eager slots are unaffected — the cost is scoped to slots that opt in.

**d. `terminate:` and `handle_info` need a diagnostic, not a redesign.** An
unguarded `late` read in `terminate:` raises; `terminate/2` wraps the
dispatch in `try … catch → 'ok'` (`gen_server/callbacks.rs:1560`), so
shutdown still completes, but the cleanup silently does not happen. Warn on
an unguarded `late`-slot read reachable from `terminate:`, with `hasField:`
as the fix. Cheap — it is a per-class walk with no dataflow — and it is the
real hazard in Symphony's `terminate: → stopProcess`. Note how much smaller
this is than the memoising version, where the same code path would have
*acquired a subprocess during shutdown in order to close it*.

**e. Reflection needs metadata for the slot kind** (§9), which is the largest
remaining cost and is `ClassInfo`/`__beamtalk_meta` plumbing rather than
codegen.

No open items block Part B. The two that blocked the memoising design —
invalidation, and an initialiser that can answer a `Result` — do not arise:
`clearField:` is the invalidation, and there is no initialiser to give a
`Result` to.

### 5. Value classes: `late field:` is rejected

A Value is fully constructed by `new`, `new:`, or its auto-generated keyword
constructor, and `self.slot :=` is a compile error (ADR 0042, ADR 0067).
"Assigned later" has no meaning for a thing that is never assigned at all, so
`late field:` is an Error whose message points at the two forms that do
express the intent:

```beamtalk
Value subclass: Config
  field: endpoint :: String = ""
  late field: client :: HttpClient
// error: 'late' is not allowed on a Value 'field:' — a Value is fully
//        constructed and never reassigned. Either make the field optional
//        (`field: client :: HttpClient | Nil = nil`) or hold the resource in
//        an Actor.
```

The survey supports this: every nilable `field:` in Exdura and Symphony —
`workspace_error`, `linear_error`, `issue`, `activity_outcome`,
`retry_snapshot_entry` — is **optional data**, where `nil` is a meaningful
value and a widened type is the honest declaration. None is a
late-assignment window. The Value auto-constructor and `new:` key validation
are therefore untouched: `compute_auto_slot_methods` returns `None` for
non-Value kinds (`value_accessors.rs:63`), and no Value construction path
changes.

### 6. Definite-assignment analysis

A new semantic-analysis pass in `beamtalk-core` reports at compile time what
ADR 0078's check reports at spawn time.

**Which slots it covers** — the identical predicate the runtime uses,
unchanged and now shared (§7): annotated, no default, type does not admit
`Nil`, **and not `late`**. So:

| Declaration | Requires assignment? |
|---|---|
| `state: count :: Integer` | Yes |
| `state: label :: String \| Nil` | No — `nil` is a valid value |
| `state: count :: Integer = 0` | No — has a default |
| `state: count` | No — untyped, defaults to `nil` |
| `late state: proc :: Subprocess` | No — declared as assigned later (§1) |

**Not gated on `typed`.** The runtime check is not, and two fixtures pin it
(`non_typed_uninitialized_state_actor.bt`,
`non_typed_no_default_actor.bt`). A static check that disagreed with the
runtime check on the same program would be worse than no check. `typed`
changes only whether the *annotation* is required (ADR 0025), never what
the annotation then means.

**What it analyses.** The ADR 0078 auto-chained `initialize` sequence,
parent-first over the flattened chain, branch-aware: a slot assigned in only
one arm of an `ifTrue:ifFalse:` is not definitely assigned, and a slot
assigned inside a block that may not run is not either.

```beamtalk
typed Actor subclass: Connection
  state: socket :: Socket
  state: retries :: Integer = 0

  initialize -> Nil =>
    self.retries > 0 ifTrue: [self.socket := Socket open]
    nil

Connection spawn
//         ^ warning: 'Connection' declares 'socket :: Socket' with no
//           default. Its 'initialize' assigns it only in the 'ifTrue:' arm,
//           and this spawn supplies no 'socket', so it may raise
//           UninitializedStateError.
```

Deciding "assigns it" needs the conditional arm to count as *not* definitely
assigning — so the per-class half is still a branch-aware walk of the
`initialize` chain. What moves to the construction site is only *where the
finding is reported* and *what counts as satisfying it*, which is what
removes the non-local suppression rule.

**Where it runs: the construction site, not the declaration.** This is the
design change review produced, and it is a strict improvement.

The obvious reading of "definite assignment" is a branch-aware dataflow pass
over the `initialize` chain, reported on the *declaration*. That design has a
non-local flaw: a slot may legitimately be supplied by `spawnWith:`, so the
declaration-site warning has to be suppressed whenever some call site
supplies the key — which makes a diagnostic on a declaration depend on
whether an unrelated call site happens to live in the same file. Add a
`spawnWith:` anywhere and every warning for that class disappears, including
for spawns that do *not* supply the key.

Reporting at the construction site inverts that, and the evidence there is
complete:

```beamtalk
Counter spawnWith: #{}
//      ^ warning: 'Counter' declares 'count :: Integer' with no default, and
//        no 'initialize' in its chain assigns it. This spawn supplies no
//        'count', so it raises UninitializedStateError. Supply it here, give
//        the field a default, widen the type to 'Integer | Nil', or declare
//        it 'late'.
```

**This needs almost no new machinery.** The literal `spawnWith:` map is
*already* inspected — `docs/beamtalk-language-features.md:2426` validates its
keys against declared `state:` slots and warns on an unknown key with a typo
suggestion naming the nearest slot. Definite assignment is one more predicate
over that same literal map: *declared, no default, non-nilable, not `late`, not
assigned in the visible `initialize` chain, and not a key of this map.* No
branch-aware dataflow pass, no whole-program scope, and near-zero false
positives, because a literal init map is complete evidence about that one
construction.

`Counter spawn` (no map) is the same check with an empty key set, which is
exactly the `uninitialized_state_actor.bt` fixture case.

The declaration-site dataflow pass remains a possible later addition for
non-literal construction (`Counter spawnWith: someMap`), where the call site
proves nothing. It is recorded under Alternatives, not adopted here.

**Value classes are a separate case, and a sharper one.** Everything above
is about Actors and the ADR 0078 `initialize` chain. Values have no
`initialize` and no gen_server, and `generate_post_initialize_check` is
called only from `generate_handle_continue`
(`gen_server/callbacks.rs:857`) — so **a typed-no-default `field:` on a Value
has no runtime check at all today.** `StoredSnapshot new` on

```beamtalk
typed Value subclass: StoredSnapshot
  field: state :: ReplaySnapshot      // typed, no default
  field: eventId :: Integer = 0
```

silently yields `state = nil` (`state.rs:36` emits `'nil'` for a defaultless
slot) behind a non-nilable declared type, with nothing to catch it. For
Actors the static check is an early warning in front of a runtime backstop;
**for Values it is the only check there will ever be.**

Values are also much closer to closed-world than Actors, which makes the
check more reliable rather than less:

| Assignment path | Actor | Value |
|---|---|---|
| `fieldAt:put:` | permitted | **blocked** — "Cannot modify slot on value type; use withSlot:" (`beamtalk_primitive.erl:988`) |
| hot-patched `initialize` (ADR 0082/0084) | yes | n/a — Values have no `initialize` |
| `spawnWith:` | yes | n/a |
| construction | `spawn`/`spawnWith:` | `new`, `new:`, the auto-generated keyword constructor |

So the construction sites to check on a Value are:

| Site | Verdict |
|---|---|
| `Cls new` (bare) | supplies nothing, so **every** typed-no-default non-nilable field is unassigned. The highest-certainty finding in the ADR — nothing can intervene between `new` and the result |
| `Cls new: #{…}` (literal map) | check keys, exactly as for `spawnWith:` |
| `Cls field1: v1 field2: v2` (auto-generated keyword constructor) | **always satisfies** — it requires every field by construction (ADR 0042). No check needed, and worth stating so implementers do not add one |
| `Cls new: someMap` (non-literal) | no evidence, report nothing |

**The class-method constructor is the right place to report, and it is
better than the call site.** Exdura's Values are constructed through
factories, not `new:` directly:

```beamtalk
typed Value subclass: WorkflowHandle
  field: workflowId :: String = ""
  field: client :: ExduraClient          // typed, no default

  class for: workflowId :: String client: aClient :: ExduraClient -> WorkflowHandle =>
    self new: #{#workflowId => workflowId, #client => aClient}
```

The literal map lives *inside* `for:client:`, which is a site the compiler
sees, and it supplies `client` — so the class passes with no diagnostic
anywhere. Because the factory is the single canonical construction site per
Value, the check fires **once per class** rather than at every caller, and
public callers of `WorkflowHandle for:client:` never need to know. That is
strictly better than a declaration-site finding, and it is the same argument
as for `spawnWith:` above.

**The limit: a Value can arrive without being constructed.** Exdura's
`StoredSnapshot` is built by deserialization, not by `new:`:

```beamtalk
  class fromBinary: content :: Binary -> StoredSnapshot =>
    result :: StoredSnapshot := Binary deserialize: content
    result
```

There is no construction site at all — the local type annotation is an
*assertion* at a type-erasure boundary, which the language guide already
names as the purpose of `name :: Type := expr`. This analysis cannot check
it, and neither can anything else: `state :: ReplaySnapshot` is unverified
for every instance that arrives this way. The decision is to **report
nothing** here, consistent with ADR 0100's silence where knowledge is
absent, and to document the gap rather than imply a guarantee. `late` is not
an escape hatch for it either, since §5 forbids `late field:` on a Value.
A class in this position should either default the field or widen it to
`| Nil` to make the unverified state honest.

**Severity** (`DiagnosticCategory::DefiniteAssignment`, a new category so
suppression can target it):

| Situation at a construction site | Severity |
|---|---|
| Literal init map (or bare `spawn`), chain fully visible, slot unassigned and not supplied | **Warning** |
| Literal map supplies the slot | **nothing** — that is an assignment |
| Chain incomplete — cross-package parent, `native:` ancestor (ADR 0056), or a `fieldAt:put:`/`perform:` writer in the class | **Hint** |
| Non-literal init map (`spawnWith: someMap`) | **nothing** — no evidence |
| Slot is `late`, defaulted, or nilable | **nothing** |

Warning, never Error by default, because the open world is real: an Actor's
slot may be written by `spawnWith:` from another package, by `ClassBuilder`
(ADR 0038), or by a hot-patched `initialize`. This matches the precedent
already set for a provably-failing construction — an unknown `spawnWith:` key
is a Warning, not an Error.

On a **Value** the same Warning severity holds, for a different reason. The
open-world escapes above are mostly closed (`fieldAt:put:` is blocked, there
is no `initialize`), so certainty is higher and an Error would be tempting —
but `ClassBuilder` and the deserialization boundary above remain, and ADR
0100 sets severity from certainty, not from appetite. What does change is the
*stakes*: a Value's Warning has no runtime backstop behind it, so a
Value-heavy codebase has materially more reason to escalate this category to
Error via ADR 0100 Rule 3's `[diagnostics]` table. That asymmetry belongs in
the docs. Escalation to Error is available per-project through ADR 0100
Rule 3's `[diagnostics]` table, and suppression is `@expect
definite_assignment` on the declaration. `StateDeclaration.expect` already
exists (`ast/class.rs:520`) and `class_variables` shares the same type
(`:174`), so the *carrier* is free — but an unknown `@expect` category is a
parse error, so this does need a new `ExpectCategory` variant plus its
`from_name` entry (`ast/expression.rs`) and unparse name (`unparse/mod.rs`),
alongside the new `DiagnosticCategory`. An earlier draft claimed "no parser
work"; that was wrong.

**Supplying a `late` slot at `spawnWith:` counts as assigned.** `init/1`
merges with the caller winning — `maps:merge(DefaultState, InitArgs)`
(`gen_server/callbacks.rs:73`, `:90`, `:221`, `:277`) — so
`CodexClient spawnWith: #{#proc => aFakeSubprocess}` puts the key in the map,
so under §2 the slot is present and no `hasField:` guard reports otherwise.
This is **legitimate dependency injection** and is decided as such rather
than warned about: it is what makes a `late` slot testable by substitution,
and Exdura already relies on the mechanism — `exdura_client.bt`'s
`class connect:` supplies both of its typed-no-default slots via
`spawnWith: #{#engine => eng, #eventStore => store}`. It does need a clause in the
`docs/beamtalk-language-features.md:2426` key-checking rule so the behaviour
is documented rather than incidental.

**`late` is the principled cure**, and the diagnostic says so. A slot that
cannot be assigned in `initialize` — because it is acquired by an explicit
lifecycle call — should be `late`, not nilable. That is what makes these two
features one ADR rather than two: **every slot gets a declared story for how
it becomes valid**, and the analysis is just the compiler checking the story.
The diagnostic's three suggested fixes are therefore: supply it at the
construction site, give it a default, or declare it `late`.

**The runtime check is retained.** This ADR adds a static signal; it does
not remove the backstop.

### 7. Single source of truth for the predicate

The "requires definite assignment" predicate currently lives in
`beamtalk-codegen` (`callbacks.rs:619`, `:709`, `:723`, `:806`). The static
check needs the same answer, and `beamtalk-core` sits *below*
`beamtalk-codegen`, so the predicate **moves down** into `beamtalk-core`
semantic analysis and `inherited_typed_no_default_fields` calls it. No copy,
no "mirrors" comment — this is the shared-leaf-module pattern applied in its
simplest form (`docs/development/architecture-principles.md` §6), and the
resulting consistency test between the static check and the emitted runtime
check becomes an ordinary unit test of one implementation
(architecture-principles §7, delete-the-copy disposition).

This is a hard requirement, not a preference. The diagnostic must appear in
the LSP, and `just check-codegen-boundary` (`Justfile:666`, part of `just
ci`) asserts via `cargo tree -i` that **`beamtalk-lsp` and `beamtalk-lint` do
not depend on `beamtalk-codegen` at all**. So a definite-assignment check that
read the predicate from its current home could not be surfaced by the LSP
without breaking CI. Leaving a copy behind in codegen would pass the boundary
check and fail the duplication rule; moving it satisfies both.

One rule genuinely does cross the Rust/Erlang boundary and therefore needs
enforcement rather than a comment: **the absent-key reconcile table**,
which `beamtalk-codegen` and `beamtalk_shape_migration`/`beamtalk_hot_reload`
must agree on. It gets a shared conformance fixture — a table of
`(slot kind, key present?, has default?) -> outcome` that drives both the
Rust golden test and the Erlang EUnit test, per architecture-principles
§7's "boundary you cannot delete" rule.

### 8. Interaction with ADR 0123 (migration, serialisation, distribution)

ADR 0123's reconcile step 3 gains the `late` case it deferred to this ADR:

| Declared slot | In migrated dictionary | Outcome |
|---|---|---|
| eager, has default | present | kept |
| eager, has default | absent | declared default |
| eager, no default | absent | `nil` (untyped) / failure (`typed`) — unchanged |
| **`late`** | **present** | **kept** — the assigned value survives the reload |
| **`late`** | **absent** | **stays absent** — still unassigned after the reload |

**Premise: ADR 0123 is implemented before Part B starts.** Its epic is in
flight — BT-3531, BT-3534 and BT-3535 have landed as of `main@80b8db4`, and
`beamtalk_shape_migration` (BT-3536) is next — and this ADR assumes it
completes first. So Phase B6 is **sequenced after ADR 0123, not blocked by
it**, and the row above is an edit to `beamtalk_shape_migration`'s step 3 in
whatever form BT-3536 lands it.

One finding needs re-measuring rather than inheriting. Against today's tree,
`beamtalk_hot_reload:migrate_fields/2` (`:192-250`) derives its keep set from
`beamtalk_behaviour_intrinsics:classAllFieldNamesByName/1`, **not** from the
defaults map — so the two `late` rows fall out with no change to
`beamtalk_hot_reload`. That is a measurement of pre-0123 code; if 0123's
later phases route reconcile through `beamtalk_shape_migration`, B6 must
re-measure it.

Either way the invariant is what matters and is silently load-bearing:
**declared-but-absent `late` slots must stay in `allFieldNames`.** If any
implementation derives the keep set from `init/1` defaults (where `late`
slots are absent by design), an assigned `late` value would be dropped on
reload with a spurious "Hot reload dropped fields" warning. That needs an
explicit invariant and a test wherever reconcile ends up living.

- `migrateFromVN:` hooks see a `Dictionary` in which an unassigned `late`
  slot is simply **not a key**. ADR 0123 already instructs hooks to read
  possibly-absent keys with `at:ifAbsent:` / `includesKey:`, so this needs no
  new hook contract.
- **Changing a slot between eager and `late` is a shape change** and bumps
  `shapeVersion:` like any other, because the reconcile row it takes changes.
- **Cross-node (BT-3527): an unassigned `late` slot travels as absent.** The
  receiving node's first read raises until something assigns it, which is the
  honest outcome — and it is an argument *for* `late` over a widened nilable
  type, because a node-local handle that should not have been shipped now
  fails loudly at the read instead of silently answering `nil`. There is no
  initialiser to re-run on arrival, so nothing is recomputed implicitly.
- **Persistence keeps assigned values.** They are ordinary values. A `late`
  slot holding a node-local handle must not be persisted — and an earlier
  draft delegated that to ADR 0103's `handleScope:`, which **does not hold
  today**: `grep handleScope stdlib/src/*.bt` returns nothing, ADR 0103
  (`:138`) deliberately deferred the declarations, and `Ets` declares none, so
  it sits at tier `Unknown` and is silent. That question has no mechanism
  behind it yet; it is an open item for the persistence work, not something
  this ADR solves.

### 9. Reflection, inspector, LSP

**The force / no-force problem disappears.** A memoising design had to split
reflective reads into those that run the initialiser and those that must not,
because observation could otherwise execute user code — opening an inspector
on an actor might open a socket. `late` slots have no initialiser, so
**nothing can force anything**, and every surface can answer honestly with no
side effects.

| Surface | Behaviour on an unassigned `late` slot |
|---|---|
| `self.slot` (direct read) | raises `uninitialized_state_error` |
| `anActor hasField: #slot` (**new**) | `false` — never raises |
| `anActor clearField: #slot` (**new**) | returns the slot to unassigned |
| `anActor fieldAt: #slot` | raises, consistently with the direct read |
| `anActor fieldAt: #slot put: v` | assigns, as today |
| `Cls fieldNames`, `Cls allFieldNames` | unchanged — the *declared* schema, so `late` slots are listed (ADR 0035 `:73`) |
| `anActor fieldNames` | unchanged — the *actual* keys, so an unassigned `late` slot is **not** listed |
| `Cls fieldKinds` / `allFieldKinds` (**new**) | `Dictionary(Symbol, Symbol)`, `#eager \| #late` |
| Inspector (ADR 0095), `sys:get_state`, `observer`, `recon` | render it as unassigned; cannot perturb anything |
| LSP hover | shows the declaration as written, including `late` |

`fieldAt:` raising rather than answering `nil` is the deliberate choice:
reflection should agree with the direct read, and `hasField:` is the
non-raising question. That also means `beamtalk_reflection:read_field/2`
(`beamtalk_reflection.erl:47`, currently `maps:get(Name, State, nil)`) needs
a declared-`late` branch — keyed on *declared late*, not on mere absence,
because `read_field/2` defaults any missing key to `nil` and a typo'd field
name is also missing. Unlike the memoising design, this branch needs no
generated per-module hook and no state threading in the `'fieldAt:'` dispatch
arm (`beamtalk_object_ops.erl:108`), which can keep returning `State`
unchanged.

The instance-vs-class `fieldNames` split is ADR 0035's deliberate design
(`:73`), so `late` slots make an existing distinction visible rather than
creating an inconsistency — but it must be documented, because
`c fieldNames` omitting `proc` while `c hasField: #proc` answers `false`
should read as consistent, not as a bug.

`printString`/`displayString` omit an unassigned slot:
`beamtalk_object_printer:structural_from_state/1` (`:113`) renders present
keys only. That is a **REPL-visible output change**, which under `CLAUDE.md`
needs explicit confirmation before implementation; it is called out here so
the gate is not discovered late. `InspectorField` gains `#lateSlot` to its
`kind` vocabulary (alongside `#slot #element #association #processInfo`) with
`value: #notAssigned` — matching the house convention for an absent reading,
`InspectorField name: #status value: #unavailable`
(`stdlib/src/inspector.bt:96`) — and `drillable: false` until assigned.
`#lateSlot` must also reach the documented cross-surface wire form
(`inspector.bt:262`) and `beamtalk_inspector:fieldsOf/1`, which reads only
the state map and so needs the `fieldKinds` metadata below to tell
"declared late and unassigned" from "not a field at all".

`fieldKinds`/`allFieldKinds`, not `slotKinds`: ADR 0035 (`:230`) considered
and **explicitly rejected** "slot" as the reflection vocabulary — "Rejected in
favor of the more universally understood 'field,' though this was a close
call". The pair mirrors `fieldNames`/`allFieldNames` because both the
hot-reload keep set and §6's predicate need the *flattened* answer.

**This is the largest remaining cost in Part B.** `ClassInfo`
(`semantic_analysis/class_hierarchy/class_info.rs:158`) carries
`state_types` and `state_has_default` as separate maps, so slot kind needs a
third — plus a matching `__beamtalk_meta/0` schema entry (ADR 0050) for
cross-file ancestors. And `class_variables` (`:171`) is `Vec<EcoString>` —
**names only, no types, no defaults** — so `late classState:` on an inherited
or cross-file class has no metadata to hang the kind off, and that field must
grow a structure. Plus a `behaviour.bt` declaration and an Erlang intrinsic,
since `fieldNames`/`allFieldNames` are `@primitive "classFieldNames"`
(`behaviour.bt:225`, `:233`).

## Prior Art

| Language | Mechanism | Adopted / rejected |
|---|---|---|
| **Kotlin** | `lateinit var` — a non-null property assigned after construction, throwing `UninitializedPropertyAccessException` on early read; only `var`, no primitives. Separately `by lazy { }` with `LazyThreadSafetyMode` | **This is the closest prior art and the model for §1.** Adopted the shape wholesale: non-null declared type, assignment deferred, defined error on early read. Improved on it in two ways — Kotlin has no presence test (`::x.isInitialized` came later and is reflection-flavoured) so §3 gives `hasField:` first-class, and Kotlin cannot un-initialise, which Symphony's relaunch case requires, so §3 adds `clearField:`. Rejected `by lazy` for the reasons in §Alternatives |
| **Swift** | Two-phase definite initialisation proves every stored property is set before `self` escapes; `lazy var` for deferred computation, which **cannot be `let`** and needs a `mutating` getter on a struct | Swift's DI is the model for §6, though Swift can make it an *error* because it has no reflective writers and no hot reload; we cannot (ADR 0100). Its `lazy var`-on-a-struct wall is precisely why §5 rejects `late field:` on a Value — though `late`'s reason is simpler: a Value is never assigned at all |
| **C#** | `required` members (C# 11) — the compiler forces every constructor path or object initialiser to set them; nullable reference types with `!` to suppress | `required` is Part A's construction-site check by another name, and it validates reporting at the construction site rather than the declaration (§6). Rejected `!`-style per-read suppression as the escape hatch; `@expect definite_assignment` on the declaration is the Beamtalk-idiomatic form |
| **Newspeak** | Slots are declared between vertical bars and accessed **exclusively via messages**; `lazy s = expr.` computes the initialiser when the getter is first run and stores the result | The first draft adopted this spelling. **Rejected** once the survey showed no memoise-on-read demand (§Context (c)), and rejected its `nil`-based storage independently: a lazy slot computing `nil` recomputes forever. Newspeak has no `late` analogue — its slots always carry an initialiser |
| **Pharo** | `Slot` metaobjects — `LazySlot`, `InitializedSlot`, `ComputedSlot`, declared `#ivar => LazySlot default: 5` | **Adapted the vocabulary, not the mechanism.** `fieldKinds` answering `#eager`/`#late` is the reflectable-slot-kind idea; user-definable slot metaobjects are rejected below |
| **Squeak/Pharo `ClassBuilder`** | Match instance variables by name on recompile, default the rest | Already Beamtalk's hot-reload behaviour (ADR 0123); this ADR adds the two `late` rows to its reconcile table (§8) |
| **Scala** | `lazy val`, thread-safe via a double-checked-locking bitmap | Relevant only to the rejected memoising design. Worth recording that the BEAM would have made the bitmap unnecessary — a gen_server serialises by construction — because that was the first draft's headline advantage and it evaporated with the feature |
| **Erlang/Elixir** | No late or lazy fields. A record field is `undefined` until set; idioms are `maps:get/3` with a default, `Map.get_lazy/3`, or an `Agent` | Confirms there is nothing to be compatible *with*. Notably Erlang records default to `undefined` and callers guard — the same overloaded-sentinel problem §2 avoids by using key absence |
| **TypeScript** | `strictPropertyInitialization` plus definite-assignment assertions (`x!: T`) | The closest analogue to the whole ADR: a compile-time check for "assigned before use" with a per-declaration opt-out. `late` is `!:` with a defined runtime error instead of undefined behaviour |

**Why not Pharo-style first-class `Slot` metaobjects**, the strongest
rejected alternative: Beamtalk compiles ahead of time to Core Erlang. A
user-defined slot with a `read:`/`write:to:` hook would make every slot read
dispatch through a metaobject, costing every eager read and defeating the
static guard §3 depends on. Pharo can afford it because it interprets and
recompiles in-image. Beamtalk takes the vocabulary — `#eager`/`#late` as
reflectable kinds — without the protocol, and `fieldKinds` is the extension
point if user-defined kinds are ever wanted.

## User Impact

**Newcomer (from Python/JS/Ruby).** `late` is `lateinit` from Kotlin and
`!:` from TypeScript, in the position they expect, and the error they get
from reading too early names the slot, its type, and the class — instead of
today's `does_not_understand` for `writeLine:` on `nil`, several frames from
the real cause. The bigger win is Part A: the current failure mode is a
`spawn` that crashes with a runtime error, which for someone learning the
`initialize` chain is hard to connect to the line they forgot. The one thing
they will try and be refused is `late field:` on a Value — so that error
carries both working alternatives written out.

**Smalltalk developer.** Instance variables in Smalltalk are always `nil`
until assigned, and every accessor guards; `late` is the declaration that the
guard is temporary and the type is not really nilable. The departure from
Pharo is that slot kinds are syntax rather than metaobjects, which is a real
loss of extensibility and is argued above. The cultural shift is Part A:
"forgetting to assign in `initialize`" is a convention violation in
Smalltalk, and Beamtalk already removed the need to write `super initialize`
(ADR 0078); this makes the remaining half visible before the image runs.

**Erlang/Elixir developer.** The generated state map stays an ordinary map: a
`late` slot is a key that is not there yet, testable with `maps:is_key/2`, and
`sys:get_state` shows it honestly with no sentinel atom to learn. They will
recognise the shape from records defaulting to `undefined`, and should
appreciate that Beamtalk uses absence rather than a magic value so no
assigned value can be confused for "unset". Nothing about reads is
process-sensitive, because reads do not mutate.

**Production operator.** Two things matter and both land their way. Opening an
inspector, attaching `observer`, or calling `sys:get_state` **cannot execute
user code** — there is no initialiser to trigger, so observation is
side-effect-free by construction rather than by rule (§9). And a
read-before-assign fails loudly at the read, naming the slot, instead of
propagating a `nil` into unrelated code. The honest cost: an actor that
spawned cleanly can now raise on its fourth message because a lifecycle call
was never made, and `fieldKinds` is what makes it introspectable which slots
are in that category. A `late` read inside `terminate:` is the one shape that
fails quietly (the dispatch is `try`-wrapped), which is why §4d warns on it.

**Tooling developer.** `late` is one field on `StateDeclaration` — which
already carries `expect`, `doc_comment` and `declared_keyword`, and is shared
by `class_variables` (`ast/class.rs:174`) — so hover, completion and the
outline get it with no new AST shape. The definite-assignment analysis is a
dataflow pass over method bodies the LSP already parses, and the
construction-site form (§6) extends a literal-map inspection that already
exists. The one genuine plumbing cost is `fieldKinds`' metadata (§9). The
rule not to get wrong is short: reads raise, `hasField:` does not.

## Steelman Analysis

### `lazy` — memoise on first read (rejected; this was the first draft's decision)

- 🧑‍💻 **Newcomer**: "`lazy` is the word I know from Kotlin, Swift and Scala.
  And it's less to remember: I declare the value once, at the slot, instead of
  writing a `launch` method and remembering to call it before anything else."
- 🎩 **Smalltalk purist**: "This is *the* Smalltalk idiom —
  `^x ifNil: [x := self compute]` is in every Pharo image — and it is
  Newspeak's actual spelling, `lazy s = expr`. `late` is a Kotlin import;
  `lazy` has ancestry in this language family."
- ⚙️ **BEAM veteran**: "On the BEAM lazy is *free* where it is expensive
  everywhere else: the gen_server serialises by construction, so no
  double-checked locking, no `LazyThreadSafetyMode`, no bitmap. That is a
  genuine platform advantage and `late` leaves it on the table."
- 🏭 **Operator**: "Deferring acquisition off the spawn path means a slow or
  flaky dependency does not block startup, and a supervisor restart
  recomputes derived state automatically."
- 🎨 **Language designer**: "It is strictly more expressive. `late` can be
  *expressed* as lazy plus a manual assignment, but not the reverse:
  compute-on-first-read has no encoding in `late`."

The last point is true and is the real cost of this decision — `late` cannot
express deferred computation at all. It loses on evidence and on price.
Evidence: **zero** memoise-on-first-read sites across 148 `.bt` files in the
two applications (§Context (c)), while every lifecycle-assigned slot wants
exactly what `late` provides. Price: because a lazy read *writes*, it needs an
ADR 0118 prelude, which drags in the `ClassVars` mutation detector that does
not see reads (`analysis.rs:253`), ADR 0110's shadow write, the loop
optimiser's field hoisting, and a `terminate:` path that would acquire a
subprocess in order to close it. It also could not serve its own best
candidate: Symphony's `proc` needs to return to unassigned so `launch` can
re-run, and `launch` answers `Result(Nil, CodexError)` which a slot
initialiser has no way to hand back. `late` needs none of that machinery and
meets both requirements directly.

The Smalltalk-ancestry argument is the one I would most expect to be pressed
on. The reply is that the idiom being ancestral does not make it the *need*
here: the corpus writes `ifNil:` guards for **temporarily-unset resources**,
not for memoisation, and `late` is what that guard is trying to say.

`lazy` remains addable later, orthogonally, if a memoisation case appears —
`fieldKinds` already answers a symbol, so `#lazy` would join `#eager`/`#late`
without a schema change.

### A `computed` slot kind — recompute on every read (rejected)

- 🧑‍💻 **Newcomer**: "Derived values are the common case, and a declaration
  reads better than a method."
- ⚙️ **BEAM veteran**: "Exdura's `timer_manager` and `currentEngine` resolve
  by name on every read *deliberately*, so a `rest_for_one` restart is picked
  up. That is a real, repeated pattern — give it a declaration."
- 🎨 **Language designer**: "It completes the set: eager, late, computed."

This one has genuine corpus support (§Context (d)) — two files, one with a
four-line comment explaining the choice. It loses because in Beamtalk that
declaration already exists and is called **a method**. `currentEngine` and
`currentEventStore` are ordinary methods today, they compose (parameters,
overriding, protocol conformance), and they are what their authors reached
for unprompted. A `computed` kind would add a third reconcile row, a third
inspector kind and a third `fieldKinds` value for no capability gain.
Recorded because `fieldKinds` deliberately leaves room for it.

### Definite assignment as an Error rather than a Warning (rejected)

- 🧑‍💻 **Newcomer**: "If the compiler knows my spawn will crash, stop me."
- ⚙️ **BEAM veteran**: "'Let it crash' means crash *early*. A spawn that
  cannot succeed is a build that should not pass."
- 🏭 **Operator**: "An Error in CI is the cheapest place to catch it."
- 🎨 **Language designer**: "Swift and C# both make this an error, and it is
  among their most-praised guarantees."

The premise "the compiler knows it will crash" is false in Beamtalk:
`fieldAt:put:`, `perform:`, `ClassBuilder` (ADR 0038) and a hot-patched
`initialize` (ADR 0082/0084) all assign a slot invisibly, and Swift and C#
have none of those. ADR 0100 settled the general policy — certainty, not
desire, sets severity. The operator and CI case get what they need via
`[diagnostics]` escalation to Error per project (ADR 0100 Rule 3), opt-in for
a codebase that knows it uses no reflective writers.

### `lateState:` / `lateField:` as distinct keywords (rejected)

- 🎩 **Smalltalk purist**: "Smalltalk's declaration forms *are* keyword
  messages. A prefix modifier is not a message send; `lateState:` is — and
  Beamtalk already chose `classState:` over `class state:` for that reason."
- ⚙️ **BEAM veteran**: "The parser stays a flat keyword match
  (`is_state_or_field_keyword`, `declarations.rs:59`) instead of growing a
  two-token lookahead."
- 🎨 **Language designer**: "Illegal combinations become unrepresentable —
  there is no `lateField:` token to parse and then reject."

The `classState:` precedent is genuinely strong and this is the closest call
in the ADR. It loses on multiplication: `late` is orthogonal to *which* slot
keyword, so keywords grow as the product (`lateState:`, `lateClassState:`,
and a `lateField:` existing only to be rejected), and a second modifier would
double it again. Beamtalk's existing `sealed`/`abstract`/`typed` prefix
position, and the modifier loop already written at `declarations.rs:192-208`,
settle it.

### `nil` as the unassigned sentinel (rejected)

- 🧑‍💻 **Newcomer**: "`nil` means no value. Nothing new to learn."
- ⚙️ **BEAM veteran**: "Every key is always present, so the state map has one
  fixed shape. `maps:get/2` never fails, and two `sys:get_state` snapshots
  stay structurally comparable."
- 🎨 **Language designer**: "Zero new vocabulary — no `maps:find`, no third
  reconcile row, no `hasField:`, no `#lateSlot` inspector kind."

The fixed-shape argument is real and this ADR takes the cost. It loses on a
correctness point no ergonomics offsets: `nil` is *already* the compiler's
representation for "no value supplied" (`state.rs:36` emits `'nil'` for a
defaultless slot; ADR 0078's check reads `'nil'` as unassigned), so a third
meaning cannot be distinguished from either. That argument is
compiler-internal and needs no corpus support — an earlier revision claimed
Exdura's `supervised :: Boolean` flag as exactly that support, and
§Context (b) retracts it.

### Tension points

- **Expressiveness vs. evidence.** `lazy` is strictly more expressive;
  nothing in the corpus needs the extra expressiveness, and it costs the
  whole state-threading lowering. The decision follows the evidence, and is
  the one most worth revisiting if a memoisation case appears.
- **Smalltalk ancestry vs. the actual need.** The `ifNil:` memo idiom is
  ancestral; the guards in this corpus are not memos.
- **Fixed state-map shape vs. an unforgeable "unassigned".** Correctness
  wins; the cost is `maps:find`, two reconcile rows and one inspector kind.
- **Newcomer/operator want Part A as an Error**; ADR 0100 and the reflective
  writers say Warning. Resolved by per-project escalation.

## Alternatives Considered

### `lazy` — memoise on first read
The first draft's decision. Rejected on evidence (zero memoise-on-read sites
in 148 `.bt` files, §Context (c)), on price (a read that writes needs an ADR
0118 prelude, and so the `ClassVars` detector, ADR 0110's shadow write, and
the loop optimiser's hoisting), and on fitness — it could not serve its own
best candidate, because Symphony's `proc` must return to unassigned and its
`launch` answers a `Result` a slot initialiser cannot hand back. Steelmanned
in full above. Addable later and orthogonal: `fieldKinds` already answers a
symbol, so `#lazy` would join `#eager`/`#late` with no schema change.

### A `computed` slot kind — recompute on every read
Has real corpus support (§Context (d)) and still loses, because in Beamtalk
that declaration is spelled "a method", which is what those authors already
wrote. Steelmanned above.

### `lateState:` / `lateField:` as distinct declaration keywords
Rejected: keywords would grow as the product of modifier × slot keyword, and
a `lateField:` token would exist only to be rejected. Steelmanned above.

### `nil` as the unassigned sentinel
Rejected: `nil` already means "no value supplied" in two places
(`state.rs:36`, ADR 0078's check), so a third structural meaning is
indistinguishable from both. Steelmanned above.

### A reserved `'__unset__'` sentinel value instead of key absence
Keeps every key present, answering the fixed-state-map-shape objection, while
avoiding the `nil` collision. Rejected as strictly worse than absence: it
still needs a guard on every read, it leaks a Beamtalk-internal atom into
`sys:get_state` output and into any Erlang code reading the map, it must be
stripped at serialisation and node boundaries, and — decisively — it can be
*written* by `fieldAt:put:`, putting a slot into a state no value should be
able to express. Key absence cannot be forged.

### `self.slot := nil` as the un-assign form
Tempting: on a non-nilable `late` slot `nil` cannot be a legitimate value, so
there is no ambiguity, and Symphony's `stopProcess` would need no edit at all.
Rejected because this ADR's central argument is that `nil` should stop
carrying structural meaning; re-introducing it as a magic un-assign trades the
thesis for one saved line. `clearField:` is explicit, and assigning `nil` to a
non-nilable slot stays the type error it already is, with a message naming the
fix.

### Definite assignment as an Error rather than a Warning
Rejected for Actors (`spawnWith:`, `fieldAt:put:`, `perform:`,
`ClassBuilder`, hot-patched `initialize` all assign invisibly) and, more
narrowly, for Values (`ClassBuilder` and the deserialization boundary remain).
Available per-project via ADR 0100 Rule 3, which §6 notes is materially more
attractive for Values since they have no runtime backstop. Steelmanned above.

### Declaration-site definite-assignment dataflow
Report on the slot declaration after a branch-aware walk of the `initialize`
chain, suppressed when some visible `spawnWith:` supplies the key. Rejected as
the primary design in favour of the construction-site check (§6): a finding
whose presence depends on whether an unrelated call site happens to live in
the same file is non-local and brittle, and one `spawnWith:` anywhere would
silence every warning for that class. The corpus settles it — Exdura and
Symphony construct through class-method factories
(`ExduraClient connect:`, `CodexClient start:config:onEvent:`,
`WorkflowHandle for:client:`), each holding exactly one literal construction
map, so the construction-site check fires once per class at the canonical
site. Still worth adding later for non-literal construction, where the call
site proves nothing.

### Whole-program definite assignment across construction sites
Warn only when neither the `initialize` chain nor *any* construction site in
the project supplies the slot. Rejected as disproportionate: it needs a
whole-program pass gated on project-complete `KnowledgeScope` (ADR 0100 Rule
2), and the factory pattern above means the cheap version already covers the
common case.

### A library answer — a `Dictionary` cache slot
`state: cache :: Dictionary = #{}` plus an `at:ifAbsentPut:`-style memo gets
caching with zero language surface. Not an alternative to `late` (it does not
address a slot temporarily unset behind a non-nilable type), but it is the
honest no-new-syntax baseline any future `lazy` proposal must beat.

### Do nothing
The status quo works: widen the type to `| Nil`, guard each read, and accept
that `UninitializedStateError` catches the Actor case at spawn.

**Rejected for Part A.** The signal is strictly additive over machinery that
already exists — the ADR 0078 chain walk and the literal-map inspection at
`beamtalk-language-features.md:2426` — and for Values there is no runtime
check at all today (§6), so declining it leaves a non-nilable declared type
with nothing behind it.

**Rejected for Part B, but a closer contest than Part A.** In favour of doing
nothing: `state: x :: T = <expr>` already allows an eager initialiser, and
§Context (d) shows some nilable slots must never change. Against: the `| Nil`
widening is not a statement about the data, it is an escape from the
post-`initialize` check, and it permanently defeats ADR 0107 nil-narrowing for
every downstream reader of `self.proc`; Exdura additionally paid an extra slot
for the resulting ambiguity. The deciding factor is that `late` costs one
guarded read and two small reflective selectors, not a lowering.

## Consequences

### Positive
- A non-nilable declared type stops being a lie for lifecycle-assigned slots.
  `state: proc :: Subprocess | Nil = nil` becomes
  `late state: proc :: Subprocess`, so every `self.proc writeLine:` reads a
  `Subprocess` and ADR 0107 narrowing works for downstream readers.
- Reading too early raises `uninitialized_state_error` naming the slot, its
  type and the class, instead of answering `nil` and failing later as a
  `does_not_understand` on `nil`.
- **Reading never writes**, so `late` needs no ADR 0118 prelude, no
  `ThreadedIr` threading, no `ClassVars` detector change, no ADR 0110 shadow
  write, and no `terminate:` redesign. `generate_field_access` stays pure.
- **Observation cannot execute user code** — there is no initialiser to
  trigger, so the inspector, `sys:get_state`, `observer` and `recon` are
  side-effect-free by construction rather than by rule (§9).
- Three states become expressible on a `late … | Nil` slot (absent / `nil` /
  a value). Note this does **not** retire Exdura's `supervised` flag — that
  flag distinguishes two meanings *of absence*, which three states cannot
  express (§Context (b)).
- Part A gives Values a definite-assignment check where **none exists today**
  — `generate_post_initialize_check` is Actor-only (`callbacks.rs:857`), so a
  typed-no-default `field:` is currently unenforced behind a non-nilable type
  (§6).
- Part A reports once per class at the canonical factory construction site
  rather than at every caller, reusing the literal-map inspection that
  already exists (`beamtalk-language-features.md:2426`).
- The definite-assignment predicate moves from `beamtalk-codegen` down to
  `beamtalk-core`, where `just check-codegen-boundary` (`Justfile:666`)
  requires it to be for the LSP to surface the diagnostic at all (§7).
- ADR 0123's deferred "absent → declared initialiser policy" is closed with
  two reconcile rows (§8).
- The auto-generated Value keyword constructor always satisfies the check by
  construction, so no Value construction path changes
  (`value_accessors.rs:63`).

### Negative
- **`late` cannot express deferred computation at all.** Nothing in the
  surveyed corpus wants it, but this is a real expressiveness loss versus the
  rejected `lazy`, and it is the decision most worth revisiting.
- **A nilable slot is not automatically a `late` candidate.** Exdura's
  resolve-by-name slots must stay as they are or supervisor-restart recovery
  breaks (§Context (d)). Only documentation guards this.
- **An actor that spawned cleanly can now raise on a later message** because
  a lifecycle call was never made — the failure moves from a silent `nil` to
  a loud raise, which is better, but it moves.
- **A `late` read in `terminate:` fails quietly.** The dispatch is
  `try`-wrapped (`callbacks.rs:1560`), so shutdown completes but cleanup
  silently does not. §4d's warning is the only guard.
- **Two new reflective selectors and one required source change.**
  `hasField:` and `clearField:` do not exist, and every `self.slot isNil`
  guard on a converted slot must become `hasField:`.
- **`printString` omits an unassigned slot** and instance `fieldNames` does
  not list it (§9). The former is a REPL-visible output change, gated on
  explicit confirmation under `CLAUDE.md`.
- **The state map no longer has one fixed key set** over an actor's lifetime.
  ADR 0123's reconcile, `changed_state_keys/2`'s output, the printer and the
  inspector each need the `late` case.
- **`fieldKinds` needs a `ClassInfo` third map plus a `__beamtalk_meta`
  entry**, and `class_variables` (`class_info.rs:171`) must grow from
  `Vec<EcoString>` to a structure. This is Part B's largest single cost (§9).
- **Reads of a `late` slot cost `maps:find` rather than `maps:get`**, and the
  ~15 sibling state `maps:get` sites need auditing for badkey exposure (§4a).
  Eager slots are unaffected.
- **Values constructed by deserialization cannot be checked** (§6). Their
  declared field types stay unverified, and `late` is not an escape hatch
  because §5 forbids it on a Value.
- **Class kinds diverge**: `late` works on `state:` and `classState:` but not
  `field:`. Defensible, still a wart to explain.
- Four new diagnostics — three Errors (`late field:`, `late` without a type,
  `late` with a default) and one advisory (definite assignment), plus the §4d
  `terminate:` warning — all to keep at parity across CLI, REPL, LSP and MCP.

### Neutral
- `late` becomes a contextual keyword in declaration position only. It is
  unused as a word anywhere in the stdlib's `.bt` sources, so `late := 1` and
  a `late` selector keep working.
- `nil` semantics are unchanged, and `nil` is freed to mean "legitimately
  none" on a `late … | Nil` slot.
- No `.bt` source changes are required in the stdlib. Every existing
  declaration keeps its current meaning, and the definite-assignment finding
  is advisory.
- `beamtalk_actor:init/1` validates only internal keys
  (`beamtalk_actor.erl:1582`), so an absent `late` slot needs no change
  there. Its "Actor started" log line reports `state_keys`, which will omit
  unassigned `late` slots — accurate, and something operators should expect.

## Implementation

Part A is shippable independently of Part B; the only coupling is the
`not late` clause in the shared predicate (§7).

### Part A — definite assignment

| # | Work | Components | Size |
|---|---|---|---|
| A1 | Move the predicate (`is_nilable_type`, annotated ∧ no-default ∧ non-nilable) down into `beamtalk-core`; `inherited_typed_no_default_fields` calls it. Required for the LSP to surface anything — `just check-codegen-boundary` (`Justfile:666`) forbids `beamtalk-lsp` depending on `beamtalk-codegen` | `beamtalk-core` (`semantic_analysis`), `beamtalk-codegen` (`gen_server/callbacks.rs`) | **S** |
| A2 | Actor construction-site check: extend the existing literal `spawnWith:` key inspection (`beamtalk-language-features.md:2426`) with the unassigned-slot predicate; branch-aware walk of the ADR 0078 flattened `initialize` chain to decide "assigns it" | `beamtalk-core` (`semantic_analysis`) | **M** |
| A3 | Value construction-site check: `new` (bare), `new:` with a literal map, and the auto-generated keyword constructor as always-satisfying (§6). No `initialize` chain to walk, so this is the simpler half | `beamtalk-core` (`semantic_analysis`) | **S** |
| A4 | `DiagnosticCategory::DefiniteAssignment`; new `ExpectCategory` variant + `from_name` + unparse name; `[diagnostics]` escalation; parity across build/LSP/REPL/MCP (ADR 0100 Rule 3, `surface-parity.md`) | `beamtalk-core`, `beamtalk-language-service`, `beamtalk-cli` | **S** |
| A5 | Docs + tests: the Actor-vs-Value asymmetry (no runtime backstop on Values) and the deserialization limit; `uninitialized_state_actor.bt` / `non_typed_uninitialized_state_actor.bt` gain compile-time expectations; a Value fixture for the `new`-with-typed-no-default case | docs, `crates/**/tests`, `stdlib/test` | **S** |

A1 is unblocked and independent. A3 is worth doing early despite being
second in value, because Values have no runtime check to fall back on.

### Part B — `late` slots

| # | Work | Components | Size |
|---|---|---|---|
| B1 | `late` modifier: contextual keyword, two-token lookahead extending the single-token dispatch at `declarations.rs:553` and reusing the modifier-loop shape at `:192-208`; `SlotKind` on `StateDeclaration`; unparse round-trip. The Errors: `late field:`, `late` without a type annotation, `late` with a default, `late state:` on `native:`/`Object` | `beamtalk-core` (`source_analysis/parser/declarations.rs`, `ast/class.rs`, `unparse`) | **S** |
| B2 | Exclude `late` slots from `generate_post_initialize_check` (its 2-arity `maps:get` would badkey) and from `init/1`'s state literal | `beamtalk-codegen` (`gen_server/callbacks.rs`, `gen_server/state.rs`) | **S** |
| B3 | Guarded read: `maps:find` + the `uninitialized_state_error` arm in `generate_field_access` (`expressions.rs:519`), keeping it pure; audit the ~15 sibling state `maps:get` sites; disable DirectParams/Hybrid field hoisting for `late` slots (`loop_mode.rs:118`) | `beamtalk-codegen` (`expressions.rs`, `control_flow/`) | **M** |
| B4 | `hasField:` / `clearField:` on `Object` plus their runtime intrinsics; `read_field/2`'s declared-`late` branch, keyed on declared-late rather than key-absence | `beamtalk-stdlib`, `beamtalk_runtime` (`beamtalk_reflection.erl`, `beamtalk_object_ops.erl`) | **S** |
| B5 | `fieldKinds`/`allFieldKinds`: the `ClassInfo` third map, the `__beamtalk_meta` schema entry, `class_variables` growing from `Vec<EcoString>` to a structure, the `behaviour.bt` declaration and Erlang intrinsic; LSP hover | `beamtalk-core`, `beamtalk-codegen`, `beamtalk_runtime`, `beamtalk-stdlib`, `beamtalk-language-service` | **M–L** |
| B6 | The §4d `terminate:`/`handle_info` unguarded-read warning | `beamtalk-core` (`semantic_analysis`) | **S** |
| B7 | Inspector: `InspectorField` `#lateSlot` / `value: #notAssigned` / `drillable: false`, the cross-surface wire form (`inspector.bt:262`) and `beamtalk_inspector:fieldsOf/1` | `beamtalk-stdlib`, `beamtalk_runtime` | **S** |
| B8 | REPL-visible output: decide and confirm `printString` rendering for an unassigned slot — **gated on explicit user confirmation** per `CLAUDE.md` | `beamtalk_runtime` (`beamtalk_object_printer.erl`), `tests/repl-protocol` | **S**, gated |
| B9 | ADR 0123 reconcile `late` rows + the shared `(slot kind, present?, has default?) -> outcome` conformance fixture, and the `allFieldNames`-keep-set invariant test. Sequenced after ADR 0123's epic (§8) | `beamtalk_runtime`, `beamtalk-codegen` | **S** |
| B10 | Docs + tests: `beamtalk-language-features.md` leading with **"a nilable slot is not automatically a `late` candidate"** and the resolve-by-name counter-example (§Context (d)), then slot kinds, the read/raise rule, `hasField:`/`clearField:`, the `spawnWith:`-injection clause at `:2426`; `surface-parity.md`; BUnit tests; REPL-protocol e2e | docs, `stdlib/test`, `tests/repl-protocol` | **S** |

**Test placement** (per `CLAUDE.md`): `late` read/raise behaviour,
`hasField:`/`clearField:`, re-assignment after clearing, and `fieldKinds` go
in `stdlib/test/*.bt` as BUnit `TestCase`s — none is a bootstrap primitive.
Diagnostic text and severity are Rust unit tests plus LSP
diagnostic-provider tests. `late classState:` on a REPL-defined class, and
slot kinds surviving a class reload, go in
`tests/repl-protocol/cases/*.btscript`.

**Recommended start:** A1, then A3 (Values have no backstop), then A2. For
Part B, B1–B3 are the core and carry no architectural risk now that reading
is not a mutation.

## Migration Path

No migration is required. `late` is additive, every existing declaration
keeps its current meaning and representation, and the definite-assignment
finding is advisory (Warning/Hint, never Error by default), so no currently
compiling program stops compiling.

**The stdlib's nilable singletons must not be converted.**
`transcript_stream.bt`, `beamtalk_interface.bt` and
`workspace_interface.bt` have their singletons injected by
`beamtalk_workspace_bootstrap:bootstrap_singleton/3` (`:149-186`), which
monitors the process and rebootstraps on death. `late` would not help: the
slot is assigned from outside the class entirely, and `classState: current ::
X | Nil = nil` is the honest declaration of a slot that may legitimately be
`nil` between bootstrap attempts.

**In the applications, migration is narrow**, and the selection matters far
more than the mechanics — most nilable slots in this corpus should stay
exactly as they are:

| Class | Verdict |
|---|---|
| `symphony/codex_client.bt` `proc` | **Convert.** `late state: proc :: Subprocess`; `launch` unchanged; `stopProcess`'s `isNil` guard becomes `hasField:` and its `:= nil` becomes `clearField:` |
| `exdura/exdura_http_server.bt` `httpServer` | **Convert**, with `destroy` using `clearField:` |
| `exdura/exdura_client.bt` `supervisor` | **Do not convert.** `nil` means "standalone mode" and selects a resolution strategy in `currentEngine`/`currentEventStore` (`:144-155`). Convertible in principle — the discriminator would become `hasField:` — but the `\| Nil` is semantically honest here, so the change buys nothing |
| `exdura/exdura_client.bt` `httpServer` | **Do not convert.** Genuinely optional: "nil unless ExduraWorker was started with an `#http` config" |
| `exdura/workflow_engine.bt` `eventStore`, `activityPool` | **Do not convert**, and **the `supervised` flag stays.** `nil` means "resolve by name on every call" so a `rest_for_one` restart is picked up (`:976-987`); `activityPool` additionally needs two meanings of absence, which `late … \| Nil` cannot express (§Context (b)) |
| `exdura/timer_manager.bt` `engine`, `eventStore`; `exdura_client.bt` `currentEngine`/`currentEventStore` | **Do not convert.** Same resolve-by-name-per-read reason; they are methods and should stay methods |
| `symphony`/`exdura` error and DTO `field:`s (`workspace_error`, `linear_error`, `issue`, `activity_outcome`, `retry_snapshot_entry`, …) | **Not candidates.** Optional data where `nil` is meaningful, and §5 forbids `late field:` on a Value |
| `exdura/stored_snapshot.bt` `state :: ReplaySnapshot` | **Not a candidate** — a Value built by deserialization. Either default it or widen to `\| Nil` to make the unverified state honest (§6) |

The docs (B10) must lead with the do-not-convert row: a nilable slot is not
automatically a `late` candidate, and the distinguishing question is whether
a stale value would be wrong.

Converting a slot **between eager and `late`** changes its ADR 0123
reconcile row and so requires a `shapeVersion:` bump.

## References
- Related issues: [BT-3525](https://linear.app/beamtalk/issue/BT-3525)
  (this ADR); parent [BT-3523](https://linear.app/beamtalk/issue/BT-3523);
  related [BT-3524](https://linear.app/beamtalk/issue/BT-3524) (versioned
  state migration — this ADR closes its deferred "absent → declared
  initialiser policy"), [BT-3527](https://linear.app/beamtalk/issue/BT-3527)
  (distribution — `late` slots crossing a node boundary); historical
  [BT-1947](https://linear.app/beamtalk/issue/BT-1947) /
  [BT-1949](https://linear.app/beamtalk/issue/BT-1949) /
  [BT-1951](https://linear.app/beamtalk/issue/BT-1951) (typed-no-default
  slots and the runtime check),
  [BT-2881](https://linear.app/beamtalk/issue/BT-2881) (the check is not
  gated on `typed`)
- Related ADRs:
  [ADR 0025](0025-gradual-typing-and-protocols.md) (gradual typing; `typed`
  classes require annotations),
  [ADR 0035](0035-field-based-reflection-api.md) (`fieldNames` / `fieldAt:`;
  `classState:`),
  [ADR 0036](0036-full-metaclass-tower.md) (the class-side gen_server that
  memoises `lazy classState:`),
  [ADR 0038](0038-subclass-classbuilder-protocol.md) (`ClassBuilder` — a
  reflective slot writer),
  [ADR 0042](0042-immutable-value-objects-actor-mutable-state.md) (why a
  Value has no process to memoise into),
  [ADR 0043](0043-sync-by-default-actor-messaging.md) (why an initialiser
  must not synchronously message its own actor),
  [ADR 0056](0056-native-erlang-backed-actors.md) (`native:` actors — an
  opaque chain link, hence Hint),
  [ADR 0067](0067-separate-state-field-keywords-by-class-kind.md)
  (`state:`/`field:`/`classState:`; the rebinding footgun memo-on-copy would
  reintroduce),
  [ADR 0078](0078-actor-initialize-inheritance.md) (the auto-chained
  `initialize` sequence and the runtime `UninitializedStateError` this ADR
  makes static),
  [ADR 0082](0082-method-level-edit-save-and-changelog.md) /
  [ADR 0084](0084-class-side-runtime-method-fun-dispatch.md) (hot-patched
  `initialize` — why definite assignment cannot be an Error),
  [ADR 0095](0095-rich-navigable-inspector.md) (`InspectorField`; the
  no-force rule),
  [ADR 0100](0100-open-world-diagnostic-policy.md) (severity from certainty;
  Rule 3 `[diagnostics]` escalation),
  [ADR 0103](0103-sendability-typing-from-class-kinds.md) (`handleScope:` for
  a `late` slot holding a node-local handle — deferred, see §8),
  [ADR 0107](0107-nil-and-type-patterns-in-match.md) (nil narrowing — what a
  non-nilable declared type buys a reader),
  [ADR 0110](0110-class-var-shadow-write-through-for-nlr-relay.md) (the `ClassVars` shadow-write
  rule; not triggered by `late`, since its reads do not write),
  [ADR 0111](0111-lowered-ir-verifier-for-state-threading.md)
  (`ThreadedIr::verify()`),
  [ADR 0117](0117-beamtalk-core-crate-split.md) (why the predicate's home is
  `beamtalk-core`),
  [ADR 0118](0118-expression-level-state-threading-preludes.md)
  (`ThreadedValue` — the prelude mechanism the rejected `lazy` design
  needed and `late` does not),
  [ADR 0120](0120-value-type-self-threading-scope.md) /
  [ADR 0122](0122-threaded-ir-storage-family-generalization-scope.md)
  (storage families; why no new family is needed),
  [ADR 0123](0123-versioned-state-migration.md) (`shapeVersion:`,
  `migrateFromVN:`, reconcile step 3)
- Documentation: `docs/beamtalk-language-features.md` (slot declarations,
  initialisation chaining, `classState:` singletons),
  `docs/development/architecture-principles.md` §6 (shared-leaf-module
  pattern) and §7 (consistency-test disposition),
  `docs/development/surface-parity.md`
- Application corpora surveyed for evidence (§Context), both cited as
  real-world Beamtalk projects by ADR 0067:
  [jamesc/beamtalk-exdura](https://github.com/jamesc/beamtalk-exdura)
  (workflow engine — `event_store.bt`, `exdura_client.bt`,
  `workflow_engine.bt`, `timer_manager.bt`, `exdura_http_server.bt`) and
  [jamesc/beamtalk-symphony](https://github.com/jamesc/beamtalk-symphony)
  (agent orchestrator — `codex/codex_client.bt`, `orchestrator.bt`,
  `workflow/workflow_watcher.bt`). Surveyed at `exdura@d6b350e` /
  `symphony@d95ce02`.
- Prior art: [Newspeak lazy slots](https://groups.google.com/g/newspeaklanguage/c/IQsw31ze-IU)
  and the [Newspeak specification](https://newspeaklanguage.org/spec/newspeak-spec.pdf);
  [Pharo ComputedSlots](https://astares.blogspot.com/2019/03/computedslots-in-pharo.html);
  [Swift `lazy var` on structs](https://www.avanderlee.com/swift/lazy-var-property/)
  and [the Swift Forums thread on lazy vars in immutable structs](https://forums.swift.org/t/allow-lazy-vars-on-immutable-structs/16417)
