# ADR 0124: Slots — Lazy Initialisation and Definite Assignment

## Status
Proposed (2026-09-18)

**Recommendation, split by risk.** This ADR decides two things that the
driving issue bundles. They are not equally ready:

- **Part A — definite assignment (§6, §7).** Small, low-risk, entirely in
  `beamtalk-core`, and it addresses the evidenced pain. **Recommend
  accepting and implementing now.**
- **Part B — lazy slots (§1–§5, §8, §9).** The design below is complete and
  the representation decision (§2) is sound, but review found no call site in
  the current corpus that needs it (§Context), and the lowering's blast radius
  is much larger than first estimated (§4). **Recommend Deferred** until the
  four open items in §4 are closed, or splitting Part B into its own ADR.
  Note that ADR 0123's epic completing does **not** lift this: it unblocks
  only the migration phase (B6, §8). None of the four open items —
  re-entrancy, the lowering choice, `terminate:`/`handle_info` discarding
  state, or the watcher notification on a read — is affected by it.

Part A does not depend on Part B. The only coupling is one clause in a shared
predicate (§7), statable as a forward-compatibility note.

## Context

### Problem statement

A Beamtalk slot declaration (ADR 0067: `state:` on an Actor, `field:` on a
Value, `classState:` on any kind) says what data a class holds and,
optionally, what it starts as. What it does *not* say is **how the slot
becomes valid**. Three gaps follow from that omission:

1. **No lazy initialisation.** A slot whose value is expensive, or whose
   construction cannot run at `init/1` time, has to be declared nilable and
   nil-checked at every read.

   **The honest state of the evidence: no call site in the current corpus
   needs this.** Three candidates were examined and all three are
   unsuitable, which matters more than the pattern count:

   | Candidate | Why it cannot take the fix |
   |---|---|
   | `stdlib/src/transcript_stream.bt:17` | `typed Actor subclass: TranscriptStream native: beamtalk_transcript_stream` — a `native:` Actor (ADR 0056), and its singleton is a *registered process*, not a constructed value |
   | `stdlib/src/beamtalk_interface.bt:27` | `classState: current :: BeamtalkInterface \| Nil = nil`, but the singleton is injected externally by `beamtalk_workspace_bootstrap:bootstrap_singleton/3` (`:149-186`), which also `erlang:monitor`s it and rebootstraps on death. A lazy initialiser cannot reproduce that |
   | `stdlib/src/workspace_interface.bt:26` | Same bootstrap-injected shape |
   | `stdlib/src/retry_policy.bt:107` | `typed Value subclass:` — §5 of this ADR *forbids* `lazy` on a Value `field:`. And `maximumInterval` is a genuinely optional user-supplied field, so its `nil` is meaningful, not a workaround |

   All three nilable singletons widen a declared type to `| Nil` — which does
   defeat ADR 0107's nil narrowing for every downstream reader — but they do
   it to describe *external injection*, not deferred construction. `lazy`
   would not fix them.

   Note also that `state: x :: T = <expr>` **already** permits an arbitrary
   eager initialiser (`gen_server/state.rs:36`, `:98`). So `lazy` buys only
   (a) deferring work off the spawn path and (b) construction that *cannot*
   run during `init/1` — and this ADR cannot produce a current (b) case. Part
   B is therefore a forward-looking feature for resource-holding actors the
   codebase does not yet have, and should be judged as one.

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
   been computed yet" — a distinction that only exists once lazy slots do.

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

Give every slot a **declared initialisation kind**, and check the
consequences at compile time where they are provable.

Two additions to the language and one analysis:

1. `lazy` as a declaration-level modifier on `state:` and `classState:`.
2. **Key absence**, not a sentinel value, as the representation of a
   not-yet-computed lazy slot.
3. A **definite-assignment analysis** in `beamtalk-core` that reports
   statically what ADR 0078's check reports at spawn time.

**Why these are one ADR and not two.** They are coupled in both directions.
Semantically, `lazy` is the answer the definite-assignment diagnostic
*recommends* — without it the only fixes are "give it a default" or "widen the
type to `| Nil`", and the second is the boilerplate this ADR exists to
delete. Mechanically, lazy slots must be **excluded** from ADR 0078's
post-`initialize` check or its `maps:get/2` crashes on an absent key, and that
exclusion is a change to the very predicate the static analysis shares. Split
across two ADRs, each would have to specify the other's behaviour anyway.

### 1. `lazy` — a declaration-level modifier

```beamtalk
Actor subclass: ReportBuilder
  state: rows :: List = #()
  lazy state: index :: Dictionary = self buildIndex

  internal buildIndex -> Dictionary =>
    self.rows inject: #{} into: [:acc :row | acc at: (row at: #id) put: row]

  lookup: id :: Symbol -> Object | Nil =>
    self.index at: id ifAbsent: [nil]
```

`rows` is supplied at `spawnWith:` and not written afterwards, which is what
makes `index` safe to memoise — see §4.

```beamtalk
sealed Object subclass: Registry
  lazy classState: table :: Ets = Ets newOrExisting: #registry type: #set

  class at: key :: Symbol -> Object | Nil => self.table at: key
  class at: key :: Symbol put: value :: Object -> Nil =>
    self.table at: key put: value
```

The modifier precedes the declaration keyword, mirroring Newspeak's own
`lazy s = expr` and Beamtalk's existing class-level modifier position
(`sealed typed Collection subclass: Array`). It composes with a type
annotation and with `@expect`:

```beamtalk
  @expect unresolved_ffi, type "handle type is erased at the FFI boundary"
  lazy state: conn :: Object = (Erlang my_driver) connect: self.url
```

**Where `lazy` is permitted:**

| Declaration | `lazy` | Rationale |
|---|---|---|
| `lazy state:` (Actor) | **Yes** | The instance gen_server memoises the value |
| `lazy classState:` (any class kind) | **Yes** | The class gen_server memoises it (ADR 0036 metaclass tower) |
| `lazy field:` (Value) | **Error** | No owning process; see §5 |
| `lazy state:`/`field:` on `Object` | **Error** | Already an error — Object holds no instance data (ADR 0067) |
| `lazy state:` on a `native:` Actor | **Error** | Already an error — ADR 0056 prohibits `state:` there outright, so no new rule is needed |
| `lazy classState:` on a `native:` Actor | **Yes** | ADR 0056 permits `classState:`; the class-side gen_server is compiler-generated even for native actors |

**An initialiser is required.** `lazy state: x :: Integer` with no `= expr`
is an Error: there is nothing to compute on first read, so the declaration
has no meaning. This is Newspeak's rule too — lazy slots always carry an
initialisation expression.

**A type annotation is *not* required.** The initialiser feeds inference
exactly as an eager default does, so `lazy state: index = self buildIndex`
infers from `buildIndex`'s return type. A `typed` class requires the
annotation for the same reason it already requires one on `state: x = 0`
(ADR 0025) — this ADR adds no new rule there. Requiring `::` only on lazy
slots would be an inconsistency with no payoff.

**Error forms:**

```beamtalk
Value subclass: Point
  field: x :: Integer = 0
  lazy field: magnitude :: Float = (self.x * self.x) sqrt
// error: 'lazy' is not allowed on a Value 'field:' — a Value has no process
//        to memoise into. Write a method instead:
//            magnitude -> Float => (self.x * self.x) sqrt

Actor subclass: Cache
  lazy state: store :: Dictionary
// error: a 'lazy' slot requires an initialiser — write
//            lazy state: store :: Dictionary = Dictionary new
```

### 2. Representation: the key is absent until first read

A lazy slot's key is **not present** in the state map until its initialiser
has run to completion. It is not `nil`, and not a reserved sentinel value.

Consequences, all of which fall out rather than needing new machinery:

- The presence test is `maps:is_key/2`. No computed value — including `nil`,
  `false`, or `'__absent__'` itself — can be mistaken for "not computed".
  This is the wart Newspeak's `nil`-based lazy slots have and we do not.
- `beamtalk_actor:changed_state_keys/2` already reads a missing key as
  `'__absent__'`, so the watch and telemetry paths need no change.
- `sys:get_state`, `observer`, and `recon` show a map with the key missing.
  That is an honest rendering of "not computed yet".
- **Lazy slots are excluded from ADR 0078's post-`initialize` check.** This
  is a required change, not a nicety: that check emits
  `maps:get(Slot, InitNewState)` with the 2-arity form, which would
  `badkey`-crash on an absent key. A lazy slot is definitely assigned *on
  demand*, so it has nothing to prove at spawn time.

`nil` remains what it is today — the value of an unannotated, undefaulted
slot. `lazy` and `nil` are now orthogonal: a lazy slot whose initialiser
answers `nil` memoises `nil` and never recomputes.

### 3. Lazy read semantics on the BEAM

A read of a lazy slot lowers to an **ADR 0118 prelude** on the slot's
storage family — `VersionPrefix::State` for `lazy state:`,
`VersionPrefix::ClassVars` for `lazy classState:`. The prelude is:

```text
if maps:is_key(Slot, State) -> {maps:get(Slot, State), State}
else                        -> V = <initialiser>,
                               {V, maps:put(Slot, V, State)}
```

expressed as `ThreadedStmt`s so that `ThreadedIr::verify()` covers it like
any other threaded read. **No new storage family is introduced** — but that
is a much weaker claim than "this is free", and an earlier draft of this ADR
overstated it. §4 is the correction.

Four semantic commitments:

**a. The gen_server is the lock.** The initialiser runs inside the owning
process, and a process handles one message at a time, so two concurrent
readers cannot both compute. Beamtalk needs no `LazyThreadSafetyMode`
(Kotlin), no double-checked-locking bitmap (Scala `lazy val`), and no
documented thread-unsafety (Swift `lazy var`). This is the single biggest
advantage of doing lazy slots on the BEAM, and it is free.

**b. "Once per successful computation", not "at most once".** The
`maps:put` happens only after the initialiser returns a value. If the
initialiser raises, the failure propagates like any other method-body
failure, the slot stays absent, and the next read retries. The alternative —
a third "poisoned" state that re-raises the stored error — was rejected: it
needs a sentinel (undoing §2), and for the motivating cases (open a port,
create an ETS table, connect a socket) retrying is what an author wants.
This is stated explicitly because it is observable: an initialiser with side
effects can run more than once.

**c. A method that reads a lazy slot is a state-mutating method.** A getter
that was previously a pure read now threads state. Inside the actor this is
mostly invisible (`handle_call` already returns `{reply, V, State}`).

`verify()` is **not** a safety net for the detector gap in §4, and an
earlier draft claimed it was. ADR 0122's sixth-gap repro shows why: its first
case (`class selfSendEnsure`) *"compiles; returns 0; class var stays 0"* — a
silent miscompilation. Only the variant with an extra local mutation tripped
`verify()`. A *detector* miss is precisely what `verify()` cannot catch,
because nothing produces the version it would check.

**d. Initialiser cycles are a compile-time Error.**

```beamtalk
Actor subclass: Tangled
  lazy state: a :: Integer = self.b + 1
  lazy state: b :: Integer = self.a + 1
// error: cyclic lazy slot initialisers: 'a' -> 'b' -> 'a'
```

The check is an SCC over the lazy-slot dependency graph, built from
`self.<slot>` reads in initialiser expressions. An Error is justified here
under ADR 0100 because *those* expressions are all in the class body and its
visible ancestors, so that graph is closed-world.

**Its limit must be stated, because the ADR's own flagship example sits
outside it.** `lazy state: index = self buildIndex` contains **zero
`self.<slot>` reads**, so a cycle mediated by a method —

```beamtalk
  lazy state: index :: Dictionary = self buildIndex
  internal buildIndex -> Dictionary => self.index at: #k ifAbsent: [#{}]
```

— is invisible to the direct graph. And ADR 0043 (`:104`) is explicit that
self-sends inside actor methods bypass the gen_server and call
`Module:dispatch` directly, so the one-message-at-a-time property does not
serialise this: it is unbounded recursion inside a single `handle_call`,
ending in a stack overflow rather than a deadlock.

Catching it needs transitive closure through self-sends, which stops being
closed-world the moment a subclass overrides the helper. The decision is
therefore: **direct cycles are an Error; method-mediated re-entrant forcing
is a documented, uncaught hazard.** Re-entrancy is listed as an open item in
§4 because a runtime in-progress guard (the classic answer, and what Scala's
`lazy val` and Kotlin's `by lazy` both implement) would require a third slot
state and so undo §2.

### REPL session

```beamtalk
b := ReportBuilder spawnWith: #{#rows => #(#{#id => #a, #n => 1})}
b lookup: #a                          // => #{#id => #a, #n => 1}
//                                       `index` computed on that read, then memoised
b lookup: #a                          // => #{#id => #a, #n => 1} — no recompute

Registry at: #k put: 42               // => nil — `table` computed on this read
Registry at: #k                       // => 42 — memoised; no second Ets table
```

```beamtalk
ReportBuilder fieldKinds               // => #{#rows => #eager, #index => #lazy}
b fieldAt: #index                     // => #{#a => #{#id => #a, #n => 1}}
b fieldNames                          // => #(#rows, #index) — only after forcing;
//                                       an unforced `index` is absent (§9)
```

### 4. What the lowering actually costs — four open items

An earlier draft of this ADR claimed the existing detectors and emitters
would carry lazy reads "without per-construct work". Review disproved that on
four counts. These are the reasons Part B is **Deferred**, not obstacles that
are merely noted.

**a. `generate_field_access` is pure, and there are ~15 other `maps:get`
sites.** `generate_field_access`
(`crates/beamtalk-codegen/src/core_erlang/expressions.rs:519`) returns
`Result<Document<'static>>` — no prelude channel — and emits 2-arity
`maps:get` for the Actor, class-method (`:526`) and Repl contexts alike. An
absent key there is a **badkey crash**, not a first read. Every caller assumes
purity. Further state `maps:get` sites needing audit: `blocks.rs:494,504,613,745,769,788`;
`control_flow/body.rs:1389,1424`; `control_flow/util.rs:53`;
`control_flow/conditionals.rs:2172`; `control_flow/while_loops.rs:896`;
`dispatch_codegen.rs:3309`; `threaded_expr.rs:533`;
`threaded_ir/emit.rs:730`.

**b. Loop optimisations hoist field reads out of the construct.**
`control_flow/loop_mode.rs:110` (`hybrid_readonly_field_params`) substitutes
the variable directly *before* the letrec instead of emitting a `maps:get`,
and `control_flow/plan.rs:1044` unpacks per iteration. A hoisted lazy read
either badkey-crashes or forces **before the loop**, changing when the
initialiser's side effects happen — including running them when the body
executes zero times. DirectParams/Hybrid hoisting must be disabled for lazy
slots.

**c. The `ClassVars` detector does not see reads, so `lazy classState:` is
not free.** `is_family_mutation`
(`control_flow/analysis.rs:253`) is:

```rust
VersionPrefix::State => true,
VersionPrefix::ClassVars => {
    (Self::is_field_assignment(expr) && self.is_class_var_assignment(expr))
        || self.is_class_method_self_send(expr)
}
```

A lazy `classState:` *read* is neither an assignment nor a self-send, so
`body_threaded_families` reports "this body does not thread `ClassVars`" for
every loop, conditional, `match:` arm, `on:do:`/`ensure:` block and Foldl
body containing one. **`State` threads unconditionally (`=> true`), so the
free-lowering claim holds for `lazy state:` and fails for `lazy
classState:`.** The ADR should have said so precisely instead of claiming
both.

**d. A `ClassVars` lazy read must emit ADR 0110's shadow write.** The §3
prelude ends in `maps:put`, i.e. a `BindOp::Put`. `threaded_ir/ir.rs:194`
(`requires_shadow_write`, `ClassVars` only) and `verify.rs:372` make a
`ClassVars` `Put` at a shadow-write-eligible frame with an NLR present and
`shadow_write: false` a `VerifyError::ShadowWriteMissing`. So every lazy
`classState:` read carries a process-dictionary side channel. The §3 prelude
as written does not respect it.

**Consequence for sizing:** phase 3 is **L–XL**, not M, and it is the only
phase with genuine architectural risk.

**The lower-risk lowering, and why it is probably the right one.** Confine
the force to **one generated forcing reader per lazy slot** rather than
making every `self.slot` read a prelude. `lazy state: index = self
buildIndex` generates an `index` reader on the class module; it is read as
`self index`, and a direct `self.index` field access inside the class is an
Error with a fix-it naming the reader. Blast radius: one generated function
and one dispatch arm, instead of the ~15 emission sites in (a), the hoisting
in (b), the detector in (c) — and `fieldAt:` forcing (§9) becomes a call to
the same function.

The cost is that `lazy` stops being invisible at the read site. That is a
real ergonomic loss, but it is **more** faithful to the prior art, not less:
Newspeak slots are "accessed exclusively via messages", and its lazy slot is
defined as an initialiser that "gets computed when the slot's getter is first
run". The transparent-`self.slot` design is the one that departs from
Newspeak. Actors also generate no auto-accessors today
(`value_accessors.rs:63` returns `None` for non-Value kinds), so this adds a
generated reader where there is currently none rather than changing one.

**Open items gating Part B:**

1. Re-entrant forcing through a method-mediated cycle (§3d) has no answer
   that preserves §2.
2. Whether to adopt the generated-reader lowering above, which changes the
   user-facing read syntax.
3. `terminate:`/`handle_info` discard the returned state
   (`gen_server/callbacks.rs:1560` wraps `dispatch('terminate:', …)` in
   `try … of _TermOk -> 'ok'`), so a lazy force there runs the initialiser,
   keeps its side effects, and loses the memo. Worse, the documented cleanup
   idiom reads the slot it closes:

   ```beamtalk
   Actor subclass: ResourceActor
     lazy state: conn :: Object = (Erlang my_driver) connect: self.url
     terminate: reason :: Symbol -> Nil => self.conn close
   ```

   An unforced `conn` is **opened during shutdown in order to be closed**.
   ADR 0111 forbids an ad-hoc `debug_assert!` at a new state-threading site,
   so this needs either a `VerifyError` or an explicit rule that lazy reads
   are forbidden in these callbacks.
4. A lazy read **publishes a state-change notification.**
   `changed_state_keys/2` unions old and new keys
   (`beamtalk_actor.erl:1966`), so a newly-computed lazy key is a changed
   key and `notify_state_change` fires (`callbacks.rs:1229`, `:1375`). The
   watch and telemetry *code* needs no change; the *behaviour* does, and it
   directly undercuts §9's rule that observation must not perturb a running
   system.

**Staleness and idempotence remain author obligations, not diagnostics.** An
earlier draft proposed a `StaleLazySlot` warning — "collect the `self.<slot>`
reads in the initialiser; warn if any method assigns one". It is dropped: as
specified it is a **no-op on its own worked example** (`self buildIndex`
reads no slot directly), and the transitive version is the false-positive
factory it was meant to avoid — any helper reading any slot any method
writes. Assignment to a lazy slot stays permitted and marks it computed;
there is no invalidation primitive, and a derived value over mutable inputs
should be a method. The docs must instead state plainly that **an initialiser
must be idempotent**, because §3b's retry-on-failure is only safe if it is —
and this ADR's own `Registry` example violated that: `Ets new:type:` raises
`already_exists` (`stdlib/src/ets.bt:56`), so a retry after a partial failure
fails permanently. `Ets newOrExisting: name type:` (`ets.bt:106`) exists for
exactly this, and the example now uses it.

**Supervisor restarts recompute**, since a restarted actor gets a fresh state
map with lazy slots absent. Stated as a positive in §User Impact, it has an
inverse: with eager init a failing resource acquisition crashes at `spawn`,
trips the supervisor's restart intensity, and escalates. With lazy
retry-per-read the actor never crashes, the supervisor sees nothing, and the
system degrades silently while re-running a side-effecting initialiser on
every message. That is the opposite of "let it crash" and is listed under
Consequences/Negative.

### 5. Value classes: `lazy field:` is rejected

A Value instance is an immutable map with no owning process (ADR 0042), so
there is nowhere to memoise. The two ways to force it through were both
rejected:

- **Memoise on a copy.** The auto-generated reader would have to answer both
  the value and an updated instance, so `p magnitude` becomes
  `(value, p')` and every call site has to rebind — reintroducing, as the
  *default* read path, precisely the silent-update-loss footgun ADR 0067
  removed from `Object subclass:` with `state:`. Swift reached the same wall
  from the other side: `lazy var` on a struct needs a `mutating` getter, so
  it cannot be read through a `let` at all.
- **Recompute on every read** (Pharo's `ComputedSlot`). In Beamtalk this is
  already spelled **a method**. It needs no syntax, it is `Sendable`
  (ADR 0103), and it stays pure:

  ```beamtalk
  Value subclass: Point
    field: x :: Integer = 0
    field: y :: Integer = 0

    magnitude -> Float => ((self.x * self.x) + (self.y * self.y)) sqrt
  ```

So the diagnostic for `lazy field:` names the method form as the fix. An
author who genuinely needs memoisation on value-shaped data wants an Actor,
and the error's hint says so.

### 6. Definite-assignment analysis

A new semantic-analysis pass in `beamtalk-core` reports at compile time what
ADR 0078's check reports at spawn time.

**Which slots it covers** — the identical predicate the runtime uses,
unchanged and now shared (§7): annotated, no default, type does not admit
`Nil`, **and not `lazy`**. So:

| Declaration | Requires assignment? |
|---|---|
| `state: count :: Integer` | Yes |
| `state: label :: String \| Nil` | No — `nil` is a valid value |
| `state: count :: Integer = 0` | No — has a default |
| `state: count` | No — untyped, defaults to `nil` |
| `lazy state: index :: Dictionary = …` | No — assigned on demand |

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
//        it 'lazy'.
```

**This needs almost no new machinery.** The literal `spawnWith:` map is
*already* inspected — `docs/beamtalk-language-features.md:2426` validates its
keys against declared `state:` slots and warns on an unknown key with a typo
suggestion naming the nearest slot. Definite assignment is one more predicate
over that same literal map: *declared, no default, non-nilable, not lazy, not
assigned in the visible `initialize` chain, and not a key of this map.* No
branch-aware dataflow pass, no whole-program scope, and near-zero false
positives, because a literal init map is complete evidence about that one
construction.

`Counter spawn` (no map) is the same check with an empty key set, which is
exactly the `uninitialized_state_actor.bt` fixture case.

The declaration-site dataflow pass remains a possible later addition for
non-literal construction (`Counter spawnWith: someMap`), where the call site
proves nothing. It is recorded under Alternatives, not adopted here.

**Severity** (`DiagnosticCategory::DefiniteAssignment`, a new category so
suppression can target it):

| Situation at a construction site | Severity |
|---|---|
| Literal init map (or bare `spawn`), chain fully visible, slot unassigned and not supplied | **Warning** |
| Literal map supplies the slot | **nothing** — that is an assignment |
| Chain incomplete — cross-package parent, `native:` ancestor (ADR 0056), or a `fieldAt:put:`/`perform:` writer in the class | **Hint** |
| Non-literal init map (`spawnWith: someMap`) | **nothing** — no evidence |
| Slot is `lazy`, defaulted, or nilable | **nothing** |

Warning, never Error by default, because the open world is real: the slot may
be written by `spawnWith:` from another package, by `ClassBuilder`, or by a
hot-patched `initialize`. This matches the precedent already set for a
provably-failing construction — an unknown `spawnWith:` key is a Warning, not
an Error. Escalation to Error is available per-project through ADR 0100
Rule 3's `[diagnostics]` table, and suppression is `@expect
definite_assignment` on the declaration. `StateDeclaration.expect` already
exists (`ast/class.rs:520`) and `class_variables` shares the same type
(`:174`), so the *carrier* is free — but an unknown `@expect` category is a
parse error, so this does need a new `ExpectCategory` variant plus its
`from_name` entry (`ast/expression.rs`) and unparse name (`unparse/mod.rs`),
alongside the new `DiagnosticCategory`. An earlier draft claimed "no parser
work"; that was wrong.

**Supplying a lazy slot at `spawnWith:` counts as computed.** `init/1`
merges with the caller winning — `maps:merge(DefaultState, InitArgs)`
(`gen_server/callbacks.rs:73`, `:90`, `:221`, `:277`) — so
`ReportBuilder spawnWith: #{#index => X}` puts the key in the map and, under
§2, the slot is already computed and the initialiser never runs. This is
**legitimate dependency injection** and is decided as such rather than
warned about: it is the one construct that makes a lazy slot testable by
substitution. It does need a clause in the
`docs/beamtalk-language-features.md:2426` key-checking rule so the behaviour
is documented rather than incidental.

**`lazy` is the principled cure**, and the diagnostic says so. A slot that
cannot be assigned in `initialize` — because construction is expensive, or
needs a resource that is not ready at `init/1` time — should be `lazy`, not
nilable. That is what makes these two features one ADR rather than two:
**every slot gets a declared story for how it becomes valid**, and the
analysis is just the compiler checking the story.

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

ADR 0123's reconcile step 3 gains the lazy case it deferred to this ADR:

| Declared slot | In migrated dictionary | Outcome |
|---|---|---|
| eager, has default | present | kept |
| eager, has default | absent | declared default |
| eager, no default | absent | `nil` (untyped) / failure (`typed`) — unchanged |
| **lazy** | **present** | **kept** — the memoised value survives the reload |
| **lazy** | **absent** | **stays absent** — recomputed on next read |

**Premise: ADR 0123 is implemented before Part B starts.** Its
implementation is in flight — BT-3531, BT-3534 and BT-3535 have landed as of
`main@80b8db4`, and `beamtalk_shape_migration` (BT-3536) is next — and this
ADR assumes the epic completes first. So Phase B6 is **sequenced after ADR
0123, not blocked by it**, and the rows below are a change to real code
rather than a contract against code that does not exist.

Two consequences of taking that premise rather than today's tree:

- **The reconcile row is an edit to `beamtalk_shape_migration`'s step 3**, in
  whatever form BT-3536 lands it, not a request for a hook that has yet to be
  designed. B6 should read that module as built and add the lazy case to it.
- **One finding below needs re-verifying against the finished epic, not
  assumed.** Measured against today's tree,
  `beamtalk_hot_reload:migrate_fields/2` (`:192-250`) derives its keep set
  from `beamtalk_behaviour_intrinsics:classAllFieldNamesByName/1`, **not**
  from the defaults map — so "lazy present → kept / lazy absent → stays
  absent" falls out with *no change to `beamtalk_hot_reload`*. That is a
  measurement of pre-0123 code. If ADR 0123's later phases route reconcile
  through `beamtalk_shape_migration` instead, the no-change finding may not
  survive, so **B6 must re-measure it rather than inherit it from this ADR**.

Either way the underlying invariant is what matters and is silently
load-bearing: declared-but-absent lazy slots must stay in `allFieldNames`. If
any implementation derives the keep set from the `init/1` defaults (where
lazy slots are absent by design), every memoised lazy value is dropped on
reload with a spurious "Hot reload dropped fields" warning. That needs an
explicit invariant and a test wherever reconcile ends up living.

- `migrateFromVN:` hooks see a `Dictionary` in which a not-yet-computed lazy
  slot is simply **not a key**. ADR 0123 already instructs hooks to read
  possibly-absent keys with `at:ifAbsent:` / `includesKey:`, so this needs no
  new hook contract.
- **Changing a slot between eager and lazy is a shape change** and bumps
  `shapeVersion:` like any other, because the reconcile row it takes changes.
- **Cross-node (BT-3527): sending state does not force lazy slots.** The
  receiving node recomputes on first read. This is the correct default and
  an argument *for* lazy slots under distribution: an initialiser that opens
  a port, an ETS table, or a file handle produces a node-local value, and
  recomputing it on arrival is what you want. A lazy slot is strictly safer
  across a node boundary than the eager slot it replaces.
- **Persistence keeps memoised values.** They are ordinary values; dropping
  them would silently discard work. A lazy slot holding a node-local handle
  must not be persisted — and an earlier draft delegated that to ADR 0103's
  `handleScope:`, which **does not hold today**: `grep handleScope stdlib/src/*.bt`
  returns nothing, ADR 0103 (`:138`) deliberately deferred the declarations,
  and `Ets` — this ADR's own example class — declares none, so it sits at
  tier `Unknown` and is silent. The hardest safety question in this section
  therefore has **no mechanism behind it yet**; it is an open item, not a
  solved one.

### 9. Reflection, inspector, LSP — the force / no-force split

The operator-facing rule that constrains the design: **observation must not
run user code.**

| Surface | Forces the initialiser? |
|---|---|
| `anActor fieldAt: #slot` | **Yes** — a deliberate program-level read |
| `anActor fieldAt: #slot put: v` | n/a — writes, marking the slot computed |
| `Cls fieldNames`, `Cls allFieldNames` | No — the *declared* schema, so lazy slots are listed (ADR 0035 `:73`) |
| `anActor fieldNames` | No — the *actual* keys, so an unforced lazy slot is **not listed** (`beamtalk_reflection.erl:39` → `beamtalk_tagged_map.erl:158`) |
| `anObject printString` / `displayString` | No — `beamtalk_object_printer:structural_from_state/1` (`:113`) renders present keys only, so an unforced lazy slot is **silently omitted** |
| `Cls fieldKinds` (**new**) | No — answers `Dictionary(Symbol, Symbol)`, `#eager \| #lazy` |
| Inspector (ADR 0095), `sys:get_state`, `observer`, `recon` | **No** — renders "not computed" |
| LSP hover | No — shows the declaration as written |

Opening an inspector on an actor must not execute an initialiser that opens
a socket. So `InspectorField` gains `#lazySlot` to its `kind` vocabulary
(alongside `#slot #element #association #processInfo`) and renders a
not-yet-computed slot as such, with `drillable: false` until it holds a
value, and `value: #notComputed` — matching the house convention for an
absent reading, `InspectorField name: #status value: #unavailable`
(`stdlib/src/inspector.bt:96`). `#lazySlot` must also be added to the
documented cross-surface wire form (`inspector.bt:262`) and to
`beamtalk_inspector:fieldsOf/1`, which reads only the state map and so cannot
know a slot is lazy-but-absent without the `fieldKinds` metadata above.
`fieldAt:` is the opposite case — the caller asked for the value, so
it computes and memoises, which is why the two are split rather than given
one policy.

**Making `fieldAt:` force is not free, and this is what it costs.** Today
`fieldAt:` is a generic runtime map read —
`beamtalk_reflection:read_field/2` is `maps:get(Name, State, nil)`
(`beamtalk_reflection.erl:47`) — dispatched by
`beamtalk_object_ops:dispatch('fieldAt:', …)`, which returns `State`
*unchanged* (`beamtalk_object_ops.erl:108`). So without new machinery an
absent lazy slot would answer `nil` through reflection: indistinguishable
from a computed `nil`, never memoised. Three small changes fix it, each on an
existing precedent:

1. **Codegen emits `force_field/2`** on any class with lazy slots:
   `force_field(Name, State) -> {Value, State1}`. It is the §3 lowering
   addressed by name at runtime rather than statically, so it shares the
   lowering rather than duplicating it.
2. **`read_field/2` gains a lazy branch**, resolving the owning module from
   the `'__class_mod__'` key already in the state map. The branch must be
   keyed on *"declared lazy"*, not merely *"key absent"* — `read_field/2`
   defaults a missing key to `nil`, so a typo'd field name is also absent,
   and forcing on absence alone would try to run an initialiser for a slot
   that does not exist.
3. **The `'fieldAt:'` dispatch arm threads state**, returning
   `{reply, Value, State1}`. The `'fieldAt:put:'` arm immediately below it
   already has exactly this shape, so this is a one-line change to match a
   sibling.

The alternative — `fieldAt:` **raises** on a not-yet-computed lazy slot
instead of forcing — costs nothing to build and is honest, but it breaks the
generic `fieldNames`-then-`fieldAt:` iteration that tooling relies on, and it
makes `object fieldAt: #x` behave differently from the getter that reads
`self.x`. Reflection equivalence is worth the three changes above.

The instance-vs-class `fieldNames` split is ADR 0035's deliberate design
(`:73`: class-side answers the declared schema, instance-side the actual
keys), so lazy slots make an existing distinction *visible* rather than
creating a new inconsistency — but it must be documented, because
`b fieldNames` omitting `index` while `b fieldAt: #index` returns a value
will otherwise read as a bug.

`printString` omitting an unforced slot is a **REPL-visible output change**,
which under `CLAUDE.md` needs explicit confirmation before implementation.
It is called out here so that gate is not discovered late.

`fieldKinds`/`allFieldKinds`, not `slotKinds`: ADR 0035 (`:230`) considered
and **explicitly rejected** "slot" as the reflection vocabulary — "Rejected in
favor of the more universally understood 'field,' though this was a close
call" — so `slotKinds` would have reopened a settled decision. The pair
mirrors `fieldNames`/`allFieldNames` because both the hot-reload keep set and
the §6 predicate need the *flattened* answer, which a single selector has no
story for.

**This is not a one-line addition.** `ClassInfo`
(`semantic_analysis/class_hierarchy/class_info.rs:158`) carries
`state_types` and `state_has_default` as separate maps, so instance-slot
laziness needs a third — and a matching `__beamtalk_meta/0` schema entry
(ADR 0050) for cross-file ancestors. Worse, `class_variables` (`:171`) is
`Vec<EcoString>` — **names only, no types, no defaults** — so
`lazy classState:` on an inherited or cross-file class has no metadata to
hang laziness off and that field must grow a structure. Plus a `behaviour.bt`
declaration and an Erlang intrinsic, since `fieldNames`/`allFieldNames` are
`@primitive "classFieldNames"` (`behaviour.bt:225`, `:233`). "One boolean on
`StateDeclaration`" covers phase 1 only.

LSP hover already renders a declaration's RHS as written
(`state_declaration_hover_info`), so `lazy state: index :: Dictionary =
self buildIndex` displays the `lazy` modifier for free, plus a line naming
when the initialiser runs. Per ADR 0100 Rule 3 and
`docs/development/surface-parity.md`, the definite-assignment diagnostic must
appear identically on `beamtalk build`, the LSP, the REPL, and MCP — all four
surfaces, not just the compiler.

## Prior Art

| Language | Mechanism | Adopted / rejected |
|---|---|---|
| **Newspeak** | `lazy s = expr.` — the initialiser runs when the getter is first run, then the result is stored in the slot and read normally | **Adopted, including the spelling.** Rejected its `nil`-based storage: a lazy slot computing `nil` recomputes forever |
| **Pharo** | `Slot` metaobjects — `LazySlot`, `InitializedSlot`, `ComputedSlot`, declared as `#ivar => LazySlot default: 5` | **Adapted.** Took the eager/lazy/computed *vocabulary* into `fieldKinds` and the inspector; rejected user-definable slot metaobjects (below) |
| **Squeak/Pharo `ClassBuilder`** | Match instance variables by name on recompile, default the rest | Already Beamtalk's hot-reload behaviour (ADR 0123); this ADR adds the lazy row to its table |
| **Kotlin** | `by lazy { }` with `LazyThreadSafetyMode`; `lateinit var` throwing `UninitializedPropertyAccessException` | **Rejected the thread-safety knob** — the gen_server makes it meaningless. `lateinit` is close to our typed-no-default slot, and its "throws on early read" is ADR 0078's `UninitializedStateError`; we add the static check Kotlin deliberately does not have |
| **Scala** | `lazy val`, thread-safe via a double-checked-locking bitmap | Confirms the BEAM advantage: no bitmap, no `@volatile`, no initialisation-order hazard |
| **Swift** | `lazy var` on a stored property; **cannot be `let`**; on a struct the getter is `mutating`, so it is unusable through a `let`; separately, two-phase definite initialisation proves every stored property is set before `self` escapes | **Decisive for §5** — this is precisely `lazy field:` on a Value. Swift's DI is the model for §6, but Swift can make it an *error* because it has no reflective writers and no hot reload; we cannot (ADR 0100) |
| **C#** | `Lazy<T>` as a library type | Rejected: a wrapper type leaks into the slot's declared type, so every reader writes `.Value` |
| **Erlang/Elixir** | No lazy fields. Idioms: `maps:get/3` with a default, `Map.get_lazy/3`, or an `Agent` | Confirms there is nothing to be compatible *with*, and that the generated state map stays an ordinary map an Erlang caller can read |

**Why not Pharo-style first-class `Slot` metaobjects**, the strongest
rejected alternative: Beamtalk compiles ahead of time to Core Erlang. A
user-defined slot with a `read:`/`write:to:` hook would require every slot
read to dispatch through a metaobject, which cannot be lowered through
`ThreadedIr` into a static prelude and would cost every eager read. Pharo
can afford it because it interprets and recompiles in-image. What Beamtalk
takes instead is the *vocabulary* — `#eager`/`#lazy` as reflectable slot
kinds — which gives the inspector and LSP what they need without a
metaobject protocol. If user-defined slot kinds are ever wanted, the
`fieldKinds` symbol is the extension point.

## User Impact

**Newcomer (from Python/JS/Ruby).** `lazy` is the word they already know
from Kotlin, Swift, and Scala, in the position they expect. The deeper win is
the definite-assignment warning: the current failure mode is a `spawn` that
crashes with a runtime error naming a class, which for someone learning the
`initialize` chain is genuinely hard to connect to the line they forgot. The
warning names the slot, the type, and three fixes. The one thing they will
try and be refused is `lazy field:` on a Value — so that error carries the
method form, written out, rather than just a prohibition.

**Smalltalk developer.** `lazy state:` would delete the `ifNil:` accessor
idiom they write by hand in Pharo, and it is *Newspeak's* spelling, not an
invention — a Newspeak reader will recognise the declaration form and its
semantics exactly. (Note the corpus finding: the Beamtalk stdlib's
`ifNil:`-shaped declarations are not this idiom, so the saving is
prospective.) The departure from Pharo is that slot kinds are syntax
rather than metaobjects, which is a real loss of extensibility and is argued
above. The definite-assignment check is the bigger cultural shift:
"forgetting `super initialize`" is a *convention* violation in Smalltalk, and
Beamtalk already removed the need to write it (ADR 0078); this makes the
remaining half — forgetting the assignment itself — visible before the image
runs.

**Erlang/Elixir developer.** The generated state map stays an ordinary map:
a lazy slot is a key that is not there yet, readable with `maps:is_key/2`,
and `sys:get_state` shows it honestly. The property they will appreciate
most is that the gen_server serialises initialisation, so `lazy` needs none
of the machinery it needs on the JVM — an observation worth putting in the
docs, because it is a case where the BEAM makes a feature *simpler* rather
than harder. An Erlang caller reading the map directly sees no sentinel to
know about.

**Production operator.** Two things matter and both are decided their way.
Opening an inspector or attaching `observer` **never** runs an initialiser,
so observation cannot perturb a running system. And a lazy slot's
initialiser failure is a retry rather than a permanent poisoning, so a
transient dependency failure at first read does not require restarting the
actor. The honest cost: an initialiser with side effects can run more than
once, and `lazy` moves work from spawn time to an arbitrary later message,
which shifts *where* a latency spike or a crash appears. Both are stated in
the docs, and `fieldKinds` makes it introspectable which slots defer work.

**Tooling developer.** `lazy` is one boolean on `StateDeclaration`, so
hover, completion, and the outline get it with no new AST shape. The
definite-assignment analysis is a plain dataflow pass over method bodies the
LSP already has parsed. `fieldKinds` gives a system browser a column. The
force/no-force table is the rule a tooling author must not get wrong, so it
is a table in this ADR rather than prose.

## Steelman Analysis

### `lazyState:` / `lazyField:` as distinct keywords (rejected)

- 🧑‍💻 **Newcomer**: "One keyword, one concept. I look up `lazyState:` and
  find exactly one thing, instead of having to learn that modifiers stack in
  a particular order."
- 🎩 **Smalltalk purist**: "Smalltalk's declaration forms *are* keyword
  messages — `instanceVariableNames:`, `classVariableNames:`. A prefix
  modifier is not a message send; `lazyState:` is. And Beamtalk already
  chose `classState:` over `class state:` for exactly this reason."
- ⚙️ **BEAM veteran**: "The parser stays a flat keyword match, which is
  easier to reason about than a modifier prefix with lookahead."
- 🏭 **Operator**: neutral.
- 🎨 **Language designer**: "It makes the illegal combinations
  unrepresentable — there is simply no `lazyField:` token to parse and then
  reject."

The `classState:` precedent is the genuinely strong point here, and it is
close. It loses on multiplication: `lazy` is orthogonal to *which* slot
keyword, so keywords would grow as their product (`lazyState:`,
`lazyClassState:`, and a `lazyField:` that exists only to be rejected), and
a future second modifier would double it again. Newspeak's own spelling and
Beamtalk's existing `sealed`/`abstract`/`typed` prefix position settle it.

### `nil` as the not-yet-computed sentinel (rejected)

- 🧑‍💻 **Newcomer**: "`nil` means 'no value'. I already know that. A slot
  with no value hasn't been computed. Nothing new to learn."
- 🎩 **Smalltalk purist**: "This is *the* Smalltalk lazy idiom, and Newspeak
  itself does it. `^slot ifNil: [slot := self compute]` is in every Pharo
  image. Matching it means no surprises."
- ⚙️ **BEAM veteran**: "Every key is always present, so the state map has one
  fixed shape. `maps:get/2` never fails, hot-reload's field matching needs no
  new case, and `record`-like access patterns stay uniform."
- 🏭 **Operator**: "`sys:get_state` output has a stable set of keys across an
  actor's whole life. Absent keys make two snapshots structurally different
  for a reason that has nothing to do with the code version."
- 🎨 **Language designer**: "It needs zero new vocabulary — no `is_key`
  check, no third reconcile row, no `#lazySlot` inspector kind."

The BEAM veteran's fixed-shape argument is real and I take the cost. It
loses on a correctness point that no amount of ergonomics offsets: `nil` is
*already* the compiler's representation for "no value was supplied"
(`state.rs:36` emits `'nil'` for a defaultless slot, and ADR 0078's check
reads `'nil'` as unassigned), so overloading it a third time makes a lazy
slot that legitimately computes `nil` recompute on every read — forever,
silently, with side effects. Newspeak has this exact bug. Key absence costs
one `maps:is_key`. (The earlier draft also leaned on `state_diff/2`
"already tolerating missing keys"; that function is named
`changed_state_keys/2` and its `'__absent__'` is a local comparison default,
never a stored value — see Current state.)

### Definite assignment as an Error (rejected)

- 🧑‍💻 **Newcomer**: "If the compiler knows my actor will crash on `spawn`,
  it should stop me, not warn me. A warning I can ignore is a crash I get
  later."
- 🎩 **Smalltalk purist**: would not want either — but if forced, prefers the
  Error for being honest about what it claims.
- ⚙️ **BEAM veteran**: "'Let it crash' means crash *early*. A spawn that
  cannot succeed is a build that should not pass."
- 🏭 **Operator**: "I would rather this never reach production. An Error in CI
  is the cheapest place to catch it."
- 🎨 **Language designer**: "Swift makes definite initialisation an error and
  it is one of Swift's most-praised guarantees. A soundness check that only
  warns is not a guarantee."

This is the sharpest tension in the ADR, and the newcomer and operator are
both right about the *cost* of a warning. It loses on a factual point: the
premise "the compiler knows it will crash" is false in Beamtalk. `spawnWith:
#{#socket => s}` satisfies the slot with no `initialize` involvement, and so
do `fieldAt:put:`, `perform:`, `ClassBuilder` (ADR 0038), and a hot-patched
`initialize` (ADR 0082/0084). Swift can be sound here because it has none of
those. Making this an Error would break working programs, and ADR 0100
settled the general policy: certainty, not desire, sets severity. The
compromise gives the operator and the CI case what they need —
`[diagnostics]` escalation to Error per project (ADR 0100 Rule 3), which is
opt-in for a codebase that knows it uses no reflective writers.

### Memoise-on-copy for Value classes (rejected)

- 🧑‍💻 **Newcomer**: "I don't see why a Value is different. I wrote `lazy`,
  it should be lazy."
- 🎩 **Smalltalk purist**: "Pharo's `LazySlot` works on any class. Splitting
  the feature by class kind is the kind of special case Smalltalk avoids."
- ⚙️ **BEAM veteran**: "Returning `{Value, NewMap}` is just how you write
  stateful code in a functional language. Beamtalk already threads state
  through `SelfVt` for value types — ADR 0120 exists — so the machinery is
  *there*."
- 🏭 **Operator**: neutral.
- 🎨 **Language designer**: "Uniformity across class kinds is worth real
  cost. A feature that works on two of three kinds is a wart."

The BEAM veteran's point is the one that made me check: `VersionPrefix::SelfVt`
does exist, so value-type self-threading is not the obstacle. The obstacle is
the *reader's protocol*. An auto-generated Value reader answers the value;
memo-on-copy makes it answer the value **and a new instance**, so `p
magnitude` must be rebound by every caller or the memoisation is lost — which
is exactly the silent-update-loss footgun ADR 0067 removed from `Object
subclass:` with `state:`, reintroduced as the default read path for a whole
class kind. Swift hit the identical wall and forbids it on a `let`. And the
uniformity argument is weaker than it looks: compute-on-every-read is what a
Value actually wants, and Beamtalk already spells that "a method" — so the
feature is not missing, it is spelled differently.

### Tension points

- **Newcomer and operator both want the Error**; ADR 0100 and the reflective
  writers say Warning. Resolved by per-project escalation rather than by
  picking one.
- **Smalltalk purists want Pharo slot metaobjects**; the ahead-of-time
  compiler cannot lower them. Resolved by taking the vocabulary
  (`fieldKinds`) without the protocol.
- **BEAM veterans want a fixed state-map shape**; correctness wants key
  absence. Correctness wins; the cost is one extra reconcile row and one
  inspector kind.
- **Uniformity across class kinds vs. the reader protocol.** Values lose
  `lazy` and keep a sound reader. This is the decision most likely to be
  revisited if `Value` ever grows a memoising read path.

## Alternatives Considered

### `lazyState:` / `lazyField:` as distinct declaration keywords
`lazy classState: table = …` becomes `lazyClassState: table = …`. Rejected:
keywords multiply as the product of modifier × slot kind, a `lazyField:`
token would exist only to be rejected, and Newspeak's own spelling is the
prefix modifier. Steelmanned above.

### `nil` as the not-yet-computed sentinel
Rejected: `nil` already means "no value supplied" in two places
(`state.rs:36`, ADR 0078's check), so a lazy slot computing `nil` would
recompute forever. Steelmanned above.

### A reserved `'__unset__'` sentinel value instead of key absence
Keeps every key present (answering the fixed-shape objection) while avoiding
the `nil` collision. Rejected as strictly worse than absence: it still needs
a `case` on a magic atom at every read, it leaks a Beamtalk-internal atom
into `sys:get_state` output and into any Erlang code reading the map, it must
be stripped on serialisation and at the node boundary, and it can be
*written* by `fieldAt:put:` — putting the slot into a state no value should be
able to express. Key absence cannot be forged.

### `lazy field:` on Value via memoise-on-copy
Rejected: changes the auto-generated reader's return shape and reintroduces
ADR 0067's rebinding footgun. Steelmanned above.

### A `computed` slot kind (recompute on every read, Pharo `ComputedSlot`)
```beamtalk
Value subclass: Person
  field: firstName :: String = ""
  field: lastName :: String = ""
  computed fullName :: String = self.firstName ++ " " ++ self.lastName
```
Rejected as pure scope creep: this is a method with a different syntax. It
composes worse than a method (no parameters, no override, no protocol
conformance) and adds a third slot kind to reflection, the inspector, the
LSP, and the reconcile table for no capability gain. Noted here because
`fieldKinds` deliberately leaves room for it should a use case appear.

### Definite assignment as an Error
Rejected: `spawnWith:`, `fieldAt:put:`, `perform:`, `ClassBuilder`, and
hot-patched `initialize` all satisfy a slot invisibly, so the Error would
reject working programs. Available per-project via ADR 0100 Rule 3.
Steelmanned above.

### Whole-program definite assignment across `spawnWith:` call sites
Warn only when neither the `initialize` chain nor *any* `spawnWith:`/`new:`
call site in the project supplies the slot. Rejected for this ADR as
disproportionate: it requires a whole-program pass gated on project-complete
`KnowledgeScope` (ADR 0100 Rule 2), and the cheap version — suppress when a
literal-keyed construction site in the same compilation unit supplies the
slot — captures the common case. The full version is the natural follow-up if
false positives show up in practice.

### Split into two ADRs
Part A (definite assignment) and Part B (lazy slots) are coupled by exactly
one clause — the shared predicate must include `&& !lazy` — which is an
ordering dependency, not a design one. Splitting would let Part A ship on its
own instead of waiting on the §4 open items. **Not rejected**: the driving
issue (BT-3525) asks for one ADR covering both, so both are decided here, and
the Status block carries the recommendation to implement and defer
separately. If the author prefers two documents, Part B lifts out cleanly —
it shares no decision with Part A beyond that clause.

### Declaration-site definite-assignment dataflow
Report the finding on the slot declaration after a branch-aware walk of the
`initialize` chain, suppressing it when some visible `spawnWith:` supplies the
key. Rejected as the *primary* design in favour of the construction-site
check (§6): a declaration-site finding whose presence depends on whether an
unrelated call site lives in the same file is non-local and brittle, and
adding one `spawnWith:` anywhere silences the warning for every spawn of that
class. Still worth adding later for non-literal construction
(`spawnWith: someMap`), where the call site proves nothing.

### A library answer — `Dictionary at:ifAbsentPut:` over one cache slot
`state: cache :: Dictionary = #{}` plus an `at:ifAbsentPut:`-style memo gets
most caching use cases with **zero language surface**, and it is what the
dropped staleness diagnostic (§4) was trying to steer authors toward anyway.
Rejected as a replacement for `lazy` because it does not address case (b) in
Context — construction that cannot run during `init/1` — and it gives
reflection and the inspector nothing to report. It is the honest no-new-syntax
baseline that Part B must beat.

### Do nothing — keep the `ifNil:` idiom and the runtime check
The status quo works: `classState: current :: X | Nil = nil` plus a nil-check
is five extra characters in the declared type and one `ifNil:` per read, and
`UninitializedStateError` does catch the unassigned-slot bug — at spawn time,
which for an actor started under a supervisor is usually immediate and loud.

**For Part B this alternative is stronger than the first draft admitted, and
review moved it from "rejected" to "not yet beaten".** Context shows no
current call site needs lazy init; `state: x :: T = <expr>` already allows an
arbitrary eager initialiser; and §4 prices the lowering at L–XL. The three
nilable singletons do widen a type to `| Nil` and so do permanently defeat
ADR 0107's narrowing for downstream readers — but they do it to describe
*external injection*, which `lazy` cannot replace. Doing nothing about lazy
slots until a real call site appears is a defensible position, and it is why
Part B is Deferred rather than Accepted.

**For Part A it is rejected.** The definite-assignment signal is a strictly
additive diagnostic over machinery that already exists — the ADR 0078 chain
walk and the literal `spawnWith:`-map inspection — so declining it is
declining something the compiler can nearly compute already, at Small cost,
for a failure mode that today first appears as a crashed `spawn`.

### Deleting the runtime `UninitializedStateError` check
Rejected outright. Static analysis is advisory in an open world; the runtime
check is the only thing that catches a reflective or hot-patched write path.
The static check is additive.

## Consequences

### Positive
- A class of spawn-time `UninitializedStateError` crashes becomes a
  compile-time warning **at the construction site**, reusing the literal
  `spawnWith:`-map inspection that already exists (§6). This is the cheapest
  part of the ADR and the one that addresses the evidenced pain.
- The definite-assignment predicate moves from `beamtalk-codegen` down to
  `beamtalk-core`, where it belongs — and where `just check-codegen-boundary`
  requires it to be for the LSP to surface the diagnostic at all (§7).
- Thread-safe lazy initialisation costs nothing on the BEAM *across*
  processes: no double-checked locking, no thread-safety mode, no bitmap.
  (Intra-process re-entrancy is a separate, unsolved problem — §3d.)
- No new storage family. For `lazy state:` that genuinely means existing
  `State` threading carries it (`analysis.rs:259` threads `State`
  unconditionally); for `lazy classState:` it does not (§4c).
- ADR 0123's deferred "absent → declared initialiser policy" is closed with
  one new reconcile row (§8). On today's pre-0123 tree that needs no change to
  `beamtalk_hot_reload` at all; once ADR 0123's epic lands, B6 re-measures
  rather than assumes that.
- The auto-generated Value keyword constructor and `new:` key validation are
  untouched: `compute_auto_slot_methods` returns `None` for non-Value kinds
  (`value_accessors.rs:63`) and §5 rejects `lazy field:`.
- Lazy slots are *safer* across a node boundary than the eager slots they
  replace: a node-local resource is recomputed on arrival rather than shipped.
- The inspector and `sys:get_state` stay side-effect-free by rule (§9).

### Negative
- **An initialiser can run more than once, and must be idempotent — nothing
  enforces that.** Failure leaves the slot absent and the next read retries,
  so a non-idempotent initialiser fails *permanently* after a partial
  failure. This ADR's own first `Registry` example was such a case
  (`Ets new:type:` raises `already_exists`, `ets.bt:56`).
- **Re-entrant forcing through a method-mediated cycle is uncaught** and
  ends in a stack overflow: self-sends bypass the gen_server (ADR 0043
  `:104`), so the one-message-at-a-time property does not serialise it
  (§3d).
- **A memoised lazy slot goes stale** when the slots its initialiser reads
  are written afterwards, with **no diagnostic and no invalidation
  primitive** — the proposed warning was dropped as unimplementable (§4).
- **A lazy read publishes a watcher state-change notification**, because
  `changed_state_keys/2` unions old and new keys
  (`beamtalk_actor.erl:1966`). Reading changes what watchers observe (§4,
  item 4).
- **`terminate:` and `handle_info` discard returned state**, so a lazy force
  in either keeps its side effects and loses the memo — including opening a
  resource during shutdown purely to close it (§4, item 3).
- **Lazy init disables OTP's restart-intensity circuit breaker** for
  resource-acquisition failures: the actor retries per read instead of
  crashing at `spawn`, so the supervisor never escalates and the system
  degrades silently. That is the inverse of "let it crash".
- **`printString` silently omits an unforced lazy slot** and instance
  `fieldNames` does not list it (§9). The former is a REPL-visible output
  change, gated on explicit confirmation under `CLAUDE.md`.
- **`lazy classState:` is not carried free**: the `ClassVars` detector does
  not see reads (§4c) and every such read must emit ADR 0110's
  process-dictionary shadow write (§4d).
- **Phase 3 is L–XL**, touching a pure `generate_field_access`, ~15 other
  state `maps:get` sites, and the loop-hoisting optimisations (§4a, §4b).
  This is the ADR's largest cost and the main reason Part B is Deferred.
- **`lazy` moves work from spawn to an arbitrary later message**, shifting
  where latency spikes and crashes appear.
- **Every read of a lazy slot pays a `maps:is_key/2`, not just the first**,
  and the read threads state, so in some contexts it costs a tuple
  allocation it would not have cost eagerly. Eager reads are unaffected.
- **The state map no longer has one fixed key set** over an actor's lifetime.
  ADR 0123's reconcile, `changed_state_keys/2`'s output, the printer, and the
  inspector each need the lazy case.
- **Class kinds diverge**: `lazy` works on `state:` and `classState:` but not
  `field:`. Defensible, and still a wart to explain.
- **`fieldKinds` needs a `ClassInfo` + `__beamtalk_meta` schema change**, and
  `class_variables` must grow from a name list to a structure (§9).
- Four new diagnostics — three Errors (`lazy field:`,
  lazy-without-initialiser, direct initialiser cycle) and one advisory
  (definite assignment) — plus two reflective selectors, all to keep at
  parity across CLI, REPL, LSP, and MCP.

### Neutral
- `lazy` becomes a contextual keyword in declaration position. It is not
  reserved elsewhere, so `lazy := 1` and a `lazy` method selector keep
  working.
- `nil` semantics are unchanged; `lazy` and `nil` are now orthogonal.
- No `.bt` source changes are required anywhere. Every existing declaration
  keeps its current meaning, and the definite-assignment finding is advisory.
- `fieldKinds` is one new class-side selector; `fieldNames` / `allFieldNames`
  are untouched.
- `beamtalk_actor:init/1` validates only internal keys (`'$beamtalk_class'`,
  `'__methods__'`), so an absent lazy slot needs no change there. Its
  "Actor started" log line reports `state_keys`, which will simply omit
  not-yet-computed lazy slots — an accurate rendering, and one operators
  should be told to expect.

## Implementation

Split by the Status recommendation. **Part A is shippable today; Part B is
gated on the §4 open items.**

### Part A — definite assignment (recommended now)

| # | Work | Components | Size |
|---|---|---|---|
| A1 | Move the definite-assignment predicate (`is_nilable_type`, annotated ∧ no-default ∧ non-nilable) down into `beamtalk-core`; `inherited_typed_no_default_fields` calls it. Required for the LSP to surface anything — `just check-codegen-boundary` (`Justfile:666`) forbids `beamtalk-lsp` depending on `beamtalk-codegen` | `beamtalk-core` (`semantic_analysis`), `beamtalk-codegen` (`gen_server/callbacks.rs`) | **S** |
| A2 | Construction-site check (§6): extend the existing literal `spawnWith:`/`new:` key inspection (`docs/beamtalk-language-features.md:2426`) with the unassigned-slot predicate; branch-aware walk of the ADR 0078 flattened `initialize` chain to decide "assigns it" | `beamtalk-core` (`semantic_analysis`) | **M** |
| A3 | `DiagnosticCategory::DefiniteAssignment`; new `ExpectCategory` variant + `from_name` + unparse name; `[diagnostics]` escalation; parity across build/LSP/REPL/MCP (ADR 0100 Rule 3, `surface-parity.md`) | `beamtalk-core`, `beamtalk-language-service`, `beamtalk-cli` | **S** |
| A4 | Docs + tests: diagnostic text and severity as Rust unit tests plus LSP diagnostic-provider tests; the `uninitialized_state_actor.bt` / `non_typed_uninitialized_state_actor.bt` fixtures gain compile-time expectations | docs, `crates/**/tests` | **S** |

A1 is unblocked and independent. Nothing in Part A introduces a new
representation, so it carries none of Part B's risk.

### Part B — lazy slots (gated)

| # | Work | Components | Size |
|---|---|---|---|
| B0 | **Napkin, and a real go/no-go.** Hand-write the `ThreadedStmt` sequence for one lazy `state:` read — `maps:is_key` guard, both arms, the `maps:put` — and confirm `verify()` accepts it with balanced `State` versions, that it renders to compilable Core Erlang, and that `just verify-threaded-ir` stays green. Then repeat for `lazy classState:`, which must also emit ADR 0110's shadow write (§4d). **If the `classState:` half fails, adopt the generated-reader lowering (§4) before building anything else** | `beamtalk-codegen` (`threaded_ir/`) | **S** |
| B1 | Decide the lowering: transparent `self.slot` prelude vs. generated forcing reader (§4). This is a user-facing syntax decision and should be settled before B3 | — | **decision** |
| B2 | `lazy` modifier: lexer contextual keyword, `parse_state_declaration` prefix, `SlotKind` on `StateDeclaration`, unparse round-trip; the rejection Errors (`lazy field:`, lazy-without-initialiser, `lazy` on `Object`, `lazy state:` on a `native:` Actor); direct initialiser-cycle SCC Error | `beamtalk-core` (`source_analysis/parser/declarations.rs`, `ast/class.rs`, `unparse`) | **S** |
| B3 | Lazy read lowering. Under the transparent design this means auditing the ~15 state `maps:get` sites (§4a), giving `generate_field_access` a prelude channel, disabling DirectParams/Hybrid hoisting for lazy slots (§4b), extending `is_family_mutation` to treat a lazy `classState:` read as a `ClassVars` mutation (§4c), and the ADR 0110 shadow write (§4d). Under the generated-reader design it is one generated function plus one dispatch arm. Also: lazy slots omitted from `init/1`'s state literal; a `VerifyError` or explicit rule for `terminate:`/`handle_info` (§4, item 3) | `beamtalk-codegen` (`threaded_ir/`, `control_flow/`, `expressions.rs`, `gen_server/state.rs`) | **L–XL** (transparent) / **M** (generated reader) |
| B4 | Reflection and tooling: `fieldKinds`/`allFieldKinds` incl. the `ClassInfo` third map, the `__beamtalk_meta` schema entry, and `class_variables` growing from `Vec<EcoString>` to a structure; `behaviour.bt` declaration + Erlang intrinsic; generated `force_field/2`; `read_field/2`'s declared-lazy branch; the `'fieldAt:'` dispatch arm threading state; `InspectorField` `#lazySlot` / `value: #notComputed` / `drillable: false` plus the wire form and `beamtalk_inspector:fieldsOf/1`; LSP hover | `beamtalk-codegen`, `beamtalk-core`, `beamtalk_runtime`, `beamtalk-stdlib`, `beamtalk-language-service` | **M–L** |
| B5 | REPL-visible output: decide and confirm `printString` rendering for an unforced lazy slot (§9) — **gated on explicit user confirmation** per `CLAUDE.md` | `beamtalk_runtime` (`beamtalk_object_printer.erl`), `tests/repl-protocol` | **S**, gated |
| B6 | ADR 0123 reconcile lazy row + the shared `(slot kind, present?, has default?) -> outcome` conformance fixture. **Sequenced after ADR 0123's epic**, which is assumed complete before Part B starts (§8) — read `beamtalk_shape_migration` as built and add the lazy case to its reconcile. Re-measure the "`beamtalk_hot_reload` needs no change" finding against the finished epic rather than inheriting it from §8. Includes the `allFieldNames`-keep-set invariant test | `beamtalk_runtime`, `beamtalk-codegen` | **S** |
| B7 | Docs + tests: `beamtalk-language-features.md` (slot kinds, the force/no-force table, once-per-successful-computation, **initialisers must be idempotent**, the `spawnWith:`-injection clause at `:2426`, restart semantics), `surface-parity.md`, BUnit tests in `stdlib/test/*.bt`, REPL-protocol e2e | docs, `stdlib/test`, `tests/repl-protocol` | **S** |

**Test placement** (per `CLAUDE.md`): lazy-slot behaviour, memoisation,
initialiser failure-and-retry, and `fieldKinds` go in `stdlib/test/*.bt` as
BUnit `TestCase`s — none of it is a bootstrap primitive. Diagnostic text and
severity are Rust unit tests plus LSP diagnostic-provider tests. `lazy
classState:` on a REPL-defined class, and slot kinds surviving a class
reload, go in `tests/repl-protocol/cases/*.btscript`.

**Recommended start:** A1, then A2. If Part B proceeds, B0 before anything
else — it is the cheapest test of the assumption the whole design rests on,
and §4 has already moved that assumption from "probably free" to "free for
`State`, not for `ClassVars`".

## Migration Path

No migration is required. `lazy` is additive, every existing declaration
keeps its current meaning and representation, and the definite-assignment
finding is advisory (Warning/Hint, never Error by default), so no currently
compiling program stops compiling.

**An earlier draft proposed converting the stdlib's nilable singletons and
was wrong to.** It offered:

```beamtalk
sealed Object subclass: TranscriptStream
  lazy classState: current :: TranscriptStream = TranscriptStream new
```

Two hard errors. `TranscriptStream` is `typed Actor subclass:
TranscriptStream native: beamtalk_transcript_stream`
(`stdlib/src/transcript_stream.bt:17`), not an `Object` subclass; and `new` on
an Actor subclass is a compile error with its own diagnostic category
(`DiagnosticCategory::ActorNew`) — Actors use `spawn`. Beyond the syntax, all
three nilable singletons are injected by
`beamtalk_workspace_bootstrap:bootstrap_singleton/3` (`:149-186`), which
monitors the process and rebootstraps on death; a lazy initialiser cannot
reproduce that. **They must not be converted.**

So there is no migration to offer, which is the same finding as Context: the
current corpus has no lazy-init call site. Converting a slot **between eager
and lazy** would change its ADR 0123 reconcile row and so require a
`shapeVersion:` bump — relevant to future code, not to any existing code.

## References
- Related issues: [BT-3525](https://linear.app/beamtalk/issue/BT-3525)
  (this ADR); parent [BT-3523](https://linear.app/beamtalk/issue/BT-3523);
  related [BT-3524](https://linear.app/beamtalk/issue/BT-3524) (versioned
  state migration — this ADR closes its deferred "absent → declared
  initialiser policy"), [BT-3527](https://linear.app/beamtalk/issue/BT-3527)
  (distribution — lazy slots crossing a node boundary); historical
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
  a lazy slot holding a node-local handle),
  [ADR 0107](0107-nil-and-type-patterns-in-match.md) (nil narrowing — what a
  non-nilable declared type buys a reader),
  [ADR 0110](0110-class-var-shadow-write-through-for-nlr-relay.md) (the `ClassVars` shadow-write
  rule a `lazy classState:` read must respect),
  [ADR 0111](0111-lowered-ir-verifier-for-state-threading.md)
  (`ThreadedIr::verify()`),
  [ADR 0117](0117-beamtalk-core-crate-split.md) (why the predicate's home is
  `beamtalk-core`),
  [ADR 0118](0118-expression-level-state-threading-preludes.md)
  (`ThreadedValue` — the mechanism a lazy read lowers to),
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
- Prior art: [Newspeak lazy slots](https://groups.google.com/g/newspeaklanguage/c/IQsw31ze-IU)
  and the [Newspeak specification](https://newspeaklanguage.org/spec/newspeak-spec.pdf);
  [Pharo ComputedSlots](https://astares.blogspot.com/2019/03/computedslots-in-pharo.html);
  [Swift `lazy var` on structs](https://www.avanderlee.com/swift/lazy-var-property/)
  and [the Swift Forums thread on lazy vars in immutable structs](https://forums.swift.org/t/allow-lazy-vars-on-immutable-structs/16417)
