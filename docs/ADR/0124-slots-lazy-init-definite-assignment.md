# ADR 0124: Slots — Lazy Initialisation and Definite Assignment

## Status
Proposed (2026-09-18)

## Context

### Problem statement

A Beamtalk slot declaration (ADR 0067: `state:` on an Actor, `field:` on a
Value, `classState:` on any kind) says what data a class holds and,
optionally, what it starts as. What it does *not* say is **how the slot
becomes valid**. Three gaps follow from that omission:

1. **No lazy initialisation.** A slot whose value is expensive, or whose
   construction cannot run at `init/1` time, has to be declared nilable and
   nil-checked at every read. The stdlib does this by hand:

   ```beamtalk
   // stdlib/src/transcript_stream.bt — the singleton idiom, written out
   sealed Object subclass: TranscriptStream
     classState: current :: TranscriptStream | Nil = nil

     class current -> TranscriptStream => self.current
     class current: instance :: TranscriptStream -> TranscriptStream =>
       self.current := instance
   ```

   The declared type is `| Nil` purely to accommodate the window before
   assignment, so every caller carries a nil case that can never happen once
   the class is set up. `stdlib/src/beamtalk_interface.bt` has the same
   shape, and `stdlib/src/retry_policy.bt` pays it per-read:

   ```beamtalk
   effectiveCap := self.maximumInterval ifNil: [
     self class unlimitedIntervalCap
   ] ifNotNil: [:m | m]
   ```

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
(`crates/beamtalk-codegen/src/core_erlang/gen_server/callbacks.rs:470`)
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
(`beamtalk_actor.erl`, `strip_local_temps/1`). `beamtalk_actor:state_diff/2`
already reads a missing key as `'__absent__'`, so **key absence is an
established, tested representation** in the state map.

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
- **An initialiser runs inside the owning gen_server**, so it is subject to
  the same rule as `initialize`: a synchronous self-send to the same process
  deadlocks (ADR 0043).
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
  lazy classState: table :: Ets = Ets new: #registry type: #set

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
- `beamtalk_actor:state_diff/2` already reads a missing key as
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
any other threaded read. **No new storage family is introduced**, so ADR
0122's site/family matrix is untouched and the existing detectors and
emitters carry lazy reads through loops, conditionals, `match:`,
`on:do:`/`ensure:`, and Foldl list-ops without per-construct work.

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
invisible (`handle_call` already returns `{reply, V, State}`). It is *not*
invisible at the class-method block boundary: a lazy read is a state write,
so the existing rules for state writes crossing into a class's gen_server
apply unchanged, and `ThreadedIr::verify()` catches a produced-but-unconsumed
version the same way it catches ADR 0122's sixth gap. No new rule is added
here — the point is that `lazy` is not a way to sneak a write past one.

**d. Initialiser cycles are a compile-time Error.**

```beamtalk
Actor subclass: Tangled
  lazy state: a :: Integer = self.b + 1
  lazy state: b :: Integer = self.a + 1
// error: cyclic lazy slot initialisers: 'a' -> 'b' -> 'a'
```

This is one of the few places an Error is justified under ADR 0100: slot
initialiser expressions are all in the class body and its visible ancestors,
so the dependency graph is closed-world, and a cycle is provable
non-termination rather than an open-world guess. The check is an SCC over
the lazy-slot dependency graph, built from `self.<slot>` reads in initialiser
expressions.

A self-send in an initialiser (`self buildIndex`) is fine — it compiles as a
state-threaded local call, not a message. A *synchronous message to the
actor's own pid* deadlocks, exactly as in `initialize`, and reuses
`initialize`'s existing diagnostic.

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
```

### 4. Hazards: staleness, and reading a lazy slot in `terminate:`

A memoised value derived from other slots goes stale when those slots
change. Every language with lazy properties has this hazard, and it is the
single easiest way to misuse the feature:

```beamtalk
Actor subclass: StaleBuilder
  state: rows :: List = #()
  lazy state: index :: Dictionary = self buildIndex

  addRow: row :: Dictionary -> List =>
    self.rows := self.rows add: row
//  ^ warning: 'addRow:' assigns 'rows', which the lazy slot 'index' reads in
//    its initialiser. 'index' will not be recomputed. Options: make 'index' a
//    method, or assign 'self.index' here as well.
```

Two decisions follow:

- **Assignment to a lazy slot is permitted** and marks it computed, exactly
  as for an eager slot. `self.index := self buildIndex` is how an author
  refreshes one.
- **There is no invalidation primitive in this ADR.** Returning a lazy slot
  to the absent state would need a new surface (`self invalidateSlot:
  #index`), and adding one now would bless lazy-slots-as-caches — which is
  the misuse the diagnostic above exists to discourage. The shape is noted
  as reserved; it should be driven by a real use case, not by symmetry.

The diagnostic is a **Warning** (`DiagnosticCategory::StaleLazySlot`) and is
class-local and cheap: for each lazy slot, collect the `self.<slot>` reads in
its initialiser; if any method in the same class assigns one of those slots,
warn at the assignment. It is closed-world enough to be useful and, because a
subclass could assign the slot invisibly, not strong enough to be an Error.
A derived value over mutable inputs should be a method — recomputation is the
correct semantics there, and `lazy` is the wrong tool.

**The same analysis catches a second, sharper hazard: `terminate:`.** The
documented cleanup idiom reads the slot it is closing:

```beamtalk
Actor subclass: ResourceActor
  lazy state: handle :: Resource = Resource open

  terminate: reason :: Symbol -> Nil =>
    self.handle close
//  ^ warning: 'terminate:' reads the lazy slot 'handle'. If it was never
//    computed, shutdown will run 'Resource open' in order to close it.
//    Guard with 'respondsTo:'-style presence, or make cleanup conditional.
```

Forcing an initialiser *during shutdown* — acquiring a resource so it can
immediately be released — is a bug in every case, and `terminate:` is exactly
where an author will write it, because the pre-`lazy` idiom (`self.handle
isNil ifFalse: [self.handle close]`) is in the language guide. Same warning
category, same walk, one extra rule: warn on any lazy-slot read reachable
from `terminate:`.

Both warnings and the §6 definite-assignment finding read the *same*
per-class structure — the lazy-slot set, each initialiser's `self.<slot>`
dependencies, and per-method slot reads and writes. They are three questions
over one analysis, not three analyses, which is why they are affordable
together.

**Supervisor restarts recompute.** A restarted actor gets a fresh state map
with its lazy slots absent, so initialisers run again on first read. That is
the correct behaviour — a restart is meant to rebuild derived state — and it
is worth stating because it differs from an eager slot, whose initialiser
runs in `init/1` on every start either way.

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
//  ^ warning: state field 'socket' (:: Socket) may not be assigned when
//    'initialize' returns — assigned only in the 'ifTrue:' arm.
//    Spawning raises UninitializedStateError. Options: give it a default,
//    widen the type to 'Socket | Nil', or declare it 'lazy'.
```

**Severity** (`DiagnosticCategory::DefiniteAssignment`, a new category so
suppression can target it):

| Situation | Severity |
|---|---|
| Chain fully visible, no `initialize` in it assigns the slot | **Warning** |
| A literal-keyed `spawnWith:`/`new:` in the compilation unit supplies it | **suppressed** |
| Chain incomplete — cross-package parent, `@native` ancestor (ADR 0056), or a `fieldAt:put:`/`perform:` writer in the class | **Hint** |
| Slot is `lazy`, defaulted, or nilable | **nothing** |

Warning, never Error by default, because the open world is real: the slot may
be written by `spawnWith:` from another package, by `ClassBuilder`, or by a
hot-patched `initialize`. This matches the precedent already set for a
provably-failing construction — an unknown `spawnWith:` key is a Warning, not
an Error. Escalation to Error is available per-project through ADR 0100
Rule 3's `[diagnostics]` table, and suppression is `@expect
definite_assignment` on the declaration, which needs no parser work:
`StateDeclaration.expect` already exists and already emits a stale-`@expect`
warning when the finding goes away.

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
  must not be persisted — and that is already ADR 0103's `handleScope:`
  mechanism's job, not a new one this ADR invents.

### 9. Reflection, inspector, LSP — the force / no-force split

The operator-facing rule that constrains the design: **observation must not
run user code.**

| Surface | Forces the initialiser? |
|---|---|
| `anActor fieldAt: #slot` | **Yes** — a deliberate program-level read |
| `anActor fieldAt: #slot put: v` | n/a — writes, marking the slot computed |
| `anActor fieldNames`, `Cls fieldNames`, `Cls allFieldNames` | No — declared/actual schema, unchanged |
| `Cls fieldKinds` (**new**) | No — answers `Dictionary(Symbol -> Symbol)`, `#eager \| #lazy` |
| Inspector (ADR 0095), `sys:get_state`, `observer`, `recon` | **No** — renders "not computed" |
| LSP hover | No — shows the declaration as written |

Opening an inspector on an actor must not execute an initialiser that opens
a socket. So `InspectorField` gains `#lazySlot` to its `kind` vocabulary
(alongside `#slot #element #association #processInfo`) and renders a
not-yet-computed slot as such, with `drillable: false` until it holds a
value. `fieldAt:` is the opposite case — the caller asked for the value, so
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

`fieldKinds`, not `slotKinds`: ADR 0035 deliberately unified the reflection
API on `field`-prefixed selectors (`fieldNames`, `fieldAt:`, `allFieldNames`,
class-side `fieldNames`), explicitly rejecting alternatives that break that
consistency. "Slot" stays the prose concept, as it already is in the language
guide; the *selector* follows 0035.

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

**Smalltalk developer.** `lazy state:` deletes the `ifNil:` accessor idiom
they write by hand in Pharo, and it is *Newspeak's* spelling, not an
invention — a Newspeak reader will recognise the declaration form and its
semantics exactly. The departure from Pharo is that slot kinds are syntax
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
one `maps:is_key`, and `state_diff/2`'s existing `'__absent__'` handling
shows the runtime already tolerates missing keys.

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

### Do nothing — keep the `ifNil:` idiom and the runtime check
The status quo works: `classState: current :: X | Nil = nil` plus a nil-check
is five extra characters in the declared type and one `ifNil:` per read, and
`UninitializedStateError` does catch the unassigned-slot bug — at spawn time,
which for an actor started under a supervisor is usually immediately and
loudly. Rejected on the evidence in Context: the idiom is already duplicated
across `transcript_stream.bt`, `beamtalk_interface.bt`, and
`retry_policy.bt`, and every one of them widens a declared type to `| Nil`
solely to describe a window that closes during startup — which then defeats
ADR 0107's nil narrowing for every downstream reader, permanently. The
definite-assignment half is a strictly additive diagnostic over machinery
(the ADR 0078 chain walk) that already exists, so "do nothing" is not the
cheap option it looks like; it is declining a signal the compiler can already
almost compute.

### Deleting the runtime `UninitializedStateError` check
Rejected outright. Static analysis is advisory in an open world; the runtime
check is the only thing that catches a reflective or hot-patched write path.
The static check is additive.

## Consequences

### Positive
- The nilable-singleton idiom collapses: `classState: current :: X | Nil =
  nil` plus a manual `current:` setter becomes one `lazy classState:` line,
  and the `| Nil` leaves the declared type, so callers stop carrying an
  impossible nil case (`transcript_stream.bt`, `beamtalk_interface.bt`).
- A class of spawn-time `UninitializedStateError` crashes becomes a
  compile-time warning naming the slot, its type, and three fixes — on all
  four surfaces.
- Thread-safe lazy initialisation costs nothing on the BEAM: no
  double-checked locking, no thread-safety mode, no bitmap. A genuine
  platform advantage, now surfaced in the language.
- No new storage family, so ADR 0122's site/family matrix and every existing
  detector/emitter carry lazy reads through loops, conditionals, `match:`,
  `on:do:`/`ensure:`, and Foldl list-ops unchanged.
- The definite-assignment predicate moves from `beamtalk-codegen` down to
  `beamtalk-core`, where it belongs, and stops being codegen-private.
- ADR 0123's deferred "absent → declared initialiser policy" is closed with
  one new reconcile row.
- Lazy slots are *safer* across a node boundary than the eager slots they
  replace: a node-local resource is recomputed on arrival rather than shipped.
- Observation stays side-effect-free by rule, so inspectors and `observer`
  cannot perturb a running system.

### Negative
- **An initialiser can run more than once.** Failure leaves the slot absent
  and the next read retries. An initialiser with side effects is not
  idempotent-by-construction, and this is observable behaviour authors must
  know about.
- **A memoised lazy slot goes stale** when the slots its initialiser reads
  are written afterwards. The §4 warning catches the class-local case; a
  subclass writing an inherited slot is not caught, and there is no
  invalidation primitive.
- **`lazy` moves work from spawn to an arbitrary later message**, shifting
  where latency spikes and crashes appear. An actor that spawned cleanly can
  now fail on its fourth message for a reason declared at the top of the file.
- **Every read of a lazy slot pays a `maps:is_key/2`, not just the first**,
  and the read threads state, so in some contexts it costs a tuple
  allocation it would not have cost eagerly. Reads of eager slots are
  bit-for-bit unaffected — the cost is scoped to slots that opt in.
- **A getter that reads a lazy slot is no longer a pure read.** It threads
  state, so it is subject to the class-method block-boundary rules for state
  writes — a real constraint that does not apply to eager slots.
- **The state map no longer has one fixed key set** over an actor's lifetime.
  ADR 0123's reconcile, `state_diff`, and the inspector each need the lazy
  case.
- **Class kinds diverge**: `lazy` works on `state:` and `classState:` but not
  `field:`. Defensible, and still a wart to explain.
- **The definite-assignment warning will produce false positives** where a
  slot is supplied by `spawnWith:` from outside the compilation unit.
  `@expect definite_assignment` is the escape hatch, and every escape hatch
  is friction.
- Six new diagnostics — three Errors (`lazy field:`,
  lazy-without-initialiser, initialiser cycle) and three advisories (stale
  lazy slot, lazy read in `terminate:`, definite assignment) — plus one new
  reflective selector, all to keep at parity across CLI, REPL, LSP, and MCP.
  The three advisories share one per-class analysis, but six findings is
  still a real documentation and false-positive surface.

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

Eight phases, ordered so each is independently shippable. Phase 0 is a
napkin that de-risks the one assumption the whole design rests on; 1–2 are
prerequisites; 3–5 are parallelisable after 2.

| # | Work | Components | Size |
|---|---|---|---|
| 0 | **Napkin: prove a lazy read passes `verify()`.** Hand-write the `ThreadedStmt` sequence for one lazy `state:` read on one actor — `maps:is_key` guard, both arms, the `maps:put` in the computing arm — and confirm `ThreadedIr::verify()` accepts it with balanced `State` versions, that it renders to compilable Core Erlang, and that `just verify-threaded-ir` stays green. **If this fails, the "no new storage family" claim is wrong and §3 needs redesigning before anything else is built.** No parser work, no diagnostics, one fixture | `beamtalk-codegen` (`threaded_ir/`) | **S** |
| 1 | `lazy` modifier: lexer contextual keyword, `parse_state_declaration` prefix, `SlotKind` on `StateDeclaration`, unparse round-trip. The three rejection Errors: `lazy field:`, lazy-without-initialiser, `lazy` on `Object` | `beamtalk-core` (`source_analysis/parser/declarations.rs`, `ast/class.rs`, `unparse`) | **S** |
| 2 | Move the definite-assignment predicate (`is_nilable_type`, annotated ∧ no-default ∧ non-nilable ∧ not-lazy) down into `beamtalk-core`; `inherited_typed_no_default_fields` calls it; **exclude lazy slots from `generate_post_initialize_check`** (its `maps:get/2` would `badkey` on an absent key) | `beamtalk-core` (`semantic_analysis`), `beamtalk-codegen` (`gen_server/callbacks.rs`) | **S** |
| 3 | Lazy read lowering: an ADR 0118 `ThreadedValue` prelude on `VersionPrefix::State`, then `ClassVars` for `lazy classState:`; lazy slots omitted from `init/1`'s state literal; `verify()` coverage; `just verify-threaded-ir` over the stdlib + bootstrap corpus. Plus the initialiser dependency graph, which both the cycle Error and the §4 `StaleLazySlot` warning read | `beamtalk-codegen` (`threaded_ir/`, `gen_server/state.rs`), `beamtalk-core` (dependency graph, cycle check, staleness check) | **M** |
| 4 | Definite-assignment analysis: branch-aware dataflow over the ADR 0078 flattened `initialize` chain; `DiagnosticCategory::DefiniteAssignment`; `@expect definite_assignment`; `[diagnostics]` escalation; parity across build/LSP/REPL/MCP | `beamtalk-core` (`semantic_analysis`), `beamtalk-language-service`, `beamtalk-cli` | **M** |
| 5 | Reflection and tooling: `fieldKinds`; generated `force_field/2`; `read_field/2`'s declared-lazy branch; the `'fieldAt:'` dispatch arm threading state; `InspectorField` `#lazySlot` + `drillable: false`; inspector and `sys:get_state` do **not** force; LSP hover shows `lazy` and when the initialiser runs | `beamtalk-codegen`, `beamtalk_runtime` (`beamtalk_reflection.erl`, `beamtalk_object_ops.erl`), `beamtalk-stdlib`, `beamtalk-language-service` | **M** |
| 6 | ADR 0123 reconcile lazy row in `beamtalk_shape_migration`/`beamtalk_hot_reload`; the shared `(slot kind, present?, has default?) -> outcome` conformance fixture driving both the Rust golden test and the Erlang EUnit test | `beamtalk_runtime`, `beamtalk-codegen` | **S** |
| 7 | Docs and tests: `docs/beamtalk-language-features.md` (slot kinds, the force/no-force table, once-per-successful-computation, staleness and when to prefer a method, the BEAM-is-the-lock note), `docs/development/surface-parity.md`, BUnit tests in `stdlib/test/*.bt`, REPL-protocol e2e in `tests/repl-protocol/cases/` | docs, `stdlib/test`, `tests/repl-protocol` | **S** |

**Test placement** (per `CLAUDE.md`): lazy-slot behaviour, memoisation,
initialiser failure-and-retry, and `fieldKinds` go in `stdlib/test/*.bt` as
BUnit `TestCase`s — none of it is a bootstrap primitive. Diagnostic text and
severity are Rust unit tests plus LSP diagnostic-provider tests. The
absent-key reconcile table is the cross-language conformance fixture from
phase 6. `lazy classState:` on a REPL-defined class, and slot kinds surviving
a class reload, go in `tests/repl-protocol/cases/*.btscript`.

**Recommended start:** phase 0. It is the cheapest possible test of the
load-bearing assumption (§3: a lazy read is just an ADR 0118 prelude on an
existing storage family), and every later phase is wasted if it does not
hold. Phases 1 and 2 are then unblocked and independent of each other.

## Migration Path

No migration is required. `lazy` is additive, every existing declaration
keeps its current meaning and representation, and the definite-assignment
finding is advisory (Warning/Hint, never Error by default), so no currently
compiling program stops compiling.

Two optional cleanups the feature enables, both mechanical:

```beamtalk
// before — nilable only to cover the pre-assignment window
sealed Object subclass: TranscriptStream
  classState: current :: TranscriptStream | Nil = nil
  class current -> TranscriptStream => self.current
  class current: instance :: TranscriptStream -> TranscriptStream =>
    self.current := instance

// after — the `| Nil` is gone from the declared type
sealed Object subclass: TranscriptStream
  lazy classState: current :: TranscriptStream = TranscriptStream new
  class current -> TranscriptStream => self.current
```

Narrowing a declared type from `T | Nil` to `T` is a **shape-compatible but
type-visible** change: the stored value's shape is unchanged, so ADR 0123's
reconcile is unaffected, but a caller that pattern-matched the `nil` case
will get a dead-branch lint. Converting a slot **between eager and lazy**
changes its reconcile row and therefore requires a `shapeVersion:` bump
(ADR 0123).

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
  [ADR 0056](0056-native-erlang-backed-actors.md) (`@native` actors — an
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
