# ADR 0124: Slots — Late Assignment and Definite Assignment

## Status
Accepted (2026-09-18)

Two parts, decided together because they are one rule seen from both sides.

- **Part A — definite assignment (§6, §7).** A compile-time diagnostic at the
  construction site for a slot that is declared, non-nilable, defaulted
  nowhere, and assigned by no `initialize` in the chain. Small, low-risk,
  entirely in `beamtalk-core`. **Accept.**
- **Part B — `late` slots (§1–§5, §8, §9).** A slot exempt from ADR 0078's
  post-`initialize` check, absent until assigned, raising the existing
  `UninitializedStateError` if read first. Reading never writes, so it needs
  no state-threading lowering. **Its evidence is two instance slots** in
  the surveyed applications (most nilable slots there encode a resolution
  strategy in their `nil` and must not change) **and three class-side
  singletons** in the stdlib, every `classState:` it declares (§Context (e)).
  The pattern is real and general, and it is a judgement call rather than a
  demand from the corpus. Applies to `state:` and `classState:`.
  **Accept — B1, B2, B5 and B3 ship together as Part A's exemption (B1–B2
  alone would regress an early read to a raw `badkey`, §Implementation); B4
  and B6–B10 stand on the two instance slots and the three class-side
  singletons.**

**Part A needs an exemption, and the exemption is a language construct.**
A lifecycle-assigned slot must be able to opt out of Part A's diagnostic.
**This ADR adopts the position that where a language construct can express
the fact, it is preferred to an annotation that silences a diagnostic.** The
precedent is that every existing class and slot modifier — `sealed`,
`abstract`, `typed`, `internal`, and ADR 0067's `state:`/`field:`/
`classState:` — is a keyword, not a pragma. So the opt-out is `late`, a
declaration that says what the slot *is*, and not an `@expect` category that
says which warning to silence (§Alternatives). That makes Part B
load-bearing for Part A rather than optional: without `late`, Part A's only
exemption would be an annotation.

Part A checks the slots that must be valid after `initialize`; Part B declares
the ones that legitimately are not yet. A slot is `late` exactly when Part A's
diagnostic would otherwise be wrong.

## Context

### Problem statement

A slot declaration (ADR 0067: `state:` on an Actor, `field:` on a Value,
`classState:` on any kind) says what data a class holds and, optionally, what
it starts as. It does not say **how the slot becomes valid**. Three gaps
follow.

**1. No way to declare a slot that is assigned after `initialize`.** A slot
acquired by an explicit lifecycle call — open a subprocess, start a listener,
attach a supervisor — must be declared nilable and nil-checked at every read,
because ADR 0078's post-`initialize` check raises `UninitializedStateError`
on any non-nilable typed slot still unset when `initialize` returns.

The evidence for instance slots is the two real applications built on
Beamtalk, both cited as real-world corpora by ADR 0067 — Exdura (a workflow
engine) and Symphony (an agent orchestrator), surveyed at `exdura@d6b350e` /
`symphony@d95ce02`. The evidence for class-side slots is the stdlib's three
singletons, (e) below. (`retry_policy.bt`'s nilable field is a genuinely
optional value and is not evidence either way.)

**(a) Every nilable resource slot is assigned by an explicit lifecycle method,
and several are unset again.**

| Slot | Assigned | Unset | A `late` candidate? |
|---|---|---|---|
| `symphony/codex_client.bt` `proc :: Subprocess \| Nil` | `launch` (`:97`) | `stopProcess` (`:71`) | **Yes** — a running client always has one; `nil` is purely "not launched" |
| `exdura/exdura_http_server.bt` `httpServer` | in `initialize` (`:256`) | `destroy` | **Yes** — `nil` is purely absence |
| `exdura/exdura_client.bt` `supervisor` | `setSupervisor:` (`:170`) | — | **No** — `nil` means "standalone mode" and selects a resolution strategy (`:144-155`); see (b) |
| `exdura/exdura_client.bt` `httpServer` | lifecycle (`:190`) | — | **No** — genuinely optional per its own doc ("nil unless started with an `#http` config") |
| `exdura/workflow_engine.bt` `eventStore`, `activityPool` | — | — | **No** — `nil` means "resolve by name"; see (b) and (d) |

Two slots across two classes. Part B is judged on those two.

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

`proc` is not optional — a running `CodexClient` always has one. The `| Nil`
exists solely to survive the window before `launch`, and it makes every later
`self.proc readLine:` / `writeLine:` / `close` a read of a nilable type.

**(b) In this corpus `nil` is overloaded with *mode*, which `late` does not
fix.** Exdura's engine (`exdura/src/workflow/workflow_engine.bt:20-26`):

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

`eventStore` (`:983`) resolves by name, freshly, on every call — the comment
says "rather than caching the lookup, means a `rest_for_one` restart of
EventStore is picked up automatically" — and never consults `supervised`.
`currentActivityPool` (`:1005`) consults it to choose between two resolve-time
behaviours ("resolve by name" vs "genuinely no pool"). So `supervised` is a
flag selecting a **resolution strategy**, and `late` does not retire it: a
`late` slot offers absent / value, while `activityPool` needs two meanings
*of absence*. `exdura_client.bt`'s `supervisor` (`:144-155`) is
the same: `nil` means "standalone, use the direct refs", a value means
"supervised, resolve by name". The `nil` is load-bearing logic, not a gap.

**(c) There is no memoise-on-first-read anywhere.** Across 148 `.bt` files in
both applications, the `ifNil: [self.x := …]` idiom occurs zero times. The
only `isNil`-adjacent writes are `codex_client.bt:71`'s `self.proc := nil`
(unsetting) and `workflow_watcher.bt:36`'s first-observation baseline.
Deferred computation is not a need these codebases have.

**(d) Some slots must never be memoised.** Exdura's timer manager
(`exdura/src/timer/timer_manager.bt:15-20`) resolves by name on every read,
deliberately:

```beamtalk
  /// (ADR 0079) instead of passing live refs, since neither exists yet
  /// when the child spec is built. `engine`/`eventStore` below resolve
  /// fresh by name in that case, so a `rest_for_one` restart of either is
  /// picked up automatically instead of leaving a nil/stale field.
  state: engine :: WorkflowEngine | Nil = nil
  state: eventStore :: EventStore | Nil = nil
```

`exdura_client.bt:144-155` does the same via `currentEngine` /
`currentEventStore`. These stay ordinary methods, and they are why this ADR
introduces no memoising slot kind.

**(e) Every `classState:` slot in the stdlib is a factory-set singleton
behind a nilable type it does not mean.** The two applications declare no
`classState:` at all; the stdlib declares three, and all three are the same
shape (`transcript_stream.bt:18-30`, `beamtalk_interface.bt:27-35`,
`workspace_interface.bt:26-29`):

```beamtalk
typed Actor subclass: TranscriptStream native: beamtalk_transcript_stream
  classState: current :: TranscriptStream | Nil = nil

  class current -> TranscriptStream => self.current
  class current: instance :: TranscriptStream -> TranscriptStream =>
    self.current := instance
  class resetCurrent -> Nil => self.current := nil
```

Set by a class-method factory (`current:`), cleared by `resetCurrent`, and
set again by workspace bootstrap writing the class variable directly through
`set_class_variable/2`. The three differ in kind, which matters for §1:
`TranscriptStream` is a `native:` Actor whose singleton is a registered
process, wired by `bootstrap_singleton/3` (`:149-172`) with an
`erlang:monitor` and re-wired on death; `BeamtalkInterface` and
`WorkspaceInterface` are `sealed typed Object subclass:`es
(`beamtalk_interface.bt:20`, `workspace_interface.bt:19`) whose singleton is
a tagged-map instance created by `Module:new()` in
`bootstrap_value_singleton/3` (`:174`), with no process and no monitor.
`class current` already declares a non-nilable return type over a nilable
slot in all three. This is the instance-slot pattern of (a) on the class
side, and it is three examples to (a)'s two.

All three accessors are relied on to answer `nil` before bootstrap.
`Object>>show:` and `Object>>cr` (`stdlib/src/object.bt:343`, `:355`) read
`TranscriptStream current ifNotNil: [:t | t show: aValue]` so that output
before bootstrap is a silent no-op, pinned by `stdlib/test/show_cr_test.bt`;
and `stdlib/test/class_variables_singleton_test.bt:11-16` asserts
`BeamtalkInterface current`, `TranscriptStream current` and
`WorkspaceInterface current` all equal `nil` in the un-bootstrapped BUnit
context. `BeamtalkInterface current` and `WorkspaceInterface current` are
bare `self.current` reads with no guard anywhere today. So the slots can be
`late`, but all three `current` accessors must stay nilable and guard with
`hasField:` (§Migration Path); converting a slot without rewriting its
accessor would raise `UninitializedStateError` where those tests expect
`nil`.

`state: x :: T = <expr>` already permits an arbitrary **eager** initialiser
(`gen_server/state.rs:36`, `:98`). The gap is not computing a value late; it
is declaring that a slot is legitimately unset for a while without lying
about its type.

**2. Definite assignment is enforced only at runtime.** ADR 0078's
post-`initialize` check works, is not gated on the `typed` keyword
(`stdlib/test/fixtures/non_typed_uninitialized_state_actor.bt` pins that),
and is a spawn-time crash for a mistake usually visible in the source:

```beamtalk
// stdlib/test/fixtures/uninitialized_state_actor.bt
typed Actor subclass: UninitializedStateActor
  state: connection :: String
  state: count :: Integer = 0

  initialize -> Integer =>
    // Deliberately does NOT set self.connection
    self.count := 1
```

The LSP is silent, `beamtalk build` is silent, and the first signal is a
failed `spawn`.

**3. Slot kind is invisible to reflection and tooling.** ADR 0035's
field-based reflection answers *which* slots exist and *what* they hold, not
*how* a slot is initialised, so ADR 0095's inspector cannot distinguish "holds
`nil`" from "not assigned yet" — a distinction that exists once `late` slots
do.

### Current state

**The runtime predicate.** `generate_post_initialize_check`
(`crates/beamtalk-codegen/src/core_erlang/gen_server/callbacks.rs:474`)
emits, per qualifying slot, a `maps:get(Slot, InitNewState)` and a `case` on
`'nil'`. Qualifying is decided by `inherited_typed_no_default_fields` (`:709`)
walking the flattened superclass chain, with the per-slot test at `:723`:

```rust
s.type_annotation.is_some()
    && s.default_value.is_none()
    && !Self::is_nilable_type(s.type_annotation.as_ref())
```

`is_nilable_type` (`:619`) and `is_nilable_type_name` (`:806`) are
codegen-local. The predicate is a semantic property of a declaration, and a
compile-time check needs the identical answer, so its current home is the
wrong layer (§7). It is called only from `generate_handle_continue` (`:857`),
so it is **Actor-only**: Values have no runtime check (§6).

**Slot defaults.** An eager initialiser compiles into `init/1`'s state map
literal (`gen_server/state.rs:36`, `:98`); a slot with no initialiser is
emitted as `'nil'`. `nil` is already the compiler's stand-in for "no value was
supplied", which is why it cannot also mean "not assigned yet".

**Reserved state-map keys.** Actor state carries `'__methods__'`,
`beamtalk_tagged_map:class_key()`, `'__class_mod__'`, ADR 0123's
`'__shape_version__'`, and transient `'__local__'`-prefixed threading temps
(`beamtalk_actor.erl`, `strip_local_temps/1`). `init/1` validates only
`'$beamtalk_class'` and `'__methods__'` (`beamtalk_actor.erl:1582`), so a
missing user slot key breaks no existing invariant.
`beamtalk_actor:changed_state_keys/2` (`:1963`) uses `'__absent__'` only as
a local `maps:get/3` default for comparing two maps; it is never stored,
returned, or observed. An absent key in a live state map is genuinely new.

**ADR 0123 deferred part of this here by name.** Its References record
BT-3525 as owning "the 'absent → declared initialiser policy' generalisation
of reconcile step 3". Step 3 currently says: a declared slot present in the
migrated dictionary is kept; absent takes its declared default; absent with no
default is `nil` on an untyped class and a failure on a `typed` one.

### Constraints

- **The runtime check cannot be deleted.** Slots are writable through
  `fieldAt:put:`, `perform:`, `spawnWith:`, `ClassBuilder` (ADR 0038), and a
  hot-patched `initialize` (ADR 0082/0084). Static analysis in an open world
  (ADR 0100) is advisory; `UninitializedStateError` stays as the backstop.
- **Value instances are plain maps with no owning process** (ADR 0042) and
  are never reassigned after construction.
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

**Why one ADR.** Part A says "a non-nilable, defaulted-nowhere slot must be
assigned by the time `initialize` returns"; `late` is the declaration that
says "not this one, and here is what happens if you read it early". Without
`late`, Part A's diagnostic is wrong for every lifecycle-assigned slot in the
corpus; without Part A, `late` is a keyword with no check behind it.

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

`launch` is unchanged from the real code — `late` adds no initialiser
expression and takes nothing over. Only the declaration changes:
`proc :: Subprocess` instead of `proc :: Subprocess | Nil = nil`, so
`self.proc writeLine:` reads a `Subprocess` and every downstream reader keeps
ADR 0107's narrowing.

The modifier precedes the declaration keyword, matching the class-header
modifier position (`sealed typed Collection subclass: Array`, parsed by the
loop at `declarations.rs:191-209`). That loop is class-header only; there is
no member-level modifier loop, so B1 reuses its shape, not its code. `late`
is unused as a word anywhere in the stdlib's `.bt` sources, so it is a
contextual keyword in this position only.

**Where `late` is permitted:**

| Declaration | `late` | Rationale |
|---|---|---|
| `late state:` (Actor) | **Yes** | The instance gen_server holds the slot |
| `late classState:` (any class kind) | **Yes** | The class gen_server holds it whatever the kind (ADR 0036) — the three stdlib singletons are one `native:` Actor, where ADR 0056 permits `classState:`, and two `Object` subclasses, where `classState:` is the only slot kind ADR 0067 allows. Needs its own mechanism at each surface (§4i): the class-method read branch is a bare `maps:get` on the `ClassVars` map (`expressions.rs:526-537`), reflective reads go through `get_class_var`, which answers `nil` for a missing key (`beamtalk_object_class.erl:1395`), `hasField:`/`clearField:` dispatch on the object state map, which is `#{}` on a class object (`beamtalk_object_ops.erl:88-92`), and `ClassInfo.class_variables` is names only (`class_info.rs:171`) |
| `late field:` (Value) | **Error** | A Value is fully constructed by `new`/`new:`/the keyword constructor and never mutated (ADR 0042); see §5 |
| `late state:`/`field:` on `Object` | **Error** | Already an error — Object holds no instance data (ADR 0067) |
| `late state:` on a `native:` Actor | **Error** | Already an error — ADR 0056 prohibits `state:` there |

**A type annotation is required.** `late state: proc` with no `::` is an
Error: the purpose is to declare a non-nilable type for a slot that is
temporarily unset, and an untyped slot already defaults to `nil` with no
check.

**A default is forbidden.** `late state: x :: Integer = 0` is an Error — a
slot with a default is never unset.

**A nilable type is forbidden.** `late state: x :: T | Nil` is an Error: a
slot whose type admits `nil` is never in the state `late` describes, since
`nil` is already a value it can hold, and ADR 0078's check never applied to
it. The message says to drop `late` or make the type non-nilable. So a `late`
slot has exactly two states, absent and assigned, and its declared type is
always non-nilable. Where absence itself must carry two meanings, as in
Exdura's `activityPool` (§Context (b)), an explicit flag remains right;
`late` does not express that and does not try to.

### 2. Representation: the key is absent until assigned

A `late` slot's key is **not present** in the state map until assigned. Not
`nil`, not a reserved sentinel.

- The presence test is `maps:is_key/2`. No assigned value — `nil`, `false`,
  or `'__absent__'` itself — can be mistaken for "unassigned".
- `nil` is already the representation for "no value supplied" in two places
  (`state.rs:36` emits `'nil'` for a defaultless slot; ADR 0078's check reads
  `'nil'` as unassigned), so a third structural meaning could not be told
  apart from either.
- `beamtalk_actor:init/1` validates only `'$beamtalk_class'` and
  `'__methods__'` (`beamtalk_actor.erl:1582`), so a missing user slot key
  breaks no existing invariant.
- **`late` slots are excluded from ADR 0078's post-`initialize` check.** That
  is the point of the feature, and it is also required: the check emits
  2-arity `maps:get(Slot, InitNewState)`, which would `badkey`-crash on an
  absent key.

`nil` keeps its present meaning — the value of an unannotated, undefaulted
slot — and is never a value a `late` slot can hold.

### 3. Read and write semantics

**Reading never writes.** This is what keeps Part B small.

A read of a `late` slot compiles to a guarded map read:

```text
case maps:find(Slot, State) of
  {ok, 'nil'} -> <raise uninitialized_state_error>
  {ok, V}     -> V
  'error'     -> <raise uninitialized_state_error, naming the slot and class>
end
```

The `'nil'` arm is required, not optional. ADR 0078's post-`initialize`
check currently catches a `nil` injected at spawn on a non-nilable slot; §2
exempts `late` slots from that check, and `spawnWith:` with a non-literal
map, `fieldAt:put:` (`beamtalk_reflection:write_field/3`,
`beamtalk_reflection.erl:58`, is `State#{Name => Value}`, no type check) and `perform:` can all put `nil` in a
`late` slot unobserved. A presence-only guard would then hand `nil` to a
reader the type checker has told to skip narrowing. So the guard keeps ADR
0078's nil test and adds the absence test. **`late` does not make a
non-nilable declared type sound** — the open-world writers above still
exist — it makes the violation raise at the read, naming the slot, instead
of surfacing as a `does_not_understand` on `nil` somewhere else.

`generate_field_access` (`expressions.rs:519`) stays pure — it returns
`Result<Document<'static>>` with no prelude channel — because the guard
produces a value and never a new state. None of the following applies to
`late` slot **reads**: ADR 0118 preludes, `ThreadedIr` state threading, the
`ClassVars` mutation detector (`analysis.rs:253`), ADR 0110's shadow write,
loop-hoisting interference (`control_flow/loop_mode.rs:118`), or a
`terminate:`/`handle_info` returned-state problem. `clearField:` is a write
and is budgeted as one (§4f).

**Writing is the existing syntax.** `self.proc := v` assigns and so marks the
slot present.

**The error is the existing one.** Reading an unassigned `late` slot raises
`uninitialized_state_error` (`beamtalk_error.erl:433`), the error ADR 0078
already raises for read-before-assign, detected at the read instead of after
`initialize`. Its hint names the slot, its declared type, and the class:

```beamtalk
c := CodexClient start: "/tmp/ws" config: cfg onEvent: nil
c sendLine: "hello"
// => UninitializedStateError in 'sendLine:' on CodexClient
//    hint: CodexClient field 'proc' (:: Subprocess) is declared `late` and
//          has not been assigned yet
```

Today reading too early answers `nil` and the failure surfaces as a
`does_not_understand` for `writeLine:` on `nil`, several frames from the
cause.

**Two new selectors.**

`hasField:` — a presence test that does not raise. Symphony's cleanup guard
is the one line of the real code that must change:

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

`clearField:` — returns a `late` slot to the unassigned state so the resource
can be re-acquired. Symphony clears `proc` so `launch` can run again;
Exdura's `ExduraHttpServer destroy` does the same for `httpServer`. Both join
ADR 0035's `field`-prefixed family (`fieldNames`, `fieldAt:`, `fieldAt:put:`,
`allFieldNames`). Neither currently exists. `self clearField:` is a state
write and lowers exactly as `self fieldAt:put:` does — a dedicated codegen
intrinsic (`generate_self_field_at_put_open`, `dispatch_codegen.rs:2353`,
which emits `maps:put` into the threaded state and must not route through
`sync_send`) with `maps:remove` in place of `maps:put`, through `ThreadedIr`
and covered by `verify()`. `hasField:` must also join the hard-coded
reflection arm list every generated Value module carries
(`value_type_codegen.rs:4753-4761`: `class`, `respondsTo:`, `fieldNames`,
`fieldAt:`, `fieldAt:put:`, `perform:`, `perform:withArguments:`), or
`aValue hasField: #x` is a DNU on a selector advertised on `Object`.

**`self.proc := nil` does not un-assign.** On a non-nilable `late` slot it
stays the type error it is today, and its message names `clearField:`. The
ADR's rule is that `nil` stops carrying structural meaning; a magic un-assign
would reintroduce it for one saved line.

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

**a. `generate_field_access` needs a guarded form for `late` slots.** It
emits 2-arity `maps:get` for the Actor, class-method (`:526`) and Repl
contexts; an absent key there is a `badkey` crash. The `late` read emits
`maps:find` plus the error arm. The function stays pure. Its ~15 sibling
state `maps:get` sites (`blocks.rs:494,504,613,745,769,788`;
`control_flow/body.rs:1389,1424`; `control_flow/util.rs:53`;
`conditionals.rs:2172`; `while_loops.rs:896`; `dispatch_codegen.rs:3309`;
`threaded_expr.rs:533`; `threaded_ir/emit.rs:730`) need auditing for the
same exposure, not re-architecting.

**b. Loop hoisting must not pre-extract a `late` slot.**
`hybrid_readonly_field_params` (`loop_mode.rs:118`) substitutes a field's
value before the letrec instead of emitting a read, which for a `late` slot
turns "raises when read inside the body" into "raises before the loop, even
if the body never runs". DirectParams/Hybrid hoisting falls back to the
guarded read for `late` slots.

**c. Reads of a `late` slot cost a `maps:find` rather than a `maps:get`.**
Eager slots are unaffected.

**d. `terminate:` and `handle_info` get a diagnostic.** `terminate/2` wraps
the dispatch in `try … catch → 'ok'` (`gen_server/callbacks.rs:1560`), so an
unguarded `late` read there lets shutdown complete but silently skips the
cleanup. Warn on an unguarded `late`-slot read reachable from `terminate:`,
with `hasField:` as the fix. A per-class walk, no dataflow. This is the real
hazard in Symphony's `terminate: → stopProcess`.

**e. Reflection needs metadata for the slot kind** (§9), and **so does the
read guard.** `generate_field_access` receives only the receiver and the
field identifier (`expressions.rs:519-590`); to emit the guarded form it must
know which slots are `late` **including inherited and cross-file ones**,
which is the `ClassInfo` third map plus `__beamtalk_meta` entry. So B5's
metadata precedes B3, and B3 cannot ship for anything but a leaf class's own
slots without it.

**f. `clearField:` is a write.** A dedicated intrinsic mirroring
`generate_self_field_at_put_open`, lowered through `ThreadedIr` (§3).

**g. The shape store must see slot kind.** `beamtalk_shape_diff`'s `shape()`
is slot name → declared type name only (`beamtalk_shape_diff.erl:38-53`),
classifying `added | removed | retyped`. A flip between
`state: proc :: Subprocess` and `late state: proc :: Subprocess` changes
neither, so ADR 0123's "shape changed but `shapeVersion:` is still N"
warning would never fire and `beamtalk_recheck:trigger_shape/2` would not
re-run `spawnWith:`-dependent findings — which Part A's diagnostic is. The
shape value encodes kind, and B9 owns `beamtalk_shape_diff` and
`beamtalk_workspace_shape_store:capture/1`.

**h. `read_field/2` needs a class lookup per reflective read.** It takes
`(Name, State)` only (`beamtalk_reflection.erl:46-48`); keying on *declared*
late means resolving the class from the tagged map and consulting
`fieldKinds` metadata on every `fieldAt:`.

**i. The class side is a parallel set of small pieces, not a shared one.**
Inside a class method, `self.x` on a `classState:` slot compiles to a bare
`maps:get` on the `ClassVars` variable (`expressions.rs:526-537`), so it gets
the same `maps:find` guard as the instance branch, keyed on a
`late_class_var_names()` that B5's metadata must supply for inherited and
cross-file classes. `self hasField:` / `self clearField:` in a class method
compile to `maps:is_key` / `maps:remove` on `ClassVars`, with the remove
threaded through ADR 0110's class-var shadow write like any class-var
assignment. From outside, `Cls hasField:` / `Cls clearField:` and `Cls
fieldAt:` reach the class gen_server, which has `get_class_var` and
`set_class_var` handle_calls (`beamtalk_object_class.erl:1395`, `:1397`) and
needs `has_class_var`, `clear_class_var`, and a declared-`late` branch in
`get_class_var`. Direct Erlang writes such as bootstrap's
`set_class_variable/2` count as assignment, exactly as `spawnWith:` does for
instance slots (§6).

### 5. Value classes: `late field:` is rejected

A Value is fully constructed by `new`, `new:`, or its auto-generated keyword
constructor, and `self.slot :=` is a compile error (ADR 0042, ADR 0067).
"Assigned later" has no meaning for it, so `late field:` is an Error whose
message points at the two forms that express the intent:

```beamtalk
Value subclass: Config
  field: endpoint :: String = ""
  late field: client :: HttpClient
// error: 'late' is not allowed on a Value 'field:' — a Value is fully
//        constructed and never reassigned. Either make the field optional
//        (`field: client :: HttpClient | Nil = nil`) or hold the resource in
//        an Actor.
```

Every nilable `field:` in Exdura and Symphony (`workspace_error`,
`linear_error`, `issue`, `activity_outcome`, `retry_snapshot_entry`) is
optional data where `nil` is meaningful. The Value auto-constructor and `new:`
key validation are untouched: `compute_auto_slot_methods` returns `None` for
non-Value kinds (`value_accessors.rs:63`).

### 6. Definite-assignment analysis

A semantic-analysis pass in `beamtalk-core` reports at compile time what ADR
0078's check reports at spawn time.

**Which slots it covers** — the identical predicate the runtime uses, shared
(§7): annotated, no default, type does not admit `Nil`, **and not `late`**.

| Declaration | Requires assignment? |
|---|---|
| `state: count :: Integer` | Yes |
| `state: label :: String \| Nil` | No — `nil` is a valid value |
| `state: count :: Integer = 0` | No — has a default |
| `state: count` | No — untyped, defaults to `nil` |
| `late state: proc :: Subprocess` | No — declared as assigned later (§1) |

**Not gated on `typed`.** The runtime check is not, and two fixtures pin it
(`non_typed_uninitialized_state_actor.bt`, `non_typed_no_default_actor.bt`).
`typed` changes only whether the annotation is required (ADR 0025), never
what it means.

**What it analyses.** The ADR 0078 auto-chained `initialize` sequence,
parent-first over the flattened chain, branch-aware: a slot assigned in only
one arm of an `ifTrue:ifFalse:`, or inside a block that may not run, is not
definitely assigned.

**Across files, via a per-class summary.** `MethodInfo`
(`class_hierarchy/class_info.rs:20-51`) carries selector, arity, kind, types
and doc — **no body** — and ADR 0100 Rule 2's cross-file channel carries
signatures for resolving sends, not bodies. So a parent `initialize` in
another file cannot be walked. Each class therefore compiles the set of slots
its own `initialize` definitely assigns into `__beamtalk_meta/0` and
`ClassInfo`, next to `state_has_default`, and the chain walk composes those
summaries exactly as `inherited_typed_no_default_fields` already composes
declarations. Without the summary the Warning tier would fire only on
single-file hierarchies. This is the main cost of A2.

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

**Where it reports: the construction site, not the declaration.** A slot may
legitimately be supplied by `spawnWith:`, so a declaration-site warning would
have to be suppressed whenever some call site supplies the key — a diagnostic
whose presence depends on an unrelated call site in the same file. At the
construction site the evidence is complete:

```beamtalk
Counter spawnWith: #{}
//      ^ warning: 'Counter' declares 'count :: Integer' with no default, and
//        no 'initialize' in its chain assigns it. This spawn supplies no
//        'count', so it raises UninitializedStateError. Supply it here, give
//        the field a default, widen the type to 'Integer | Nil', or declare
//        it 'late'.
```

The literal `spawnWith:` map is already inspected in `beamtalk-core`:
`TypeChecker::spawn_with_map_pairs` and `check_spawn_with_map_keys`
(`type_checker/validation.rs:2343`, `:2356`, called from
`inference/send/receiver.rs:352`, `:410`) validate keys against declared
`state:` slots with a typo suggestion, are not gated on `typed`, and already
read `all_state` and `state_field_type`. Definite assignment is one more
predicate over that literal: *declared, no default, non-nilable, not `late`,
not in the composed `initialize` summary, and not a key of this map.* Two
gaps in the reuse: `spawn_with_map_pairs` answers `None` for any selector
but `spawnWith:`, so bare `Counter spawn` (the `uninitialized_state_actor.bt`
fixture case) needs its own call-site hook; and `check_spawn_with_map_keys`
bails when any chain link is missing or `all_state` is empty
(`validation.rs:2382-2396`), which are deliberate false-negative choices the
new predicate inherits. Non-literal construction (`Counter spawnWith:
someMap`) reports nothing; a declaration-site pass for that case is recorded
under Alternatives.

**Value classes have no runtime check at all today.** Values have no
`initialize` and no gen_server, and `generate_post_initialize_check` is called
only from `generate_handle_continue` (`gen_server/callbacks.rs:857`).
`StoredSnapshot new` on

```beamtalk
typed Value subclass: StoredSnapshot
  field: state :: ReplaySnapshot      // typed, no default
  field: eventId :: Integer = 0
```

silently yields `state = nil` (`state.rs:36`) behind a non-nilable declared
type. For Actors the static check is an early warning in front of a runtime
backstop; **for Values it is the only check.**

Values are closer to closed-world than Actors, which makes the check more
reliable:

| Assignment path | Actor | Value |
|---|---|---|
| `fieldAt:put:` | permitted | **blocked** — "Cannot modify slot on value type; use withSlot:" (`beamtalk_primitive.erl:988`) |
| hot-patched `initialize` (ADR 0082/0084) | yes | n/a — no `initialize` |
| `spawnWith:` | yes | n/a |
| construction | `spawn`/`spawnWith:` | `new`, `new:`, the auto-generated keyword constructor |

Construction sites on a Value:

| Site | Verdict |
|---|---|
| `Cls new` (bare) | supplies nothing, so every typed-no-default non-nilable field is unassigned — **only when `new` resolves to the auto-generated default.** `class new`/`new:` are overridable and overridden throughout the stdlib (`announcer.bt:48`, `set.bt:41`, `uuid.bt:33`, …), and `compute_auto_slot_methods` skips selectors the user defined (`value_accessors.rs:59-66`), so the check first proves no class method on `Cls` or an ancestor answers `new` |
| `Cls new: #{…}` (literal map) | check keys, exactly as for `spawnWith:` |
| `Cls field1: v1 field2: v2` (auto-generated keyword constructor) | **always satisfies** — it requires every field by construction (ADR 0042). No check needed |
| `Cls new: someMap` (non-literal) | no evidence, report nothing |

**The class-method factory is the canonical site.** Exdura's Values are
constructed through factories:

```beamtalk
typed Value subclass: WorkflowHandle
  field: workflowId :: String = ""
  field: client :: ExduraClient          // typed, no default

  class for: workflowId :: String client: aClient :: ExduraClient -> WorkflowHandle =>
    self new: #{#workflowId => workflowId, #client => aClient}
```

The literal map inside `for:client:` supplies `client`, so the class passes
with no diagnostic. Because the factory is the single construction site per
Value, the check fires once per class, and public callers never see it.

**The limit: a Value can arrive without being constructed.** Exdura's
`StoredSnapshot` is built by deserialization:

```beamtalk
  class fromBinary: content :: Binary -> StoredSnapshot =>
    result :: StoredSnapshot := Binary deserialize: content
    result
```

There is no construction site; the local annotation is an assertion at a
type-erasure boundary. This analysis reports nothing there, consistent with
ADR 0100's silence where knowledge is absent, and `late` is no escape (§5).
A class in this position should default the field or widen it to `| Nil`.

**Severity** (`DiagnosticCategory::DefiniteAssignment`, a new category so
suppression can target it):

| Situation at a construction site | Severity |
|---|---|
| Literal init map (or bare `spawn`/`new`), chain fully visible, slot unassigned and not supplied | **Warning** |
| Literal map supplies the slot | nothing |
| Chain incomplete — cross-package parent (rests on ADR 0100 WS3 cross-package metadata, still pending), `native:` ancestor (ADR 0056), or a `fieldAt:put:`/`perform:` writer in the class | **Hint** |
| Non-literal init map | nothing |
| Slot is `late`, defaulted, or nilable | nothing |

Warning, never Error by default: an Actor's slot may be written by
`spawnWith:` from another package, by `ClassBuilder` (ADR 0038), or by a
hot-patched `initialize`, and an unknown `spawnWith:` key is already a
Warning. On a Value the same severity holds for a narrower reason —
`ClassBuilder` and the deserialization boundary remain — but a Value's
Warning has no runtime backstop, so a Value-heavy codebase has more reason to
escalate this category to Error via ADR 0100 Rule 3's `[diagnostics]` table,
provided the bare-`new` row's override check above is in place, since an
Error with a known false-positive class and no backstop is worse than a
Warning. That asymmetry belongs in the docs.

**No `@expect` category is added for this diagnostic.** The exemption is
the language construct `late`: a slot that is legitimately unassigned after
`initialize` says so in its declaration, and the diagnostic reads it. The
three fixes the diagnostic offers — supply it at the construction site, give
it a default, or declare it `late` — are all constructs, and each changes
what the program means rather than which warning is shown. Per-project
severity is still ADR 0100 Rule 3's `[diagnostics]` table, which is
configuration, not source annotation.

**Supplying a `late` slot at `spawnWith:` counts as assigned.** `init/1`
merges with the caller winning — `maps:merge(DefaultState, InitArgs)`
(`gen_server/callbacks.rs:73`, `:90`, `:221`, `:277`) — so
`CodexClient spawnWith: #{#proc => aFakeSubprocess}` puts the key in the map
and the slot is present. This is legitimate dependency injection: it makes a
`late` slot testable by substitution, and Exdura already relies on the
mechanism (`exdura_client.bt`'s `class connect:` supplies both typed-no-default
slots via `spawnWith: #{#engine => eng, #eventStore => store}`). It needs a
clause in the `beamtalk-language-features.md:2426` key-checking rule.

**The diagnostic's fixes are three:** supply it at the construction site, give
it a default, or declare it `late`. **The runtime check is retained.**

### 7. Single source of truth for the predicate

The predicate lives in `beamtalk-codegen` (`callbacks.rs:619`, `:709`,
`:723`, `:806`), and **it is wrong there.** `is_nilable_type` matches only
`TypeAnnotation::Simple("Nil")` inside a `Union`, and `is_nilable_type_name`
string-splits on `" | "`. So it misses `UndefinedObject` (the canonical nil
class — `beamtalk-core` already has `WellKnownClass::is_nil_class`,
`type_checker/well_known.rs:95-105`, covering both spellings), misses ADR
0108 aliases (`type JsonValue = Nil | …`, `stdlib/src/json.bt:8`, so a
defaultless `state: v :: JsonValue` is reported non-nilable and ADR 0078
raises on a legitimately-nil value), and misses ADR 0102 intersection and
negation types. The `spawnWith:` value check already resolves aliases through
`AliasRegistry::resolve_alias_structural` for exactly this reason
(`validation.rs:2475`).

So the predicate is **reimplemented in `beamtalk-core`** on
`WellKnownClass::is_nil_class` plus `AliasRegistry`, and
`inherited_typed_no_default_fields` calls it; the codegen copy is deleted.
That is the shared-leaf-module pattern
(`docs/development/architecture-principles.md` §6), and the consistency test
between static and runtime check becomes a unit test of one implementation.
It also changes runtime behaviour for alias-typed and `UndefinedObject`-typed
slots — the post-`initialize` check stops raising on them — which needs its
own fixture.

The LSP cannot read the codegen copy either way: `just
check-codegen-boundary` (`Justfile:666`, part of `just ci`) asserts via
`cargo tree -i` that `beamtalk-lsp` and `beamtalk-lint` do not depend on
`beamtalk-codegen`. `beamtalk-core` already owns every input
(`ClassInfo.state_types`, `state_has_default`, `is_nil_class`), which is why
the answer is a reimplementation in core rather than a move of the existing
code.

One rule crosses the Rust/Erlang boundary: **the absent-key reconcile
table**, which `beamtalk-codegen` and
`beamtalk_shape_migration`/`beamtalk_hot_reload` must agree on. It gets a
shared conformance fixture — `(slot kind, key present?, has default?) ->
outcome` — driving both the Rust golden test and the Erlang EUnit test
(architecture-principles §7).

### 8. Interaction with ADR 0123 (migration, serialisation, distribution)

ADR 0123's reconcile step 3 gains the `late` case it deferred here:

| Declared slot | In migrated dictionary | Outcome |
|---|---|---|
| eager, has default | present | kept |
| eager, has default | absent | declared default |
| eager, no default | absent | `nil` (untyped) / failure (`typed`) — unchanged |
| **`late`** | **present** | **kept** |
| **`late`** | **absent** | **stays absent** |

**Where reconcile lives.** ADR 0123's runtime leaf has landed:
`beamtalk_shape_migration:migrate/3` (BT-3536, on `main` at `1e6780f`) owns
reconcile, and `beamtalk_hot_reload` delegates to it (`beamtalk_hot_reload.erl:210`);
the earlier `migrate_fields/2` no longer exists. `reconcile/5`
(`beamtalk_shape_migration.erl:147`) takes its keep set from
`beamtalk_behaviour_intrinsics:classAllFieldNamesByName/1`, its has-default
map from `classAllFieldHasDefaultByName/1`, and its default values from
`Module:init(#{'__skip_initialize__' => true})`; `reconcile_declared/6`
(`:164`) then walks the declared list: present → kept; absent with a default
→ that default; absent with no default → `nil` on an untyped class and
`{error, {typed_field_unset, Field}}` on a `typed` one.

So the two `late` rows above do **not** fall out for free. A `late` slot is
absent from the migrated dictionary, absent from `init/1`'s defaults (B2
excludes it from the literal) and has no default, so today's walk reaches the
no-default branch and fails the migration of every `typed` class with an
unassigned `late` slot. B9 adds the `late` case to `reconcile_declared/6`
before that branch — absent and declared `late` → stays absent — keyed on a
`classAllFieldKindsByName/1` intrinsic beside the two it already calls
(B5). The keep set already comes from `allFieldNames`, so an *assigned*
`late` value survives today; the invariant that **declared-but-absent `late`
slots stay in `allFieldNames`** still gets an explicit test, because the
`init/1`-derived defaults map is exactly where a future refactor could
mistakenly source the keep set from. The remaining ADR 0123 phases
(BT-3537 language surface, BT-3538 reload findings) touch the same modules,
so B9 is sequenced after them.

- `migrateFromVN:` hooks see a `Dictionary` in which an unassigned `late`
  slot is not a key. ADR 0123 already instructs hooks to read possibly-absent
  keys with `at:ifAbsent:` / `includesKey:`; no new hook contract.
- **Changing a slot between eager and `late` is a shape change** and bumps
  `shapeVersion:`.
- **Cross-node (BT-3527): an unassigned `late` slot travels as absent.** The
  receiving node's first read raises until something assigns it — a
  node-local handle that should not have been shipped fails loudly instead of
  answering `nil`. There is no initialiser to re-run on arrival.
- **Persistence keeps assigned values.** A `late` slot holding a node-local
  handle must not be persisted, and no mechanism exists for that today: ADR
  0103's `handleScope:` declarations are deferred (`:138`), none exist in
  `stdlib/src`, and `Ets` sits at tier `Unknown`. Open item for the
  persistence work.

### 9. Reflection, inspector, LSP

`late` slots have no initialiser, so nothing can force anything and every
surface can answer with no side effects.

| Surface | Behaviour on an unassigned `late` slot |
|---|---|
| `self.slot` (direct read) | raises `uninitialized_state_error` |
| `anActor hasField: #slot` (**new**) | `false` — never raises |
| `anActor clearField: #slot` (**new**) | returns the slot to unassigned |
| `anActor fieldAt: #slot` | raises, consistently with the direct read |
| `anActor fieldAt: #slot put: v` | assigns, as today |
| `Cls fieldNames`, `Cls allFieldNames` | unchanged — the declared schema, so `late` slots are listed (ADR 0035 `:73`) |
| `anActor fieldNames` | unchanged — the actual keys, so an unassigned `late` slot is not listed |
| `Cls fieldKinds` / `allFieldKinds` (**new**) | `Dictionary(Symbol, Symbol)`, `#eager \| #late` |
| Inspector (ADR 0095), `sys:get_state`, `observer`, `recon` | render it as unassigned; cannot perturb anything |
| LSP hover | shows the declaration as written, including `late` |

`fieldAt:` raises rather than answering `nil` so reflection agrees with the
direct read; `hasField:` is the non-raising question. So
`beamtalk_reflection:read_field/2` (`beamtalk_reflection.erl:47`, currently
`maps:get(Name, State, nil)`) needs a declared-`late` branch — keyed on
*declared late*, not on mere absence, because a typo'd field name is also
missing — which costs a class-metadata lookup per reflective read (§4h). No
state threading in the `'fieldAt:'` dispatch arm
(`beamtalk_object_ops.erl:108`).

**`ClassBuilder` gets `late` too, and the gap it closes is older than
`late`.** ADR 0038's `fields:` takes `#{name => default}`
(`stdlib/src/class_builder.bt:69`) — a name and a default value, **no type**
— so a dynamically built class cannot declare a typed slot at all today.
That means neither Part A's diagnostic nor `late` can reach it, and the
reason is not slot kind but the missing type channel. A dynamic class also
has no `__beamtalk_meta/0`: `classFieldNames/1` falls back to a
`gen_server:call` and `classClassVarNames/1` answers `[]`
(`beamtalk_behaviour_intrinsics.erl:436-460`).

The parity fix is B11: `ClassBuilder`'s field spec grows from a bare default
to a structure carrying type, default and kind, with a `lateFields:` setter
beside `fields:` (named as `classVars:` is, since `late` is a declaration
keyword and cannot be a selector); the class gen_server stores the kinds so
`fieldKinds`, the read guard's `late` set and `read_field/2`'s
declared-`late` branch answer for a dynamic class through the same
`gen_server:call` fallback `classFieldNames/1` already uses. Until B11
lands, `fieldKinds` answers `#eager` for every field of a `ClassBuilder`
class and `read_field/2` takes the plain `maps:get(…, nil)` path — a
documented interim, not the decision.

B11 is runtime and reflection parity only. Part A is a compile-time
analysis over AST declarations, `ClassInfo`/`__beamtalk_meta` and literal
construction maps at a statically resolved call site (§6); a class assembled
at runtime through `Object classBuilder … register` has none of those, so
its slots are never definite-assignment checked, however much typed metadata
the class gen_server holds. That is the same limit as a Value built by
deserialization (§6), and it is documented rather than worked around.

The instance-vs-class `fieldNames` split is ADR 0035's design (`:73`), so
`late` slots make an existing distinction visible. Documented so that
`c fieldNames` omitting `proc` while `c hasField: #proc` answers `false` reads
as consistent.

`printString`/`displayString` omit an unassigned slot:
`beamtalk_object_printer:structural_from_state/1` (`:113`) renders present
keys only. That is a **REPL-visible output change**, which under `CLAUDE.md`
needs explicit confirmation before implementation. `InspectorField` gains
`#lateSlot` to its `kind` vocabulary (alongside
`#slot #element #association #processInfo`) with `value: #notAssigned` —
matching `InspectorField name: #status value: #unavailable`
(`stdlib/src/inspector.bt:96`) — and `drillable: false` until assigned.
`#lateSlot` also reaches the cross-surface wire form (`inspector.bt:262`) and
`beamtalk_inspector:fieldsOf/1`, which reads only the state map and needs the
`fieldKinds` metadata to tell "declared late and unassigned" from "not a
field".

`fieldKinds`/`allFieldKinds`, not `slotKinds`: ADR 0035 (`:230`) explicitly
rejected "slot" as reflection vocabulary. The pair mirrors
`fieldNames`/`allFieldNames` because the hot-reload keep set and §6's
predicate both need the flattened answer.

**Cost.** `ClassInfo` (`semantic_analysis/class_hierarchy/class_info.rs:158`)
carries `state_types` and `state_has_default` as separate maps, so slot kind
needs a third, plus a matching `__beamtalk_meta/0` schema entry (ADR 0050)
for cross-file ancestors — and the read guard depends on it (§4e). Plus a
`behaviour.bt` declaration and an Erlang intrinsic, since `fieldNames` and
`allFieldNames` are `@primitive "classFieldNames"` and
`@primitive "classAllFieldNames"` (`behaviour.bt:225`, `:233`), a **sealed**
family `fieldKinds` joins. And `class_variables` (`:171`) is `Vec<EcoString>`
— names only, no types, no defaults — so `late classState:` on an inherited
or cross-file class has no metadata to hang the kind off; that field grows to
a structure carrying type, default and kind.

## Prior Art

| Language | Mechanism | Adopted / rejected |
|---|---|---|
| **Kotlin** | `lateinit var` — a non-null property assigned after construction, throwing `UninitializedPropertyAccessException` on early read; separately `by lazy { }` | **The model for §1.** Adopted wholesale: non-null declared type, deferred assignment, defined error on early read. Two additions: `hasField:` as a first-class presence test (Kotlin's `::x.isInitialized` is reflection-flavoured), and `clearField:` to un-initialise, which Symphony's relaunch requires. `by lazy` rejected (§Alternatives) |
| **Swift** | Two-phase definite initialisation proves every stored property is set before `self` escapes; `lazy var`, which cannot be `let` and needs a `mutating` getter on a struct | The model for §6. Swift makes it an *error* because it has no reflective writers and no hot reload; Beamtalk cannot (ADR 0100). Its `lazy var`-on-a-struct wall parallels §5's rejection of `late field:` |
| **C#** | `required` members (C# 11) — every constructor path or object initialiser must set them; nullable reference types with `!` | `required` is Part A's construction-site check by another name and validates reporting at the construction site. `!`-style per-read suppression rejected; `late` on the declaration is the Beamtalk form, a construct rather than an assertion |
| **Newspeak** | Slots accessed exclusively via messages; `lazy s = expr.` computes the initialiser on first getter run | Rejected: no memoise-on-read demand (§Context (c)), and its `nil`-based storage recomputes forever when the value is `nil`. Newspeak has no `late` analogue |
| **Pharo** | `Slot` metaobjects — `LazySlot`, `InitializedSlot`, `ComputedSlot` | **Adopted the vocabulary, not the mechanism.** `fieldKinds` answering `#eager`/`#late` is the reflectable-slot-kind idea; user-definable metaobjects rejected below |
| **Squeak/Pharo `ClassBuilder`** | Match instance variables by name on recompile, default the rest | Already Beamtalk's hot-reload behaviour (ADR 0123); this ADR adds two `late` rows (§8) |
| **Scala** | `lazy val` via a double-checked-locking bitmap | Relevant only to the rejected memoising design; a gen_server serialises by construction, so the BEAM would not have needed the bitmap |
| **Erlang/Elixir** | No late or lazy fields. A record field is `undefined` until set; idioms are `maps:get/3` with a default, `Map.get_lazy/3`, or an `Agent` | Nothing to be compatible with. Erlang records defaulting to `undefined` is the overloaded-sentinel problem §2 avoids by using key absence |
| **TypeScript** | `strictPropertyInitialization` plus definite-assignment assertions (`x!: T`) | The closest analogue to the whole ADR. `late` is `!:` with a defined runtime error instead of undefined behaviour |

**Why not Pharo-style first-class `Slot` metaobjects:** Beamtalk compiles
ahead of time to Core Erlang. A user-defined slot with a `read:`/`write:to:`
hook would route every slot read through a metaobject, costing every eager
read and defeating §3's static guard. Beamtalk takes the vocabulary without
the protocol; `fieldKinds` is the extension point if user-defined kinds are
ever wanted.

## User Impact

**Newcomer (from Python/JS/Ruby).** `late` is Kotlin's `lateinit` and
TypeScript's `!:` in the expected position, and the early-read error names
the slot, its type, and the class instead of today's `does_not_understand` on
`nil` several frames away. Part A's compile-time warning replaces a `spawn`
crash that is hard to connect to the forgotten line. The one refusal they
will hit is `late field:` on a Value, whose error spells out both working
alternatives.

**Smalltalk developer.** Instance variables are always `nil` until assigned
and every accessor guards; `late` declares that the guard is temporary and
the type is not really nilable. The departure from Pharo is that slot kinds
are syntax, not metaobjects — a real loss of extensibility, argued above.
Part A makes "forgot to assign in `initialize`" visible before the image
runs.

**Erlang/Elixir developer.** The state map stays an ordinary map: a `late`
slot is a key not yet present, testable with `maps:is_key/2`, shown honestly
by `sys:get_state` with no sentinel atom. Reads do not mutate, so nothing is
process-sensitive.

**Production operator.** Opening an inspector, attaching `observer`, or
calling `sys:get_state` cannot execute user code — there is no initialiser.
A read-before-assign fails loudly at the read, naming the slot. The cost: an
actor that spawned cleanly can raise on a later message because a lifecycle
call was never made; `fieldKinds` makes those slots introspectable. A `late`
read inside `terminate:` fails quietly (the dispatch is `try`-wrapped), which
is why §4d warns on it.

**Tooling developer.** `late` is one field on `StateDeclaration`, shared by
`class_variables` (`ast/class.rs:174`), so hover, completion and the outline
get it with no new AST shape. The construction-site check extends a
literal-map inspection that already exists. The plumbing cost is
`fieldKinds`' metadata (§9). The rule: reads raise, `hasField:` does not.

## Steelman Analysis

### `lazy` — memoise on first read (rejected)

- 🧑‍💻 **Newcomer**: "`lazy` is the word I know from Kotlin, Swift and Scala,
  and I declare the value once at the slot instead of writing a `launch`
  method and remembering to call it."
- 🎩 **Smalltalk purist**: "`^x ifNil: [x := self compute]` is in every
  Pharo image, and it is Newspeak's actual spelling. `late` is a Kotlin
  import; `lazy` has ancestry in this language family."
- ⚙️ **BEAM veteran**: "On the BEAM lazy is free where it is expensive
  elsewhere: the gen_server serialises by construction, so no double-checked
  locking, no `LazyThreadSafetyMode`. `late` leaves that on the table."
- 🏭 **Operator**: "Deferring acquisition off the spawn path means a slow
  dependency does not block startup, and a restart recomputes derived state."
- 🎨 **Language designer**: "Strictly more expressive. `late` is lazy plus a
  manual assignment; compute-on-first-read has no encoding in `late`."

The last point is true and is the real cost of this decision. `lazy` loses on
evidence and on price. Evidence: zero memoise-on-first-read sites across 148
`.bt` files (§Context (c)), while every lifecycle-assigned slot wants exactly
what `late` provides. Price: a lazy read *writes*, so it needs an ADR 0118
prelude, which drags in the `ClassVars` mutation detector that does not see
reads (`analysis.rs:253`), ADR 0110's shadow write, the loop optimiser's
field hoisting, and a `terminate:` path that would acquire a subprocess in
order to close it. It also cannot serve its own best candidate: Symphony's
`proc` must return to unassigned so `launch` can re-run, and `launch` answers
`Result(Nil, CodexError)`, which a slot initialiser has no way to hand back.
On the ancestry argument: the corpus writes `ifNil:` guards for
temporarily-unset resources, not for memoisation, and `late` is what that
guard is trying to say. `lazy` stays addable later, orthogonally — `#lazy`
would join `#eager`/`#late` in `fieldKinds` without a schema change.

### A `computed` slot kind — recompute on every read (rejected)

- 🧑‍💻 **Newcomer**: "Derived values are the common case; a declaration
  reads better than a method."
- ⚙️ **BEAM veteran**: "Exdura's `timer_manager` and `currentEngine` resolve
  by name on every read deliberately, so a `rest_for_one` restart is picked
  up. Give that real, repeated pattern a declaration."
- 🎨 **Language designer**: "It completes the set: eager, late, computed."

Genuine corpus support (§Context (d)). It loses because in Beamtalk that
declaration already exists and is called a method: `currentEngine` and
`currentEventStore` compose (parameters, overriding, protocol conformance)
and are what their authors reached for unprompted. A `computed` kind adds a
reconcile row, an inspector kind and a `fieldKinds` value for no capability.

### Definite assignment as an Error rather than a Warning (rejected)

- 🧑‍💻 **Newcomer**: "If the compiler knows my spawn will crash, stop me."
- ⚙️ **BEAM veteran**: "'Let it crash' means crash early. A spawn that
  cannot succeed is a build that should not pass."
- 🏭 **Operator**: "An Error in CI is the cheapest place to catch it."
- 🎨 **Language designer**: "Swift and C# both make this an error."

"The compiler knows" is false in Beamtalk: `fieldAt:put:`, `perform:`,
`ClassBuilder` (ADR 0038) and a hot-patched `initialize` (ADR 0082/0084) all
assign invisibly, and Swift and C# have none of those. ADR 0100 sets severity
from certainty. The CI case gets what it needs via `[diagnostics]` escalation
per project (ADR 0100 Rule 3).

### `lateState:` / `lateField:` as distinct keywords (rejected)

- 🎩 **Smalltalk purist**: "Declaration forms *are* keyword messages. A prefix
  modifier is not a message send; `lateState:` is — and Beamtalk chose
  `classState:` over `class state:` for that reason."
- ⚙️ **BEAM veteran**: "The parser stays a flat keyword match
  (`is_state_or_field_keyword`, `declarations.rs:59`) instead of a two-token
  lookahead."
- 🎨 **Language designer**: "Illegal combinations become unrepresentable."

The `classState:` precedent is strong and this is the closest call in the
ADR. It loses on multiplication: `late` is orthogonal to which slot keyword,
so keywords grow as the product (`lateState:`, `lateClassState:`, a
`lateField:` existing only to be rejected), and a second modifier doubles it
again. The existing `sealed`/`abstract`/`typed` prefix position and the
modifier loop at `declarations.rs:192-208` settle it.

### `nil` as the unassigned sentinel (rejected)

- 🧑‍💻 **Newcomer**: "`nil` means no value. Nothing new to learn."
- ⚙️ **BEAM veteran**: "Every key always present, one fixed shape,
  `maps:get/2` never fails, two `sys:get_state` snapshots stay comparable."
- 🎨 **Language designer**: "Zero new vocabulary — no `maps:find`, no third
  reconcile row, no `hasField:`, no `#lateSlot`."

The fixed-shape argument is real and this ADR takes the cost. It loses on
correctness: `nil` is already the compiler's representation for "no value
supplied" (`state.rs:36`; ADR 0078's check), so a third meaning cannot be
distinguished from either.

### Tension points

- **Expressiveness vs. evidence.** `lazy` is strictly more expressive;
  nothing in the corpus needs it, and it costs the whole state-threading
  lowering. The decision most worth revisiting if a memoisation case appears.
- **Smalltalk ancestry vs. the actual need.** The `ifNil:` memo idiom is
  ancestral; the guards in this corpus are not memos.
- **Fixed state-map shape vs. an unforgeable "unassigned".** Correctness
  wins; the cost is `maps:find`, two reconcile rows and one inspector kind.
- **Newcomer/operator want Part A as an Error**; ADR 0100 and the reflective
  writers say Warning. Resolved by per-project escalation.

## Alternatives Considered

### `lazy` — memoise on first read
Rejected on evidence (zero memoise-on-read sites in 148 `.bt` files), on
price (a read that writes needs an ADR 0118 prelude, the `ClassVars`
detector, ADR 0110's shadow write, and the loop optimiser's hoisting), and on
fitness (Symphony's `proc` must return to unassigned, and its `launch` answers
a `Result` a slot initialiser cannot hand back). Steelmanned above. Addable
later and orthogonal.

### A `computed` slot kind — recompute on every read
Real corpus support (§Context (d)); loses because that declaration is spelled
"a method", which is what those authors wrote. Steelmanned above.

### `lateState:` / `lateField:` as distinct declaration keywords
Keywords would grow as the product of modifier × slot keyword, and a
`lateField:` token would exist only to be rejected. Steelmanned above.

### `nil` as the unassigned sentinel
`nil` already means "no value supplied" in two places (`state.rs:36`, ADR
0078's check), so a third meaning is indistinguishable from both. Steelmanned
above.

### A reserved `'__unset__'` sentinel value instead of key absence
Keeps every key present while avoiding the `nil` collision. Strictly worse
than absence: it still needs a guard on every read, it leaks an internal atom
into `sys:get_state` and any Erlang code reading the map, it must be stripped
at serialisation and node boundaries, and it can be *written* by
`fieldAt:put:`. Key absence cannot be forged.

### `self.slot := nil` as the un-assign form
On a non-nilable `late` slot `nil` cannot be legitimate, so there is no
ambiguity, and Symphony's `stopProcess` would need no edit. Rejected because
the ADR's rule is that `nil` stops carrying structural meaning; `clearField:`
is explicit, and assigning `nil` to a non-nilable slot stays the type error
it is, with a message naming the fix.

### Definite assignment as an Error rather than a Warning
Rejected for Actors (`spawnWith:`, `fieldAt:put:`, `perform:`,
`ClassBuilder`, hot-patched `initialize` all assign invisibly) and, more
narrowly, for Values (`ClassBuilder` and the deserialization boundary
remain). Available per-project via ADR 0100 Rule 3. Steelmanned above.

### Declaration-site definite-assignment dataflow
Report on the declaration after a branch-aware walk of the `initialize`
chain, suppressed when some visible `spawnWith:` supplies the key. Rejected
as the primary design: a finding whose presence depends on an unrelated call
site in the same file is non-local and brittle, and one `spawnWith:` anywhere
silences every warning for the class. Exdura and Symphony construct through
class-method factories (`ExduraClient connect:`,
`CodexClient start:config:onEvent:`, `WorkflowHandle for:client:`), each
holding one literal construction map, so the construction-site check fires
once per class at the canonical site. Worth adding later for non-literal
construction.

### Whole-program definite assignment across construction sites
Warn only when neither the `initialize` chain nor any construction site in
the project supplies the slot. Rejected as disproportionate: a whole-program
pass gated on project-complete `KnowledgeScope` (ADR 0100 Rule 2), when the
factory pattern means the cheap version already covers the common case.

### `@expect uninitialized_state` on the declaration as the exemption
The smallest design that serves Part A. `StateDeclaration.expect` exists
(`ast/class.rs:520`) and `parse_pending_declaration_expect` already runs
before member dispatch (`declarations.rs:499`), so
`state: proc :: Subprocess @expect uninitialized_state` gives the
non-nilable declared type and exemption from ADR 0078's check for one clause
in `inherited_typed_no_default_fields`. No keyword, no `maps:find`, no
reconcile rows, no `fieldKinds`, no inspector kind, no printer change, no
`hasField:`/`clearField:`. Rejected on two grounds. First, it is an
annotation where a construct is available: `@expect` names a diagnostic to
silence, while `late` states a fact about the slot that the compiler,
runtime, reflection and inspector all act on. This ADR adopts the position
(§Status) that a construct is preferred wherever one can express the fact,
following the precedent that every existing class and slot modifier is a
keyword. Second, it is unsound in
a way `late` is not: reading too early answers `nil` and fails as a
`does_not_understand` somewhere else, `nil` keeps carrying structural
meaning, and the declared type becomes a claim the checker believes with
nothing behind it — the §3 nil-injection problem with no read guard at all.

### A `Cell(T)` library value
`state: proc :: Cell(Subprocess) = Cell empty`, with `value` raising when
empty and `clear` returning it to empty. Zero compiler change, no metadata,
no reconcile rows, works on Values, and the loud raise at the read comes for
free. Rejected for Part B because every read becomes `self.proc value`, the
wrapper is visible to every caller and in every `sys:get_state`, and it does
not compose with ADR 0107 narrowing on the slot itself. It is the honest
no-new-syntax baseline for Part B and would be the right answer if the corpus
grew many such slots in Values.

### Guard every typed-no-default read instead of adding a declaration
Move ADR 0078's check from spawn time to first read for *all* typed-no-default
slots, so `state: proc :: Subprocess` with no default already means "raises
if read before assignment". Delivers §3's diagnostic with no declaration
surface. Rejected because it removes fail-fast at spawn for every existing
class — the Actor case Part A exists to catch earlier, not later — and
because it cannot express "this one is legitimately unset" separately from
"this one is a bug".

### A lifecycle precondition / typestate check
What the corpus wants is "`launch` must run before `sendLine:`"; `late` is
its one-bit degenerate case and §4d's `terminate:` warning a hand-rolled
instance. Out of scope: a general typestate system is a separate ADR, and
`late` does not preclude it.

### A library answer — a `Dictionary` cache slot
`state: cache :: Dictionary = #{}` plus an `at:ifAbsentPut:`-style memo gets
caching with zero language surface. Not an alternative to `late` (it does not
address a slot temporarily unset behind a non-nilable type), but it is the
no-new-syntax baseline any future `lazy` proposal must beat.

### Do nothing
Widen the type to `| Nil`, guard each read, and accept that
`UninitializedStateError` catches the Actor case at spawn.

**Rejected for Part A.** The signal is additive over machinery that already
exists (the ADR 0078 chain walk and the literal-map inspection at
`beamtalk-language-features.md:2426`), and for Values there is no runtime
check at all, so declining leaves a non-nilable declared type with nothing
behind it.

**Rejected for Part B, but a closer contest.** For doing nothing:
`state: x :: T = <expr>` already allows an eager initialiser, and §Context (d)
shows some nilable slots must never change. Against: the `| Nil` widening is
not a statement about the data, it is an escape from the post-`initialize`
check, and it permanently defeats ADR 0107 nil-narrowing for every downstream
reader of `self.proc`. The deciding factor is that `late` costs one guarded
read and two small reflective selectors, not a lowering.

## Consequences

### Positive
- A non-nilable declared type stops being a lie for lifecycle-assigned slots.
  `state: proc :: Subprocess | Nil = nil` becomes
  `late state: proc :: Subprocess`, and ADR 0107 narrowing works downstream.
- Reading too early raises `uninitialized_state_error` naming the slot, its
  type and the class, instead of answering `nil` and failing later as a
  `does_not_understand`.
- **Reading never writes**, so a `late` read needs no ADR 0118 prelude, no
  `ClassVars` detector change, no ADR 0110 shadow write, and no `terminate:`
  redesign; `generate_field_access` stays pure. `clearField:` is the one
  write and lowers like `fieldAt:put:` (§4f).
- **Observation cannot execute user code** — no initialiser, so the
  inspector, `sys:get_state`, `observer` and `recon` are side-effect-free by
  construction (§9).
- A `late` slot has exactly two states, absent and assigned, and its type is
  always non-nilable. This does not retire Exdura's `supervised` flag, which
  distinguishes two meanings of absence (§Context (b)).
- Part A gives Values a definite-assignment check where none exists —
  `generate_post_initialize_check` is Actor-only (`callbacks.rs:857`).
- Part A reports once per class at the canonical factory construction site,
  reusing the literal-map inspection that exists
  (`beamtalk-language-features.md:2426`).
- The predicate moves from `beamtalk-codegen` to `beamtalk-core`, where
  `just check-codegen-boundary` (`Justfile:666`) requires it to be for the
  LSP to surface the diagnostic (§7).
- ADR 0123's deferred "absent → declared initialiser policy" closes with two
  reconcile rows (§8).
- The auto-generated Value keyword constructor always satisfies the check, so
  no Value construction path changes (`value_accessors.rs:63`).

### Negative
- **`late` cannot express deferred computation at all.** Nothing in the
  corpus wants it, but this is a real expressiveness loss versus `lazy` and
  the decision most worth revisiting.
- **A nilable slot is not automatically a `late` candidate.** Exdura's
  resolve-by-name slots must stay as they are or supervisor-restart recovery
  breaks (§Context (d)). Only documentation guards this.
- **An actor that spawned cleanly can raise on a later message** because a
  lifecycle call was never made — the failure moves from a silent `nil` to a
  loud raise.
- **A `late` read in `terminate:` fails quietly.** The dispatch is
  `try`-wrapped (`callbacks.rs:1560`). §4d's warning is the only guard.
- **Two new reflective selectors and one required source change.**
  `hasField:` and `clearField:` do not exist, and every `self.slot isNil`
  guard on a converted slot becomes `hasField:`.
- **`printString` omits an unassigned slot** and instance `fieldNames` does
  not list it (§9). The former is a REPL-visible output change, gated on
  explicit confirmation under `CLAUDE.md`.
- **The state map no longer has one fixed key set** over an actor's lifetime.
  ADR 0123's reconcile, the shape store (§4g), the printer and the inspector
  each need the `late` case. `changed_state_keys/2` does not: its
  `'__absent__'` default already registers absent→assigned and
  assigned→cleared as changes.
- **`fieldKinds` needs a `ClassInfo` third map plus a `__beamtalk_meta`
  entry**, and the read guard cannot ship before it (§4e). Part B's largest
  single cost.
- **`ClassBuilder` needs a typed field spec before it can carry `late`.**
  `fields:` is `#{name => default}` with no type today, so B11 is a protocol
  change to ADR 0038's builder, not just a new setter (§9). Until it lands,
  a dynamic class has no `late` slots. It never gains the compile-time
  definite-assignment check, with or without B11 (§9).
- **The shared predicate changes runtime behaviour** for alias-typed and
  `UndefinedObject`-typed slots once it is reimplemented correctly (§7).
- **Reads of a `late` slot cost `maps:find` rather than `maps:get`**, and
  ~15 sibling state `maps:get` sites need auditing (§4a).
- **Values constructed by deserialization cannot be checked** (§6), and
  `late` is not an escape hatch because §5 forbids it on a Value.
- **Class kinds diverge**: `late` works on `state:` and `classState:` but not
  `field:` (§5). Defensible, still a wart to explain.
- **The class side is a second implementation of each piece** — guard,
  presence, clear, reflective branch (§4i) — because class variables live in
  the class gen_server, not the object state map.
- Five new diagnostics — four Errors (`late field:`, `late` without a type,
  `late` on a nilable type, `late` with a default) and one advisory (definite
  assignment), plus the §4d `terminate:` warning — all kept at parity across
  CLI, REPL, LSP and MCP.

### Neutral
- `late` is a contextual keyword in declaration position only. It is unused
  as a word in the stdlib's `.bt` sources, so `late := 1` and a `late`
  selector keep working.
- `nil` semantics are unchanged.
- No `.bt` source changes are required in the stdlib. Every existing
  declaration keeps its meaning, and the definite-assignment finding is
  advisory. The three singleton conversions in §Migration Path are optional
  follow-ups, each slot plus getter.
- `beamtalk_actor:init/1` validates only internal keys
  (`beamtalk_actor.erl:1582`), so an absent `late` slot needs no change
  there. Its "Actor started" log line reports `state_keys`, which will omit
  unassigned `late` slots.

## Implementation

Part A can be built first — A1–A5 do not depend on B1–B10 — but it is not
complete without B1: until `late` parses, a lifecycle-assigned slot has no
construct with which to opt out of the diagnostic. **The minimum shippable
set is A1–A5 plus B1, B2, B5 and B3.** B1–B2 alone are not releasable: with
`late` parsed and excluded from the post-`initialize` check but the read
still an unguarded 2-arity `maps:get`, an early read of an unassigned `late`
slot crashes with a raw `{badkey, Slot}` rather than the
`UninitializedStateError` §3 specifies — a worse failure than the status quo
`nil`. B3 needs B5's metadata, so both ship in the same release as B1–B2.

### Part A — definite assignment

| # | Work | Components | Size |
|---|---|---|---|
| A1 | Reimplement the predicate (annotated ∧ no-default ∧ non-nilable) in `beamtalk-core` on `WellKnownClass::is_nil_class` + `AliasRegistry`; `inherited_typed_no_default_fields` calls it; delete the codegen copy; fixture for the alias-typed / `UndefinedObject`-typed slots whose runtime behaviour changes (§7) | `beamtalk-core` (`semantic_analysis`), `beamtalk-codegen` (`gen_server/callbacks.rs`), `stdlib/test/fixtures` | **M** |
| A2 | Actor construction-site check: extend `check_spawn_with_map_keys` (`validation.rs:2356`) with the unassigned-slot predicate; a call-site hook for bare `spawn`; the per-class "`initialize` definitely assigns" summary in `__beamtalk_meta`/`ClassInfo` and its composition across the ADR 0078 flattened chain, branch-aware (§6); bare-`new`-resolves-to-default check shared with A3 | `beamtalk-core` (`semantic_analysis`), `beamtalk-codegen` (`__beamtalk_meta` emission) | **L** |
| A3 | Value construction-site check: `new` (bare), `new:` with a literal map, and the auto-generated keyword constructor as always-satisfying (§6). No `initialize` chain to walk | `beamtalk-core` (`semantic_analysis`) | **S** |
| A4 | `DiagnosticCategory::DefiniteAssignment`; `[diagnostics]` escalation; parity across build/LSP/REPL/MCP (ADR 0100 Rule 3, `surface-parity.md`). No `@expect` category — `late` is the exemption (§6) | `beamtalk-core`, `beamtalk-language-service`, `beamtalk-cli` | **S** |
| A5 | Docs + tests: the Actor-vs-Value asymmetry (no runtime backstop on Values) and the deserialization limit; `uninitialized_state_actor.bt` / `non_typed_uninitialized_state_actor.bt` gain compile-time expectations; a Value fixture for the `new`-with-typed-no-default case | docs, `crates/**/tests`, `stdlib/test` | **S** |

A1 is unblocked and independent. A3 is worth doing early, because Values
have no runtime check to fall back on.

### Part B — `late` slots

| # | Work | Components | Size |
|---|---|---|---|
| B1 | `late` modifier: contextual keyword, two-token lookahead extending the single-token dispatch at `declarations.rs:553`, following the shape of the class-header loop at `:191-209`; `SlotKind` on `StateDeclaration`; unparse round-trip. The Errors: `late field:`, `late` without a type annotation, `late` on a nilable type, `late` with a default, `late state:` on `native:`/`Object` | `beamtalk-core` (`source_analysis/parser/declarations.rs`, `ast/class.rs`, `unparse`) | **S** |
| B2 | Exclude `late` slots from `generate_post_initialize_check` (its 2-arity `maps:get` would badkey) and from `init/1`'s state literal | `beamtalk-codegen` (`gen_server/callbacks.rs`, `gen_server/state.rs`) | **S** |
| B3 | Guarded read: `maps:find` + the `'nil'` and absence arms in both branches of `generate_field_access` (`expressions.rs:519` instance, `:526` class-method on `ClassVars`), keeping it pure, reading the `late` sets from B5's metadata; audit the ~15 sibling state `maps:get` sites; disable DirectParams/Hybrid field hoisting for `late` slots (`control_flow/loop_mode.rs:118`). **After B5** | `beamtalk-codegen` (`expressions.rs`, `control_flow/`) | **M** |
| B4 | `hasField:` / `clearField:` on `Object` plus runtime intrinsics; `clearField:` codegen intrinsic mirroring `generate_self_field_at_put_open` through `ThreadedIr`; `hasField:` in the Value reflection arm list (`value_type_codegen.rs:4753`); `read_field/2`'s declared-`late` branch with its per-read metadata lookup; class side (§4i): `self hasField:`/`clearField:` in class methods on `ClassVars` through the ADR 0110 shadow write, and `has_class_var`/`clear_class_var` handle_calls plus a declared-`late` branch in `get_class_var` | `beamtalk-stdlib`, `beamtalk-codegen` (`dispatch_codegen.rs`, `value_type_codegen.rs`), `beamtalk_runtime` (`beamtalk_reflection.erl`, `beamtalk_object_ops.erl`, `beamtalk_object_class.erl`) | **M** |
| B5 | `fieldKinds`/`allFieldKinds`: the `ClassInfo` third map, the `__beamtalk_meta` schema entry, `class_variables` growing from `Vec<EcoString>` to a structure with type, default and kind, the `behaviour.bt` declaration and Erlang intrinsic; LSP hover. **Before B3** | `beamtalk-core`, `beamtalk-codegen`, `beamtalk_runtime`, `beamtalk-stdlib`, `beamtalk-language-service` | **M–L** |
| B6 | The §4d `terminate:`/`handle_info` unguarded-read warning | `beamtalk-core` (`semantic_analysis`) | **S** |
| B7 | Inspector: `InspectorField` `#lateSlot` / `value: #notAssigned` / `drillable: false`, the cross-surface wire form (`inspector.bt:262`) and `beamtalk_inspector:fieldsOf/1` | `beamtalk-stdlib`, `beamtalk_runtime` | **S** |
| B8 | REPL-visible output: decide and confirm `printString` rendering for an unassigned slot — **gated on explicit user confirmation** per `CLAUDE.md` | `beamtalk_runtime` (`beamtalk_object_printer.erl`), `tests/repl-protocol` | **S**, gated |
| B9 | The `late` case in `beamtalk_shape_migration:reconcile_declared/6` (absent and declared `late` → stays absent, before the typed-no-default failure) on a `classAllFieldKindsByName/1` intrinsic; the shared `(slot kind, present?, has default?) -> outcome` conformance fixture; the `allFieldNames`-keep-set invariant test; slot kind in the shape store so an eager↔`late` flip is a `shape_change` that triggers recheck (§4g). Sequenced after BT-3537/BT-3538 (§8) | `beamtalk_runtime` (`beamtalk_shape_migration.erl`, `beamtalk_behaviour_intrinsics.erl`), `beamtalk_workspace` (`beamtalk_shape_diff.erl`, `beamtalk_workspace_shape_store.erl`), `beamtalk-codegen` | **M** |
| B10 | Docs + tests: `beamtalk-language-features.md` leading with **"a nilable slot is not automatically a `late` candidate"** and the resolve-by-name counter-example (§Context (d)), then slot kinds, the read/raise rule, `hasField:`/`clearField:`, the `spawnWith:`-injection clause at `:2426`; `surface-parity.md`; BUnit tests; REPL-protocol e2e | docs, `stdlib/test`, `tests/repl-protocol` | **S** |
| B11 | `ClassBuilder` parity (§9): field spec grows from `#{name => default}` to type + default + kind; `lateFields:` beside `fields:`; the class gen_server stores kinds; `fieldKinds`, the read guard's `late` set and `read_field/2` resolve them for a dynamic class via the existing `gen_server:call` fallback. Runtime and reflection parity only: a dynamic class has no AST declaration and no statically resolvable construction site, so Part A's compile-time check stays out of reach for it (§9) | `beamtalk-stdlib` (`class_builder.bt`), `beamtalk_runtime` (`beamtalk_object_class.erl`, `beamtalk_behaviour_intrinsics.erl`, `beamtalk_reflection.erl`), `beamtalk-core` (`generated_builtins.rs`) | **M** |

**Test placement** (per `CLAUDE.md`): `late` read/raise behaviour (absent
and injected-`nil`), `hasField:`/`clearField:`, re-assignment after clearing,
and `fieldKinds` go in `stdlib/test/*.bt` as BUnit `TestCase`s. Diagnostic
text and severity are Rust unit tests plus LSP diagnostic-provider tests.
`late classState:` on a REPL-defined class, slot kinds surviving a class
reload, and the eager↔`late` shape-change recheck go in
`tests/repl-protocol/cases/*.btscript`.

**Recommended start:** A1, then B1, B2, B5, B3 as one unit (the exemption
and its guard land together), then A3, then A2. B4 and the rest of Part B
follow.

## Migration Path

No migration is required. `late` is additive, every existing declaration
keeps its meaning and representation, and the definite-assignment finding is
advisory (Warning/Hint, never Error by default).

**The stdlib's three singletons convert, with the accessor kept nilable.**
`transcript_stream.bt`, `beamtalk_interface.bt` and `workspace_interface.bt`
become `late classState: current :: X`; `current:` is unchanged;
`resetCurrent` becomes `self clearField: #current`; bootstrap's
`set_class_variable/2` keeps writing the class variable directly and counts
as assignment. **All three `current` accessors are rewritten** to stay
nilable, because `Object>>show:`/`cr` (`object.bt:343`, `:355`),
`show_cr_test.bt` and `class_variables_singleton_test.bt:11-16` rely on
`nil` before bootstrap for all three classes, and two of the three getters
(`BeamtalkInterface`, `WorkspaceInterface`) are bare `self.current` reads
today with no guard at all:

```beamtalk
  // transcript_stream.bt, beamtalk_interface.bt, workspace_interface.bt alike
  class current -> TranscriptStream | Nil =>
    (self hasField: #current) ifTrue: [self.current] ifFalse: [nil]
```

Converting the slot without the accessor rewrite is a regression: the getter
raises `UninitializedStateError` where those tests expect `nil`. The three
conversions are one change each, slot plus getter, and
`class_variables_singleton_test.bt` is the test that catches a partial one.

What that buys is smaller than the instance case and should be said plainly:
the slot can no longer hold `nil` and its type stops lying, but the public
`current` keeps answering `nil` before bootstrap because callers depend on
it. The win is in the declaration and in `fieldKinds`, not at the call site.

**In the applications, migration is narrow.** Most nilable slots in this
corpus should stay exactly as they are:

| Class | Verdict |
|---|---|
| `symphony/codex_client.bt` `proc` | **Convert.** `late state: proc :: Subprocess`; `launch` unchanged; `stopProcess`'s `isNil` guard becomes `hasField:` and its `:= nil` becomes `clearField:` |
| `exdura/exdura_http_server.bt` `httpServer` | **Convert**, with `destroy` using `clearField:` |
| `exdura/exdura_client.bt` `supervisor` | **Do not convert.** `nil` means "standalone mode" and selects a resolution strategy in `currentEngine`/`currentEventStore` (`:144-155`). The `\| Nil` is semantically honest |
| `exdura/exdura_client.bt` `httpServer` | **Do not convert.** Genuinely optional: "nil unless ExduraWorker was started with an `#http` config" |
| `exdura/workflow_engine.bt` `eventStore`, `activityPool` | **Do not convert**, and **the `supervised` flag stays.** `nil` means "resolve by name on every call" so a `rest_for_one` restart is picked up (`:976-987`); `activityPool` needs two meanings of absence, which `late` cannot express (§Context (b)) |
| `exdura/timer_manager.bt` `engine`, `eventStore`; `exdura_client.bt` `currentEngine`/`currentEventStore` | **Do not convert.** Resolve-by-name-per-read; they are methods and stay methods |
| `symphony`/`exdura` error and DTO `field:`s (`workspace_error`, `linear_error`, `issue`, `activity_outcome`, `retry_snapshot_entry`, …) | **Not candidates.** Optional data where `nil` is meaningful, and §5 forbids `late field:` on a Value |
| `exdura/stored_snapshot.bt` `state :: ReplaySnapshot` | **Not a candidate** — a Value built by deserialization. Default it or widen to `\| Nil` (§6) |

The docs (B10) lead with the do-not-convert rule: a nilable slot is not
automatically a `late` candidate, and the distinguishing question is whether
a stale value would be wrong.

Converting a slot between eager and `late` changes its ADR 0123 reconcile row
and requires a `shapeVersion:` bump, which the shape store detects once B9
encodes kind (§4g).

## Implementation Tracking

**Epic:** BT-3544
**Issues:** BT-3545 (B1), BT-3546 (A1), BT-3547 (B5a), BT-3548 (B2),
BT-3549 (B3), BT-3550 (B5b), BT-3551 (B4), BT-3552 (A3+A4), BT-3553 (A2a),
BT-1948 (A2b, repurposed), BT-3554 (A5), BT-3555 (B6), BT-3556 (B9),
BT-3557 (B8, `needs-spec` until the `printString` rendering is confirmed),
BT-3558 (B10), BT-3559 (stdlib singleton conversions)
**Not planned:** B7 (inspector `#lateSlot`), B11 (`ClassBuilder` parity) —
by decision, remain future work
**Status:** Planned

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
  `classState:`; "field" not "slot" as reflection vocabulary),
  [ADR 0036](0036-full-metaclass-tower.md) (the class-side gen_server that
  holds a `late classState:` slot — `get_class_var`/`set_class_var`, §4i),
  [ADR 0038](0038-subclass-classbuilder-protocol.md) (`ClassBuilder` — a
  reflective slot writer; gains a typed field spec and `lateFields:` in
  B11, §9),
  [ADR 0042](0042-immutable-value-objects-actor-mutable-state.md) (why a
  Value is never reassigned, hence §5),
  [ADR 0056](0056-native-erlang-backed-actors.md) (`native:` actors — an
  opaque chain link, hence Hint),
  [ADR 0067](0067-separate-state-field-keywords-by-class-kind.md)
  (`state:`/`field:`/`classState:`; the source of the two application
  corpora),
  [ADR 0078](0078-actor-initialize-inheritance.md) (the auto-chained
  `initialize` sequence and the runtime `UninitializedStateError`),
  [ADR 0082](0082-method-level-edit-save-and-changelog.md) /
  [ADR 0084](0084-class-side-runtime-method-fun-dispatch.md) (hot-patched
  `initialize` — why definite assignment cannot be an Error),
  [ADR 0095](0095-rich-navigable-inspector.md) (`InspectorField`),
  [ADR 0100](0100-open-world-diagnostic-policy.md) (severity from certainty;
  Rule 3 `[diagnostics]` escalation),
  [ADR 0103](0103-sendability-typing-from-class-kinds.md) (`handleScope:` —
  deferred, see §8),
  [ADR 0102](0102-set-theoretic-type-operators.md) /
  [ADR 0108](0108-named-union-type-aliases.md) (types the shared predicate must resolve,
  §7),
  [ADR 0107](0107-nil-and-type-patterns-in-match.md) (nil narrowing),
  [ADR 0110](0110-class-var-shadow-write-through-for-nlr-relay.md) (the
  `ClassVars` shadow-write rule; not triggered by `late`),
  [ADR 0111](0111-lowered-ir-verifier-for-state-threading.md)
  (`ThreadedIr::verify()`),
  [ADR 0117](0117-beamtalk-core-crate-split.md) (why the predicate's home is
  `beamtalk-core`),
  [ADR 0118](0118-expression-level-state-threading-preludes.md)
  (`ThreadedValue` — the prelude mechanism the rejected `lazy` design needed
  and `late` does not),
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
