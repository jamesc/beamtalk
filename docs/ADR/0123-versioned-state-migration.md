# ADR 0123: Versioned State Migration as a Language Surface

## Status
Proposed (2026-09-17)

## Context

### Problem statement

"State of shape *N* must become state of shape *N+1*" recurs in four places
in Beamtalk's roadmap, and today only one of them has any mechanism at all —
and that one has no user-facing hook:

| Boundary | State crosses… | Status today |
|---|---|---|
| **Hot reload** | code versions, in one process | `beamtalk_hot_reload.erl` does *structural* migration (add defaulted fields, drop removed ones) inside `code_change/3`. No way for the author to compute a new field from old ones. |
| **Persistence** | time (a Kephri-style object store) | Planned. Unsafe without a versioned on-disk shape. |
| **Distribution** | nodes running different code versions | Planned — BT-3527. Needs a versioned wire shape. |
| **OTP release upgrades** | appup/relup driving `code_change/3` in production | Planned — BT-3528. Consumes whatever hook this ADR defines. |

`beamtalk_actor.erl` § *Code Hot Reload* says "Generated actors can override
this to migrate state schemas", but no language surface exists to do so. The
only sketch in the corpus is ADR 0004's `codeChange:state:extra:` — stale
syntax (trailing `end`, no `=>`), never implemented.

### Current state

`beamtalk_repl_loader:hot_reload_class/2` runs on every class reload:

```erlang
IVars = fetch_instance_vars(ClassName),          % new field list, from the class registry
Extra = {IVars, ModuleName},
beamtalk_runtime_api:trigger_code_change(ModuleName, Pids, Extra)
```

and `beamtalk_hot_reload:code_change/3`, when `Extra` is `{NewInstanceVars,
Module}`, calls `Module:init(#{})` to obtain the new defaults, keeps every
old field that is still declared, fills new fields from defaults, and drops
the rest with a `?LOG_WARNING`. `OldVsn` is ignored (the loader passes
`undefined`). Five properties of this path matter for the design — and two
of them are **pre-existing bugs** that this ADR's review found and that
are worth fixing whether or not the rest of the ADR ships:

1. **It is already the right fallback.** Additive, defaulted changes — the
   common case — need no author involvement, and Squeak/Pharo's
   `ClassBuilder` does exactly this (match instance variables by name,
   default the rest). This ADR layers on top of it; it does not replace it.
2. **Nothing records what version a live state map is.** The loader
   migrates *all* instances on every reload, so in the happy path every
   instance is at the current shape. But if one `code_change` fails (it is
   caught and collected — `trigger_code_change/3` returns
   `{ok, Upgraded, Failures}`), that instance is left with an old-shaped map
   and the next reload has no way to know.
3. **The load/suspend ordering has a window.** `activate_module` loads the
   new BEAM *before* `trigger_hot_reload` suspends each instance. A
   `gen_server` calls its callback module fully qualified on every message,
   so a message that lands in that window runs new code on old-shaped
   state. OTP's `release_handler` avoids this by suspending first
   (`{update, Mod, {advanced, Extra}}`: suspend → load → `code_change` →
   resume) — but see §3 for why that ordering is *not* adopted here.
4. **Bug: the field list the reload path passes is local-only, so
   inherited fields are dropped.**
   `fetch_instance_vars/1` → `beamtalk_runtime_api:instance_variables/1` →
   `beamtalk_object_class`'s `#class_state.fields`, which is populated from
   `__beamtalk_meta`'s `'fields'` key — and `class_meta.rs` builds that from
   `class.state`, **this class's own declarations only** (the same split the
   stdlib names explicitly: `fieldNames` is "not inherited",
   `allFieldNames` is "including inherited"). But a subclass instance's
   state map *does* carry inherited fields, because `init/1` calls the
   parent's `init` and merges. So `migrate_fields/3`'s
   `NewVarSet = sets:from_list(NewInstanceVars)` does not contain the
   inherited names, and every inherited field is dropped — with a
   "Hot reload dropped fields" warning — on any reload of a subclass with
   live instances. `beamtalk_hot_reload_tests` has no inherited-field case.
   The flattened list already exists as `classAllFieldNames/1` ("Combined
   field names via superclass chain", `beamtalk_behaviour_intrinsics`).
5. **Bug: `init(#{})` returns a 3-tuple for most real classes, so field
   migration silently no-ops for them.** `migrate_fields/3` matches only
   `{ok, NewDefaults}`. But the generated `init/1`
   (`gen_server/callbacks.rs`, `init_initialize_guarded_doc`) returns
   `{ok, State, {continue, initialize}}` — and fires lifecycle start
   telemetry — whenever the class chain defines `initialize` *or* has any
   typed-no-default field (ADR 0078), unless `InitArgs` carries
   `'__skip_initialize__' => true`, in which case it returns the 2-tuple
   and skips both. So for every such class the `_ -> OldState` clause
   fires and the reload preserves the old map unchanged: new fields are
   *not* added, removed fields are *not* dropped, and nothing is logged.
   The class registry's `field_defaults` is no help — it is empty for
   compiled classes by design ("defaults are baked into the generated
   init"). The fix is one map key: call
   `Module:init(#{'__skip_initialize__' => true})`.

Values (`Value subclass:` with `field:`) have no process, so hot reload never
touches a live Value; they are only ever *re-created* by new code. Their
versioning problem is entirely the persistence/distribution one: a Value
written to disk under shape *N* read back under shape *N+1*.

### Constraints

- **Verified syntax only.** Every form below is either an existing form
  (`class sel: arg => body` class methods, `#{k => v}` dictionaries,
  `at:put:` / `removeKey:` / `at:ifAbsent:` / `includesKey:`) or a single
  new class-header clause with a direct precedent (`handleScope:`,
  ADR 0103).
- **Kind-neutral.** One feature must serve actors (`state:`) and values
  (`field:`); the vocabulary cannot be "state" (ADR 0067 reserves that word
  for actors). ADR 0105 already calls the `state:`/`field:` slot set a class's
  **shape**; this ADR adopts that word.
- **Advisory diagnostics** (ADR 0100): a reload has already happened when the
  re-check runs; findings inform, never veto.
- **Sendability** (ADR 0103): the serialised shape must respect tiers — a
  class whose fields include a `SendableRef` or `HandleScoped` value has no
  meaningful on-disk form.
- **Shared-leaf rule** (`architecture-principles.md` § Duplication): the
  chain runner is one Erlang module consumed by hot reload, persistence, and
  distribution; the rule "which selectors are migrations" must not be
  re-derived on both sides of the Rust/Erlang boundary.
- **No data loss on failure.** A failed migration must leave the instance's
  state recoverable. Today's default `supervisionPolicy` is `#temporary`
  (`stdlib/src/actor.bt`), so any design that kills the actor on failure
  loses its state by default.
- The compiler must be able to read the version **statically**: it goes
  into `__beamtalk_meta`, the ADR 0105 shape fingerprint, and the
  unreachable-migration lint.

## Decision

Two additions to the language, one runtime contract, and one tooling rule —
on top of two bug fixes (Current state ¶4–¶5) that ship first and stand on
their own.

### 1. `shapeVersion:` — an explicit, declared version

A class declares its shape version with a **class-header clause**, exactly
as `handleScope:` is (ADR 0103): parsed after the header and before the
class body, conventionally on its own line before any `state:`/`field:`
lines:

```beamtalk
Actor subclass: Cart
  shapeVersion: 2
  state: items :: List = #()
  state: total :: Integer = 0
```

- The argument is a **positive integer literal**. Anything else is a compile
  error: the compiler must read it without evaluating code.
- **Absent means `shapeVersion: 1`.** Every existing class is at version 1;
  no migration of the corpus is needed.
- At most one per class (a second is a compile error). It is *not*
  inherited: versions and chains are **per concrete class, over the
  flattened field dictionary** — inherited fields included, because that is
  what an instance's state map actually holds (`init/1` chains parent init
  and merges). A subclass's chain consists of the `migrateFromVN:` methods
  *it* defines — inherited class methods are not pulled into a subclass's
  chain, because the two classes' version numbers are unrelated. To reuse
  an ancestor's step, call it explicitly (`Base migrateFromV1: old`; see §2
  for what that send is). An abstract ancestor's `migrateFromVN:` is a
  helper for its subclasses, never run on its own (it has no instances).
  The cost — a superclass shape change must be answered by a bump in each
  concrete subclass — is caught by the §4 warning, which fingerprints the
  flattened shape.
- It is a **header clause, not a body declaration**: it is parsed by the
  same path as `handleScope:` (`parse_optional_handle_scope`), and a
  `shapeVersion:` reached inside the body loop is the same misplaced-clause
  error `handleScope:` gets. It deliberately does *not* join
  `is_state_like_declaration_keyword`'s set, which is the single source for
  `is_at_member_boundary` and `parse_method_body`'s exit condition — adding
  a keyword there changes where method bodies end, for no benefit when the
  clause is conventionally first anyway. The unparser, LSP completion, the
  `class_definition_text/7` skeleton generator (which `Workspace flush`
  uses to regenerate a class header, ADR 0082), and `ClassBuilder`
  (`shapeVersion:` builder keyword, ADR 0038) treat it as one more header
  clause.
- The compiler emits it into `__beamtalk_meta` as `'shape_version' => N`
  and the class registry answers `Cart shapeVersion` (a sealed `Behaviour`
  reader backed by a `beamtalk_behaviour_intrinsics` entry, default `1`).

**Explicit, not derived.** A fingerprint of the field list cannot be the
version: migrations must be *named* by a number a human can chain
(v1 → v2 → v3), and two different field lists can legitimately share a
version (a purely additive step). The fingerprint is used — but only by
tooling, to detect a shape that changed without a bump (§4).

**Why "shape", not "state".** `stateVersion:` reads wrong on a `Value`
(ADR 0067 made `state:` actor-only vocabulary), `version:` collides with
package versions (`Beamtalk version`), and ADR 0105 already named the
`state:`/`field:` slot set "shape". `shapeVersion:` is the one word that is
already in the corpus and kind-neutral.

### 2. `class migrateFromVN:` — one class-side method per step

A migration from shape *N* to shape *N+1* is a **class-side method** named
`migrateFromVN:`, taking the old fields as a `Dictionary` and returning the
new fields as a `Dictionary`:

```beamtalk
Actor subclass: Cart
  shapeVersion: 2
  state: items :: List = #()
  state: total :: Integer = 0

  /// v1 had only `items`; v2 caches their sum.
  class migrateFromV1: old -> Dictionary =>
    old at: #total put: (old at: #items) sum
```

A rename, which the structural fallback cannot express (it would drop
`owner` and default `ownerName`):

```beamtalk
Actor subclass: Account
  shapeVersion: 3
  state: balance :: Integer = 0
  state: ownerName :: String = ""

  class migrateFromV2: old -> Dictionary =>
    (old at: #ownerName put: (old at: #owner)) removeKey: #owner
```

A `Value`, retyping a field (integer cents → float amount) and adding one:

```beamtalk
Value subclass: Money
  shapeVersion: 2
  field: amount :: Float = 0.0
  field: currency :: Symbol = #USD

  class migrateFromV1: old -> Dictionary =>
    (old at: #amount put: (old at: #cents) / 100.0) removeKey: #cents
```

A three-step chain where two steps were purely additive and need no
method — the structural fallback covers them at the end of the chain. The
one hook must not assume those earlier steps have run: a hook sees the
**raw** accumulated dictionary, and a v1 instance reaching `migrateFromV3:`
has never had `#tags` (it was added in v3 and only the *final* shape is
known to the runtime), so the hook guards:

```beamtalk
Actor subclass: Session
  shapeVersion: 4
  state: user :: String = ""
  state: startedAt = nil        // added in v2 — defaulted at reconcile, no method needed
  state: tags :: List = #()     // added in v3 as a comma string; v4 makes it a List

  class migrateFromV3: old -> Dictionary =>
    (old includesKey: #tags)
      ifTrue: [old at: #tags put: ((old at: #tags) splitOn: ",")]
      ifFalse: [old]              // pre-v3 instance: reconcile defaults #tags to #()
```

Rules, all checked statically by the class validators:

- `migrateFromVN:` is recognised only on the class side, with exactly one
  parameter; `N` is a positive integer. The compiler collects the class's
  **own** such methods into `__beamtalk_meta` as
  `'shape_migrations' => #{1 => 'migrateFromV1:', …}`. **This table is the
  Rust↔Erlang conformance mechanism**: the runtime never parses selector
  names, it reads the table codegen produced. `ClassBuilder` (which
  compiles to a real module, ADR 0038) emits the same table from its
  `classMethods:` at `register`.
- A method with `N ≥ shapeVersion` is a **warning** (unreachable migration).
  Gaps are allowed — a missing step is a **no-op on the dictionary**; the
  structural fallback runs once, after the whole chain, against the final
  declared shape. Consequently a hook receives the raw dictionary as left
  by the hooks before it, and a key introduced at an earlier *un-hooked*
  step may be absent: read such keys with `at:ifAbsent:` or `includesKey:`
  (the `Session` example above).
- **No class-variable access** in the body — compile error. On the class
  side there are no instance slots; `self.x` in a class method *is* a
  class variable (`classState:`), and the compiler already tracks exactly
  this (`class_var_write` in `semantic_analysis/facts.rs`; the
  `class_var_names` set in `gen_server/methods.rs`), so the check is a
  small addition to the existing validators. §3 explains why this is a
  correctness rule, not a style rule. Ordinary sends are allowed — so a
  migration is **not** pure and the ADR does not claim it is. What *is*
  guaranteed, and what the class-side form buys, is narrower and real:
  the method needs **no process context** (no `self` instance, no class
  process), and it is **callable standalone** at the REPL with a literal
  dictionary.
- The return type is `Dictionary`; on a `typed` class the annotation is
  required as for any other method.

The naming convention is Beamtalk's equivalent of Erlang's per-version
`code_change` clauses — one selector per version rather than one function
with pattern-matched heads — and, unlike a single dispatcher method or a
table of blocks, each step is an ordinary method: it has a doc comment,
appears in `senders`/xref, and is callable on its own at the REPL. It is
also individually editable — with one caveat that follows from the table
being authoritative: a migration installed by a path that **recompiles the
class** (`Cart class >> migrateFromV1: …`, which recompiles the recorded
source; `compile:source:`) regenerates `__beamtalk_meta` and participates
in the chain; one installed as a bare fun with no recompile (a
`ClassBuilder addClassMethod:body:` after `register`, or any future
fun-only patch path) is invisible to the table and to `local_call/3`, and
**does not run**. The runtime emits a warning when a class-method fun whose
selector matches `migrateFromV*` exists outside the table — the one place
the runtime looks at a selector's *name*, and only to warn.

**Reusing an ancestor's step.** `Base migrateFromV1: old` inside a
migration body is an ordinary class send: it goes through `Base`'s class
gen_server (`class_send/3`), not through `local_call/3`. That is safe
during a reload of `Cart` — class processes are updated synchronously by
`register_class/0` at load time, before any migration runs, and are never
suspended — but it is serialised through `Base`'s process and subject to
the class-call timeout, and inside `Base`'s process class variables *are*
visible, unlike in the chain proper. Authors who want the exact chain
semantics can write `Base performLocally: #migrateFromV1: withArguments:
#(old)` (`ProtoObject >> performLocally:withArguments:`, the Beamtalk
surface of `local_call/3`). Lowering `migrateFromVN:` sends inside
migration bodies to `local_call/3` automatically is the recorded
refinement; it is not done here because it is precisely the call-site
interception ADR 0109 chose not to generalise.

### 3. Runtime contract

**Two new runtime modules**, split so the shared-leaf rule is actually
honoured rather than merely invoked:

- **`beamtalk_shape_chain`** — a genuine leaf, pure: given a migrations
  table, a `(From, To)` version pair, a field dictionary, and an invoker
  fun, produce the new dictionary. No registry, no meta, no process. This
  is the module persistence and distribution share.
- **`beamtalk_shape_migration`** — the lookups and the effects: reads
  `__beamtalk_meta`, resolves the flattened field list, invokes hooks via
  `local_call/3`, reconciles, packs and unpacks. It sits *beside*
  `beamtalk_hot_reload`, not below it (it needs the class registry and
  `beamtalk_object_class`), and `beamtalk_hot_reload:code_change/3` calls
  it.

(Neither is named `beamtalk_shape`: `beamtalk_shape_diff`,
`beamtalk_workspace_shape_store`, `beamtalk_workspace_shape_recheck_worker`
and `beamtalk_workspace_reshape` already exist with ADR 0105's meaning of
"shape" — the declared slot set — and a bare `beamtalk_shape` meaning
"versioned migration" would confuse every future reader.)

```erlang
%% beamtalk_shape_migration
%% Run the chain from FromVersion to the class's current shapeVersion, then
%% reconcile against the flattened declared fields. Fields is user fields
%% only (no internal keys). Runs in the calling process.
-spec migrate(Class :: atom(), FromVersion :: pos_integer(), Fields :: map()) ->
    {ok, NewFields :: map(), ToVersion :: pos_integer()} | {error, #beamtalk_error{}}.

%% Versioned envelope for disk and wire.
-spec pack(Instance :: map()) -> {ok, envelope()} | {error, #beamtalk_error{}}.
-spec unpack(envelope()) -> {ok, Instance :: map()} | {error, #beamtalk_error{}}.
-type envelope() :: {beamtalk_shape, Class :: atom(), ShapeVersion :: pos_integer(), Fields :: map()}.
```

**Chain semantics** (`migrate/3`), in order:

1. `V = FromVersion`, `T = Cart shapeVersion` (read from `__beamtalk_meta`).
2. For each `K` in `V, V+1, …, T-1`: if `'shape_migrations'` has `K`, apply
   `migrateFromVK:` to the current dictionary; otherwise the step is a
   **no-op** — nothing is defaulted or dropped between hooks, because the
   runtime knows only the *final* declared shape, not what shape `K+1` was.
   The result of a hook must be a `Dictionary`, else the step fails.
3. **Reconcile** against the declared field list — the **flattened** one
   (`classAllFieldNames/1` semantics, walking the superclass chain), not
   today's local-only `instance_variables/1` list (Current state ¶4).
   Defaults come from `Module:init(#{'__skip_initialize__' => true})` —
   the 2-tuple, no-telemetry branch (Current state ¶5) — never from bare
   `init(#{})`. A declared field present in the dictionary is kept;
   absent → its declared default; absent with no default → `nil` on an
   untyped class, **failure** on a `typed` class (the same post-`initialize`
   validation ADR 0078 runs — a migration may not leave a typed slot
   unset); an undeclared key → dropped with `?LOG_WARNING`, as today.
4. Return `{ok, NewFields, T}`.

`T < V` (a downgrade, OTP's `{down, Vsn}`) runs step 3 only and logs a
warning. Downgrade hooks (`migrateToVN:`) are **reserved, not defined**;
BT-3528 decides whether relups need them.

`V =:= T` runs step 3 only, so `migrate/3` is idempotent on already-current
state. A single hook need not be idempotent — it runs exactly once per
instance per step, because the version is written *with* the state.

**Failure** is a `#beamtalk_error{kind = shape_migration_failed}` carrying the
class, the step (`from`/`to`), and the underlying error, with a hint naming
the method (`"Cart class >> migrateFromV1: raised …"`).

**Migrations run in the migrating process**, not the class's gen_server —
and the mechanism already exists: `beamtalk_object_class:local_call/3`
("Execute a class method in the caller's process… calls
`Module:class_<Selector>(nil, #{}, Args)` directly — bypassing the class
object's gen_server"; its Beamtalk surface is
`performLocally:withArguments:`). That matters for more than latency: it
keeps a migration from being serialised through, or deadlocking on, the
class process.

`local_call/3`'s existing contract is also *why* the class-variable rule is
a compile error rather than a convention. It passes `nil` as ClassSelf and
`#{}` as the class variables, and discards any `{class_var_result, Value,
NewClassVars}` mutation the method returns. A migration that read a class
variable would therefore silently see `nil`, and one that wrote would have
the write silently dropped — wrong data, no error. The validator must
reject both at compile time so that contract is never reached by accident
— the same lesson ADR 0109's BT-3047 amendment drew for class identity
(never read it from the executing process; it must be supplied). `local_call/3`
also does not walk the superclass chain, which matches "chains are per
concrete class" exactly.

**Not** a general "class methods run in the caller" rule: ADR 0109's Scope
explicitly excludes generalising its call-site interception, and this ADR
does not widen it. `local_call/3` is an existing, narrowly-contracted entry
point for exactly the "method does not touch class state" case, which the
validator enforces.

**Hot reload** (`beamtalk_hot_reload` + `beamtalk_repl_loader`):

- Actor state maps gain one internal key, `'__shape_version__'`, added to
  `beamtalk_tagged_map:internal_fields/0` (the single source of truth for
  internal keys). Every existing consumer of that list —
  `beamtalk_object_printer`, `beamtalk_reflection` (`fieldNames`),
  `beamtalk_class_instantiation`, `beamtalk_inspector`, and
  `migrate_fields/3` itself — *filters internal keys out*, so the addition
  is additive and safe. One invariant becomes load-bearing and gets a test:
  the key **never appears on a Value tagged map** (Values are not
  versioned in-memory, and `beamtalk_class_instantiation`'s
  `ancestor_compiled_defaults/1` derives Value defaults with
  `maps:without(internal_fields(), Module:new())`). `init/1` writes the
  current version; absent means `1`.
- **Read before seed.** Today's `migrate_fields/3` seeds `BaseState` from
  the *new* init's internal keys before overlaying the old state. Once
  `init/1` writes `'__shape_version__'`, that order would stamp the new
  version onto the state *before* the chain reads the old one. The old
  version is read from the incoming `State` first, as an explicit ordering
  requirement, not an accident of code layout.
- The `Extra` contract changes from `{NewInstanceVars, Module}` to a map
  carrying **only what the state cannot tell you**:

  ```erlang
  #{module := atom()}
  ```

  Everything else is derived, in one place, by `beamtalk_shape_migration`:
  the class from the state's `'$beamtalk_class'` tag, the target version
  and the migrations table from the module's `__beamtalk_meta`, and the
  flattened field list from the class registry walk (¶4). The loader no
  longer computes a field list at all — the old `{IVars, Module}` shape
  was the *cause* of ¶4, because it let the caller choose which list to
  pass. `code_change(_OldVsn, State, Extra)` reads `V` from the state map
  (not from `OldVsn`, which stays ignored — the *state* is the source of
  truth, which is exactly what lets the same chain run for persistence,
  where there is no `OldVsn` at all), calls
  `beamtalk_shape_migration:migrate/3` on the user fields of `State`, and
  re-attaches the internal keys plus the new `'__shape_version__'`. The one
  caller (`hot_reload_class/2`) is updated in the same change; there is no
  transitional tuple clause.
- **On failure the actor stays suspended.** Not "stops": raising inside
  `code_change/3` cannot stop a `gen_server` — `sys`'s
  `system_code_change/4` wraps the callback in a bare `catch`, so a throw
  or exit becomes `{error, Reason}` returned to `sys:change_code/4`, and
  today's `try_change_code/3` then unconditionally `sys:resume`s the
  process **with its old-shaped map under the new code** — the exact
  outcome this ADR most wants to rule out. And killing from the driver is
  no better under the default `supervisionPolicy` of `#temporary`: the
  actor, its mailbox and its state are simply gone, unrecoverably; under
  `#permanent` it is reincarnated with fresh `init/1` state, which reads as
  *success* to anything watching liveness. So the driver
  (`trigger_code_change/3`, which already collects `{Pid, Reason}`) does
  the one thing that loses nothing: **it does not resume a pid whose
  `code_change` failed.** The actor keeps its state, `'__shape_version__'`
  is unchanged, `sys:get_state` still works for inspection, and the next
  `Cart reload` — after the author fixes the hook — re-suspends (idempotent),
  re-runs the chain from the *old* version, and resumes on success.
  Synchronous callers meanwhile time out with the existing structured
  timeout error, whose pid the §4 `Error` finding names; an operator who
  wants the instance gone uses `Workspace actorAt: kill`. This is fail-closed
  without data loss, and it is what OTP itself does with a failed upgrade:
  the upgrade fails, the process does not.
- **Ordering stays per-instance** (suspend → change_code → resume, one pid
  at a time, after the load) — today's ordering, **not** suspend-all-first.
  Suspend-all was considered and rejected: under sync-by-default messaging
  (ADR 0043) an instance blocked in a `gen_server:call` to a sibling of
  the same class is not in its receive loop and cannot answer
  `sys:suspend`; if the sibling was suspended first, the reload wedges
  until `sys:suspend`'s timeout. Per-pid ordering keeps that window one pid
  wide and self-clearing. It also keeps reload latency per-instance rather
  than freezing every instance for the whole compile+load. The
  load→suspend window from Current state ¶3 therefore **remains**, narrow
  and pre-existing; closing it properly is `release_handler`'s job and is
  deferred to BT-3528, where the relup does suspend first and there is no
  interactive compile in the window. A dispatch-time guard (an actor whose
  `'__shape_version__'` is behind its module's refuses or lazily migrates)
  is the recorded workspace-side refinement if the window bites.
- **Subclass instances are migrated on a superclass reload.**
  `hot_reload_class/2` is driven from the reloaded module's own class
  list, and `beamtalk_object_instances:all/1` is keyed by exact class name,
  so today reloading `Cart` touches `Cart` instances only — live
  `SpecialCart` instances, which carry `Cart`'s fields, are never
  reconciled. The loader walks `beamtalk_class_registry:direct_subclasses/1`
  (already used elsewhere in `beamtalk_repl_loader`) and runs
  `hot_reload_class/2` for every loaded descendant, each with its **own**
  flattened list and its own chain (`V =:= T` for an un-bumped subclass,
  so step 3 only). Without this, the §4 warning would describe a migration
  that never runs.
- **`native:` actor classes are out of scope.** They compile to a facade,
  not a gen_server (ADR 0056, `native_facade.rs`): no generated `init/1`,
  no `code_change/3` delegating to `beamtalk_hot_reload`, and state that
  belongs to the backing Erlang module in whatever shape it chose.
  `shapeVersion:` on such a class is a compile error, and the §4 checks
  skip classes whose meta carries `native`.

**Envelope** (`pack/1`, `unpack/1`) — the contract BT-3527 and the
persistence ADR consume:

- `{beamtalk_shape, Class, ShapeVersion, Fields}`: a tagged 4-tuple, trivially
  pattern-matched from Erlang/Elixir and `term_to_binary`-friendly
  (`Binary serialize:`). `Fields` holds user fields only; nested `Value`
  instances are packed recursively so each carries its own version.
  Builtin tagged maps that are not user classes — `Array`'s index→value
  `'data'` map (ADR 0090), `Dictionary`, `String` — are neither user fields
  nor internal keys and pass through `pack/1` **as terms**, unversioned:
  their shape is the runtime's, not the user's, and is covered by the
  runtime's own release compatibility (BT-3528).
- `pack/1` is defined only for **`Sendable`-tier** classes (ADR 0103). An
  instance whose class has a `SendableRef` (pid) or `HandleScoped` field
  returns `{error, #beamtalk_error{kind = not_serialisable}}` naming the
  field. Determining the tier at runtime is a walk over the class's
  flattened field types (`__beamtalk_meta`'s `field_types`) and the
  referenced classes' kinds; that walk does not exist today and is real
  Phase 2 work, not a lookup. Actors pack their *state* (they are
  `SendableRef` themselves; the envelope is what a store or a remote node
  receives, never a pid).
- `unpack/1` runs `migrate/3` from the envelope's version, so a store
  written under v1 reads back as v3 without the store knowing. Whether
  cross-node *messages* are wrapped, or versions negotiated per class at
  connect time, is BT-3527's decision; this ADR only guarantees that a
  wrapped term migrates.

### 4. Tooling

**Compile time** (class validators, `beamtalk-core`): the literal/duplicate
rules for `shapeVersion:`, the arity/class-variable rules for
`migrateFromVN:`, and the unreachable-migration warning (§2).

**Reload time** — built on ADR 0105's shape machinery **where it actually
lives: in Erlang, in `beamtalk_workspace`**, not in the language service.
`beamtalk_workspace_shape_store:capture/1` already stores a per-generation
`shape()` (`#{FieldName => TypeName}`) per class, and
`beamtalk_shape_diff:diff/2` already classifies each field as
`{added, F} | {removed, F} | {retyped, F, Old, New}`. That *is* the
fingerprint §4 needs; the additions are (i) capturing the **flattened**
shape, so an ancestor's change shows up in every concrete subclass's diff
(the diff is in a class the author may not be looking at, which is exactly
the case worth catching), (ii) joining the diff with the class's
`'shape_version'` across generations, and (iii) the migration outcome from
`trigger_code_change/3`. The language service's part is LSP completion and
hover only. Findings go through the existing reload-findings channel on
every surface (LSP, workspace UI, REPL), as a **structured payload**
(class, kind, fields, counts, pids) that each surface renders — the REPL
text below is one rendering, per `docs/development/surface-parity.md`:

| Reload observed | Finding |
|---|---|
| Field removed or retyped, `shapeVersion:` unchanged | **Warning** — `shape of Cart changed (dropped: #discount) but shapeVersion is still 2; 3 live instances lost #discount — bump shapeVersion: and add class migrateFromV2:` |
| Fields only added with defaults, version unchanged | **Hint** — structural fallback applies; nothing to do |
| Version bumped N→N+1, no `migrateFromVN:` | **Hint** — `Cart shapeVersion 2 → 3 has no migrateFromV2:; structural fallback applies` |
| Version decreased | **Warning** |
| Any instance left suspended by a failed migration | **Error** — it records a failure that has happened, not a prediction: `2 instances of Cart suspended: migrateFromV1: raised … (Cart class >> migrateFromV1:) — fix the hook and reload again, or Workspace actorAt: kill` |

All advisory (ADR 0100); the reload proceeds. `Behaviour >> reload` keeps
its return value; the finding is the report.

**Why advisory, when ADR 0113 gates file deletion behind
`confirmDestructive`.** Dropping a field from live instances is
irreversible data loss and, unlike a `.bt` file, not recoverable from git —
so the asymmetry needs stating. Two reasons it is nonetheless advisory:
a reload is an action that has *already happened* when the re-check runs
(ADR 0105's governing constraint — a post-hoc gate cannot veto it), and the
place a consent-like gate genuinely can live is **before** the install:
ADR 0105's pre-save advisory (`precheck`, Phase 3) sees the edit before the
reload, so the "dropped without bump" warning fires there too, in the
editor, while the author can still add the migration. A hard tier-2 gate
on `reload` of a class with live instances losing fields — ADR 0113's
model — is a plausible later addition and is recorded, not decided.

**REPL-level testing** — the pieces are ordinary methods, so a migration is
exercised before any instance depends on it:

```
bt> Cart shapeVersion
=> 2
bt> Cart migrateFromV1: #{#items => #(3, 4)}
=> #{#items => #(3, 4), #total => 7}
bt> Cart migrateShape: #{#items => #(3, 4)} from: 1      // whole chain + reconcile
=> #{#items => #(3, 4), #total => 7}
bt> Cart reload
=> Cart
ℹ reload check: Cart shape v1 → v2; 3 instances migrated
```

`migrateShape:from:` is a sealed `Behaviour` class-side method over
`beamtalk_shape_migration:migrate/3` (a `beamtalk_behaviour_intrinsics`
entry, like every other `Behaviour` reader); the shown reload notice is
illustrative and is confirmed with the REPL's existing display conventions
at implementation.

### Error examples

```beamtalk
Actor subclass: Cart
  shapeVersion: "two"
// ⛔ error: shapeVersion: expects a positive integer literal, got a String

Actor subclass: Cart
  shapeVersion: 2
  shapeVersion: 3
// ⛔ error: duplicate shapeVersion: declaration (already 2)

Actor subclass: Cart
  shapeVersion: 2
  class migrateFromV7: old => old
// ⚠️ warning: migrateFromV7: is unreachable — Cart's shapeVersion is 2

Actor subclass: Cart
  shapeVersion: 2
  state: items = #()
  classState: taxRate = 0.2
  class migrateFromV1: old => old at: #tax put: (old at: #total) * self.taxRate
// ⛔ error: migration methods may not access class variables (`self.taxRate`);
//    they run outside the class process, where class variables are not available
```

At reload, a hook that raises:

```
bt> Cart reload
=> Cart
⛔ reload: 1 instance of Cart left suspended — migrateFromV1: raised
   does_not_understand: List>>summ (Cart class >> migrateFromV1:, cart.bt:9)
   state intact at v1; fix the hook and `Cart reload` again, or `Workspace actorAt: kill`
```

## Prior Art

| System | Mechanism | Take / leave |
|---|---|---|
| **Erlang/OTP** | `code_change(OldVsn, State, Extra)` written by hand, one clause per `OldVsn`; `-vsn` attribute; appup `{advanced, Extra}`; `release_handler` suspends before loading; a failed `code_change` fails the upgrade, not the process | **Take** the per-version clause idea (one selector per step), `Extra` as the loader's channel, and "a failed upgrade is a failed upgrade" (the suspended-on-failure rule). **Leave** `OldVsn` as the version source: it is a *module* attribute, and method-only changes must not bump a shape version. The state carries its own version. Suspend-all-first is *deferred* to BT-3528, not taken for the workspace (§3). |
| **Elixir / Ecto** | Same `code_change`; Ecto migrations are numbered, ordered, one function each (`change/0`) | **Take** the "numbered, ordered, one function per step, gaps are fine" model. Ecto versions a *database*, not process state — the step shape transfers, the timestamp naming does not. |
| **Squeak / Pharo** | `ClassBuilder` migrates live instances on shape change by matching ivars by name, defaulting the rest (`updateInstancesFrom:`); serialised objects use `Object>>convertToCurrentVersion:refStream:` with a `varDict` of old ivars; Fuel has `FLMigration` | **Take** name-matching structural migration as the always-on fallback (it already is), and the "dictionary of old ivars" argument shape. **Adapt**: Pharo's hook is instance-side on a half-built object; on the BEAM a class-side function needing no process is REPL-callable and works for Values too. |
| **GemStone/S** | Versioned classes coexist (`ClassHistory`); `migrateFrom:instVarMap:` on the new instance; `migrateInstances` walks the repository | **Take** explicit versions and the instance-map hook. **Leave** class coexistence: BEAM has exactly two module versions, and reload migrates every instance eagerly. |
| **Gleam** | FAQ: hot reload is "the usual Erlang amount of safety"; no schema hook | The gap. |
| **Akka Persistence** | Schema evolution via `EventAdapter`s and serializer manifests; "additive changes free, everything else explicit" | **Take** the additive-is-free rule (our structural fallback + defaults). Akka versions *events*; we version *state*, which is the simpler problem. |
| **Protobuf / Avro** | Field numbers/defaults make additive evolution free; renames and retypes are explicit | Same rule, confirms it from the wire side — and why `pack/1` needs a version, not just a field list. |
| **Livebook / Jupyter** | No state migration: re-evaluate, re-derive | The "no image" persona; explains why the workspace must never *block* a reload on a shape change. |

## User Impact

- **Newcomer**: never sees this until they remove or rename a field on a
  class with live instances — then the reload finding tells them what to
  type (`bump shapeVersion:, add class migrateFromV2:`). `migrateFromV1:` is
  a dictionary in, dictionary out — no new concepts. A typo in a hook
  leaves the instance suspended with its state, not dead; the finding says
  how to recover.
- **Smalltalk developer**: recognises Pharo's ivar-name matching as the
  fallback and `convertToCurrentVersion:` in the hook; may miss the
  instance-side form (see Steelman). Everything is a message send; nothing
  is a pragma.
- **Erlang/BEAM developer**: this *is* `code_change` with the clauses turned
  into selectors and the version kept in the state instead of `-vsn`. The
  envelope is a tagged tuple they can pattern-match. A failed migration
  behaves like a failed OTP upgrade — the process survives, the upgrade
  doesn't — rather than like a crash.
- **Operator**: a failed migration is a suspended process with intact
  state and a structured finding naming the hook and the pids — not a
  silent old-shape process, and not a `#temporary` actor quietly gone.
  `'__shape_version__'` is inspectable with `sys:get_state`, `observer`, and
  `recon`, on suspended processes too. Reload reports migrated/suspended
  counts. Alert on the `Error` finding, not on liveness.
- **Tooling developer**: `shapeVersion:` is one more header clause in the
  AST; `'shape_migrations'` in `__beamtalk_meta` is the only thing the
  runtime reads, so the LSP can offer `migrateFromV<N-1>:` as a completion
  when the version bumps and hover can show a class's chain. The reload
  findings are a structured payload, so every surface renders the same
  facts.

## Steelman Analysis

### Version: header clause (chosen) vs `class shapeVersion => 3` vs derived fingerprint

| Cohort | For `class shapeVersion => 3` | For a derived fingerprint |
|---|---|---|
| 🧑‍💻 **Newcomer** | "It's just a method — same as `supervisionPolicy`; I don't learn a keyword." | "I never have to remember to bump anything." |
| 🎩 **Smalltalk purist** | "Class-side `version` is a forty-year convention; declarations belong in methods." | — (Pharo has no shape version either) |
| ⚙️ **BEAM veteran** | "Overridable, so a subclass can compute it." | "Erlang already derives `vsn` from an MD5 when you don't declare one." |
| 🏭 **Operator** | "`Cart shapeVersion` works either way." | "Impossible to forget to bump." |
| 🎨 **Language designer** | "One fewer keyword." | "Zero syntax." |

Why the header clause still wins: the compiler needs the value *statically*
(meta, fingerprint, unreachable lint), which for a method means a
"must return an integer literal" rule — a method that isn't really a method.
The fingerprint fails the chaining requirement outright: you cannot write
`migrateFrom<hash>:`, and a purely additive change would produce a new
version with nothing to migrate. Its real strength — catching the forgotten
bump — is kept as the §4 warning, which ADR 0105's `beamtalk_shape_diff`
already mostly computes.

### Hook: per-version methods (chosen) vs one table vs one dispatcher vs instance-side

| Cohort | `class shapeMigrations => #{1 => [:old \| …]}` | `class migrateShape: old from: v` | instance-side `migrateFromV1: old` |
|---|---|---|---|
| 🧑‍💻 **Newcomer** | "All the history in one place, in order." | "One method to find." | "I can just write `self.total := …`." |
| 🎩 **Smalltalk purist** | "Blocks are the Smalltalk way to defer code." | — | "This is `convertToCurrentVersion:`; migration belongs to the instance." |
| ⚙️ **BEAM veteran** | "Reads like an appup." | "This *is* `code_change/3` with `case`." | — |
| 🏭 **Operator** | "One diff hunk per release." | "Grep one selector." | — |
| 🎨 **Language designer** | "Data, not naming convention." | "No magic in selector names." | "Uniform with `initialize`." |

Why per-version methods still win: the table and the dispatcher put every
version's logic in one method body, so doc comments, xref, and per-method
edit all lose their unit; the dispatcher additionally forces
`version = 1 ifTrue: [^…]` chains. The instance-side form is the strongest
rejected option — but it needs a process (no REPL test on a Value), runs on
a half-migrated `self`, and gives actors (`self.x :=`) and values
(`self withX:`) two idioms for one feature. The convention cost ("magic
name") is contained: the compiler emits the table, so the only runtime
code that looks at a selector *name* is the warning for a fun installed
outside it.

### Failure: leave suspended (chosen) vs stop the actor vs resume with old state

| Cohort | Stop the actor | Resume with old state |
|---|---|---|
| ⚙️ **BEAM veteran** | "Let it crash; the supervisor is the policy." | "It's what today's code does; the next message's error is diagnosable." |
| 🧑‍💻 **Newcomer** | — | "Don't freeze my actor; at least it answers." |
| 🏭 **Operator** | "A dead process is a clear signal." | "No timeouts cascading into callers." |
| 🎨 **Language designer** | "Fail fast, fail loud." | "Least mechanism." |

Why suspended wins: "stop" is unachievable from inside `code_change/3`
(`sys` catches it) and, driven from outside, is unrecoverable under the
default `#temporary` policy and *silently* lossy under `#permanent` (fresh
`init/1` state reads as success). "Resume with old state" — today's
behaviour — is new code on an old-shaped map; the errors surface later,
far from the cause, and a handler that writes state in that window leaves
a hybrid map the retried migration cannot reason about. Suspended is the
only outcome that loses nothing and retries cleanly, and it is what OTP's
own release handling does. Its cost — callers time out until the author
acts — is real and is in Consequences.

### Tension points

- Smalltalk purists and newcomers lean instance-side; BEAM veterans and
  operators lean class-side. "No process needed; REPL-callable" decided it.
- Whether the "shape changed without bump" finding should be `Error` rather
  than `Warning` on a *typed* class, or a hard ADR 0113-style gate: ADR
  0100 says advisory; the pre-save advisory is where a gate can live;
  revisit if experience shows data loss in practice.
- Whether the workspace should adopt suspend-all-first now (closing the
  load→suspend window) or wait for BT-3528: the deadlock under
  sync-by-default messaging decided it for now.

## Alternatives Considered

### Do nothing
Keep the structural fallback and add no surface. Genuinely worth stating,
because three of the four motivating boundaries are unbuilt and the fourth
already handles additive change. Rejected, but with two things carried
over from it: the two pre-existing bugs (Current state ¶4–¶5) are fixed
*first and alone* — they are causing data loss today and need no new
surface — and the envelope and hook are phased so each ships value
independently (Implementation). What "do nothing" cannot give is rename,
retype, or derive, which are the three cases the persistence and
distribution ADRs need and which block BT-3527/BT-3528 on this ADR.

### Bug fixes only (the incremental baseline)
Fix the inherited-field drop and the `init(#{})` 3-tuple no-op; add
`'__shape_version__'`; stop there. This is the first implementation phase
of the chosen design, shipped on its own — so it is not so much rejected
as sequenced first. It is the honest baseline against which the rest of
the ADR's value should be measured.

### Erlang-style manual `code_change` (`@native` actor or an Erlang override)
Write `code_change/3` by hand in a backing module. Rejected: not a language
surface, actor-only (no Values, no persistence), and it is exactly the
"can override" promise that has gone unused because it forces the author
out of Beamtalk. (It remains the *only* route for `native:` classes, §3.)

### Ecto-style migration files
Separate `migrations/` files keyed by timestamp. Rejected: Beamtalk's unit of
code is the class (ADR 0040); a migration that lives away from the
`state:` lines it explains is the Pharo "senders of" problem in reverse.
The numbered-step model is kept, in the class.

### Pharo `ClassRedefinition` / `instVarNamed:` (structural only)
Keep only name-matching migration, add nothing. Rejected: it is already the
fallback, and it cannot express rename, retype, or derive — the three
cases the persistence and distribution ADRs need.

### GemStone-style class versioning (old and new classes coexist)
Rejected: BEAM holds at most two versions of a module, and the workspace
reloads every instance eagerly; coexistence would need a second class
registry entry per version for no benefit the eager chain does not give.

### Storing `'__shape_version__'` on Value instances too
Would let a live reload of a Value class find stale embedded values.
Rejected for v1: a per-instance key on every Value costs memory and breaks
structural equality between a pre- and post-reload `Point` with equal
fields. Values are immutable and re-created by code; their versioning need
is the envelope. Recorded as a deferred follow-up (Consequences).

### Ship the migration chain now, defer the envelope (narrower scope)
Hot reload — the only consumer that exists today — migrates a live map in
place and never serialises anything, so a narrower ADR could ship §1–§2
plus `migrate/3` and leave `pack`/`unpack` to the ADRs that consume them.
Rejected for one reason: BT-3527 and BT-3528 are *blocked on this ADR*
precisely for the envelope, and defining it separately in each would
duplicate the version/tier rule across two documents — the exact
shared-leaf failure this project's architecture principles name. The
envelope also constrains the design above it in a useful way: `migrate/3`
takes a version and a plain field map *because* an envelope must be able
to call it with no `OldVsn` and no live process. It stays — as its own
phase, shippable alone, with EUnit but no production consumer.

### Single dispatcher, table of blocks, instance-side hook
See Steelman Analysis.

## Consequences

### Positive
- Two present-day data-loss bugs in hot reload (inherited fields dropped;
  migration silently skipped for any class with `initialize` or a typed
  field) are fixed first, on their own, with the regression tests they
  lack.
- One feature serves hot reload today and unblocks BT-3527 (distribution)
  and BT-3528 (releases) plus persistence, with one pure leaf module
  (`beamtalk_shape_chain`) they all share.
- Existing classes are untouched: absent `shapeVersion:` is v1, and the
  structural fallback is unchanged behaviour.
- Migrations need no process, are REPL-callable, and are individually
  editable through the recompile path.
- A failed migration loses no state and retries on the next reload.
- Superclass reloads finally reach subclass instances.
- The Rust↔Erlang rule lives in one place (`'shape_migrations'` emitted by
  codegen); the §4 findings reuse ADR 0105's existing shape store and
  diff rather than a second fingerprint.

### Negative
- A new header clause: parser, AST, unparser, `class_definition_text/7`,
  `ClassBuilder`, LSP completion, meta, class registry, two
  `beamtalk_behaviour_intrinsics` entries — ADR 0103's `handleScope:`
  precedent says budget ~M, not S.
- **A failed migration leaves the instance suspended**, so its synchronous
  callers time out until the author fixes the hook and reloads, or kills
  the instance. That is a visible stall where today's code silently
  continues on a wrong-shaped map. It is the intended trade, and the
  finding says what to do.
- The load→suspend window (Current state ¶3) **remains** in the workspace;
  closing it is deferred to BT-3528's `release_handler` ordering because
  suspend-all-first deadlocks under sync-by-default messaging.
- `Extra` changes shape; `beamtalk_hot_reload_tests` and the one caller are
  updated together.
- Live **Values of an old shape** (held in a binding or another actor's
  state across a reload of the Value's class) are not migrated — same as
  today, now documented. Follow-up: either version Value instances or
  deep-reconcile actor state on reload of an embedded Value class.
- Downgrades run the structural fallback only; if BT-3528 needs symmetric
  hooks, `migrateToVN:` is reserved for it.
- Migrations may not read class variables — a real restriction, forced by
  `local_call/3`'s contract — and are *not* pure: sends can reach anything.
  The ADR promises "no process context needed", not purity.
- Chains are per concrete class, so a superclass shape change is answered
  once per subclass (each calling the shared `Base migrateFromVN:` helper,
  which is a class send through `Base`'s process — serialised, and with
  class variables visible there, unlike in the chain). A composed
  per-ancestor chain, with `'__shape_version__'` becoming a per-class map,
  is the recorded refinement if hierarchies with many stateful subclasses
  make this tedious.
- Hooks see the raw dictionary: a key introduced at an earlier un-hooked
  step is absent until the final reconcile, so hooks reading such keys
  must guard. The alternative — reconciling after every step — is not
  available, because only the final shape is declared.
- A `migrateFromVN:` installed as a bare fun without recompiling the class
  does not run; the runtime warns, but only by selector name.
- **Class rename interacts with the envelope.** The envelope keys on the
  class *atom*, so a class renamed via ADR 0114 (`renameTo:`, shipped)
  makes previously packed terms unreadable — the persistence and
  distribution ADRs will need a rename-aware resolution step (an alias
  table, or a stable class identity in the envelope). Separately,
  `renameSelector: #migrateFromV1: to: …` silently breaks a chain and
  stales `'shape_migrations'` until recompile; ADR 0114's rename validator
  should refuse or warn on `migrateFromV*` selectors. Nothing in hot reload
  is affected, since it never unpacks. Recorded in Deferred.
- `pack/1`'s tier check needs a field-type walk over the flattened shape
  that does not exist yet.

### Neutral
- Codegen of methods and dispatch is unchanged; `code_change/3` still
  delegates to `beamtalk_hot_reload`.
- `OldVsn` stays ignored; a module `-vsn` (if BT-3528 emits one) is a
  release concern, not a shape concern.
- No new severity levels — findings slot into ADR 0100.
- `native:` classes are untouched (out of scope, §3).

## Migration Path

**No `.bt` source changes are required, anywhere.** An absent
`shapeVersion:` is v1 and the structural fallback is unchanged, so every
existing class in the stdlib, the tests, and downstream projects keeps
compiling and reloading exactly as today. There is no deprecation period
because nothing is deprecated.

Three internal changes need coordinated updates, all inside this repo:

| Change | Who updates | When |
|---|---|---|
| Reconcile uses the flattened field list and `init(#{'__skip_initialize__' => true})` (the two bug fixes) | `beamtalk_hot_reload`, `hot_reload_class/2`, `beamtalk_hot_reload_tests` | Phase 0 |
| `Extra` becomes `#{module := atom()}` (`{NewInstanceVars, Module}` → the field list is derived, never passed) | same three | Phase 0, one commit — no transitional tuple clause, since the only caller is in-tree |
| Failed migration leaves the instance suspended instead of being resumed on old state | `trigger_code_change/3`'s resume path; workspace reload reporting; any test asserting resume-on-failure | Phase 0 |

The two bug fixes are **behaviour changes users will notice and want**: a
subclass with live instances stops silently losing its inherited fields,
and classes with `initialize` start getting field migration at all. They
are called out here rather than buried because anything that came to
depend on the old behaviour (nothing in-tree does) would see different
state after reload.

## Implementation

Phases sized for `/plan-adr`, ordered so that **each ships value on its
own** and the riskiest decision (the language surface) comes after the
cheap wins are banked:

0. **Phase 0 — bug fixes + version key (M, ships alone):** flattened field
   list via `classAllFieldNames/1` semantics; `init(#{'__skip_initialize__'
   => true})` as the defaults source; `'__shape_version__'` in
   `internal_fields/0` with read-before-seed ordering and the
   never-on-a-Value test; `Extra` reduced to `#{module}`; descendants walk via
   `direct_subclasses/1`; failed `code_change` leaves the pid suspended
   (conditional `sys:resume`). Reads `'shape_version'`/`'shape_migrations'`
   from `__beamtalk_meta` with defaults `1`/`#{}`, so it works before the
   compiler emits them. **Tests:** `beamtalk_hot_reload_tests` gains
   inherited-field preservation, a class with `initialize`, a class with a
   typed-no-default field, subclass-on-superclass-reload, and
   failure-stays-suspended; one REPL-protocol case.
1. **Phase 1 — hook napkin (S, the risky assumption):** one hand-written
   `class_migrateFromV1:` export in a fixture module, one live instance,
   through a real `Cart reload`, invoked from inside `code_change/3` via
   `beamtalk_object_class:local_call/3` **while that class's own module is
   being reloaded** — the one ordering none of `local_call/3`'s existing
   callers exercise. No keyword, no validators, no envelope. If this fails,
   the class-side-function hook is the wrong shape and the ADR is
   revisited, which is why it is not folded into Phase 3.
   **Tests:** one EUnit case, one REPL-protocol case.
2. **Phase 2 — chain + envelope (M, unblocks BT-3527):**
   `beamtalk_shape_chain` (pure leaf) and `beamtalk_shape_migration`
   (`migrate/3`, `pack/1`, `unpack/1`, the tier walk over flattened field
   types); `beamtalk_hot_reload:code_change/3` delegates to `migrate/3`.
   **Tests:** EUnit for chain order, gaps, downgrade, idempotency,
   typed-slot failure, `not_serialisable`, nested-Value pack/unpack,
   builtin-tagged-map pass-through.
3. **Phase 3 — language surface (M):** `shapeVersion:` header clause
   (parser via the `handleScope:` path, `ClassDefinition`, unparser,
   `class_definition_text/7`, `ClassBuilder shapeVersion:`), validators
   (literal, duplicate, `migrateFromVN:` arity/class-variable/unreachable,
   `native:` refusal), meta emission of `'shape_version'` and
   `'shape_migrations'` (compiler and `ClassBuilder`), `Behaviour
   shapeVersion` and `migrateShape:from:` intrinsics, `init/1` writing the
   version, the fun-outside-table warning. **Tests:** parser/unparse
   round-trip incl. flush skeleton, validator diagnostics, codegen meta
   snapshot, BUnit for `migrateShape:from:` on a Value.
4. **Phase 4 — findings (M):** flattened shape capture in
   `beamtalk_workspace_shape_store`; version-aware join over
   `beamtalk_shape_diff:diff/2`; migration outcomes from
   `trigger_code_change/3`; the five findings as a structured payload on
   every surface (`surface-parity.md` entry); pre-save advisory hook; LSP
   completion for `migrateFromV<N-1>:` after a bump; hover showing the
   chain. **Tests:** one workspace-level test per row of the §4 table;
   LSP completion test.
5. **Phase 5 — docs and e2e (S):** `beamtalk-language-features.md` § Live
   Patching gains a *Shape Versioning* section; REPL-protocol e2e extending
   the existing `hot_counter.bt` / `hot_counter_v2.bt` fixtures to a
   v1 → v2 → v3 chain with a failing step and a recovery reload; status
   flip.

**Affected:** `crates/beamtalk-core` (parser header-clause path, AST,
validators, unparse), `crates/beamtalk-codegen` (`class_meta.rs`,
`init/1`), `crates/beamtalk-language-service` (completion, hover only),
`runtime/apps/beamtalk_runtime` (`beamtalk_shape_chain` and
`beamtalk_shape_migration` new; `beamtalk_hot_reload`,
`beamtalk_tagged_map`, `beamtalk_behaviour_intrinsics`,
`beamtalk_object_class`), `runtime/apps/beamtalk_workspace`
(`beamtalk_repl_loader` incl. `class_definition_text/7`,
`beamtalk_workspace_shape_store`, `beamtalk_shape_diff`, recheck),
`stdlib/src/behaviour.bt`, `stdlib/src/class_builder.bt`. **Not:** method
dispatch, expression codegen, the artifact build, `native:` classes.

**Deferred:** downgrade hooks (BT-3528); suspend-all-first ordering
(BT-3528's `release_handler`); a dispatch-time version guard for the
workspace window; Value-instance versioning or deep-reconcile on live
reload; cross-node wrapping policy (BT-3527); rename-aware envelope
resolution and a `migrateFromV*` guard in ADR 0114's rename validator; a
composed per-ancestor chain; automatic `local_call` lowering of ancestor
reuse; an ADR 0113-style hard gate on lossy reloads.

## References
- Related issues: [BT-3524](https://linear.app/beamtalk/issue/BT-3524)
  (this ADR); blocks [BT-3527](https://linear.app/beamtalk/issue/BT-3527)
  (distribution) and [BT-3528](https://linear.app/beamtalk/issue/BT-3528)
  (OTP releases); related [BT-3525](https://linear.app/beamtalk/issue/BT-3525)
  (slots / definite assignment — owns the "absent → declared initialiser
  policy" generalisation of reconcile step 3)
- Related ADRs: [ADR 0004](0004-persistent-workspace-management.md)
  (original `codeChange:` sketch), [ADR 0038](0038-subclass-classbuilder-protocol.md)
  (`ClassBuilder`), [ADR 0043](0043-sync-by-default-actor-messaging.md)
  (sync-by-default — why suspend-all-first deadlocks),
  [ADR 0056](0056-native-erlang-backed-actors.md) (`native:` actors — out of
  scope), [ADR 0067](0067-separate-state-field-keywords-by-class-kind.md)
  (class kinds), [ADR 0078](0078-actor-initialize-inheritance.md)
  (typed-slot post-initialize validation; `__skip_initialize__`),
  [ADR 0082](0082-method-level-edit-save-and-changelog.md)
  / [ADR 0084](0084-class-side-runtime-method-fun-dispatch.md) (per-method
  patching; recompile vs fun install), [ADR 0090](0090-array-canonical-representation.md)
  (`Array` tagged-map shape — pass-through in `pack/1`),
  [ADR 0100](0100-open-world-diagnostic-policy.md) (advisory severity),
  [ADR 0103](0103-sendability-typing-from-class-kinds.md) (tiers; `handleScope:`
  header-clause precedent), [ADR 0105](0105-live-image-recheck-on-reload.md)
  (shape re-check — `beamtalk_shape_diff`, `beamtalk_workspace_shape_store`,
  pre-save advisory, findings channel), [ADR 0109](0109-block-scoped-class-methods-run-blocks-in-the-caller.md)
  (BT-3047 amendment — class identity must be supplied, not read from the
  executing process; its call-site interception is *not* generalised here),
  [ADR 0113](0113-destructive-workspace-operations.md) (destructive-operation
  gating — the asymmetry §4 argues), [ADR 0114](0114-class-and-method-rename.md)
  (rename — interacts with the envelope's `Class` atom and with
  `migrateFromV*` selectors)
- Code: `runtime/apps/beamtalk_runtime/src/beamtalk_hot_reload.erl`,
  `runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl`
  (`local_call/3` — the in-caller class-method entry point; `#class_state.fields`),
  `runtime/apps/beamtalk_runtime/src/beamtalk_behaviour_intrinsics.erl`
  (`classAllFieldNames/1` — the flattened field list; `performLocally:withArguments:`),
  `runtime/apps/beamtalk_runtime/src/beamtalk_tagged_map.erl`,
  `runtime/apps/beamtalk_runtime/src/beamtalk_class_registry.erl`
  (`direct_subclasses/1`),
  `runtime/apps/beamtalk_workspace/src/beamtalk_repl_loader.erl`
  (`hot_reload_class/2`, `class_definition_text/7`),
  `runtime/apps/beamtalk_workspace/src/beamtalk_shape_diff.erl`,
  `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_shape_store.erl`,
  `crates/beamtalk-codegen/src/core_erlang/class_meta.rs`,
  `crates/beamtalk-codegen/src/core_erlang/gen_server/callbacks.rs`
  (`init_initialize_guarded_doc` — the 3-tuple), `stdlib/src/proto_object.bt`
  (`performLocally:withArguments:`), `stdlib/test/perform_locally_test.bt`
- Documentation: `docs/beamtalk-language-features.md` § Live Patching,
  `docs/beamtalk-ddd-model.md` § Hot Reload Context,
  `docs/development/architecture-principles.md` § Duplication & the
  Shared-Leaf-Module Pattern, `docs/development/surface-parity.md`
- External: [OTP Appup Cookbook](https://www.erlang.org/doc/system/appup_cookbook.html),
  [`sys:change_code/4`](https://www.erlang.org/doc/apps/stdlib/sys.html#change_code/4),
  [Ecto.Migration](https://hexdocs.pm/ecto_sql/Ecto.Migration.html),
  [Akka Persistence schema evolution](https://doc.akka.io/docs/akka/current/persistence-schema-evolution.html),
  [Gleam FAQ on hot code reloading](https://gleam.run/frequently-asked-questions/)
