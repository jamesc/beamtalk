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
`undefined`). Three properties of this path matter for the design:

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
   resume). BT-3528 will need that ordering anyway.

4. **The field list the reload path passes is local-only, and that is a
   pre-existing bug this ADR must not inherit.**
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
   live instances. `beamtalk_hot_reload_tests` has no inherited-field case,
   which is why this has gone unnoticed. The fix belongs to Phase 0
   regardless of the rest of this ADR: the reconcile step takes the
   **flattened** field list (`classAllFieldNames/1`'s semantics — it walks
   the superclass chain — rather than `instance_variables/1`), and gets the
   subclass regression test that is missing today.

Values (`Value subclass:` with `field:`) have no process, so hot reload never
touches a live Value; they are only ever *re-created* by new code. Their
versioning problem is entirely the persistence/distribution one: a Value
written to disk under shape *N* read back under shape *N+1*.

### Constraints

- **Verified syntax only.** Every form below is either an existing form
  (`classState:`-style declaration keywords, `class sel: arg => body` class
  methods, `#{k => v}` dictionaries, `at:put:` / `removeKey:` / `at:ifAbsent:`)
  or a single new declaration keyword with a precedent (`handleScope:`,
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
- The compiler must be able to read the version **statically**: it goes
  into `__beamtalk_meta`, the ADR 0105 shape fingerprint, and the
  unreachable-migration lint.

## Decision

Two additions to the language, one runtime contract, and one tooling rule.

### 1. `shapeVersion:` — an explicit, declared version

A class declares its shape version with a **declaration keyword line** in
the class body, conventionally first, before any `state:`/`field:` lines:

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
  and merges). Note this is *not* what the reload path passes today: see
  Current state ¶4. A subclass's chain consists
  of the `migrateFromVN:` methods *it* defines — inherited class methods
  are not pulled into a subclass's chain, because the two classes' version
  numbers are unrelated. To reuse an ancestor's step, call it explicitly
  (`Base migrateFromV1: old`); an abstract ancestor's `migrateFromVN:` is a
  helper for its subclasses, never run on its own (it has no instances).
  The cost — a superclass shape change must be answered by a bump in each
  concrete subclass — is caught by the §4 warning, which fingerprints the
  flattened shape.
- It parses wherever `state:`/`field:`/`classState:` may appear (it joins
  `is_state_like_declaration_keyword`'s set), so the unparser, LSP
  completion, and `ClassBuilder` (`shapeVersion:` builder keyword, ADR 0038)
  treat it as one more declaration.
- The compiler emits it into `__beamtalk_meta` as `'shape_version' => N`
  and the class registry answers `Cart shapeVersion` (a sealed `Behaviour`
  reader, default `1`).

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
  parameter; `N` is a positive integer. The compiler collects them into
  `__beamtalk_meta` as `'shape_migrations' => #{1 => 'migrateFromV1:', …}`.
  **This table is the Rust↔Erlang conformance mechanism**: the runtime never
  parses selector names, it reads the table codegen produced.
- A method with `N ≥ shapeVersion` is a **warning** (unreachable migration).
  Gaps are allowed — a missing step is a **no-op on the dictionary**; the
  structural fallback runs once, after the whole chain, against the final
  declared shape. Consequently a hook receives the raw dictionary as left
  by the hooks before it, and a key introduced at an earlier *un-hooked*
  step may be absent: read such keys with `at:ifAbsent:` or `includesKey:`
  (the `Session` example above).
- The body may not read or write `self.` slots or class variables
  (compile error). Migrations are **pure functions of `old`**: that is what
  makes them REPL-testable, dry-runnable, and safe to run outside the class
  process (below). Ordinary sends to other classes are allowed.
- The return type is `Dictionary`; on a `typed` class the annotation is
  required as for any other method.

The naming convention is Beamtalk's equivalent of Erlang's per-version
`code_change` clauses — one selector per version rather than one function
with pattern-matched heads — and, unlike a single dispatcher method or a
table of blocks, each step is an ordinary method: it has a doc comment, is
patched individually with `Cart class >> migrateFromV1: …` (ADR 0084),
appears in `senders`/xref, and is callable on its own at the REPL.

### 3. Runtime contract

**A new leaf module, `beamtalk_shape`** (runtime, below `beamtalk_hot_reload`
and any future persistence/distribution module), owns the whole rule:

```erlang
%% Run the chain from FromVersion to the class's current shapeVersion, then
%% reconcile against the declared fields. Fields is user fields only (no
%% internal keys). Pure; runs in the calling process.
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
   today's local-only `instance_variables/1` list (Current state ¶4): a
   declared field present in the dictionary is kept;
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
object's gen_server"). That matters for more than latency: it keeps a
migration from being serialised through, or deadlocking on, the class
process while that class is itself mid-reload.

`local_call/3`'s existing contract is also *why* the purity rule is a
compile error rather than a convention. It passes `nil` as ClassSelf and
`#{}` as the class variables, and discards any `{class_var_result, Value,
NewClassVars}` mutation the method returns. A migration that read a class
variable would therefore silently see an empty map, and one that wrote
would have the write silently dropped. The validator must reject both at
compile time so that contract is never reached by accident — the same
lesson ADR 0109's BT-3047 amendment drew for class identity (never read it
from the executing process; it must be supplied, not inferred).

**Not** a general "class methods run in the caller" rule: ADR 0109's Scope
explicitly excludes generalising its call-site interception, and this ADR
does not widen it. `local_call/3` is an existing, narrowly-contracted entry
point for exactly the "method does not touch class state" case, which the
purity rule enforces.

**Hot reload** (`beamtalk_hot_reload`):

- Actor state maps gain one internal key, `'__shape_version__'`, added to
  `beamtalk_tagged_map:internal_fields/0` (the single source of truth for
  internal keys — `fieldNames`, `user_field_keys/1`, and the reconcile step
  all filter through it). `init/1` writes the current version; absent means
  `1`.
- The `Extra` contract changes from `{NewInstanceVars, Module}` to a map:

  ```erlang
  #{module := atom(), fields := [atom()], shape_version := pos_integer()}
  ```

  `code_change(_OldVsn, State, Extra)` reads `V` from the state map (not
  from `OldVsn`, which stays ignored — the *state* is the source of truth,
  which is exactly what lets the same chain run for persistence, where
  there is no `OldVsn` at all), calls `beamtalk_shape:migrate/3` on
  `user_field_keys(State)`, and re-attaches the internal keys plus the new
  `'__shape_version__'`. The one caller (`hot_reload_class/2`) is updated in
  the same change; there is no transitional tuple clause.
- **On failure the actor stops** with exit reason
  `{shape_migration_failed, #beamtalk_error{}}` — non-`normal`, so the
  actor's `supervisionPolicy` decides (`#permanent`/`#transient` restart
  with fresh `init/1` state; `#temporary` stays down). Continuing with an
  old-shaped map under new code is the one outcome ruled out: it fails
  later, far from the cause, as `badkey`/DNU. `trigger_code_change/3` keeps
  collecting `{Pid, Reason}` failures and the reload surfaces them (§4).
- **Ordering becomes suspend → load → change_code → resume** for all
  instances of the class (OTP `release_handler` semantics). This closes the
  pre-existing window from Context and is what BT-3528's relup generation
  will rely on. Messages arriving during the suspension queue in the
  mailbox and are handled by the new code on migrated state; if the actor
  stopped, queued sync callers get the existing "actor exited" error path.

**Envelope** (`pack/1`, `unpack/1`) — the contract BT-3527 and the
persistence ADR consume:

- `{beamtalk_shape, Class, ShapeVersion, Fields}`: a tagged 4-tuple, trivially
  pattern-matched from Erlang/Elixir and `term_to_binary`-friendly
  (`Binary serialize:`). `Fields` holds user fields only; nested `Value`
  instances are packed recursively so each carries its own version.
- `pack/1` is defined only for **`Sendable`-tier** classes (ADR 0103). An
  instance whose class has a `SendableRef` (pid) or `HandleScoped` field
  returns `{error, #beamtalk_error{kind = not_serialisable}}` naming the
  field. Actors pack their *state* (they are `SendableRef` themselves; the
  envelope is what a store or a remote node receives, never a pid).
- `unpack/1` runs `migrate/3` from the envelope's version, so a store
  written under v1 reads back as v3 without the store knowing. Whether
  cross-node *messages* are wrapped, or versions negotiated per class at
  connect time, is BT-3527's decision; this ADR only guarantees that a
  wrapped term migrates.

### 4. Tooling

**Compile time** (class validators, `beamtalk-core`): the literal/duplicate
rules for `shapeVersion:`, the arity/purity rules for `migrateFromVN:`, and
the unreachable-migration warning (§2).

**Reload time** (extends ADR 0105's shape re-check, which already fires on
`state:`/`field:` changes): the language service keeps a per-class shape
**fingerprint** — the sorted `(field, declared type)` list over the
**flattened** field set, inherited fields included, since that is the
dictionary migrations see — alongside the signature-generation store. A
superclass that removes or retypes a field therefore changes the
fingerprint of every concrete subclass, and each one with live instances
that has not bumped its own `shapeVersion:` gets the warning below (the
diff is in a class the author may not be looking at, which is exactly why
the check is on the flattened shape). On reload the service emits, through
the existing reload-findings channel on every surface (LSP, workspace UI,
REPL):

| Reload observed | Finding |
|---|---|
| Field removed or retyped, `shapeVersion:` unchanged | **Warning** — `shape of Cart changed (dropped: #discount) but shapeVersion is still 2; 3 live instances will lose #discount — bump shapeVersion: and add class migrateFromV2:` |
| Fields only added with defaults, version unchanged | **Hint** — structural fallback applies; nothing to do |
| Version bumped N→N+1, no `migrateFromVN:` | **Hint** — `Cart shapeVersion 2 → 3 has no migrateFromV2:; structural fallback applies` |
| Version decreased | **Warning** |
| Any instance stopped by a failed migration | **Error** — it records a failure that has happened, not a prediction: `2 instances of Cart stopped: migrateFromV1: raised … (Cart class >> migrateFromV1:)` |

All advisory (ADR 0100); the reload proceeds. `Behaviour >> reload` keeps
its return value; the finding is the report.

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
`beamtalk_shape:migrate/3`; the shown reload notice is illustrative and is
confirmed with the REPL's existing display conventions at implementation.

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
  class migrateFromV1: old => old at: #total put: self.items sum
// ⛔ error: migration methods may not access slots (`self.items`);
//    migrations are pure functions of their argument
```

At reload, a hook that raises:

```
bt> Cart reload
=> Cart
⛔ reload: 1 instance of Cart stopped — migrateFromV1: raised
   does_not_understand: List>>summ (Cart class >> migrateFromV1:, cart.bt:9)
   supervisionPolicy #temporary: not restarted
```

## Prior Art

| System | Mechanism | Take / leave |
|---|---|---|
| **Erlang/OTP** | `code_change(OldVsn, State, Extra)` written by hand, one clause per `OldVsn`; `-vsn` attribute; appup `{advanced, Extra}`; `release_handler` suspends before loading | **Take** the per-version clause idea (one selector per step), the suspend→load→change→resume ordering, and `Extra` as the loader's channel. **Leave** `OldVsn` as the version source: it is a *module* attribute, and method-only changes must not bump a shape version. The state carries its own version. |
| **Elixir / Ecto** | Same `code_change`; Ecto migrations are numbered, ordered, one function each (`change/0`) | **Take** the "numbered, ordered, one function per step, gaps are fine" model. Ecto versions a *database*, not process state — the step shape transfers, the timestamp naming does not. |
| **Squeak / Pharo** | `ClassBuilder` migrates live instances on shape change by matching ivars by name, defaulting the rest (`updateInstancesFrom:`); serialised objects use `Object>>convertToCurrentVersion:refStream:` with a `varDict` of old ivars; Fuel has `FLMigration` | **Take** name-matching structural migration as the always-on fallback (it already is), and the "dictionary of old ivars" argument shape. **Adapt**: Pharo's hook is instance-side on a half-built object; on the BEAM a pure class-side function is dry-runnable and process-free. |
| **GemStone/S** | Versioned classes coexist (`ClassHistory`); `migrateFrom:instVarMap:` on the new instance; `migrateInstances` walks the repository | **Take** explicit versions and the instance-map hook. **Leave** class coexistence: BEAM has exactly two module versions, and reload migrates every instance eagerly. |
| **Gleam** | FAQ: hot reload is "the usual Erlang amount of safety"; no schema hook | The gap. |
| **Akka Persistence** | Schema evolution via `EventAdapter`s and serializer manifests; "additive changes free, everything else explicit" | **Take** the additive-is-free rule (our structural fallback + defaults). Akka versions *events*; we version *state*, which is the simpler problem. |
| **Protobuf / Avro** | Field numbers/defaults make additive evolution free; renames and retypes are explicit | Same rule, confirms it from the wire side — and why `pack/1` needs a version, not just a field list. |
| **Livebook / Jupyter** | No state migration: re-evaluate, re-derive | The "no image" persona; explains why the workspace must never *block* a reload on a shape change. |

## User Impact

- **Newcomer**: never sees this until they remove or rename a field on a
  class with live instances — then the reload finding tells them what to
  type (`bump shapeVersion:, add class migrateFromV2:`). `migrateFromV1:` is
  a dictionary in, dictionary out — no new concepts.
- **Smalltalk developer**: recognises Pharo's ivar-name matching as the
  fallback and `convertToCurrentVersion:` in the hook; may miss the
  instance-side form (see Steelman). Everything is a message send; nothing
  is a pragma.
- **Erlang/BEAM developer**: this *is* `code_change` with the clauses turned
  into selectors and the version kept in the state instead of `-vsn`. The
  envelope is a tagged tuple they can pattern-match. Suspend-before-load
  matches `release_handler`.
- **Operator**: a failed migration is a visible, supervised crash with a
  structured reason, not a silent old-shape process. `'__shape_version__'`
  is inspectable with `sys:get_state`, `observer`, and `recon`. Reload
  reports migrated/failed counts.
- **Tooling developer**: `shapeVersion:` is one more declaration keyword in
  the AST; `'shape_migrations'` in `__beamtalk_meta` is the only thing the
  runtime reads, so the LSP can offer `migrateFromV<N-1>:` as a completion
  when the version bumps and hover can show a class's chain.

## Steelman Analysis

### Version: header keyword (chosen) vs `class shapeVersion => 3` vs derived fingerprint

| Cohort | For `class shapeVersion => 3` | For a derived fingerprint |
|---|---|---|
| 🧑‍💻 **Newcomer** | "It's just a method — same as `supervisionPolicy`; I don't learn a keyword." | "I never have to remember to bump anything." |
| 🎩 **Smalltalk purist** | "Class-side `version` is a forty-year convention; declarations belong in methods." | — (Pharo has no shape version either) |
| ⚙️ **BEAM veteran** | "Overridable, so a subclass can compute it." | "Erlang already derives `vsn` from an MD5 when you don't declare one." |
| 🏭 **Operator** | "`Cart shapeVersion` works either way." | "Impossible to forget to bump." |
| 🎨 **Language designer** | "One fewer keyword." | "Zero syntax." |

Why the keyword still wins: the compiler needs the value *statically*
(meta, fingerprint, unreachable lint), which for a method means a
"must return an integer literal" rule — a method that isn't really a method.
The fingerprint fails the chaining requirement outright: you cannot write
`migrateFrom<hash>:`, and a purely additive change would produce a new
version with nothing to migrate. Its real strength — catching the forgotten
bump — is kept as the §4 warning.

### Hook: per-version methods (chosen) vs one table vs one dispatcher vs instance-side

| Cohort | `class shapeMigrations => #{1 => [:old \| …]}` | `class migrateShape: old from: v` | instance-side `migrateFromV1: old` |
|---|---|---|---|
| 🧑‍💻 **Newcomer** | "All the history in one place, in order." | "One method to find." | "I can just write `self.total := …`." |
| 🎩 **Smalltalk purist** | "Blocks are the Smalltalk way to defer code." | — | "This is `convertToCurrentVersion:`; migration belongs to the instance." |
| ⚙️ **BEAM veteran** | "Reads like an appup." | "This *is* `code_change/3` with `case`." | — |
| 🏭 **Operator** | "One diff hunk per release." | "Grep one selector." | — |
| 🎨 **Language designer** | "Data, not naming convention." | "No magic in selector names." | "Uniform with `initialize`." |

Why per-version methods still win: the table and the dispatcher put every
version's logic in one method body, so ADR 0082 method-level edit, ADR 0084
class-side `>>` patching, doc comments, and xref all lose their unit; the
dispatcher additionally forces `version = 1 ifTrue: [^…]` chains. The
instance-side form is the strongest rejected option — but it needs a
process (no dry-run, no REPL test on a Value), runs on a half-migrated
`self`, and gives actors (`self.x :=`) and values (`self withX:`) two idioms
for one feature. The convention cost ("magic name") is contained: the
compiler emits the table, so nothing but the validator ever knows the
naming rule.

### Failure: stop the actor (chosen) vs keep running with old state

- ⚙️ **BEAM veteran, for stopping**: "Let it crash; the supervisor is the
  policy."
- 🧑‍💻 **Newcomer, against**: "A typo in a migration killed my actor and its
  mailbox."
- Resolution: the alternative — new code on an old map — is a *later*,
  *less attributable* crash. Stopping now, with the method named in the
  reason and REPL-testable pure hooks to try first, is the honest choice.

### Tension points

- Smalltalk purists and newcomers lean instance-side; BEAM veterans and
  operators lean class-side/pure. Purity (dry-run, REPL test, no process)
  decided it.
- Whether the "shape changed without bump" finding should be `Error` rather
  than `Warning` on a *typed* class: ADR 0100 says advisory; revisit there
  if experience shows data loss in practice.

## Alternatives Considered

### Erlang-style manual `code_change` (`@native` actor or an Erlang override)
Write `code_change/3` by hand in a backing module. Rejected: not a language
surface, actor-only (no Values, no persistence), and it is exactly the
"can override" promise that has gone unused because it forces the author
out of Beamtalk.

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
This ADR decides two things: the migration language surface, and the
`{beamtalk_shape, Class, Version, Fields}` envelope. Hot reload — the only
consumer that exists today — needs the first and not the second: it
migrates a live map in place and never serialises anything. A narrower ADR
could ship §1–§2 plus `migrate/3`, and leave `pack`/`unpack` to the
persistence and distribution ADRs that actually consume them.

This is the most defensible scope reduction on offer, and it was rejected
for one reason: BT-3527 and BT-3528 are *blocked on this ADR* precisely for
the envelope, and defining it separately in each would duplicate the
version/tier rule across two documents — the exact shared-leaf failure
this project's architecture principles name. The envelope is also small
(a tagged tuple plus a tier check) and, more importantly, its existence
constrains the design above it: `migrate/3` takes a version and a plain
field map *because* an envelope must be able to call it with no `OldVsn`
and no live process. Designing the chain without that constraint risks a
hook shape that only works for hot reload. It stays, and Phase 1 ships it
with EUnit but no production consumer.

### Single dispatcher, table of blocks, instance-side hook
See Steelman Analysis.

## Consequences

### Positive
- One feature serves hot reload today and unblocks BT-3527 (distribution)
  and BT-3528 (releases) plus persistence, with one runtime leaf module.
- Existing classes are untouched: absent `shapeVersion:` is v1, and the
  structural fallback is unchanged behaviour.
- Migrations are pure, REPL-testable, individually patchable methods.
- The load/suspend window in today's reload path is closed as a side
  effect, and the ordering BT-3528's relup needs is established.
- The Rust↔Erlang rule lives in one place (`'shape_migrations'` emitted by
  codegen), satisfying the shared-leaf rule without a "keep in sync"
  comment.

### Negative
- A new declaration keyword: parser, AST, unparser, `ClassBuilder`, LSP
  completion, meta, class registry — ADR 0103's `handleScope:` precedent
  says budget ~M, not S.
- A failed migration now *stops* an actor where today it is logged and
  skipped. This is deliberate but is a behaviour change for the workspace.
- `Extra` changes shape; `beamtalk_hot_reload_tests` and the one caller are
  updated together.
- Live **Values of an old shape** (held in a binding or another actor's
  state across a reload of the Value's class) are not migrated — same as
  today, now documented. Follow-up: either version Value instances or
  deep-reconcile actor state on reload of an embedded Value class.
- Downgrades run the structural fallback only; if BT-3528 needs symmetric
  hooks, `migrateToVN:` is reserved for it.
- Migrations that need class variables or other process-bound context
  cannot be written; the purity rule is a real restriction, accepted for
  dry-runnability.
- Chains are per concrete class, so a superclass shape change is answered
  once per subclass (each calling the shared `Base migrateFromVN:` helper).
  A composed per-ancestor chain, with `'__shape_version__'` becoming a
  per-class map, is the recorded refinement if hierarchies with many
  stateful subclasses make this tedious in practice.
- Hooks see the raw dictionary: a key introduced at an earlier un-hooked
  step is absent until the final reconcile, so hooks reading such keys
  must guard. The alternative — reconciling after every step — is not
  available, because only the final shape is declared.
- **A restart is not a migration.** When a `#permanent`/`#transient` actor
  is stopped by a failed migration, its supervisor restarts it with fresh
  `init/1` state — the process comes back healthy and its previous state is
  gone. That reads as success to anything watching liveness, so the §4
  `Error` finding and the exit reason are the only signal that data was
  lost; operators should alert on them rather than on process liveness.
  A fleet of instances failing the same buggy hook also restarts as a
  group, which the supervisor's restart intensity may escalate into a
  wider shutdown — the usual OTP behaviour, but newly reachable from a
  one-line typo in a migration.
- **Class rename interacts with the envelope.** The envelope keys on the
  class *atom*, so a class renamed via ADR 0114 makes previously packed
  terms unreadable — the persistence and distribution ADRs will need a
  rename-aware resolution step (an alias table, or storing the renamed-from
  atom). Nothing in hot reload is affected, since it never unpacks.
  Recorded in Deferred, and flagged to ADR 0114.

### Neutral
- Codegen of methods and dispatch is unchanged; `code_change/3` still
  delegates to `beamtalk_hot_reload`.
- `OldVsn` stays ignored; a module `-vsn` (if BT-3528 emits one) is a
  release concern, not a shape concern.
- No new severity levels — findings slot into ADR 0100.

## Migration Path

**No `.bt` source changes are required, anywhere.** An absent
`shapeVersion:` is v1 and the structural fallback is unchanged, so every
existing class in the stdlib, the tests, and downstream projects keeps
compiling and reloading exactly as today. There is no deprecation period
because nothing is deprecated.

Two internal changes do need coordinated updates, both inside this repo:

| Change | Who updates | When |
|---|---|---|
| `Extra` becomes a map (`{NewInstanceVars, Module}` → `#{module, fields, shape_version}`) | `beamtalk_hot_reload`, its one caller `hot_reload_class/2`, `beamtalk_hot_reload_tests` | Phase 1, one commit — no transitional tuple clause, since the only caller is in-tree |
| Failed migration stops the actor instead of being logged and skipped | workspace reload reporting; any test asserting the old log-and-continue behaviour | Phase 1 |

The inherited-field fix (Current state ¶4) is a **behaviour change users
will notice and want**: a subclass with live instances stops silently
losing its inherited fields on reload. It is called out here rather than
buried because anything that came to depend on the old dropping behaviour
(nothing in-tree does) would see different state after reload.

## Implementation

Phases sized for `/plan-adr`:

0. **Phase 0 — napkin / walking skeleton (S, and this is the risky part):**
   one actor class, one hand-written `class_migrateFromV1:` function, one
   live instance, through a *real* reload — proving the three assumptions
   the rest of the design rests on, before any of it is built:
   (a) `beamtalk_object_class:local_call/3` can invoke a class-side method
   from inside `code_change/3` **while that class's own module is being
   reloaded** (the one ordering the existing `local_call/3` callers never
   exercise); (b) `'__shape_version__'` can join
   `beamtalk_tagged_map:internal_fields/0` without disturbing its existing
   consumers (`fieldNames`, `user_field_keys/1`, `printString`, the
   Inspector); (c) the suspend → load → change → resume reorder leaves the
   existing reload path green. No keyword, no validators, no envelope. If
   (a) fails, the "pure class-side function" hook is the wrong shape and
   the ADR needs revisiting — which is precisely why this is not folded
   into Phase 1. **Tests:** `beamtalk_hot_reload_tests` +
   one REPL-protocol case.
1. **Phase 1 — runtime leaf (M):** `beamtalk_shape` with `migrate/3`,
   `pack/1`, `unpack/1`, reconcile moved out of `beamtalk_hot_reload` and
   corrected to the **flattened** field list (Current state ¶4), with the
   subclass regression test that is missing today; new `Extra` map
   contract; stop-on-failure exit reason. Reads
   `'shape_version'`/`'shape_migrations'` from `__beamtalk_meta`
   (absent → `1` / `#{}`), so it works before the compiler emits them.
   **Tests:** EUnit for chain order, gaps, downgrade, idempotency,
   typed-slot failure, inherited-field preservation, `not_serialisable`.
2. **Phase 2 — language surface (M):** `shapeVersion:` keyword (parser,
   `ClassDefinition`, unparser, `ClassBuilder shapeVersion:`), validators
   (literal, duplicate, `migrateFromVN:` arity/purity/unreachable), meta
   emission of `'shape_version'` and `'shape_migrations'`, `Behaviour
   shapeVersion` and `migrateShape:from:`, `init/1` writing the version.
   **Tests:** parser/unparse round-trip, validator diagnostics, codegen
   meta snapshot.
3. **Phase 3 — reload integration (M):** migration invocation via
   `local_call/3` generalised from the Phase 0 skeleton, post-migration
   typed validation reuse (ADR 0078 path), reload result reporting of
   migrated/failed counts, `docs/development/surface-parity.md` entry.
   **Tests:** REPL-protocol reload cases (success, hook raises, typed slot
   left unset).
4. **Phase 4 — tooling (M):** shape fingerprint in the ADR 0105 signature
   store; the five findings in §4 on all surfaces; LSP completion for
   `migrateFromV<N-1>:` after a bump; hover showing the chain.
   **Tests:** language-service finding tests per row of the §4 table.
5. **Phase 5 — docs and e2e (S):** `beamtalk-language-features.md` § Live
   Patching gains a *Shape Versioning* section; REPL-protocol e2e extending
   the existing `hot_counter.bt` / `hot_counter_v2.bt` fixtures to a
   v1 → v2 → v3 chain with a failing step; BUnit tests for
   `migrateShape:from:` on a Value; status flip.

**Affected:** `crates/beamtalk-core` (parser, AST, validators, unparse),
`crates/beamtalk-codegen` (`class_meta.rs`, `init/1`),
`crates/beamtalk-language-service` (completion, hover, recheck findings),
`runtime/apps/beamtalk_runtime` (`beamtalk_shape` new, `beamtalk_hot_reload`,
`beamtalk_tagged_map`, `beamtalk_actor`), `runtime/apps/beamtalk_workspace`
(`beamtalk_repl_loader`, recheck), `stdlib/src/behaviour.bt`. **Not:**
method dispatch, expression codegen, the artifact build.

**Deferred:** downgrade hooks (BT-3528); Value-instance versioning or
deep-reconcile on live reload; cross-node wrapping policy (BT-3527);
rename-aware envelope resolution (ADR 0114 × the envelope's `Class` atom —
for whichever of persistence/BT-3527 unpacks first).

## References
- Related issues: [BT-3524](https://linear.app/beamtalk/issue/BT-3524)
  (this ADR); blocks [BT-3527](https://linear.app/beamtalk/issue/BT-3527)
  (distribution) and [BT-3528](https://linear.app/beamtalk/issue/BT-3528)
  (OTP releases); related [BT-3525](https://linear.app/beamtalk/issue/BT-3525)
  (slots / definite assignment — owns the "absent → declared initialiser
  policy" generalisation of reconcile step 3)
- Related ADRs: [ADR 0004](0004-persistent-workspace-management.md)
  (original `codeChange:` sketch), [ADR 0067](0067-separate-state-field-keywords-by-class-kind.md)
  (class kinds), [ADR 0078](0078-actor-initialize-inheritance.md)
  (typed-slot post-initialize validation), [ADR 0082](0082-method-level-edit-save-and-changelog.md)
  / [ADR 0084](0084-class-side-runtime-method-fun-dispatch.md) (per-method patching),
  [ADR 0100](0100-open-world-diagnostic-policy.md) (advisory severity),
  [ADR 0103](0103-sendability-typing-from-class-kinds.md) (tiers; `handleScope:`
  keyword precedent), [ADR 0105](0105-live-image-recheck-on-reload.md)
  (shape re-check, findings channel), [ADR 0109](0109-block-scoped-class-methods-run-blocks-in-the-caller.md)
  (BT-3047 amendment — class identity must be supplied, not read from the
  executing process; its call-site interception is *not* generalised here),
  [ADR 0114](0114-class-and-method-rename.md) (rename — interacts with the
  envelope's `Class` atom)
- Code: `runtime/apps/beamtalk_runtime/src/beamtalk_hot_reload.erl`,
  `runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl`
  (`local_call/3` — the in-caller class-method entry point; `#class_state.fields`),
  `runtime/apps/beamtalk_runtime/src/beamtalk_behaviour_intrinsics.erl`
  (`classAllFieldNames/1` — the flattened field list),
  `runtime/apps/beamtalk_runtime/src/beamtalk_tagged_map.erl`,
  `runtime/apps/beamtalk_workspace/src/beamtalk_repl_loader.erl`
  (`hot_reload_class/2`), `crates/beamtalk-codegen/src/core_erlang/class_meta.rs`
- Documentation: `docs/beamtalk-language-features.md` § Live Patching,
  `docs/beamtalk-ddd-model.md` § Hot Reload Context,
  `docs/development/architecture-principles.md` § Duplication & the
  Shared-Leaf-Module Pattern
- External: [OTP Appup Cookbook](https://www.erlang.org/doc/system/appup_cookbook.html),
  [Ecto.Migration](https://hexdocs.pm/ecto_sql/Ecto.Migration.html),
  [Akka Persistence schema evolution](https://doc.akka.io/docs/akka/current/persistence-schema-evolution.html),
  [Gleam FAQ on hot code reloading](https://gleam.run/frequently-asked-questions/)
