# ADR 0057: Authoritative Superclass Source for Abstract Stdlib Classes

## Status
Accepted (2026-03-07)

## Summary

**Phase 1 (this ADR):** Fix `apply_class_info/2` to update the `superclass` field
from `__beamtalk_meta/0` when the compiled module loads. One field, targeted fix,
restores correctness immediately.

**Phase 5 (follow-up):** Migrate static structural metadata out of the class
gen_server entirely, making `__beamtalk_meta/0` the sole source for immutable facts
and the gen_server responsible only for mutable runtime state (live method table,
class variables, hot-patch state). `superclass` happens to be the only field that
is currently wrong — but the architectural direction is to stop duplicating static
metadata in process state at all.

## Context

### The Problem

Every Beamtalk class gen_server holds a `superclass` field set at `init/1` time and
never updated by `apply_class_info/2`. For user-defined classes (those compiled from
`.bt` source) this is fine: the compiled module's `register_class/0` on-load hook
calls `update_class` which calls `apply_class_info`, and the superclass passed by the
compiler in `ClassInfo` is always correct.

For the **abstract stdlib classes** (`ProtoObject`, `Object`, `Behaviour`, `Class`,
`Metaclass`) the picture is different:

1. Bootstrap stubs (`beamtalk_protoobject_bt.erl`, `beamtalk_class_bt.erl`,
   `beamtalk_behaviour_bt.erl`, etc.) register these classes early — before compiled
   stdlib BEAM modules are loaded — so they use *placeholder* superclass values to
   satisfy the class registry before anything else starts.

2. `beamtalk_class_bt` registers `'Class'` with `superclass => 'Object'`.
   `beamtalk_behaviour_bt` registers `'Behaviour'` with `superclass => 'Object'`.
   The actual hierarchy (`Class → Behaviour → Object`) is only declared in the
   compiled stdlib sources.

3. When the compiled stdlib modules load (e.g. `bt@stdlib@class.beam`), their
   on-load hook calls `update_class → apply_class_info`. `apply_class_info` updates
   fields, methods, and flags — but **not** the gen_server `superclass` field.

4. The gen_server therefore permanently holds the bootstrap stub's placeholder
   superclass, even after the compiled module is loaded and running.

5. Any hierarchy traversal that relies on `gen_server:call(Pid, superclass)` — most
   importantly `walk_hierarchy/3` in `beamtalk_behaviour_intrinsics.erl` — produces
   incorrect results for these classes. `walk_hierarchy('Class', ...)` follows
   `Class → Object → ProtoObject` instead of the correct
   `Class → Behaviour → Object → ProtoObject`, silently skipping `Behaviour` and
   all the protocol methods it defines (`reload`, `superclass`, `allMethods`, etc.).

### Symptom Trail

BT-1169 (`Counter class allMethods` returning wrong results) and the parallel fix to
`metaclassSuperclass/1` are both direct consequences. Both required a
`superclass_name_from_meta_or_state/1` workaround that prefers `__beamtalk_meta/0`
over the stale gen_server field. Without a root-cause fix, every new intrinsic that
traverses the hierarchy will need the same workaround.

### The Two Sources of Truth

After the compiled stdlib loads, every abstract stdlib class has two representations
of its superclass:

| Source | Value for `'Class'` | When set | Reliability |
|--------|--------------------|-----------|----|
| `gen_server #class_state.superclass` | `'Object'` (stale) | Bootstrap stub `init/1` | Wrong for abstract stdlib classes |
| `bt@stdlib@class:'__beamtalk_meta'()` | `'Behaviour'` (correct) | Compiled into the BEAM module | Always correct |

`__beamtalk_meta/0` is the **canonical metadata source** established by ADR 0050.
The gen_server field was never intended to diverge from it.

### Why `apply_class_info` Skips `superclass`

The omission was deliberate at the time (ADR 0032): the superclass of a class is
immutable post-definition, so there was no reason to update it on hot reload.
What was not anticipated was the bootstrap ordering gap — bootstrap stubs registering
with incorrect placeholder superclasses that the compiled stdlib would later correct.

### Constraints

- **No change to class semantics**: Superclass relationships are still immutable
  post-definition. This ADR only fixes stale data, not the model.
- **Dynamic classes** (created via `beamtalk_class_builder`, no compiled module)
  never have a `__beamtalk_meta/0`. The gen_server `superclass` field must remain
  the primary source for them — the fix must not break this.
- **on_load ordering**: `erlang:function_exported/3` returns false during on_load.
  ADR 0050 already solved this by passing `meta` inline in `ClassInfo`. The same
  mechanism must be used here.
- **`beamtalk_class_hierarchy_table`**: The ETS hierarchy table is written at
  `init/1` time from `ClassInfo`. It also needs to be kept in sync.

## Decision

Patch `apply_class_info/2` in `beamtalk_object_class.erl` to update the `superclass`
field when `Meta` explicitly provides a `superclass` entry (i.e. via
`__beamtalk_meta/0` in `ClassInfo`). Simultaneously update the ETS hierarchy table
entry.

The logic:

```erlang
%% In apply_class_info/2 — derive updated superclass from Meta:
NewSuperclass =
    case maps:find(superclass, Meta) of
        error      -> State#class_state.superclass; %% key absent — keep existing
        {ok, nil}  -> none;                         %% root class (codegen emits 'nil')
        {ok, S}    -> S                             %% corrected value from compiled module
    end,
%% Keep ETS hierarchy table in sync:
beamtalk_class_hierarchy_table:insert(State#class_state.name, NewSuperclass),
```

And in the returned `#class_state{}`:

```erlang
State#class_state{
    ...
    superclass = NewSuperclass,
    ...
}
```

This ensures that when `bt@stdlib@class.beam` loads and its on-load hook calls
`update_class('Class', ClassInfo)`, `apply_class_info` overwrites the bootstrap
stub's `'Object'` with the correct `'Behaviour'`.

After this change:

- `gen_server:call(ClassPid, superclass)` returns the correct value for all classes.
- `walk_hierarchy/3` becomes correct for all callers without modification.
- `beamtalk_object_class:superclass/1` is correct for all callers.
- The `superclass_name_from_meta_or_state/1` workarounds in
  `beamtalk_behaviour_intrinsics.erl` can be removed (replaced by direct
  `gen_server:call(Pid, superclass)`).
- No new intrinsic will need the same workaround.

### What Changes

| Component | Change | Phase |
|-----------|--------|-------|
| `beamtalk_object_class.erl` | `apply_class_info/2` — update superclass from Meta; update ETS table | 1 |
| `beamtalk_object_class_tests.erl` | Regression test: update_class corrects stale superclass | 3 |
| `beamtalk_behaviour_intrinsics.erl` | Remove `superclass_name_from_meta_or_state/1`; revert `metaclassSuperclass/1` to direct gen_server call | 2 |
| `beamtalk_behaviour_intrinsics.erl` | Replace `collect_instance_methods_via_meta/3` with `walk_hierarchy` call | 4 |
| `beamtalk_behaviour_intrinsics_tests.erl` | Update tests for removed workaround helpers | 2, 4 |

### REPL Verification

After the change, the following must hold:

```beamtalk
Counter class allMethods includes: #reload     // => true
Counter class allMethods includes: #superclass // => true
Counter class allMethods includes: #new        // => true

Counter class class superclass == Actor class class        // => true
Actor class class superclass == Object class class         // => true
Object class class superclass == ProtoObject class class   // => true
```

And in Erlang (observable via `sys:get_state/1`):

```erlang
Pid = beamtalk_class_registry:whereis_class('Class'),
State = sys:get_state(Pid),
'Behaviour' = State#class_state.superclass.   %% was 'Object' before this fix
```

## Prior Art

### Smalltalk / Pharo

In Pharo, the metaclass tower is fully self-describing — class objects introspect
their own hierarchy via live Smalltalk message sends rather than reading from a
separate process state. There is no equivalent of a "bootstrap stub superclass" — the
image loads all class definitions simultaneously. The problem does not exist in Pharo.

### Erlang OTP Hot Reload

Erlang's standard pattern for hot code reloading (`code_change/3`) explicitly updates
gen_server state to match the new module version. The principle: **process state must
match the module it runs**. The current Beamtalk bootstrap approach violates this by
allowing process state to diverge after a module upgrade (bootstrap stub → compiled
stdlib transition). This ADR restores OTP alignment: when the compiled module loads
and calls `update_class`, the resulting state is fully in sync with the module.

### Elixir Module Attributes

Elixir stores structural metadata (module attributes, type specs, behaviour
declarations) directly in the compiled `.beam` module via `__info__/1`. There is no
separate process holding a copy of this data. When a module is recompiled and
hot-reloaded, the new `__info__/1` is immediately authoritative. This is the same
pattern as `__beamtalk_meta/0` — the compiled module is the source of structural
truth. Beamtalk diverges by *also* caching this in a gen_server, which creates
the dual-source problem. Phase 5 (Option C) would align Beamtalk with Elixir's
model: compiled module for static facts, process state only for mutable runtime data.

### Erlang ETS as Authoritative Store

A common Erlang pattern is to treat ETS as the authoritative store and gen_server
state as a write-through cache. The `beamtalk_class_hierarchy_table` already plays
this role for class lookups. Keeping the ETS table and gen_server in sync follows
that pattern.

## User Impact

**Newcomer**: Transparent — they observe that `Counter class allMethods` returns
the expected Behaviour protocol methods and do not need to know why it previously
failed.

**Smalltalk developer**: Correct hierarchy traversal matches Smalltalk expectations.
`Counter class allMethods` including `reload` and `superclass` matches Pharo's
behaviour. Removes a surprising gap.

**Erlang/BEAM developer**: `sys:get_state/1` on a class process now shows the
correct superclass. Standard BEAM observability tools (`observer`, `sys`) tell the
truth. The OTP `code_change` pattern is honoured.

**Production operator**: No user-visible change. The fix is internal to the runtime
bootstrap sequence and completes before any user code runs.

**Tooling developer**: `beamtalk_object_class:superclass/1` is now reliable for all
classes. The LSP and compiler server can trust gen_server hierarchy data without
cross-referencing `__beamtalk_meta/0`.

## Steelman Analysis

### Option A (Chosen): Patch `apply_class_info` to update `superclass`

- **Newcomer**: "Fix the data. I want `allMethods` to work — don't scatter workarounds."
- **Smalltalk purist**: "The class process state *is* the class definition. If it
  says the wrong superclass, it's lying. Fix the lie."
- **BEAM veteran**: "The correct OTP pattern — `code_change` updates state to match
  the new module. Process state must be authoritative."
- **Operator**: "`sys:get_state` and `observer` tell the truth. I can trust the
  runtime."
- **Language designer**: "One source of truth. `apply_class_info` is the right seam —
  one targeted change fixes all callers."

### Option B (Rejected): Meta-aware `walk_hierarchy`

Change `walk_hierarchy/3` to use `superclass_name_from_meta_or_state` per hop.

- **Newcomer**: "Works, but I don't care about internals."
- **Smalltalk purist**: "Band-aid — the process state still lies."
- **BEAM veteran**: "This is the zero-risk option. `apply_class_info` runs during
  bootstrap on every startup — touching it means touching the most sensitive
  initialization code in the runtime. Option B is purely additive: `walk_hierarchy`
  gets a smarter lookup, nothing in the init path changes. If bootstrap ordering
  breaks in a subtle way, we haven't touched it."
- **Operator**: "`sys:get_state` still shows the wrong superclass. Confusing."
- **Language designer**: "Fixes the symptom in one place but leaves the root
  inconsistency. Every new caller of `gen_server:call(Pid, superclass)` will hit the
  same trap. Does not scale."

**Rejected** because it perpetuates stale state and requires all future callers to
defend against it independently. The BEAM veteran's risk concern is real but
addressed by the `error` guard in Option A's `maps:find/2` pattern: `apply_class_info`
only overwrites `superclass` when `Meta` contains the key. If Meta is absent or the
key is not present, the existing value is preserved unchanged — the init path is not
affected.

### Option C (Planned Phase 2): `__beamtalk_meta/0` as sole source for static metadata

Remove static structural fields from `#class_state{}` entirely. The gen_server
becomes responsible only for mutable runtime state: live method table, class
variables, hot-patch state. All structural queries (`superclass`, `is_sealed`,
`fields`, etc.) read directly from `__beamtalk_meta/0`. Dynamic classes (no compiled
module) use a separate lightweight in-memory store populated at `register_class` time.

- **Newcomer**: "No visible difference."
- **Smalltalk purist**: "More principled — the compiled module *is* the class."
- **BEAM veteran**: "Option A just moves the problem — you still have a latent
  `code_change` hazard. If a class gen_server process survives a hot-reload where the
  compiled module changes its superclass, `apply_class_info` runs again and re-fixes
  it. But if `apply_class_info` doesn't run for any reason (e.g. a class registered
  dynamically before the module loaded, or a future bootstrap ordering change), you're
  back to stale state with no warning. Option C makes stale state structurally
  impossible — the process never holds the data, so there is nothing to go stale."
- **Language designer**: "This is the destination. Option A is the first step — it
  makes gen_server state correct. Option C makes gen_server state *minimal*. The
  right sequence: fix correctness now, migrate to cleanliness later."

**Planned Phase 2**: `superclass` is the only field that is currently wrong, so
Option A fixes all active bugs. Option C is the follow-up architectural migration
once the gen_server's role as mutable-state-only process is fully defined. The two
phases are independent — Option C can proceed whenever the codebase is ready, without
blocking on any other work.

### Tension Points

- **Option B vs Option A (bootstrap risk)**: The strongest case for B over A is
  bootstrap safety — Option A touches `apply_class_info` which runs during startup,
  Option B is purely additive. Option A wins because the `error` guard in `maps:find/2` makes the
  change safe-by-default, and Option B's per-hop `__beamtalk_meta/0` calls leave
  `sys:get_state` permanently wrong.
- **Option C vs Option A (structural impossibility)**: The strongest case for C over
  A is that Option A is still process-state-based — a future bootstrap change could
  reintroduce stale state with no compiler warning. Option C makes it structurally
  impossible. Option A wins for now because C's scope (auditing all 17+ callers of
  `superclass/1`, defining a dynamic-class fallback) exceeds what's needed to fix
  the active bugs. Option C is the planned Phase 5 follow-up precisely because this
  argument is valid — the goal is to get there, just incrementally.
- Language designers and cautious BEAM veterans both land on A+C sequenced: fix the
  data now, eliminate the data duplication later.

## Alternatives Considered

### Option B: Meta-aware `walk_hierarchy`

See Steelman Analysis above. Rejected because it perpetuates stale state and does not
scale: every new hierarchy-traversal intrinsic must independently defend against the
inconsistency.

### Option C: Remove static metadata from gen_server (Phase 2)

See Steelman Analysis above. Not rejected — planned as a follow-up architectural
migration once Option A restores correctness. `superclass` is the only field
currently wrong; Option A fixes the active bug. Option C then cleans up the
remaining ~9 static fields that are duplicated between `#class_state{}` and
`__beamtalk_meta/0`. The two phases are independent and sequenced deliberately:
correctness first, architectural cleanliness second.

### Do Nothing (per-site workarounds)

Continue adding `superclass_name_from_meta_or_state` call sites as new intrinsics
need hierarchy traversal. Already rejected in BT-1169 — two sites existed and a
third was anticipated before the issue closed.

## Consequences

### Positive

- `gen_server:call(Pid, superclass)` is correct for all registered classes after
  stdlib loads.
- `walk_hierarchy/3`, `beamtalk_object_class:superclass/1`, and all
  hierarchy-traversal code work correctly without per-site workarounds.
- `sys:get_state/1` on any class process reflects the correct hierarchy.
- `superclass_name_from_meta_or_state/1` and its call sites are removed — net
  code reduction.

### Negative

- `apply_class_info/2` becomes slightly more complex (one new field assignment and
  one ETS write).
- A small ordering risk: if `Meta` is empty (no `__beamtalk_meta/0` available),
  `apply_class_info` must leave `superclass` unchanged. Handled by the `error` clause
  from `maps:find/2` — the existing value is preserved when the key is absent.

### Neutral

- Bootstrap stubs (`beamtalk_class_bt.erl`, `beamtalk_behaviour_bt.erl`) retain
  their placeholder superclass values — they are still needed for the window between
  bootstrap stub registration and compiled module load.
- Hot-reload of user-defined classes is unaffected: their bootstrap superclass was
  always correct (set by the compiler, not a stub).
- `superclass` is the only field currently set incorrectly by bootstrap stubs. The
  other ~9 static fields duplicated between `#class_state{}` and `__beamtalk_meta/0`
  are correct but redundant — cleaning them up is the Phase 5 (Option C) migration.

## Implementation

### Phase 1 — Fix `apply_class_info` (beamtalk_object_class.erl)

1. After computing `Meta` in `apply_class_info/2`, derive `NewSuperclass`:
   - `maps:find(superclass, Meta)` returns `error` (key absent) → keep `State#class_state.superclass`
   - Returns `{ok, nil}` → root class (codegen emits `nil`); normalise to `none`
   - Returns `{ok, S}` → use `S` (overrides the bootstrap stub value)
2. Call `beamtalk_class_hierarchy_table:insert(State#class_state.name, NewSuperclass)`.
3. Include `superclass = NewSuperclass` in the returned `#class_state{}`.

### Phase 2 — Remove Workarounds (beamtalk_behaviour_intrinsics.erl)

1. Remove `superclass_name_from_meta_or_state/1`.
2. Replace its two call sites in `metaclassSuperclass/1` with direct
   `gen_server:call(Pid, superclass)`.
3. Update the EUnit test that exercises the workaround path.

### Phase 3 — Add Regression Test (beamtalk_object_class_tests.erl)

Add a test that:
- Registers a class with a placeholder superclass (simulating bootstrap stub behaviour).
- Calls `update_class` with a `ClassInfo` carrying `meta => #{superclass => 'Behaviour'}`.
- Asserts `gen_server:call(Pid, superclass)` returns `'Behaviour'`.
- Asserts `beamtalk_class_hierarchy_table:lookup(ClassName)` returns `{ok, 'Behaviour'}`.

Run `just test` and `just test-stdlib` to verify the metaclass tower tests pass
without the workaround.

### Phase 4 — Simplify `collect_instance_methods_via_meta`

With `walk_hierarchy/3` now producing correct results for all classes,
`collect_instance_methods_via_meta/3` in `beamtalk_behaviour_intrinsics.erl` can be
replaced by a `walk_hierarchy` call. This helper was introduced specifically to bypass
the stale gen_server superclass; after Phase 1, its raison d'etre is gone. The
method-collection logic can use the same `walk_hierarchy` + `gen_server:call` pattern
as `metaclassClassMethods` and other intrinsics.

### Phase 5 (follow-up ADR) — Migrate Static Metadata out of gen_server

Once Phases 1-4 land and correctness is established, a follow-up ADR covers Option C:

- Define the gen_server's role as **mutable runtime state only**: live method table
  (with hot-patch deltas), class variables, runtime-added docs.
- Remove static fields from `#class_state{}`: `superclass`, `is_sealed`,
  `is_abstract`, `fields`, `method_return_types`, `class_method_return_types`,
  `method_signatures`, `class_method_signatures`, `doc` (~9 fields of 17).
- All structural queries read from `__beamtalk_meta/0` directly (or a thin ETS
  cache over it for dynamic classes).
- Dynamic classes (no compiled module) populate the ETS cache at `register_class`
  time from `ClassInfo` — same data, different source.

Phase 5 is tracked separately. It does not block Phases 1-4.

## References

- Related issues: BT-1169 (immediate trigger — `Counter class allMethods` bug)
- Related ADRs:
  - ADR 0007: Compilable Standard Library with Primitive Injection
  - ADR 0032: Early Class Protocol (introduced `apply_class_info`)
  - ADR 0036: Full Metaclass Tower
  - ADR 0050: Incremental Compiler Class Hierarchy via BEAM Metadata Streaming
    (established `__beamtalk_meta/0` as canonical metadata source)
- Bootstrap stubs: `beamtalk_class_bt.erl`, `beamtalk_behaviour_bt.erl`
- Workaround introduced in: BT-1169 PR #1223 (`superclass_name_from_meta_or_state/1`)
