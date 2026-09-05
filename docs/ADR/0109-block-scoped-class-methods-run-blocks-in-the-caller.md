# ADR 0109: Block-Scoped Class Methods Run Their Block in the Caller's Process

## Status
Implemented (2026-07-28) — base decision (BT-3018) and the BT-3047 amendment shipped; BT-3020 (handle-ownership follow-up) partial

## Implementation Tracking

**Issues:** BT-3018 (class-process block execution) · BT-3020 (handle ownership/leak, partially)
**Related:** BT-2975 (FileHandle incremental I/O — surfaced the problem), BT-3019 (unbounded `readAll` — same blast radius)
**Amendment (Accepted, 2026-08-05):** BT-3047 — class self-identity resolution for self-sends and instantiation intrinsics inside a block that executes in a foreign class's process. See the Amendment section below for the accepted decision and implementation plan.

## Context

### Problem statement

A class-side message send does not run in the caller's process. `beamtalk_class_dispatch:class_send_dispatch/3` performs a `gen_server:call(ClassPid, {class_method_call, Selector, Args, Ctx}, 60000)`, so the method body executes inside the singleton gen_server for that class.

That is fine for `File readAll:` — a short, self-contained operation. It is not fine when the class method takes a **Block**, because the *user's* block then runs inside the class process too. `File open:do:` has always had this shape; BT-2975 added `File open:mode:do:` and made it much easier to reach, because the whole point of that API is a loop of writes with an fsync per record.

Three distinct problems fall out, all currently mitigated only by documentation:

**1. Deadlock on any re-entrant `File` send.** The block cannot message `File` again — it is already inside that process:

```beamtalk
File open: "a.txt" mode: #read do: [:h | File exists: "b.txt"]
```

fails with `{calling_self, {gen_server, call, [<0.138.0>, {class_method_call, 'exists:', ...}, 60000]}}` — a raw gen_server tuple, not a `#beamtalk_error{}`. There is precedent for doing better: BT-2005 added `handle_metaclass_self_call/2`, which intercepts `Pid =:= self()` on the *metaclass* path and raises a structured `dispatch_error` "rather than an opaque `calling_self` timeout." `class_send/3` has the same `ClassPid =:= self()` guard, but only for `new`/`new:`/`spawn`/`spawnWith:` — every other selector falls through to the deadlocking call.

**2. Global serialization.** Every `File` operation in the node queues behind the block. A block holding the class process for the duration of an append-only write loop turns `File` into a global I/O lock. `open:do:` had this too, but it only streamed lines; `open:mode:do:` invites long write loops.

**3. A 60-second ceiling.** `class_send_dispatch/3` uses `Timeout = 60000` for non-test selectors. A block that runs longer raises a timeout **in the caller** while the block keeps running in the class process — the caller cannot tell whether its writes landed. There is no way to opt out.

### Why this is worth an ADR

The obvious framing — "make class methods run blocks in the caller" — reads like a change to `beamtalk_class_dispatch`, i.e. to how *every* class method in the system dispatches. That framing is wrong in a way worth recording, because it is the expensive and risky reading of the problem.

A general "run the Block argument in the caller" mechanism would require the class process to hand a continuation back to the caller and resume afterwards: a two-phase protocol on every class-method call, replacing one `gen_server:call` with a call/callback/resume handshake. That is a large change to the hottest dispatch path in the language, it would need new failure semantics (what if the caller dies mid-block?), and it buys nothing for the overwhelming majority of class methods, which take no Block at all.

The narrower reading gets the same user-visible outcome: **the block never needs to reach the class process in the first place.**

## Decision

Lower block-scoped resource methods **at the call site**, so the class process only performs the resource acquisition and the block runs in the caller.

`File open: p mode: m do: blk` becomes, in the caller's own process:

```
case File open: p mode: m of         % short class-process call, returns a handle
  ok Handle  -> [blk value: Handle] ensure: [Handle close]
  error E    -> Result error: E
end
```

The compiler already has the machinery: `WellKnownSelector` intercepts selectors at the call site today (`ifTrue:`, `on:do:`, `ensure:`, `value*`), and `ensure:` in particular is already lowered to Core Erlang `try`/`after` in the caller. This decision adds `open:do:` and `open:mode:do:` to that set, keyed on a `File` receiver.

Three consequences follow immediately, without touching class dispatch:

- **The deadlock disappears** rather than being reported — the block runs in the caller, so a nested `File exists:` is an ordinary send.
- **Serialization disappears** — the class process is held only for the `open`, not the block.
- **The 60-second ceiling disappears** — the only class call is the open.

Independently, and because a raw `calling_self` tuple is a bad diagnostic regardless of this API, **`class_send/3` gains a general `ClassPid =:= self()` clause** that raises a structured `#beamtalk_error{}` naming the deadlock, mirroring BT-2005's `handle_metaclass_self_call/2` catch-all. This is a safety net for every other class method, not just `File`.

### Scope

In scope: `File open:do:`, `File open:mode:do:`, and the general `class_send/3` self-call diagnostic.

Not in scope: a general continuation protocol for class methods; changing the 60-second class-call timeout; making arbitrary user-defined block-taking class methods run their block in the caller. If a third such method appears, this ADR should be revisited to decide whether the interception becomes a declarable property (e.g. a `blockScoped` marker on the method) rather than a hard-coded selector list.

## Consequences

### Positive

- The advertised BT-2975 use case — an append-only log with `sync` per record — stops being a global lock and stops being capped at 60s.
- Re-entrant `File` calls inside an open block simply work, removing a documented footgun that only reads as a footgun once you already know about it.
- Every other class method gets a structured self-call error instead of a gen_server tuple.
- No change to the dispatch hot path, so no regression surface for the ~100 stdlib classes that take no Block.

### Negative

- The selector list is hard-coded in the compiler, so a fourth block-scoped class method needs a compiler change rather than just a stdlib change. This is the explicit trade for not building the general mechanism; the "Not in scope" note records the trigger for revisiting.
- Two lowering paths now exist for `open:...do:` — the intercepted call site and the Erlang `'open:mode:do:'/3` that remains for dynamic dispatch (`perform:`). They must stay semantically identical, which needs a test asserting both paths close the handle on a raised error.
- The block no longer runs in the class process, so a block that relied on that serialization for mutual exclusion between concurrent writers would lose it. Nothing in-tree relies on this, and relying on it was never documented or intended.

### Neutral

- `File open:mode:` (no block) is unaffected — it already returns to the caller immediately.
- The handle remains non-`raw` (BT-2975): the descriptor is still opened in the class process, so it must stay usable from another process.

## Amendment (Accepted, 2026-08-05, BT-3047): Class Self-Identity Must Be Closure-Captured, Not Process-Read

**Status: Accepted.**

### Problem

This ADR's premise — "the block never needs to reach the class process" — is deliberately narrow (see Scope): only `File open:do:`/`open:mode:do:` are lowered at the call site. Every other class method that takes a Block keeps the pre-existing, unchanged behavior this ADR's Context section describes: the method body, including any block argument, executes inside the *target* class's own gen_server. That was always true, and remains true here — this amendment does not change it.

What it exposes is a bug in how a **self-send inside such a block** resolves its target class. A block literal is lexically part of one class's method but, when handed to another class's block-taking method, executes physically inside *that other class's* process. Three codegen sites assume the executing process's identity always equals the correct "self" identity, which fails exactly in this shape:

- **Inherited self-dispatch** (`crates/beamtalk-core/src/codegen/core_erlang/dispatch_codegen.rs:1449-1450`): `self <inheritedSelector>` emits `class_self_dispatch(erlang:get('beamtalk_class_name'), Selector, ClassVars, Args)`. Confirmed still present (BT-3047).
- **Instantiation intrinsics** (`dispatch_codegen.rs:1471-1525`): `self new`/`self new:`/`self spawn`/`self spawnWith:`/`self spawnAs:`/`self spawnWith:as:` read `erlang:get('beamtalk_class_name')`, `'beamtalk_class_module'`, `'beamtalk_class_is_abstract'` the same way.
- **`basicNew`/`basicNewWith`** (`crates/beamtalk-core/src/codegen/core_erlang/mod.rs:3433-3458`): the `@intrinsic basicNew` path (e.g. `Value sealed new`) reads the same `beamtalk_class_name`/`beamtalk_class_module` pair, gated by the identical `self.in_class_method()` check. Confirmed by inspection during this amendment's review, not merely flagged.

Concrete repro (from BT-3047): `Ancestor` defines `class foo`; subclass `P` (no override) does `C someMutatingMethod: [:x | self foo. ^x] over: aList`. The block executes inside `C`'s process (this ADR's own baseline semantics, unchanged). `self foo` is inherited on `P`, so it hits the fallback branch — which, evaluated inside `C`'s process, resolves against **`C`'s** hierarchy, not `P`'s: a confusing `does_not_understand` blaming the wrong class, or worse, silently executing a same-named method against `C`'s own live state.

### Why this isn't a new problem for this ADR to "cause," but is one this ADR's context makes newly reachable

Passing a block to another class's method, and having that block execute in the callee's process, predates this ADR (it is simply how a class-side gen_server call has always worked). What ADR 0110's BT-3039 amendment already established, though, is the fix pattern: `ClassSelf` — the value `self` is bound to in every class method (`crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs:2977-2978`, `self.bind_var("self", "ClassSelf")`) — is an ordinary Core Erlang closure variable. A block literal that references `self` closes over `ClassSelf`'s value *at the point the block was created*, which is always the block's lexical home class, regardless of which process later calls `value:` on it. ADR 0110's amendment already relies on exactly this to key the class-var shadow write correctly (`element(2, ClassSelf)`); this amendment applies the same fact to the process-dictionary reads at all three sites above.

### Decision

Replace the process-dictionary reads at the three sites above with values derived from `ClassSelf`, which is already in scope (bound to `self`) everywhere a class method body — including a nested block — can reference it:

1. **Inherited self-dispatch** (`dispatch_codegen.rs:1450`): replace
   ```erlang
   call 'erlang':'get'('beamtalk_class_name')
   ```
   with
   ```erlang
   call 'beamtalk_primitive':'class_name_from_tag'(call 'erlang':'element'(2, ClassSelf))
   ```
   `class_name_from_tag/1` already exists (`beamtalk_primitive.erl:975-981`) and already strips the `' class'` tag suffix (`'P class'` → `'P'`) — it is the exact untagging step BT-3047 flagged as an open question (point 3 in its description), answered by reusing existing code rather than inventing a new one.

2. **`class_mod`** (used by the instantiation intrinsics): replace `erlang:get('beamtalk_class_module')` with `call 'erlang':'element'(3, ClassSelf)` — already stored on `ClassSelf` at every construction site (`beamtalk_class_dispatch.erl:392`, `:714`), no lookup needed.

3. **`is_abstract`** (used by `self spawn`/`self spawnWith:`/etc.): **not** currently derivable from `ClassSelf` or any existing name-keyed table without either a gen_server call (which would reintroduce the BT-893 self-call deadlock in the common, non-block case, where `ClassSelf`'s owning process *is* the currently-executing one) or a new lookup structure. Rather than adding a new ETS table, extend the existing unified `#class_metadata{}` row (`beamtalk_class_metadata.erl:83-92` — `name`, `module`, `selectors`, `superclass`, `has_runtime_class_methods`) with an `is_abstract` field:
   ```erlang
   -record(class_metadata, {
       name :: class_name(),
       module :: module() | undefined,
       selectors :: [selector()] | undefined,
       superclass :: superclass() | undefined,
       has_runtime_class_methods = false :: boolean(),
       is_abstract :: boolean() | undefined   % new — undefined sentinel, same convention as module/selectors/superclass above, deliberately NOT a boolean default (see below)
   }).
   ```
   This table already exists for exactly this purpose — BT-2222 consolidated three separate per-class ETS tables into one specifically to avoid table proliferation for class-lifetime static metadata, and it is already read on hot dispatch paths without a gen_server hop (`lookup_module/1`, `lookup_superclass/1`, `has_runtime_class_methods/1` all use `ets:lookup_element/4`, no messaging). Both existing write sites (`beamtalk_object_class.erl:453` in `init/1`, `:1619` in the hot-reload path) already compute `IsAbstract`/`NewIsAbstract` locally at the call site.
   `insert/4` has 86 call sites in the tree (verified) — 2 production (`beamtalk_object_class.erl:453,1619`) and 84 across 8 test files (`beamtalk_class_metadata_tests.erl` ×25, `beamtalk_supervisor_tests.erl` ×21, `beamtalk_class_registry_tests.erl` ×18, `beamtalk_announcements_tests.erl` ×6, `beamtalk_workspace_shape_store_tests.erl` ×6, `beamtalk_workspace_signature_store_tests.erl` ×4, `beamtalk_logging_config_tests.erl` ×3, `beamtalk_repl_server_tests.erl` ×1). `insert/4` becomes `insert/5` outright — every call site updated to pass an explicit fifth argument, `undefined` at the 84 sites that don't know or care about abstractness, `IsAbstract`/`NewIsAbstract` at the two that do. Rejected a thin `insert/4` wrapper over `insert/5` (defaulting `is_abstract = undefined`) as a way to dodge the mechanical update: a permanent second entry point on a hot-path API, kept alive solely to save a one-time bulk edit, is a worse long-term cost than the edit itself — every future reader of `beamtalk_class_metadata.erl` has to know two arities exist and why, for the life of the module, to save touching 84 call sites once. The 86-site diff is large but mechanical (append `, undefined` or `, IsAbstract`), verifiable by compilation (a missed site is a compile error, not a latent bug), and each row's shape stays uniform — one arity, one meaning, matching the rest of `#class_metadata{}`'s fields.
   A new `lookup_is_abstract/1` mirrors `lookup_superclass/1`'s shape exactly: `{ok, boolean()} | not_found`, **not** a silently-collapsed boolean. This deliberately does *not* follow `has_runtime_class_methods/1`'s pattern of defaulting an unset/missing row to `false` — that collapse is safe there because `false` only skips an optional funs-table read (a low-stakes default in either direction), whereas `is_abstract` gates whether an abstract class can be instantiated at all: a silent `false`-on-miss default can only ever fail toward *permitting* instantiation, never toward raising, which is the less-safe direction for a correctness guard, not the safer one. Since the row is written unconditionally in the same `init`/reload code path that establishes `ClassSelf`'s own identity, a genuine `not_found` here means generated code dispatched a self-send before its own class's metadata was visible — a bug worth surfacing loudly. The instantiation intrinsics therefore raise the same structured `#beamtalk_error{}` on `not_found` that they already raise for the confirmed-abstract case, rather than picking either boolean default.
   The instantiation intrinsics then become: derive the bare class name as in (1), then `beamtalk_class_metadata:lookup_is_abstract(ClassName)`. No new table, no gen_server call, no change to the deadlock-avoidance property BT-893 established (this remains a plain function call, never a message send).

4. **`basicNew`/`basicNewWith`** (`crates/beamtalk-core/src/codegen/core_erlang/mod.rs:3433-3458`, the `@intrinsic basicNew` path used by e.g. `Value sealed new`): confirmed by inspection to have the identical bug — `erlang:get('beamtalk_class_name')`/`'beamtalk_class_module'` at `:3438-3439` and `:3450-3451`, gated by the same `self.in_class_method()` check as the self-dispatch and instantiation-intrinsic sites above, reachable the same way (a `basicNew`-based factory method whose block argument executes in a foreign class's process). No `is_abstract` read at this site (it calls `class_self_new` directly, which doesn't take one), so only fixes (1) and (2) apply here.

5. Apply (1)-(2) at `mod.rs:3438-3439,3450-3451` and (1)-(3) at every `try_instantiation_intrinsic` call site (`dispatch_codegen.rs:1482`, `:1497`, `:1542`) as well as the inherited self-dispatch site — closing BT-3047's point 2 (whether the instantiation intrinsics need the same treatment) with an explicit yes, for the reasoning below, rather than leaving it a separate open question, and folding in the `basicNew` site rather than deferring it as unconfirmed follow-up.

### Implementation note (2026-08-05): two points above diverged during implementation

Points 2 and 3 above describe the plan as accepted; the code that shipped deviates from
each in a way worth flagging so this section isn't read as a literal account of the final
implementation:

- **Point 2** said `class_mod` needs "no lookup" — `erlang:element(3, ClassSelf)` would do.
  In practice `apply_class_method_in_context/6` (`beamtalk_class_dispatch.erl:704-714`)
  constructs `ClassSelf` for an *inherited* class method with `class_mod = DefiningModule`
  (the ancestor that defines the method), not the calling subclass's own module — so
  `element(3, ClassSelf)` is unsafe for exactly the inherited-dispatch case this amendment
  exists to fix. The shipped code instead adds `resolve_module_or_raise/2`, a name-keyed
  `beamtalk_class_metadata:lookup_module/1` call, and resolves the module from the
  already-corrected class *name* (point 1) rather than trusting `ClassSelf.class_mod`.
- **Point 3** said a metadata miss on `is_abstract` should raise "the same structured
  error ... for the confirmed-abstract case." The shipped code instead raises a distinct
  `class_metadata_missing_error/2` (a dedicated `internal_error`), reusing
  `abstract_class_error/2` here would misleadingly claim the class *is* abstract when the
  real failure is "its metadata row isn't visible yet" — a different condition that
  deserves a different message.

Both divergences are the more correct choice, not implementation shortcuts; this note
exists so the "Decision" text above isn't mistaken for what actually shipped. See
`beamtalk_class_instantiation.erl`'s doc comments on `resolve_module_or_raise/2` and
`class_metadata_missing_error/2` for the in-code version of this reasoning.

### Why this is a no-op for every call outside a block

For a class method executing in its own process (the overwhelming majority of calls — no block involved), `ClassSelf`'s `.class`/`.class_mod` were populated from the *same* source as the process-dictionary values, at the same moment: both are seeded from `ClassName`/`Module`/`IsAbstract` at `beamtalk_object_class.erl` registration/`init` (`beamtalk_class_registry:class_object_tag(ClassName)` for `ClassSelf.class`, `put(beamtalk_class_name, ClassName)` for the pdict copy — literally adjacent lines). They were never two independent sources of truth that happened to usually agree; they were the same value, copied twice. This amendment removes the copy that breaks across a block boundary and keeps the one that doesn't.

### Why this preserves BT-908's intent rather than overriding it

BT-908 chose "an inherited factory method creates an instance of the *calling* class" (the polymorphic-factory pattern) specifically so that `self new` inside an ancestor-defined `make` behaves correctly when invoked via a subclass's own gen_server process. Reading `ClassSelf` continues to produce exactly that value in every case BT-908 was written for — it's the same value, sourced differently. What changes is only the definition of "calling" in the one case BT-908's author could not have had in view (ADR 0109 postdates BT-908): inside a block, "calling" today accidentally means "whichever process happens to execute this fun," which nothing in the language's semantics elsewhere means by `self`. After this amendment it means "the class that lexically owns this self-send" — consistent with how `self` already behaves for actor and value-type state (ADR 0041) and for the class-var shadow key (ADR 0110). This is closing an inconsistency between two things that were supposed to be the same value, not introducing a new semantic for `self`.

### Test plan

Extend `stdlib/test/class_var_nlr_shadow_test.bt` (or a sibling file if scope grows) with:

- A case mirroring this amendment's repro: a block lexically part of `P`'s method, handed to `C`'s block-taking method, performs `self <selector>` for a selector defined only on `P`'s own ancestor (not on `C`'s hierarchy) — asserts it resolves and executes against `P`, not `C`.
- A companion where `C`'s hierarchy *does* define a same-named selector — asserts `P`'s version runs, pinning the "silently executes the wrong method" half of the bug as fixed, not just the DNU half.
- An instantiation-intrinsic case: `self new` (or `self spawn`) inside such a foreign block creates an instance of the block's lexical home class, not the class whose process is executing the block.
- A `basicNew`-based case (e.g. a `Value sealed new`-style factory method) inside such a foreign block, pinning the `mod.rs:3433-3458` fix.
- A `beamtalk_class_metadata` EUnit case for `lookup_is_abstract/1`: write via `insert/5` and read back `{ok, true}`/`{ok, false}`; assert `not_found` on an absent row (not a defaulted boolean).

### Consequences

**Positive:** Closes BT-3047 for the confirmed self-dispatch bug, the previously-undecided instantiation-intrinsic question, and the structurally-identical `basicNew`/`basicNewWith` site, with one coherent rule instead of a partial fix plus deferred follow-ups. Makes `ClassSelf` the single closure-captured source of class self-identity across the whole class-method surface (self-sends, instantiation, `basicNew`, and the ADR 0110 shadow key), removing every remaining process-dictionary read for class identity reachable from a self-send or instantiation intrinsic. Reuses existing infrastructure throughout (`class_name_from_tag/1`, `ClassSelf.class_mod`, the `beamtalk_class_metadata` table's existing `undefined`-sentinel/`not_found` convention) — no new ETS table, no new module.

**Negative:** Adds one untag call (`class_name_from_tag/1`, itself a cheap binary-suffix check plus `binary_to_existing_atom`) to the compiled hot path for inherited self-dispatch — negligible next to the `maps:put`/gen_server-call costs already paid there. Widens `#class_metadata{}` with a new field and changes `insert/4` to `insert/5` outright — a 86-site mechanical diff (2 production, 84 test, verified) across 9 files, all compile-checked, no dual-arity API left behind. `beamtalk_class_name`/`beamtalk_class_module`/`beamtalk_class_is_abstract` remain seeded in the process dictionary for any other reader of those keys (this amendment stops all four confirmed call sites — self-dispatch, the three instantiation intrinsics, and `basicNew`/`basicNewWith` — from depending on them; it does not remove the keys themselves, since nothing else in the audited files reads them, but a codebase-wide grep before implementation is still warranted in case a reader exists outside `dispatch_codegen.rs`/`mod.rs`).

**Neutral:** No change to where a block executes (still governed by the unchanged, pre-ADR-0109 baseline: it runs in whichever process's method received it) — only to how a self-send or instantiation *inside* that block resolves its target class. No change to the `File open:do:`/`open:mode:do:` call-site lowering this ADR's original Decision covers.

## Alternatives Considered

**Restructure `beamtalk_class_dispatch` for a general call/callback/resume protocol.** Rejected: large change to the hottest path, new failure semantics to define, and no benefit to the ~99% of class methods that take no Block. Revisit only if block-taking class methods become common.

**Document the constraints and stop there.** This is the status quo after BT-2975, which documented all three consequences in `file.bt` and `beamtalk-language-features.md`. Rejected because the append-only-log pattern the API exists to serve is precisely the one that trips the 60s ceiling, and "don't message `File` from inside `File`" is a rule users only learn by hitting an opaque gen_server tuple.

**Make the class-call timeout configurable.** Addresses only the third problem, and by making the global-lock window longer. Rejected as strictly worse than not holding the lock.

**Run the block in a spawned helper process owned by the class.** Fixes serialization but not the deadlock (the helper still isn't the caller, so `self`-relative state and the process dictionary diverge), and adds a process per open. Rejected.

## References

- BT-3018 — the issue this ADR resolves
- BT-2975 — added `open:mode:do:`; documented all three consequences in lieu of fixing them
- ADR 0056 — actor dispatch and `self delegate`
- `beamtalk_class_dispatch.erl` — `class_send/3`, `class_send_dispatch/3`, `handle_metaclass_self_call/2` (BT-2005 precedent)
- `crates/beamtalk-core/src/ast/well_known.rs` — the call-site interception mechanism this decision extends

### Amendment (BT-3047) references

- BT-3047 — the issue driving the amendment; found during BT-3039's review
- BT-908 — origin of the polymorphic-factory intent for instantiation intrinsics the amendment preserves
- BT-2007 — introduced the inherited self-dispatch fallback branch the amendment fixes
- BT-893 — established the gen_server-bypass deadlock-avoidance property the amendment's `is_abstract` mechanism must not reintroduce
- BT-2222 — consolidated the three predecessor class-keyed ETS tables into `beamtalk_class_metadata`, the table the amendment extends rather than duplicating
- ADR 0110 (Class-Variable Shadow Write-Through) — prior art: its BT-3039 amendment first established that `ClassSelf` is closure-safe across a block's foreign-process boundary (`element(2, ClassSelf)`), the fact this amendment generalizes
- `crates/beamtalk-core/src/codegen/core_erlang/dispatch_codegen.rs` — `generate_class_method_self_send` (:1450, the confirmed-broken site), `try_instantiation_intrinsic` (:1471-1525)
- `crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs:2977-2978` — `self` bound to `ClassSelf` in every class method
- `runtime/apps/beamtalk_runtime/src/beamtalk_primitive.erl:975-981` — `class_name_from_tag/1`
- `runtime/apps/beamtalk_runtime/src/beamtalk_class_metadata.erl` — `#class_metadata{}`, `insert/4`, `lookup_superclass/1` (the shape the amendment's `lookup_is_abstract/1` mirrors)
- `runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl:453,1619` — the two `beamtalk_class_metadata:insert/4` call sites that become `insert/5`
