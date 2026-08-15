# ADR 0112: Method-Level Removal Language Primitive (`Behaviour removeSelector:`)

## Status
Proposed

## Context

### Problem

ADR 0082 (Method-Level Edit and Save in the Live Workspace) shipped `Behaviour compile:source:` / `tryCompile:source:` — durable and ephemeral live *patches* — plus `Workspace newClass:at:` for whole-file *creation*. It deliberately deferred *removal*. Its Out of Scope table says so explicitly:

> **Method-level removal** (`aClass removeSelector:`) — The language primitive does not exist yet. Adding it is a separate design question (raise vs no-op on absent selector? cascade to overrides? extension-method handling?) — not bundled with persistence. The runtime can erase a method's `method_signatures` entry (`beamtalk_object_class.erl:640`) but there is no first-class Beamtalk method that calls it.

That line number has since moved; see *Implementation* below for the current one. This ADR is the "separate design question" ADR 0082 promised.

### Current State

| Concern | Today |
|---|---|
| Class-level removal | `Counter removeFromSystem` — works, ships since BT-785 |
| Method-level live *patch* | `Counter compile: #sel source: body` / `Counter >> sel => body` — works, instance-side only (ADR 0082) |
| Method-level *removal* | **Does not exist.** No Beamtalk method, no MCP tool, no REPL command, no LSP command. |
| Runtime capability | The class gen_server can drop a selector from its signature/return-type caches as a *side effect* of patching (see below), but there is no dedicated "delete this method" gen_server call. |

**The runtime hook, precisely.** `beamtalk_object_class.erl`'s `put_method`/`put_class_method` handlers clear the stale signature and return-type cache entries for a selector every time that selector is (re)installed via a hot patch — because a dynamically-patched method has no AST, so any previously-recorded static signature is now wrong:

```erlang
%% instance-side, put_method handler
method_signatures = maps:remove(Selector, State#class_state.method_signatures),
method_return_types = maps:remove(Selector, State#class_state.method_return_types)
```

```erlang
%% class-side, put_class_method handler
class_method_signatures = maps:remove(
    Selector, State#class_state.class_method_signatures
),
class_method_return_types = maps:remove(
    Selector, State#class_state.class_method_return_types
)
```

This is cache invalidation *inside an install path*, not a removal primitive — `instance_methods`/`class_methods` (the actual method tables) are simultaneously being *added to* (`maps:put(Selector, MethodInfo, ...)`) in the same handler. There is no existing gen_server call that removes a selector from `instance_methods`/`class_methods` without installing a replacement. The *Implementation* section below treats this distinction precisely: it is real prior art for "the class gen_server already touches these maps by selector," not a shipped removal path.

**Class-level removal, for comparison.** `Behaviour removeFromSystem` (BT-785) already exists and is the closest sibling design:

```beamtalk
Counter removeFromSystem   // => nil (Counter class removed)
Integer removeFromSystem   // => Error: cannot remove stdlib class
```

Its safety checks (`beamtalk_behaviour_intrinsics:classRemoveFromSystemByName/1`) are: refuse if the class's module is a stdlib module (`bt@stdlib@*`), refuse if the class has direct subclasses, otherwise stop live actors, stop the class gen_server, purge the BEAM module, and purge five *derived* registries via a single call, `beamtalk_class_lifecycle:class_removed/2` (BT-3105):

```erlang
class_removed(ClassName, Module) ->
    purge_xref(ClassName),
    purge_extensions(ClassName),
    purge_protocol(Module),
    purge_compiler_cache(ClassName),
    purge_workspace_class_source(ClassName),
    ok.
```

That single-call-site design (plus `beamtalk_class_metadata:merge_identity/5`, BT-3107's per-field metadata update that never implicitly wipes unrelated fields) exists precisely because BT-3105/BT-3107 found and fixed five places where a *class*-level teardown silently left stale rows in derived registries. This ADR's implementation plan reuses that same infrastructure at selector granularity rather than re-deriving its own purge list — see *Implementation*.

### Constraints

1. **ADR 0032 (chain-walk dispatch).** Dispatch has no flattened/cached method table. Every message send walks `Class → Behaviour → Object → ProtoObject` (class-side) or the instance hierarchy (instance-side) live, checking each level's local method table (and, per ADR 0066, its extension table) in turn. This means removal needs no invalidation step of its own — deleting a map entry is instantaneously visible to the next dispatch. See *Decision* for what this implies for overridden methods.
2. **ADR 0066 (Open Class Extension Methods).** Extension methods (`Counter >> #foo` defined from a *different* file than `Counter.bt`) are not stored in the class gen_server's `instance_methods`/`class_methods` maps at all — they live in a separate ETS table (`beamtalk_extensions`) keyed by `{Class, Selector}`, checked at each hierarchy level interleaved with local methods. A selector-removal primitive that only knows how to clear the class gen_server's own maps would silently no-op against an extension method resolved from that table. `beamtalk_extensions:unregister/2` already exists and does exactly this removal (dispatch entry + stored source + xref rows) — see *Decision* and *Implementation*.
3. **ADR 0082 (ChangeLog).** The ChangeLog schema's `kind` field is explicitly open — `"instance"`, `"class"`, `"new-class"` today, with `"remove-method"`, `"remove-class"`, `"rename"` reserved for future ADRs to slot in "without breaking the format." This ADR is one of those future ADRs for the method case; the class case (`"remove-class"`, full destructive-flush UX) is BT-2192's.
4. **BT-785 sealing precedent.** `removeFromSystem`'s stdlib check is a *module-name* check (`bt@stdlib@*` prefix), not a `sealed`-keyword check — a user's own `sealed Object subclass: MyRegistry` class is not blocked from `removeFromSystem` today. Any new sealing rule this ADR adds at selector granularity needs to say explicitly whether it's stricter than that precedent, and why.
5. **Surface parity** (`docs/development/surface-parity.md`). Whatever this ADR designs must be reachable equivalently from REPL, MCP, LSP, and browser, consistent with ADR 0082's "every tool op is a structured invocation of a Beamtalk expression" principle — no workspace-only removal op.
6. **Scope boundary with BT-2192.** This ADR designs the *language primitive* — its name, receiver, error behaviour, and the shape of the ChangeLog entry it produces. It explicitly does **not** design what `Workspace flush` does with a `"remove-method"` entry (splicing dead text out of a `.bt` file is a destructive disk operation that deserves its own confirmation/undo UX, same reasoning ADR 0082 used to defer class-level flush-removal to BT-2192). Implementation of the runtime removal path itself is also out of scope — downstream work.

## Decision

**Add `Behaviour>>removeSelector:` (raises if absent) and `Behaviour>>removeSelector:ifAbsent:` (caller supplies fallback behaviour), both sealed class-side methods on `Behaviour`, operating on the instance-side method table when sent to a class object and on the class-side method table when sent to that class's metaclass object (`Counter class removeSelector: #foo`). Removal reaches whichever table currently supplies dispatch for that selector — the class gen_server's own method table, or, if the selector resolves to an extension, the `beamtalk_extensions` registry — and produces a durable `"remove-method"` ChangeLog entry attributed to whichever file owns what was removed.**

### Method name and signature

`removeSelector:` — following Pharo's `Behavior>>#removeSelector:` naming exactly. Beamtalk already has zero-friction precedent for adopting Pharo protocol names verbatim on `Behaviour` (`compile:source:`, `includesSelector:`, `canUnderstand:`, `whichClassIncludesSelector:` are all Pharo-shaped), and `removeSelector:` reads naturally next to the sibling `removeFromSystem` this ADR is modelled on. No alternative name was seriously considered — Smalltalk developers already know this exact selector, and there is no Beamtalk-specific reason to depart from it.

```beamtalk
sealed removeSelector: aSelector :: Symbol -> Behaviour =>
  @primitive "classRemoveSelector"

sealed removeSelector: aSelector :: Symbol ifAbsent: absentBlock :: Block -> Behaviour =>
  @primitive "classRemoveSelectorIfAbsent"
```

Both return the receiver (`Behaviour`) on success, matching `compile:source:` / `tryCompile:source:`'s "returns the receiver class" convention so removal chains the same way patches do. `removeSelector:ifAbsent:`'s block runs (and its value is returned instead of the receiver) only when the selector is not found — mirroring `Dictionary>>at:ifAbsent:`'s established shape in this codebase, not inventing a new block-callback convention.

### Receiver

Class-side message on `Behaviour`, sent to the class object being edited — same receiver shape as `removeFromSystem`, `compile:source:`, and `reload`. This is a natural consequence of ADR 0032: `Behaviour` is where the class protocol lives, every class object dispatches through `Class → Behaviour → Object → ProtoObject`, and `compile:source:`/`removeFromSystem` already established "the class you're editing is the receiver" as the idiom.

**Side selection follows the existing `Counter` vs `Counter class` convention**, not a boolean parameter. ADR 0036's full metaclass tower makes `Counter class` a real, independently-dispatchable receiver (the metaclass-tagged object at the same pid, per ADR 0013's virtual-metaclass design); ADR 0066's `>>` syntax already uses this to pick a side for *definition* (`Counter class >> ofSize:` vs `Counter >> increment`). Removal reuses the same signal:

```beamtalk
Counter removeSelector: #increment          // instance-side: touches instance_methods
Counter class removeSelector: #ofSize:      // class-side: touches class_methods
```

This is a deliberate asymmetry with `compile:source:`, which is scoped instance-side only in ADR 0082 (class-side patches still go exclusively through `Counter class >> sel => body` syntax, with no `Behaviour` primitive backing them). Removal needs both sides from day one because the runtime hook this ADR builds on already touches both maps symmetrically (`method_signatures` at the instance level, `class_method_signatures` at the class level — see *Context*), and because there is no `>>`-shaped syntactic sugar for "delete" the way there is for "define" — `removeSelector:` **is** the only front door, so it cannot leave class-side methods unreachable the way `compile:source:` can (class-side patch has the `>>` escape hatch; class-side *removal* would have none). Extending `compile:source:` to cover class-side patches symmetrically is a reasonable independent follow-up but is out of scope here.

### Error behaviour on absent selector

**`removeSelector:` raises; `removeSelector:ifAbsent:` is the paired escape hatch.** This mirrors Pharo, where `Behavior>>#removeSelector:` signals an error when the selector is not present and `Behavior>>#removeSelector:ifAbsent:` exists precisely to let a caller supply different behaviour (see *Prior Art* — noted with appropriate uncertainty about Pharo's exact current implementation).

It also matches this codebase's own conventions more directly than Pharo does:
- CLAUDE.md's DNU rule: "An unrecognised message raises `does_not_understand`. Use `respondsTo:` to check before calling." Calling `removeSelector:` for a selector the receiver does not locally define is the same shape of "you asked for something that isn't there" as an unrecognised message — silently doing nothing would hide a caller bug (a typo'd selector, a method already removed by a concurrent session) exactly the way DNU exists to surface.
- BT-785's `removeFromSystem` raises loudly for every misuse case (class not found, stdlib class, has subclasses) rather than returning a boolean or silently no-op'ing. `removeSelector:` follows the same house style: destructive class-protocol operations raise on misuse, full stop.
- `Behaviour`'s existing `includesSelector:` (local-only presence check) already gives callers who *want* the no-op-on-absent shape a one-line way to build it (`(aClass includesSelector: #foo) ifTrue: [aClass removeSelector: #foo]`) or, more idiomatically, to use `removeSelector:ifAbsent:` directly.

The raised error is a structured `#beamtalk_error{}` with kind `does_not_understand` (mirroring the "unrecognised selector" shape a DNU already has, since from the class's point of view "I don't have this selector to remove" is informationally identical to "I don't understand this selector") carrying the class name, selector, and a hint pointing at `removeSelector:ifAbsent:` and `includesSelector:`.

```beamtalk
Counter removeSelector: #bogus
// => error: Counter does not define #bogus (locally or as an extension)
//    hint: use `includesSelector:` to check first, or `removeSelector:ifAbsent:`

Counter removeSelector: #bogus ifAbsent: ['not found']
// => 'not found'
```

### Overridden methods: chain-walk makes removal correct by construction

**Yes — removing `Counter >> #initialize` re-exposes `Actor >> #initialize`, with no cache to invalidate.** This is a direct, load-bearing consequence of ADR 0032, not a new mechanism this ADR has to build. ADR 0032 removed the flattened/cached method table specifically because "correct by construction" was worth more than an O(1) cache: dispatch walks `instance_methods` at each hierarchy level, live, on every call. Deleting `#initialize` from `Counter`'s `instance_methods` map means the *very next* `aCounter initialize` send simply does not find it at Counter's level and continues up to `Actor`'s level, exactly as if `Counter` had never defined it. There is no flattened table entry to evict, no subclass-invalidation broadcast to fire (the BT-510 race window ADR 0032 eliminated), and no notification needed beyond the ordinary hot-patch notification (`notify_hot_patch/1`) that keeps the compiler's ambient type cache in sync.

```beamtalk
Actor >> initialize => Transcript show: 'base init'
Counter subclass: #Counter instanceVariableNames: 'value'
Counter >> initialize => Transcript show: 'counter init'

Counter new   // prints 'counter init'
Counter removeSelector: #initialize
Counter new   // prints 'base init' — inherited Actor>>initialize, no restart needed
```

### Extension methods: removed via the same registry that installed them, attributed to the extending file

**Yes, an extension method can be removed via `removeSelector:` — sent to the *target* class, exactly as it would be sent to install one via `>>`.** `beamtalk_extensions:unregister/2` (`{Class, Selector}` keyed) already exists and already does the full job: it drops the dispatch ETS row, the stored source-text row, and the xref rows for just that method (via `beamtalk_xref:purge_method/3`, a per-selector purge that already exists and is distinct from the whole-class `purge_class/1` BT-785 uses). `removeSelector:`'s implementation therefore branches on *where the selector currently resolves* rather than needing a separate "remove extension" entry point:

1. Selector defined locally on the receiver's own class → clear it from the class gen_server's `instance_methods`/`class_methods` map (new gen_server call — see *Implementation*).
2. Selector not local, but present in `beamtalk_extensions` for `{Class, Selector}` → call `beamtalk_extensions:unregister/2`.
3. Neither → absent; raise (or run the `ifAbsent:` block).

**Ownership of the removal follows ADR 0082's existing attribution precedent, not a new rule.** ADR 0082 already decided that a *patch* to an extension method is logged against the extender's file, not the extended class's file ("Extension methods (ADR 0066) — A class adding extension methods to a foreign class has its own `sourceFile`; the patch is logged against the extender's file, not the extended class's file"). Removal inherits the identical rule: `Counter removeSelector: #shout` where `#shout` was defined in `String+Shouting.bt` produces a ChangeLog entry with `sourceFile` = `String+Shouting.bt`, not `Counter.bt` — because that is the file a flush would eventually need to touch, and it is the file whose `.bt` source actually contains the method text being deleted. The multi-extender ambiguity ADR 0082 already accepted (two packages both extending `String >> shout`; last-writer-wins decides which is "the" active extension) applies unchanged: `removeSelector:` removes whichever extension is currently dispatchable, faithfully, the same way a patch would faithfully patch it. Restoring a shadowed prior extender is not this primitive's job (that is what `Workspace changes revert:` and ADR 0082's ChangeLog audit trail are for).

### Sealing rules

Three separate refusal conditions, deliberately narrower in scope than they might first appear:

| Rule | Refuses when | Rationale |
|---|---|---|
| **Stdlib classes** | The receiver's module is a stdlib module (`bt@stdlib@*` prefix) | Direct mirror of `removeFromSystem`'s existing check (`is_stdlib_module_name/1`). Reproducible-build guarantee: `removeSelector:` must never leave a stdlib class's dispatch surface silently different from what its shipped source defines. |
| **`sealed` methods** | The target method (local or extension) is declared `sealed` | **New rule, stricter than `removeFromSystem`'s precedent** (which checks module origin, not the `sealed` keyword, and so does not block removal of a user's own `sealed`-declared class). `sealed` on a *method* is a stability promise specifically about that method's identity — ADR 0032 introduced it precisely so "no override, no surprise dispatch change" could be guaranteed for the class protocol; removing a sealed method changes dispatch just as much as overriding one would (the next call falls through to a different implementation), so it violates the same guarantee `sealed` exists to make. In practice this mostly overlaps with the stdlib check (`Behaviour`, `Class`, `Result`, `Stream`, etc. are all stdlib *and* sealed), but the rule needs to hold independently for a hypothetical future where user code declares its own sealed methods on non-stdlib classes. |
| **Extension conflicts, not blocked but faithfully resolved** | N/A — not a refusal | Noted separately here only to be explicit that removing a *contested* extension selector is allowed and removes whichever extension currently wins dispatch (see above); this is a behaviour clarification, not a sealing rule. |

**Dynamic classes (ADR 0038 ClassBuilder) are *not* specially refused.** Unlike stdlib classes, a dynamic class's entire purpose is runtime mutability — `ClassBuilder` exists to add/change methods on such a class at will, so refusing removal on it would be inconsistent with everything else about how dynamic classes work. Removal on a dynamic class installs in memory exactly like removal on any other class; the only difference (inherited unchanged from ADR 0082) is that its resulting ChangeLog entry is `flushable: false` (`not_flushable_reason: "dynamic"`) because it has no `sourceFile` to splice — the same treatment ADR 0082 already gives `compile:source:` patches against dynamic classes.

```beamtalk
Integer removeSelector: #printString
// => error: cannot remove 'printString' from Integer — stdlib classes are sealed
//    against selector removal (module bt@stdlib@integer)

Behaviour removeSelector: #removeFromSystem
// => error: cannot remove 'removeFromSystem' from Behaviour — it is a sealed method
```

### ChangeLog interaction

**`removeSelector:` is durable-only and produces a `kind: "remove-method"` ChangeLog entry.** No ephemeral (`tryRemoveSelector:`) variant ships in v1 — a "spike a removal, see what happens, discard it" workflow is plausible but was not part of BT-2191's acceptance criteria, adds a second front door before the first has real usage data, and is trivially added later, symmetrically, if usage shows the need (exactly the kind of incremental-add ADR 0082's open `kind` enum was designed to accommodate). Every `removeSelector:` call — like every `compile:source:` call — installs in memory unconditionally and *attempts* a best-effort ChangeLog append; a logging failure never blocks or reverts the in-memory removal (same "audit trail is best-effort, install is authoritative" rule ADR 0082 established for patches).

**Entry shape** (extending ADR 0082's schema, reusing its open `kind` enum exactly where it said it would extend):

```text
{ts, seq, epoch, class, selector,
 kind: "remove-method",
 side: "instance" | "class",
 source_ref: null,
 prev_source_ref: "<seq>-prev.bt" | null,
 sourceFile: "<path>" | null,
 span: {start, end} | null,
 intent: "durable",
 flushable: bool,
 not_flushable_reason: "stdlib" | "dynamic" | "dependency:<path>" | null,
 author, author_kind: "human" | "agent"}
```

- `source_ref` is always `null` — there is no new body to store; the operation deletes text, it does not replace it.
- `prev_source_ref` records the removed method's prior source body (resolved at hook time, same byte-span resolver ADR 0082 built for patches) so `Workspace changes revert:` can re-install it — removal's revert is structurally identical to ADR 0082's existing "revert an add" case (`add-removal needs no prior body` becomes, symmetrically, `remove-undo needs exactly the prior body`).
- `span` is the byte span of the method definition *as it stands on disk*, resolved the same way `compile:source:` resolves it for a patch — but for `remove-method`, flush's job is to *excise* that span rather than replace its contents. This is exactly the destructive-disk-write shape ADR 0082's Out of Scope table flagged as belonging to BT-2192, and this ADR does not design it further than recording the span needed to eventually do it.
- `side` is new: `"instance"` or `"class"`, needed because (unlike existing patch entries, where `kind` itself doubled as the side discriminator) `kind` is now spent on distinguishing *removal* from *patch*. Existing "instance"/"class"-kind patch entries are unaffected — this is an additive field, not a breaking schema change; a reader that only understands `kind` still gets the right operation type.
- `sourceFile`/`flushable`/`not_flushable_reason` follow exactly the same rules ADR 0082 established for patches: extension removals are attributed to the extender's file (see above); stdlib/dynamic/dependency classes get `flushable: false` with the matching reason.

**What `Workspace flush` does with a `"remove-method"` entry is explicitly out of scope for this ADR.** Splicing dead text out of a live `.bt` file is a destructive disk operation — unlike a patch's in-place body replacement, there is no way to make it non-destructive, and it deserves the same confirmation/tombstone/undo design ADR 0082 already deferred class-level removal-flush to BT-2192 for. This ADR's job ends at "here is the entry BT-2192's flush design needs to consume."

## Prior Art

### Pharo / Squeak Smalltalk

`Behavior>>#removeSelector:` removes the method associated with a selector from the receiver's method dictionary. From general Smalltalk knowledge (not verified against current Pharo source, flagged accordingly): the conventional shape is that the bare `removeSelector:` signals an error when the selector is not present, and `Behavior>>#removeSelector:ifAbsent:` exists as the paired variant that runs a supplied block instead of raising — the same `at:` / `at:ifAbsent:` pattern Smalltalk dictionaries use throughout. Some Smalltalk dialects' method-removal additionally triggers `#objectsAsMethodsFor:` / recompilation hooks in inheriting classes for optimization reasons that have no Beamtalk analogue (no flattened cache to invalidate, per ADR 0032).

**Adopted:** the selector name itself, the raise/ifAbsent: pairing shape.
**Adapted:** Beamtalk's version needs no cache-invalidation step at all (ADR 0032's chain-walk-only dispatch), where Pharo/Squeak dialects with method caches historically needed one.

### GemStone/S (GemTalk Systems)

GemStone/S has `Behavior>>#removeSelector:` and, per general knowledge of the platform (explicitly noted as uncertain — not verified against GemStone documentation), a coarser `Behavior>>#removeAllMethods` for wiping every method a class defines in one call, used in bulk-teardown/reset scenarios. Beamtalk's closest existing analogue to that bulk operation is `removeFromSystem` (BT-785), which removes the *class* (and therefore everything defined on it) rather than emptying its method dictionary while leaving the class itself alive; this ADR does not add a "remove all methods, keep the class" bulk primitive — no acceptance criterion called for it, and `removeSelector:` composed in a loop covers the same ground for the rare case it's needed.

**Adopted:** the selector-removal shape.
**Not adopted / uncertain:** a bulk "remove all methods" primitive — flagged as a possible future addition if usage shows demand, not designed here.

### Erlang / Elixir

Neither has a method-removal analogue — Erlang modules are all-or-nothing (`code:purge/1` unloads a whole module; there is no "delete this one exported function and leave the rest"). This is expected: Beamtalk's method-level granularity is a language-level abstraction the class gen_server provides *on top of* whole-module BEAM loading, not something the BEAM itself offers. No prior art to adopt or reject here beyond what ADR 0082 already established about the memory/disk split.

## User Impact

### Newcomer (from Python/JS/Ruby)

- `removeSelector:` reads plainly even without Smalltalk background — "remove this selector" — and the raise-on-absent default matches the "fail loudly on typos" behaviour most modern languages already train newcomers to expect (a Python `del obj.attr` on a missing attribute raises `AttributeError`, not a silent no-op).
- Discoverability: `Counter methods` (existing) shows what's removable; `Counter includesSelector: #foo` (existing) lets a newcomer check before removing, without needing to know `ifAbsent:` exists yet.

### Smalltalk developer

- `removeSelector:` / `removeSelector:ifAbsent:` is exact, muscle-memory-familiar Pharo protocol — no relearning.
- The chain-walk re-exposure of an inherited method on removal (no restart, no cache flush) is exactly what a Smalltalk developer already expects from a live image; ADR 0032 already earned this property for the whole class protocol, and this ADR just exercises it.

### Erlang/BEAM developer

- No new BEAM-level concept: removal is a map-key delete inside an already-running gen_server, immediately visible, no code reload involved unless the class also has a `sourceFile` and a later `flush` happens.
- The stdlib/sealed refusal and the derived-registry purge both reuse infrastructure (`beamtalk_class_lifecycle`, `beamtalk_extensions:unregister/2`, `beamtalk_xref:purge_method/3`) a BEAM-literate contributor can trace directly — no hidden magic layer.

### Production operator

- Refuses on stdlib/sealed exactly like `removeFromSystem` — an accidental `removeSelector:` against a production node cannot silently degrade a sealed-guarantee method.
- Every removal is ChangeLog-audited the same way a patch is — "was this method removed, by whom, when" has a definitive answer via `Workspace changes`, matching ADR 0082's audit-trail guarantee.

### Tooling developer (LSP/MCP/browser)

- No new workspace-side dispatcher op needed, by the same reasoning ADR 0082 used for `compile:source:`/`flush`: an MCP `remove_method` tool, an LSP `executeCommand`, and a REPL `:remove-method` shortcut (if one is added) all just construct `Counter removeSelector: #foo` and submit it through the existing `evaluate` op. The surface-parity table gains one more expression-backed row, not a new protocol surface.
- `includesSelector:` and `whichClassIncludesSelector:` (both pre-existing) give an IDE everything it needs to grey out or confirm a "Remove" action before sending it.

## Steelman Analysis

### No-op-on-absent vs raise-on-absent (for the bare `removeSelector:`)

- 🧑‍💻 **Newcomer:** "No-op is friendlier — I shouldn't have to think about whether something is already gone before I ask to remove it, the same way `rm -f` doesn't complain."
- 🎩 **Smalltalk purist:** "Some Smalltalks *do* treat bare removal as forgiving — 'do nothing if it's not there' is a defensible reading of `removeSelector:`'s contract, and it avoids forcing every call site to wrap in `ifAbsent:` just to be safe."
- ⚙️ **BEAM veteran:** "Idempotent operations are good BEAM hygiene — a supervisor restarting a removal step twice shouldn't need special-casing to avoid an error on the second attempt."
- 🏭 **Operator:** "Fewer raised errors in a removal script means fewer places a batch cleanup job has to add try/catch around routine no-ops."
- 🎨 **Language designer:** "Symmetric with `Dictionary removeKey:` style APIs elsewhere that treat absence as a normal outcome, not exceptional."
- **Why rejected as the *default*:** this codebase's own DNU convention (CLAUDE.md) and `removeFromSystem`'s existing raise-on-every-misuse precedent both point the other way — silent no-op on a mistyped selector or an already-removed method hides exactly the kind of bug DNU exists to surface, and "was this actually removed, or did I just typo the selector and nothing happened" is a worse debugging experience than a loud error. The idempotency argument is real and is fully satisfied by `removeSelector:ifAbsent:` (see next section) — nothing is lost, the forgiving path is opt-in rather than the silent default.

### `removeSelector:` alone vs paired with `removeSelector:ifAbsent:`

- 🧑‍💻 **Newcomer:** "One method to learn is simpler than two — just make `removeSelector:` itself take an optional default."
- 🎩 **Smalltalk purist:** "Pharo pairs `removeSelector:` with `removeSelector:ifAbsent:` for a reason — it's the same `at:`/`at:ifAbsent:` idiom every Smalltalk dictionary already teaches. One selector, two call shapes, is *more* idiomatic, not less, once you already know the pattern."
- ⚙️ **BEAM veteran:** "A block-based `ifAbsent:` composes naturally with `catch`/pattern-matching style error handling already common in Erlang-influenced code — better than requiring a try/catch-shaped wrapper for the common 'don't care if it's there' case."
- 🏭 **Operator:** "Explicit `ifAbsent:` at call sites makes intent legible in code review — 'this removal is expected to sometimes be a no-op' is visible in the source, not hidden behind a flag."
- 🎨 **Language designer:** "Splitting 'must succeed' from 'may already be absent' into two selectors keeps each one's contract simple and matches every other `ifAbsent:`-shaped API in the stdlib (`Dictionary>>at:ifAbsent:`, potentially `Collection>>detect:ifNone:`) rather than inventing a bespoke optional-parameter shape."
- **Why the pair wins:** both sides of this tension are satisfiable simultaneously, and the pair is *strictly* more expressive than a single method with a default-value parameter — a caller who wants "run this cleanup block on absence" (not just "return a sentinel") needs a block, not a boolean flag. There is no real cost to shipping both: `removeSelector:ifAbsent:` is a thin variant of the same primitive, not a second implementation to maintain.

### Language-level (`Behaviour` primitive) vs workspace-level (REPL/MCP-only op)

- 🧑‍💻 **Newcomer:** "A workspace-only op means I can't discover or use this from plain Beamtalk code the same way I discover `compile:source:` — it'd be tool-specific magic, not something I can read about in the language docs and try at the REPL like everything else."
- 🎩 **Smalltalk purist:** "The class protocol lives on `Behaviour` — that's the entire point of ADR 0032. A tool-only removal op would be the first crack in 'messages all the way down' for exactly the kind of operation (editing the running system) Smalltalk's philosophy is built around."
- ⚙️ **BEAM veteran:** "A workspace-only op is easier to gate behind environment checks (dev vs prod) without touching the language surface at all — Erlang/Elixir have no 'delete this exported function' language primitive either, and for good reason: it's an operational concern, not a language one."
- 🏭 **Operator:** "Confining destructive class-protocol edits to the workspace layer, not the language, means a plain compiled release build can never accidentally expose `removeSelector:` to production code — the blast radius is architecturally smaller."
- 🎨 **Language designer:** "A workspace-scoped op is simpler to design in isolation — no `Behaviour` signature to get right forever, no sealing-rule precedent to reconcile with `removeFromSystem`, easier to iterate on before commitment."
- **Why language-level wins:** the operator's "smaller blast radius" argument is the strongest one here, but it is answered directly by the sealing rules already in *Decision* (stdlib + sealed refusal) rather than by moving the whole operation out of the language — `removeFromSystem` faced the identical concern and ADR 0082/BT-785 already solved it with receiver-side checks, not by making class removal workspace-only. Making removal workspace-only while patch (`compile:source:`) and creation (`newClass:at:`) are both language-level would also violate ADR 0082's own "every tool op is a structured invocation of a Beamtalk expression" principle for no compensating benefit — MCP/LSP/REPL removal ops would still just be constructing and evaluating `Counter removeSelector: #foo` under the hood regardless, so refusing to expose that same expression directly to Beamtalk code buys nothing and costs consistency.

### Tension points

- **BEAM-veteran/operator caution vs Smalltalk-purist/newcomer discoverability:** the strongest real tension in this ADR. It resolves the same way ADR 0082's analogous tension resolved for `compile:source:` — land the primitive at the language level, and put the safety margin in receiver-side sealing checks (stdlib, `sealed`) rather than in surface restriction. A future ADR could still add release-build gating (e.g., `removeSelector:` unconditionally raising outside a workspace) if production incidents ever show that's needed — this ADR does not preclude it, it just doesn't design it preemptively without evidence, matching ADR 0082's "don't design UX ahead of usage data" stance on destructive flush.
- **Raise-by-default vs idempotent-by-default:** resolved by shipping both shapes rather than picking one — see above.

## Alternatives Considered

### Alternative: single `removeSelector:` with a boolean `raiseIfAbsent:` parameter instead of a paired method

Rejected — see Steelman above. A boolean flag is less legible at call sites than a distinct selector name, doesn't compose with a fallback *value* (only a fallback *behaviour*), and departs from the `at:`/`at:ifAbsent:` idiom this codebase and Pharo both already use everywhere else.

### Alternative: `removeSelector:` returns a `Boolean` (success/absent) instead of raising

Rejected for the bare form for the same DNU-convention and BT-785-precedent reasons covered in the Steelman section — a boolean return silently converts "this failed because you asked for the wrong thing" into a value the caller must remember to check, which this codebase's error-handling conventions (structured `#beamtalk_error{}`, DNU-on-misuse) consistently avoid for destructive class-protocol operations elsewhere.

### Alternative: workspace-level-only removal (REPL/MCP op, no `Behaviour` primitive)

Rejected — see Steelman above. Breaks ADR 0082's "every tool op is a structured Beamtalk expression" principle for no compensating safety benefit once sealing checks are in place.

### Alternative: `tryRemoveSelector:` ephemeral variant shipped alongside `removeSelector:` in v1

Considered and deferred, not rejected outright. Symmetric with `compile:source:`/`tryCompile:source:`, and the open `kind` enum accommodates adding it later without a schema break. Not included in v1 because no acceptance criterion called for it and there is no usage data yet suggesting "spike a removal, then discard it" is a real workflow distinct from "spike a removal, then `revert:` it" (which the `prev_source_ref`-backed undo already covers). Revisit if implementation or early usage shows real demand.

## Consequences

### Positive

- Closes the gap ADR 0082 explicitly left open — `Behaviour` now has a full patch/create/remove trio (`compile:source:`, `newClass:at:`, `removeSelector:`) with consistent receiver, error, and ChangeLog conventions across all three.
- Overridden-method re-exposure and extension-method removal both fall out of existing mechanisms (ADR 0032's chain-walk, `beamtalk_extensions:unregister/2`) rather than requiring new invalidation/cache logic — the design cost is almost entirely "wire up existing pieces," which lowers implementation risk for the downstream issue.
- Sealing rules reuse `removeFromSystem`'s stdlib check verbatim and extend it with one narrow, well-justified addition (`sealed` methods) rather than inventing a parallel rule set.
- ChangeLog schema extension is purely additive (`kind: "remove-method"`, new `side` field) — no existing entries or readers break.

### Negative

- The `side` field is a schema wrinkle: existing `kind: "instance"|"class"` patch entries already encode side *via* `kind`, while new `remove-method` entries encode side via the separate `side` field. A reader has to know which convention applies to which `kind` value. This is called out explicitly rather than hidden; a future schema cleanup (out of scope here) could unify the two, but doing so now would mean touching ADR 0082's already-shipped format for a cosmetic win.
- `removeSelector:` sent to a metaclass receiver (`Counter class removeSelector: #foo`) is new receiver-shape plumbing the runtime doesn't yet have for any *other* `Behaviour` primitive (`compile:source:` is instance-only) — the primitive implementation needs to detect which side `Self` is tagged as, which is a small but real asymmetry with the existing `classCompileSource` code path that instance-side-only primitives don't have to handle.
- No bulk "remove all methods" primitive ships (GemStone's `removeAllMethods` analogue) — a caller needing that has to loop over `Counter methods` and call `removeSelector:` per selector.

### Neutral

- No ephemeral removal variant in v1 (see Alternatives Considered) — revisit if usage shows demand.
- Flush behaviour for `"remove-method"` entries is fully deferred to BT-2192, same boundary ADR 0082 already drew for class-level removal.
- The `sealed`-method refusal mostly overlaps with the stdlib refusal in practice today (nearly every `sealed` method currently lives on a stdlib class); it becomes load-bearing only once/if user code declares its own sealed methods outside the stdlib.

## Implementation

*(For downstream implementation work — this ADR does not implement any of the below.)*

### Runtime hook, current line numbers

`runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl`:
- Line 951 (`put_method` handler, instance-side): `method_signatures = maps:remove(Selector, State#class_state.method_signatures)` — clears the stale signature cache as a side effect of a hot *patch*, confirmed still present at this line as of this ADR's drafting. A genuine removal handler (e.g. `{remove_method, Selector}`) does not yet exist alongside it and needs to be added — it must clear `instance_methods`, `method_source`, `method_signatures`, `method_return_types`, and any method-doc table, keep the self-dispatch process-dictionary cache (`beamtalk_class_instance_methods`) in sync the same way `put_method` already does, and call `notify_hot_patch/1` so the compiler's ambient cache reflects the removal.
- Line 980 (`put_class_method` handler, class-side): `class_method_signatures = maps:remove(Selector, State#class_state.class_method_signatures)` — the parallel class-side cache clear. Needs an analogous `{remove_class_method, Selector}` handler.

### Reuse, don't re-derive, the existing purge infrastructure

- **Local (non-extension) selector removal** needs a new gen_server call as above, plus a call into `beamtalk_xref:purge_method(ClassName, ClassSide, Selector)` (already exists, already used by `beamtalk_extensions:unregister/2` for the extension case) so the per-method xref rows don't outlive the method.
- **Extension selector removal** needs no new registry code at all — `beamtalk_extensions:unregister/2` already does the complete job (dispatch entry, source text, xref rows) for a single `{Class, Selector}`.
- **Metadata writes**, wherever this path needs to touch `beamtalk_class_metadata`'s ETS row (e.g. if selector lists are cached there), must go through `beamtalk_class_metadata:merge_identity/5` (BT-3107's per-field update) rather than an `insert/5` full-row overwrite — `insert/5` silently resets `has_runtime_class_methods`, which BT-3107 exists specifically to prevent callers from doing by accident. This mirrors BT-3105/BT-3107's core lesson: class-removal bugs came from ad hoc per-registry cleanup with no single write path; selector-removal should call into the *same* single write path (`beamtalk_class_lifecycle`-style orchestration, `merge_identity/5` for metadata), not duplicate its own copy of "which five things need to know about this."
- Whether selector removal needs its own `beamtalk_class_lifecycle`-style orchestrating function (a `method_removed/3` sibling to `class_removed/2`) or can inline its two-or-three calls directly is an implementation-time judgment call — the precedent this ADR sets is "one call site drives every derived-registry purge," however that ends up factored.

### Beamtalk-level surface to add

| Layer | Addition |
|---|---|
| `stdlib/src/Behaviour.bt` | `removeSelector:` and `removeSelector:ifAbsent:`, sealed, backed by new `@primitive`s (`classRemoveSelector`, `classRemoveSelectorIfAbsent`). |
| `runtime/apps/beamtalk_runtime/src/beamtalk_behaviour_intrinsics.erl` | New primitive functions, following `classRemoveFromSystemByName/1`'s existing shape for safety-check ordering (stdlib check, then sealed-method check, then side dispatch to the instance/class gen_server call). |
| `runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl` | New `{remove_method, Selector}` / `{remove_class_method, Selector}` gen_server handlers (see above). |
| ChangeLog append path (wherever ADR 0082's `compile:source:`/`newClass:at:` append their entries — `beamtalk_repl_eval`/`beamtalk_workspace_changelog`) | `"remove-method"` kind + `side` field support. |
| MCP / LSP / REPL / browser | A `remove_method` MCP tool (mirrors `save_method`), no new workspace-side dispatcher op — constructs and submits `Counter removeSelector: #sel` via the existing `evaluate` path, per ADR 0082's surface-parity principle. `docs/development/surface-parity.md` gains one row. |
| `docs/beamtalk-language-features.md` | Document `removeSelector:`/`removeSelector:ifAbsent:` alongside the existing `removeFromSystem` section. |

## References
- Related issues: BT-2191 (this ADR), BT-2192 (destructive workspace flush UX — blocked by this ADR per the Linear issue), BT-785 (shipped `removeFromSystem`), BT-3105 / BT-3107 (single class-removal teardown path and metadata write path this ADR's implementation plan reuses)
- Related ADRs: ADR 0082 (Method-Level Edit and Save in the Live Workspace — this ADR's direct predecessor and deferral source), ADR 0032 (Early Class Protocol — chain-walk dispatch, the mechanism that makes overridden-method re-exposure automatic), ADR 0066 (Open Class Extension Methods — the extension registry this ADR's removal path must also reach), ADR 0036 (Full Metaclass Tower — `Counter class` as an independently-dispatchable receiver, used here to select instance vs class side)
- Documentation: `docs/beamtalk-language-features.md`, `docs/development/surface-parity.md`
