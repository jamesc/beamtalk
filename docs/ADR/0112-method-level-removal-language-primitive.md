# ADR 0112: Method-Level Removal Language Primitive (`Behaviour removeSelector:`)

## Status
Implemented (2026-08-15) — shipped via Epic BT-3183, Phases 1–5 (BT-3184–BT-3190)

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

This is cache invalidation *inside an install path*, not a removal primitive — `instance_methods`/`class_methods` (the actual method tables) are simultaneously being *added to* (`maps:put(Selector, MethodInfo, ...)`) in the same handler; this hot-patch path removes nothing on its own. **A genuine removal mechanism does exist, however — just not here.** ADR 0082's `revert:`-of-an-add case (BT-2663/BT-2665) already needed "delete a live method entirely," and shipped `beamtalk_repl_eval:remove_method/3` for it: splice the method's byte span out of the class's current source, then recompile and hot-reload the whole class. This ADR's *Implementation* section generalizes that existing mechanism for `removeSelector:` rather than building a second, independent one alongside it — see *Implementation* for what has to change to make it reachable from a general-purpose primitive rather than only from `revert:`.

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
7. **Dangling senders.** Chain-walk dispatch (Constraint 1) makes removal *safe* in the memory-corruption sense — there is nothing to invalidate. It does not make removal safe in the *correctness* sense: other methods elsewhere in the codebase may still send the removed selector, and those call sites are not scanned, warned about, or updated by removal. `beamtalk_xref:purge_method/3` purges only the removed method's own *outgoing* sends/references and its own implementor row — rows recording that *other* methods call the now-removed selector are untouched (correctly — those call sites still exist in source) and become latent runtime `does_not_understand` failures the next time they execute, invisible until then. See *Decision* and *Consequences* for how this ADR treats the risk.

## Decision

**Add `Behaviour>>removeSelector:` (raises if absent) and `Behaviour>>removeSelector:ifAbsent:` (caller supplies fallback behaviour), both sealed class-side methods on `Behaviour`, operating on the instance-side method table when sent to a class object and on the class-side method table when sent to that class's metaclass object (`Counter class removeSelector: #foo`). Removal reaches whichever table currently supplies dispatch for that selector — the class gen_server's own method table, or, if the selector resolves to an extension, the `beamtalk_extensions` registry — and produces a durable `"remove-method"` ChangeLog entry attributed to whichever file owns what was removed.**

### Method name and signature

`removeSelector:` — following Pharo's `Behavior>>#removeSelector:` naming exactly. Beamtalk already has zero-friction precedent for adopting Pharo protocol names verbatim on `Behaviour` (`compile:source:`, `includesSelector:`, `canUnderstand:`, `whichClassIncludesSelector:` are all Pharo-shaped), and `removeSelector:` reads naturally next to the sibling `removeFromSystem` this ADR is modelled on. No alternative name was seriously considered — Smalltalk developers already know this exact selector, and there is no Beamtalk-specific reason to depart from it.

```beamtalk
sealed removeSelector: aSelector :: Symbol -> Behaviour =>
  @primitive "classRemoveSelector"

sealed removeSelector: aSelector :: Symbol ifAbsent: absentBlock :: Block(T) -> Behaviour | T =>
  @primitive "classRemoveSelectorIfAbsent"
```

Both return the receiver (`Behaviour`) on success — a deliberate departure from `removeFromSystem`'s `Nil` return (the sibling this ADR models most of its other decisions on), chosen because `removeSelector:` follows `compile:source:` / `tryCompile:source:`'s "returns the receiver class" convention instead, so removal chains the same way patches do (`Counter removeSelector: #a; removeSelector: #b`). `removeSelector:ifAbsent:`'s block runs (and its value is returned instead of the receiver) only when the selector is not found — mirroring `Dictionary>>at:ifAbsent:`'s established shape in this codebase, not inventing a new block-callback convention. Unlike `at:ifAbsent:` (whose success case returns a stored value, not the receiver, so its declared return is just `V`), `removeSelector:ifAbsent:`'s two outcomes have genuinely different types — receiver on success, arbitrary block-return on absence — so the return type is the union `Behaviour | T`, following the same union-return convention `whichClassIncludesSelector:` already uses (`-> Behaviour | Nil`) rather than the single-type `-> Behaviour` the bare form above can use.

### Receiver

Class-side message on `Behaviour`, sent to the class object being edited — same receiver shape as `removeFromSystem`, `compile:source:`, and `reload`. This is a natural consequence of ADR 0032: `Behaviour` is where the class protocol lives, every class object dispatches through `Class → Behaviour → Object → ProtoObject`, and `compile:source:`/`removeFromSystem` already established "the class you're editing is the receiver" as the idiom.

**Side selection follows the existing `Counter` vs `Counter class` convention**, not a boolean parameter. ADR 0036's full metaclass tower makes `Counter class` a real, independently-dispatchable receiver (the metaclass-tagged object at the same pid, per ADR 0013's virtual-metaclass design); ADR 0066's `>>` syntax already uses this to pick a side for *definition* (`Counter class >> ofSize:` vs `Counter >> increment`). Removal reuses the same signal:

```beamtalk
Counter removeSelector: #increment          // instance-side: touches instance_methods
Counter class removeSelector: #ofSize:      // class-side: touches class_methods
```

This is a deliberate asymmetry with `compile:source:`, which is scoped instance-side only in ADR 0082 (class-side patches still go exclusively through `Counter class >> sel => body` syntax, with no `Behaviour` primitive backing them). Removal needs both sides from day one because the existing removal mechanism this ADR generalizes (see *Implementation*) already takes a `Side` parameter covering both, and because there is no `>>`-shaped syntactic sugar for "delete" the way there is for "define" — `removeSelector:` **is** the only front door, so it cannot leave class-side methods unreachable the way `compile:source:` can (class-side patch has the `>>` escape hatch; class-side *removal* would have none). Extending `compile:source:` to cover class-side patches symmetrically is a reasonable independent follow-up but is out of scope here.

### Error behaviour on absent selector

**`removeSelector:` raises; `removeSelector:ifAbsent:` is the paired escape hatch.** This mirrors Pharo, where `Behavior>>#removeSelector:` signals an error when the selector is not present and `Behavior>>#removeSelector:ifAbsent:` exists precisely to let a caller supply different behaviour (see *Prior Art* — noted with appropriate uncertainty about Pharo's exact current implementation).

It also matches this codebase's own conventions more directly than Pharo does:
- CLAUDE.md's DNU rule: "An unrecognised message raises `does_not_understand`. Use `respondsTo:` to check before calling." Calling `removeSelector:` for a selector the receiver does not locally define is the same shape of "you asked for something that isn't there" as an unrecognised message — silently doing nothing would hide a caller bug (a typo'd selector, a method already removed by a concurrent session) exactly the way DNU exists to surface.
- BT-785's `removeFromSystem` raises loudly for every misuse case (class not found, stdlib class, has subclasses) rather than returning a boolean or silently no-op'ing. `removeSelector:` follows the same house style: destructive class-protocol operations raise on misuse, full stop.
- `Behaviour`'s existing `includesSelector:` (local-only presence check) already gives callers who *want* the no-op-on-absent shape a one-line way to build it (`(aClass includesSelector: #foo) ifTrue: [aClass removeSelector: #foo]`) or, more idiomatically, to use `removeSelector:ifAbsent:` directly.

The raised error is a structured `#beamtalk_error{}` with a distinct kind, `selector_not_found` — deliberately **not** `does_not_understand`, despite the surface-level similarity ("I don't have this selector to remove" reads like "I don't understand this selector"). The two are different failures: `does_not_understand` means the receiver failed to handle the *message actually sent* (`removeSelector:` itself, here, was understood and executed); `selector_not_found` means it handled that message and failed on its *argument*. Collapsing them would make any `on:do:` handler or telemetry filter keyed on `does_not_understand` silently swallow or miscount removal failures, and would produce the confusing `Counter removeSelector: #printString // => does_not_understand` for a selector `Counter` demonstrably does understand (it's simply inherited, not local). The error carries the class name, selector, and a hint pointing at `includesSelector:`, `whichClassIncludesSelector:` (which explains *why* a selector that responds to `respondsTo:` still can't be removed locally — it's inherited), and `removeSelector:ifAbsent:`.

```beamtalk
Counter removeSelector: #bogus
// => error: Counter does not define #bogus (locally or as an extension)
//    hint: use `includesSelector:` to check first, or `removeSelector:ifAbsent:`

Counter removeSelector: #printString
// => error: Counter does not define #printString locally — inherited from
//    ProtoObject (see `whichClassIncludesSelector:`); nothing to remove here

Counter removeSelector: #bogus ifAbsent: ['not found']
// => 'not found'
```

**`removeSelector:ifAbsent:`'s block runs inside the receiver class's gen_server process, with the same restrictions CLAUDE.md already documents for any block passed into a class method** ("Blocks into class methods": values and `^` cross the boundary; process-local side effects and `self()` do not; messaging the same class back raises `dispatch_error`). Concretely, `Counter removeSelector: #foo ifAbsent: [Counter compile: #foo source: '...']` — a natural-looking "recreate it if it's missing" handler — raises `dispatch_error`, because the block would be messaging `Counter` back from inside `Counter`'s own gen_server call. This is a real, non-obvious restriction on the primitive, not a "thin variant with no real cost" as the Steelman below characterises it; documented here so it isn't rediscovered as a bug report.

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

### Dangling senders: not prevented, but the tooling to surface them already exists

**`removeSelector:` does not check, warn about, or block on other code that still calls the selector being removed.** Unlike `removeFromSystem`, which refuses when the class has subclasses, there is no analogous guard here — the runtime already has the query needed to surface the risk (`SystemNavigation default sendersOf: #sel`, xref-index-backed, and the `unimplementedSelectors` lint that already exists precisely to catch dangling sends), but this ADR does not wire it into `removeSelector:` itself. Two things follow from this, deliberately:

- The error raised by `removeSelector:` on an absent selector (see *Error behaviour*) should surface a sender count when the target selector has senders and no inherited implementation, so a caller gets a warning shot at the moment of removal rather than discovering it as a runtime DNU later — see *Implementation*'s hint text.
- `beamtalk_xref:purge_method/3` must purge only the removed method's *own* outgoing sends/references, never incoming ones — see Constraint 7 and *Implementation*. Deleting incoming rows would be a real bug: it would silently disarm `sendersOf:` and `unimplementedSelectors` for exactly the call sites they exist to catch.

Fully preventing dangling sends (e.g. refusing removal when `sendersOf:` is non-empty and no inherited fallback exists) is not designed here — it would need its own steelman against the cases where removal-despite-senders is intentional (the sender is also being deleted in the same batch, or the caller is dead code). Flagged as an open question for BT-2192 or a future revision, not resolved by this ADR.

### Extension methods: removed via the same registry that installed them, attributed to the extending file

**Yes, an extension method can be removed via `removeSelector:` — sent to the *target* class, exactly as it would be sent to install one via `>>`.** `beamtalk_extensions:unregister/2` (`{Class, Selector}` keyed) drops the dispatch ETS row, the stored source-text row, and the method's own xref rows (via `beamtalk_xref:purge_method/3`, a per-selector purge distinct from the whole-class `purge_class/1` BT-785 uses) — see *Implementation* for the gaps that still need closing before it's reusable as-is. `removeSelector:`'s implementation therefore branches on *where the selector currently resolves*, and the branch order must match dispatch order exactly, not invert it:

1. **Extension registry checked first**, mirroring `beamtalk_dispatch:lookup/5`'s own resolution order ("Step 1: Check extension registry first", *before* the class's own method table) — an extension shadows a same-named local method, so removal must resolve the same winner dispatch would. Present in `beamtalk_extensions` for `{Class, Selector}` → call (the corrected) `beamtalk_extensions:unregister/2`.
2. **Local method table checked second** — selector defined in the class gen_server's own `instance_methods`/`class_methods` map → remove it via the existing recompile-based removal mechanism, generalized for this caller (see *Implementation*).
3. **Both exist** (a local method shadowed by an extension of the same name) → remove only the dispatch-winning extension; the local method is thereby re-exposed, symmetric with the inherited-method story above. A second `removeSelector:` call removes the now-dispatching local method. This case is rare but must not silently no-op against the wrong table.
4. **Neither** → absent; raise (or run the `ifAbsent:` block).

**Ownership of the removal follows ADR 0082's existing attribution precedent, not a new rule.** ADR 0082 already decided that a *patch* to an extension method is logged against the extender's file, not the extended class's file ("Extension methods (ADR 0066) — A class adding extension methods to a foreign class has its own `sourceFile`; the patch is logged against the extender's file, not the extended class's file"). Removal inherits the identical rule: `Counter removeSelector: #shout` where `#shout` was defined in `String+Shouting.bt` produces a ChangeLog entry with `sourceFile` = `String+Shouting.bt`, not `Counter.bt` — because that is the file a flush would eventually need to touch, and it is the file whose `.bt` source actually contains the method text being deleted. The multi-extender ambiguity ADR 0082 already accepted (two packages both extending `String >> shout`; last-writer-wins decides which is "the" active extension) applies unchanged: `removeSelector:` removes whichever extension is currently dispatchable, faithfully, the same way a patch would faithfully patch it. Restoring a shadowed prior extender is not this primitive's job (that is what `Workspace changes revert:` and ADR 0082's ChangeLog audit trail are for).

### Flushability, not refusal

**`removeSelector:` never refuses based on where a class lives — it always installs the removal in memory. What varies is whether the resulting ChangeLog entry is flushable**, exactly mirroring `compile:source:`'s existing rule (ADR 0082) rather than `removeFromSystem`'s outright block. An earlier draft of this ADR modelled stdlib handling on `removeFromSystem`'s hard refusal instead, justified as a "reproducible-build guarantee." That didn't hold up on review: ADR 0082 already breaches that exact guarantee for `compile:source:` — stdlib patches install in memory today, specifically to "restore a piece of Smalltalk muscle memory" the platform used to block — so the stated rationale didn't actually distinguish removal from patch. Corrected to match the sibling operation this ADR otherwise mirrors throughout:

| Class kind | `sourceFile` | Removal installs in memory? | ChangeLog `flushable` |
|---|---|---|---|
| Ordinary project class | in-project path | Yes | `true` |
| Stdlib class (`bt@stdlib@*`) | `nil` | Yes | `false`, `not_flushable_reason: "stdlib"` |
| Dynamic class (ADR 0038 ClassBuilder) | `nil` | Yes | `false`, `not_flushable_reason: "dynamic"` |
| Package dependency class | outside project tree | Yes | `false`, `not_flushable_reason: "dependency:<path>"` |

This is identical to ADR 0082's existing three-row table for `compile:source:` patches, reused rather than re-derived, and it makes the `not_flushable_reason` schema slot in *ChangeLog interaction* below (already designed with `"stdlib"`/`"dynamic"`/`"dependency:<path>"` as options) reachable for all three cases instead of `"stdlib"` being unreachable behind a refusal. `Workspace flush` skips non-flushable entries with a status line, same as it does for `compile:source:` patches today. The reproducible-build guarantee is preserved the same way ADR 0082 preserves it for patches — flush will not write into the stdlib source tree, the dependency cache, or a dynamic class's (nonexistent) file — not by refusing the in-memory operation itself.

Removing a *contested* extension selector (two extenders defining the same selector on the same class) is likewise not refused — it removes whichever extension currently wins dispatch, per *Extension methods* above.

**No separate `sealed`-method rule either.** A draft of this ADR refused removal of any `sealed` method, grounded in ADR 0032's introduction of `sealed`. That grounding didn't hold up: ADR 0032's actual text — *"users cannot override the class protocol"* — constrains third parties shadowing a sealed method, not the method's own class deleting it. It also produced an asymmetry with `removeFromSystem`: deleting an entire `sealed`-declared class outright was allowed, but deleting one `sealed` method on it was not, with no escape hatch either way. Dropped; a `sealed` method is removable the same way any other method is — subject to the same flushability table above if it happens to live on a stdlib/dynamic/dependency class. (ADR 0032 separately reserves "sealed method promotion" as a future dispatch-cache optimization; if that ships later and needs sealed methods to be immutable-once-loaded, it should add its own narrow guard scoped to that need, not reintroduce a blanket refusal here speculatively.)

```beamtalk
Integer removeSelector: #printString      // installs in memory, no error
Workspace changes dirtyMethods            // => #{Integer -> #{#printString}}
Workspace flush                           // skips it: not flushable (stdlib)

Counter removeSelector: #bogus
// => error: Counter does not define #bogus (locally or as an extension)
//    hint: use `includesSelector:` to check first, or `removeSelector:ifAbsent:`
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

### Ruby

Ruby ships **two** method-removal primitives, not one, and the distinction is directly relevant to this ADR's "removal always re-exposes the inherited method" design: `Module#remove_method` deletes the method from the receiver only — an inherited implementation of the same name, if any, becomes visible again on the next call, exactly like this ADR's `removeSelector:`. `Module#undef_method` goes further: it deletes the method **and** installs a marker that blocks fallback to any inherited implementation — the receiver raises `NoMethodError` even though a superclass defines the selector. Ruby has both because they answer different questions: "I want this class to stop having its *own* opinion on this message" (`remove_method`) versus "I want this class to categorically not respond to this message at all, superclass or not" (`undef_method`).

**Adopted:** the `remove_method` shape — this ADR's `removeSelector:` is Ruby's `remove_method`, not `undef_method`.
**Not adopted:** the `undef_method` shape ("block fallback to any inherited implementation, don't just remove the local one"). Out of scope for this ADR — no acceptance criterion calls for it, and Beamtalk's single-inheritance, no-mixin design doesn't have the multiple-module conflict scenarios that motivate `undef_method` in Ruby. Not designed here, and not treated as a cheap future add-on: an `undef`-style block would need a tombstone value in `instance_methods`/`class_methods` (today every entry is `#{block := Fun, arity := N}`-shaped), so it is left as a genuinely separate primitive to design from scratch if a real need for it ever emerges, not as a variant of `removeSelector:`.

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
- The flushability handling and the derived-registry purge both reuse infrastructure (`beamtalk_class_lifecycle`, `beamtalk_extensions:unregister/2`, `beamtalk_xref:purge_method/3`) a BEAM-literate contributor can trace directly — no hidden magic layer.

### Production operator

- A stdlib/dynamic/dependency removal can never reach disk — `flush` skips non-flushable entries, so an accidental `removeSelector:` against a production node cannot silently ship a stripped stdlib dispatch surface, even though it does take effect in that node's memory (same exposure `compile:source:` already has against stdlib today).
- Every removal is ChangeLog-audited the same way a patch is — "was this method removed, by whom, when" has a definitive answer via `Workspace changes`, matching ADR 0082's audit-trail guarantee.

### Tooling developer (LSP/MCP/browser)

- No new workspace-side dispatcher op needed, by the same reasoning ADR 0082 used for `compile:source:`/`flush`: an MCP `remove_method` tool, an LSP `executeCommand: remove_method`, a REPL `:remove-method` meta-command, and a browser "Remove Method" action all just construct `Counter removeSelector: #foo` and submit it through the existing `evaluate` op. All four are committed surfaces (see *Implementation*), not a "maybe REPL, unspecified browser" set — the surface-parity table gains one more expression-backed row, not a new protocol surface.
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
- **Why the pair wins:** both sides of this tension are satisfiable simultaneously, and the pair is *strictly* more expressive than a single method with a default-value parameter — a caller who wants "run this cleanup block on absence" (not just "return a sentinel") needs a block, not a boolean flag. Implementation cost is genuinely low: `removeSelector:ifAbsent:` is a thin variant of the same primitive, not a second implementation to maintain. It is not, however, entirely free at the *design* level — the absent-block runs inside the receiver class's gen_server, with the same restrictions as any block passed into a class method (see *Error behaviour on absent selector*), which rules out some otherwise-natural handlers (e.g. recreating the method from inside the block). That is a real, if narrow, cost worth weighing against the expressiveness gain, not a reason to reject the pairing.

### Language-level (`Behaviour` primitive) vs workspace-level (REPL/MCP-only op)

- 🧑‍💻 **Newcomer:** "A workspace-only op means I can't discover or use this from plain Beamtalk code the same way I discover `compile:source:` — it'd be tool-specific magic, not something I can read about in the language docs and try at the REPL like everything else."
- 🎩 **Smalltalk purist:** "The class protocol lives on `Behaviour` — that's the entire point of ADR 0032. A tool-only removal op would be the first crack in 'messages all the way down' for exactly the kind of operation (editing the running system) Smalltalk's philosophy is built around."
- ⚙️ **BEAM veteran:** "A workspace-only op is easier to gate behind environment checks (dev vs prod) without touching the language surface at all — Erlang/Elixir have no 'delete this exported function' language primitive either, and for good reason: it's an operational concern, not a language one."
- 🏭 **Operator:** "Confining destructive class-protocol edits to the workspace layer, not the language, means a plain compiled release build can never accidentally expose `removeSelector:` to production code — the blast radius is architecturally smaller."
- 🎨 **Language designer:** "A workspace-scoped op is simpler to design in isolation — no `Behaviour` signature to get right forever, no sealing-rule precedent to reconcile with `removeFromSystem`, easier to iterate on before commitment."
- **Why language-level wins:** the operator's "smaller blast radius" argument is the strongest one here, but it is answered directly by the flushability rule already in *Decision* — flush can never write a removal into stdlib source, the dependency cache, or a dynamic class's file, exactly the disk-side guarantee `removeFromSystem` relies on — rather than by moving the whole operation out of the language. `removeSelector:` installing in a production node's memory is no larger a blast radius than `compile:source:` already has against the same classes today; nothing about placing removal at the language level introduces a risk category ADR 0082 hasn't already accepted. Making removal workspace-only while patch (`compile:source:`) and creation (`newClass:at:`) are both language-level would also violate ADR 0082's own "every tool op is a structured invocation of a Beamtalk expression" principle for no compensating benefit — MCP/LSP/REPL removal ops would still just be constructing and evaluating `Counter removeSelector: #foo` under the hood regardless, so refusing to expose that same expression directly to Beamtalk code buys nothing and costs consistency.

### Tension points

- **BEAM-veteran/operator caution vs Smalltalk-purist/newcomer discoverability:** the strongest real tension in this ADR. It resolves the same way ADR 0082's analogous tension resolved for `compile:source:` — land the primitive at the language level, and put the safety margin in flush-side non-flushability (stdlib/dynamic/dependency source is never written) rather than in surface restriction — exactly what ADR 0082 already does for `compile:source:`. A future ADR could still add release-build gating (e.g., `removeSelector:` unconditionally raising outside a workspace) if production incidents ever show that's needed — this ADR does not preclude it, it just doesn't design it preemptively without evidence, matching ADR 0082's "don't design UX ahead of usage data" stance on destructive flush.
- **Raise-by-default vs idempotent-by-default:** resolved by shipping both shapes rather than picking one — see above.

## Alternatives Considered

### Alternative: do nothing — leave method removal undesigned in v1

A real workaround already exists without any new primitive: `Counter compile: #foo source: "^ super foo"` (or an explicit error body) overwrites a method in place, in memory, today. Rejected as sufficient because it does not achieve what "removal" actually means: it cannot restore the inherited implementation to primary status (the overwritten method is still Counter's own, just delegating), it leaves `Counter methods` and `includesSelector: #foo` reporting the selector as present when the user's intent was "gone," and it has no clean disk story distinct from any other patch. It is also not a demand-free deferral — ADR 0082 explicitly named method-level removal as a "separate design question" to solve, not an open-ended maybe, and BT-2192 (destructive workspace flush UX) is blocked on this ADR existing. Doing nothing here does not avoid a design decision, it just leaves ADR 0082's explicitly deferred one unmade.

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
- Stdlib/dynamic/dependency flushability reuses ADR 0082's existing three-row rule verbatim — no new schema or safety mechanism invented, and it makes the `not_flushable_reason: "stdlib"` slot in the removal ChangeLog schema reachable rather than dead code behind a refusal.
- ChangeLog schema extension is purely additive (`kind: "remove-method"`, new `side` field) — no existing entries or readers break.

### Negative

- **The `side` field is a required fix to ADR 0082's shipped flush/revert logic, not a cosmetic schema wrinkle.** ADR 0082 states the flush shadow key is `(class, selector)` — "on flush, only the **most recent** entry for each `(class, selector)` is applied to disk" — and that `revert:` derives side *from* `kind`: "the entry's `kind` (`instance`/`class`) carries the side." Once `kind` is spent distinguishing `"remove-method"` from a patch, it can no longer also carry side for removal entries, so a `(class, selector)` shadow key can no longer disambiguate `Counter class removeSelector: #foo` from a pending instance-side patch of `Counter >> #foo` — one could incorrectly shadow the other. **This ADR's implementation must update ADR 0082's flush shadow key and `revert:`'s side-resolution to `(class, selector, side)`**, deriving `side` from `kind` for legacy `"instance"`/`"class"` patch entries (backward-compatible) and reading it from the new `side` field for `"remove-method"` entries. This is a required change to already-shipped BT-2280-epic machinery, not an optional future cleanup — flagging it here so it doesn't surprise BT-2192's implementer.
- `removeSelector:` sent to a metaclass receiver (`Counter class removeSelector: #foo`) is new receiver-shape plumbing the runtime doesn't yet have for any *other* `Behaviour` primitive (`compile:source:` is instance-only) — the primitive implementation needs to detect which side `Self` is tagged as, which is a small but real asymmetry with the existing `classCompileSource` code path that instance-side-only primitives don't have to handle.
- No bulk "remove all methods" primitive ships (GemStone's `removeAllMethods` analogue) — a caller needing that has to loop over `Counter methods` and call `removeSelector:` per selector.
- **Dangling senders are not detected or warned about.** Removing a selector that other, unrelated methods still call — with no inherited implementation to fall through to — silently converts every one of those call sites into a latent runtime `does_not_understand`, invisible until the next time they execute. `beamtalk_xref:purge_method/3` only purges the removed method's own outgoing sends/references and its own implementor row; it deliberately leaves other methods' "I call this selector" xref rows intact (correctly — those call sites still exist), which means the existing `SystemNavigation default sendersOf:` query and `unimplementedSelectors` lint can still surface the risk after the fact, but `removeSelector:` itself does not consult them before acting. Statically typed call sites (`x :: SomeProtocol`) are not protected either — protocol conformance is structural, and `notify_hot_patch/1` only refreshes the compiler's cache for *future* compiles, not already-compiled code. `removeFromSystem`'s subclass-refusal is the closest existing analogue of a pre-flight safety check, and this ADR ships no equivalent for method-level removal; see *Decision* § Dangling senders.
- **`removeSelector:` can remove itself, with no fallback.** `removeSelector:`/`removeSelector:ifAbsent:`/`compile:source:` are themselves `sealed` methods on `Behaviour` — the root of the class protocol — and dropping the general sealed-method refusal (above) makes `Behaviour removeSelector: #removeSelector:` a legal in-memory call. Unlike an ordinary sealed method, there is no superclass above `Behaviour` for chain-walk to fall back to, so this permanently disables the removal (or patch) primitive for that running node until restart, with no drain signal. This is a more extreme instance of the same category of risk as the dangling-senders and in-flight-actor consequences above (a widely-relied-upon stdlib method disappearing mid-session), not a new mechanism — recoverable the same way any other memory-only stdlib patch is (restart), and self-inflicted rather than something ordinary usage would trigger by accident. No new refusal rule is added for it, consistent with this ADR's decision to keep the flushability rule uniform rather than special-case `Behaviour`'s own methods.
- **Already-running actor instances see the change immediately, with no drain or restart.** `removeFromSystem` stops live actors before tearing down a class; this primitive does not — chain-walk dispatch means the very next message to a running actor for the removed selector resolves to whichever inherited implementation is now visible (or DNU), with no guarantee that its state invariants match what that implementation expects. This is a live-system consequence distinct from the memory-safety property chain-walk otherwise guarantees, and distinct from the disk-side guarantee described above — flush never touching stdlib/dynamic/dependency source says nothing about what an in-flight actor observes the moment the in-memory removal takes effect.

### Neutral

- No ephemeral removal variant in v1 (see Alternatives Considered) — revisit if usage shows demand.
- Flush behaviour for `"remove-method"` entries is fully deferred to BT-2192, same boundary ADR 0082 already drew for class-level removal.

## Implementation

*(For downstream implementation work — this ADR does not implement any of the below.)*

### Local (non-extension) selector removal: generalize the existing revert-of-add mechanism, don't build a second one

**A local-method removal mechanism already exists, built for ADR 0082's `revert:`-of-an-add case (BT-2663/BT-2665), and `removeSelector:` must reuse it rather than add a second, independent removal path** — CLAUDE.md's no-duplicate-implementations rule leaves no real alternative once this is known. `beamtalk_repl_eval:remove_method/3` (delegating to `beamtalk_repl_loader:remove_method/3`) already does the following, today: resolve the target method's byte span against the class's own current source (`beamtalk_workspace_meta:get_class_source/1` + `beamtalk_compiler:resolve_method_span/4`), splice it out, and recompile + hot-reload the whole class from the resulting source via the normal reload pipeline (`reload_class_without_method/2`). Because this goes through the *ordinary* class-reload path rather than a hand-rolled map mutation, it already gets, for free, several things an earlier draft of this ADR assumed would need building from scratch: ADR 0105's dependent re-check (`maybe_trigger_recheck/4`) and signature-generation tracking, and the compiler's ambient-cache sync that a bespoke `notify_hot_patch/1` call would otherwise have to reproduce by hand. `Side` (`instance | class`) is already a parameter, so both receiver sides this ADR needs are already covered symmetrically.

**Two changes this ADR requires of the existing function, not two changes it can inherit for free:**
- **Its hardcoded stdlib refusal must be relaxed to match this ADR's flushability decision.** `beamtalk_repl_eval:remove_method/3` currently returns `{error, stdlib_method_read_only_error(...)}` outright for any stdlib class — the exact `removeFromSystem`-style hard block *Flushability, not refusal* (above) rejected for `removeSelector:`. This check needs to become conditional on the caller: `revert:`-of-an-add can reasonably keep refusing stdlib (there is no legitimate "revert" of a patch that was never flushable in the first place — see *ChangeLog interaction*), but `removeSelector:` must be able to reach it. The cleanest shape is to pass flushability-awareness (or simply "is this a `removeSelector:` call") into the shared function rather than forking it, so there remains exactly one code path, per the same no-duplicate-implementations rule this whole section exists to satisfy.
- **It does not itself log a ChangeEntry** — its own doc comment already says so explicitly ("The removal does NOT itself emit a ChangeEntry — the caller curtails the original add entry separately"), which happens to already match exactly the separation *ChangeLog interaction* below needs: `removeSelector:`'s primitive wrapper (`classRemoveSelector`) does its own best-effort ChangeLog append after a successful call, while `revert:`-of-an-add's caller keeps doing what it already does (curtailing the original add entry, not appending a new one). No change needed here — flagged so the downstream implementer doesn't accidentally add logging inside the shared function and break `revert:`'s contract.

**Cost tradeoff, noted rather than resolved here:** recompiling and reloading the whole class on every removal is heavier than the direct `instance_methods`-map deletion an earlier draft of this ADR assumed. This ADR does not treat that as a blocker — correctness and not duplicating a second mechanism outweigh the performance difference for a `removeSelector:` call, which is not expected to be a hot path — but a future optimization pass could revisit it if usage data shows otherwise.

### Extension selector removal: `beamtalk_extensions:unregister/2`, with two gaps to close

**The recompile-based mechanism above cannot reach extension methods and must not be used for them.** `beamtalk_workspace_meta:get_class_source/1` returns only the target class's *own* file content; per ADR 0066, an extension method's source lives in a *different* file entirely and is never merged into the extended class's source, so splicing the extended class's own source can't remove — or even see — an extension method. Extension removal stays on the separate path *Extension methods* above already establishes: `beamtalk_extensions:unregister/2`, a direct ETS delete, no recompile. Two gaps this ADR must close there, not inherit for free:
- `unregister/2` hardcodes `beamtalk_xref:purge_method(Class, false, Selector)` — its own doc comment says why: "extension methods are instance-side, so ClassSide = false." That assumption predates this ADR's `Counter class removeSelector:` front door. Class-side extensions exist and are stored under a *different* ETS key (`purge_class/1`'s doc: pass the class name atom for instance extensions, or `beamtalk_class_registry:class_object_tag/1`'s result for class-side ones). The removal path needs `ClassSide` threaded through — either a new `unregister/3`, or a side parameter on the existing function — so a class-side extension removal purges the right xref rows.
- `unregister/2` does not clear `?CONFLICTS_TABLE`; only `purge_class/1` does, with an explicit rationale ("so `conflicts/0` doesn't keep surfacing methods that no longer exist"). Removing a *contested* extension — the exact case *Extension methods* above allows — needs the same conflict-history cleanup for the single `{Class, Selector}` key, not just the whole-class sweep `purge_class/1` does today.
- `index_extension_xref` registers an extension's xref rows under `{Class, false, Selector}` — the identical key an instance-side *local* method uses. For the local-shadowed-by-extension case in *Extension methods* above, `purge_method/3` on either one's rows will collide with the other's. Not a blocker (the shadowed local method is re-exposed but not re-indexed until its own next recompile), but worth a code comment at the call site so the downstream implementer doesn't discover it via a debugger.

**Metadata writes**, wherever either path needs to touch `beamtalk_class_metadata`'s ETS row (e.g. if selector lists are cached there — the recompile path's normal reload flow may already handle this; verify before adding a second write), must go through `beamtalk_class_metadata:merge_identity/5` (BT-3107's per-field update) rather than an `insert/5` full-row overwrite — `insert/5` silently resets `has_runtime_class_methods`, which BT-3107 exists specifically to prevent callers from doing by accident.

### Beamtalk-level surface to add

| Layer | Addition |
|---|---|
| `stdlib/src/behaviour.bt` | `removeSelector:` and `removeSelector:ifAbsent:`, sealed, backed by new `@primitive`s (`classRemoveSelector`, `classRemoveSelectorIfAbsent`). |
| `runtime/apps/beamtalk_runtime/src/beamtalk_behaviour_intrinsics.erl` | New primitive functions. Unlike `classRemoveFromSystemByName/1` (which refuses before acting), these install unconditionally — no receiver-side check. Branch on where the selector resolves (extension vs. local, per *Extension methods* above), then call either `beamtalk_extensions:unregister/2` (relaxed for class-side, see above) or `beamtalk_repl_eval:remove_method/3` (relaxed to permit stdlib for this caller, see above) based on which metaclass tag `Self` carries. Let the ChangeLog-append layer derive `flushable`/`not_flushable_reason` from `sourceFile`, same as a patch. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_eval.erl` / `beamtalk_repl_loader.erl` | Relax `remove_method/3`'s hardcoded stdlib refusal so it's reachable from `removeSelector:` while `revert:`-of-an-add keeps refusing (see above) — no new function, an existing one gains a caller-aware conditional. |
| ChangeLog append path (wherever ADR 0082's `compile:source:`/`newClass:at:` append their entries — `beamtalk_repl_eval`/`beamtalk_workspace_changelog`) | `"remove-method"` kind + `side` field support. |
| MCP | A `remove_method` tool (mirrors `save_method`) — constructs and submits `Counter removeSelector: #sel` (or `Counter removeSelector: #sel ifAbsent: ...` if a fallback is requested) via the existing `evaluate` path. |
| LSP | `workspace/executeCommand: remove_method`, same construct-and-`evaluate` pattern as ADR 0082's `flush`/`save_class` commands. |
| REPL | `:remove-method <Class> <selector>` meta-command, CLI-side shortcut that constructs the equivalent expression, matching `:flush`/`:changes`. |
| Browser | A "Remove Method" action in the method browser, wired the same way as "Save" (per-method) is in ADR 0082. |
| All four | No new workspace-side dispatcher op needed — every surface above compiles to `evaluate` of `Counter removeSelector: #sel`, per ADR 0082's surface-parity principle. `docs/development/surface-parity.md` gains one row. |
| `docs/beamtalk-language-features.md` | Document `removeSelector:`/`removeSelector:ifAbsent:` alongside the existing `removeFromSystem` section. |

## References
- Related issues: BT-2191 (this ADR), BT-2192 (destructive workspace flush UX — blocked by this ADR per the Linear issue), BT-785 (shipped `removeFromSystem`), BT-2663 / BT-2665 (ADR 0082's `revert:`-of-an-add — shipped the `beamtalk_repl_eval:remove_method/3` mechanism this ADR's implementation plan generalizes rather than duplicates), BT-3105 / BT-3107 (single class-removal teardown path and metadata write path this ADR's implementation plan reuses)
- Related ADRs: ADR 0082 (Method-Level Edit and Save in the Live Workspace — this ADR's direct predecessor and deferral source), ADR 0032 (Early Class Protocol — chain-walk dispatch, the mechanism that makes overridden-method re-exposure automatic), ADR 0066 (Open Class Extension Methods — the extension registry this ADR's removal path must also reach), ADR 0036 (Full Metaclass Tower — `Counter class` as an independently-dispatchable receiver, used here to select instance vs class side)
- Documentation: `docs/beamtalk-language-features.md`, `docs/development/surface-parity.md`

## Implementation Tracking

**Epic:** BT-3183
**Issues:** BT-3184, BT-3185 (Phase 1 — Foundation), BT-3186 (Phase 2 — Core primitive), BT-3187 (Phase 3 — ChangeLog), BT-3188, BT-3189 (Phase 4 — Tool surfaces), BT-3190 (Phase 5 — Validation)
**Status:** Shipped — all phases complete (BT-3184–BT-3190)
