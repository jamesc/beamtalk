# ADR 0113: Destructive Workspace Operations — File Deletion and Rename in Flush

## Status
Proposed (2026-08-18)

## Context

### Problem

ADR 0082 (Method-Level Edit and Save in the Live Workspace) shipped a full patch/create story — `Behaviour compile:source:` / `tryCompile:source:`, `Workspace newClass:at:`, `Workspace flush` — and explicitly deferred anything that deletes or moves a file:

> **Class-level removal flush UX** — `aClass removeFromSystem` already exists (BT-785) for memory removal. What it should mean to *flush* a class removal — deleting a `.bt` file from disk — is irreversibly destructive and wants its own UX... **Renames** — Touches two paths (the old and the new), needs cross-file rename detection in the splice machinery, and benefits from concrete usage data from the patch-and-create case before its UX is locked in.

ADR 0112 (Method-Level Removal Language Primitive) then shipped `Behaviour removeSelector:` / `removeSelector:ifAbsent:` — the in-memory half of method removal — but drew its own scope boundary at the same line:

> **What `Workspace flush` does with a `"remove-method"` entry is explicitly out of scope for this ADR.** Splicing dead text out of a live `.bt` file is a destructive disk operation... it deserves the same confirmation/tombstone/undo design ADR 0082 already deferred class-level removal-flush to BT-2192 for.

This ADR is that design. Three flows share one root cause — flush turning a durable in-memory change into an on-disk file mutation that cannot be undone by re-running the operation, the way a patch's byte-span replacement can:

1. **Method removal flushed to disk.** `Counter removeSelector: #increment` already installs in memory and logs a `kind: "remove-method"` ChangeLog entry (ADR 0112, BT-3187). `Workspace flush` today explicitly excludes these entries from being spliced (see *Current State*) — they sit in `Workspace changes` forever, never applied, never reported as skipped-for-a-real-reason.
2. **Class removal flushed to disk.** `Counter removeFromSystem` (BT-785) already removes the class from memory, purges every derived registry (BT-3105), and returns — but it does not append a ChangeLog entry at all today. There is no record that a removal happened, and no way for `flush` to know a `.bt` file should be deleted.
3. **Renames.** No primitive exists for either class rename or method rename today. Unlike removal, rename has no existing partial implementation to extend from.

### Current State

| Concern | Today |
|---|---|
| `Counter removeSelector: #sel` | Installs in memory, logs `kind: "remove-method"` with `span`, `prev_source_ref`, `side`, `flushable` (ADR 0112, BT-3187) |
| `Workspace flush` handling of `"remove-method"` entries | **Explicitly excluded before splicing** (`beamtalk_workspace_flush:exclude_remove_method/1`) — the entry survives shadowing (so a stale patch to the same selector doesn't wrongly resurrect it) but is never written to disk and never marked flushed. It stays in `Workspace changes` indefinitely with no path forward. |
| `Counter removeFromSystem` | Removes class from memory, purges xref/extensions/protocol/compiler-cache/`class_sources` (BT-3105). **Does not append a ChangeLog entry.** No record exists that the removal happened; `Workspace changes` shows nothing. |
| Class rename | **Does not exist.** No primitive, no ChangeLog kind, no flush behaviour. |
| Method rename | **Does not exist.** No primitive, no ChangeLog kind, no flush behaviour. |
| LSP `workspace/applyEdit` | Runtime emits one `Change`-shaped edit per touched file on flush (`FlushEvent { files: Vec<String> }`, `crates/beamtalk-lsp/src/runtime.rs`) — no `DeleteFile`, `RenameFile`, or `CreateFile` operation is ever constructed. `Workspace newClass:at:` flush already creates a file that didn't exist, so even *creation* rides the generic `Change` shape today rather than the LSP's dedicated `CreateFile` resource operation. |
| Undo (`Workspace changes revert:`) | Handles patch-modify, patch-add, and `new-class` (ADR 0082). Untested/undesigned for `remove-method` (though the schema already carries `prev_source_ref` for it), and has no case for `remove-class` or any rename kind. |
| `Workspace flush` confirmation | Single mode: writes every `intent: durable AND flushable: true` entry, no distinction by how destructive the write is. |

### Constraints

1. **ADR 0082's two-phase flush protocol** (Phase A: validate + stage every write; Phase B: commit renames in sequence) is the existing atomicity mechanism for multi-file flush. Whatever this ADR adds must compose with it, not replace it — CLAUDE.md's no-duplicate-implementations rule leaves no other option once a working two-phase primitive exists.
2. **ADR 0082 Amendment 1** split the workspace into two audiences with different default postures: the LiveView cockpit is **human, git-first, `autoflush: true` by default**; MCP is **agent, ChangeLog-first, `autoflush: false` always**. Any confirmation UX this ADR designs has to work for both — a synchronous "are you sure?" dialog makes sense for a human clicking a button and is meaningless for a programmatic MCP call.
3. **ADR 0112's flushability precedent.** `compile:source:` and `removeSelector:` both install unconditionally in memory and vary only `flushable`/`not_flushable_reason` for stdlib/dynamic/dependency classes ("flushability, not refusal"). `removeFromSystem` (BT-785) instead hard-refuses stdlib and subclassed classes *before* installing. This ADR has to decide, for each new operation, which precedent it follows — they are not interchangeable, and picking wrong reopens a settled ADR 0112 argument for no reason.
4. **ADR 0032 chain-walk dispatch** makes method *removal* correct by construction (deleting a map entry is instantaneously and correctly visible — the inherited implementation reappears). Rename does not get this for free: a compiled call site's selector is baked into its own bytecode. Renaming `#increment` to `#incrementBy:` does not change what any *existing* compiled sender sends; those sends keep going to the old, now-vacated selector and become live `does_not_understand` failures the moment the definition moves, unless senders are rewritten too. This is a materially different correctness problem than ADR 0112 solved and cannot reuse its "no cache to invalidate" argument. **Class rename has the identical problem, at the class-name level:** a class reference (`Counter new`, `Counter class`, a `:: Counter` type annotation, a `subclass: Counter` superclass reference, an extension declaration in another file) compiles to a runtime lookup by name atom (`beamtalk_class_registry:whereis_class/1`), and that atom is exactly as baked into the referencing call site's bytecode as a method selector is. Re-registering a class under a new name without rewriting those references produces the same class of silent, delayed `class_not_found`/`does_not_understand` failure `renameSelector:to:` exists to prevent — this ADR treats the two as one mechanism applied at two granularities, not two independent problems (see *Decision*).
5. **One-class-per-file convention** (ADR 0082's `newClass:at:` validation: declared class name must match the file's basename). A class rename that changes the name but leaves the file path alone violates this convention the instant it's flushed — so class rename is not just a ChangeLog-schema question, it is inherently also a *file move* question.
6. **Reproducible-build guarantee** (ADR 0082, ADR 0112): flush must never write into the stdlib source tree or a dependency's cache, for any operation this ADR adds, exactly as it doesn't today for patches or method removal.
7. **Surface parity** (`docs/development/surface-parity.md`, ADR 0082's "every tool op is a structured invocation of a Beamtalk expression"). Whatever primitives this ADR adds must be reachable identically from REPL, MCP, LSP, and browser.

## Decision

**Add two new sealed `Behaviour` primitives — `renameTo:` (class rename) and `renameSelector:to:` / `renameSelector:to:ifAbsent:` (method rename) — alongside a ChangeLog logging fix to the existing `removeFromSystem`. Both rename primitives use one shared mechanism: compute every in-project reference to the old name via the xref index, and rewrite the definition plus every found reference transactionally, as one ChangeLog entry carrying a `sites` list — `renameTo:` at class-name granularity, `renameSelector:to:` at selector granularity. Extend `Workspace flush` with a second, explicit tier: ordinary `Workspace flush` applies every entry that only *edits* an existing file (patch, new-class, and now method removal, since excising a span leaves the file in place); a new unscoped `Workspace flushIncludingDestructive` selector (plus the keyword-argument `flush: aClass confirmDestructive: true` / `flushKinds: aSet confirmDestructive: true` variants, where the class/kind argument gives the second keyword a real partner) is required to apply any entry that deletes or moves a file (class removal, class rename). This tiering — not autoflush, not a tombstone file — is the safety mechanism, and it applies uniformly regardless of the `autoflush` setting.**

### Why two tiers, not three

The Linear issue's three flows collapse into two risk classes once you ask "does flushing this entry destroy or relocate a file, or only its contents?":

| Flow | What flush does | File survives? | Tier |
|---|---|---|---|
| Method removal | Excise the recorded byte span from the (still-existing) source file | Yes | 1 — ordinary flush |
| Class removal | Delete the `.bt` file | No | 2 — `confirmDestructive` |
| Class rename | Move (and possibly rewrite the declaration line of) the `.bt` file | Moved, not destroyed, but the old path is gone | 2 — `confirmDestructive` |
| Method rename | Rewrite the target file's span **and every confirmed sender's span** (`self_recv`/`super_recv` sites — see Decision; `other_recv` senders are reported as `candidate_sites`, never rewritten) | Yes, but N files are touched that the caller didn't directly ask to edit | 2 — `confirmDestructive` |

Method removal reads, at first glance, like it belongs with class removal — both stem from a `remove*` primitive. But the actual risk a confirmation gate exists to catch is "flush is about to make a file disappear or a set of files change that you didn't directly author," and splicing dead text out of an existing file is mechanically identical to what a normal patch already does unconfirmed today (byte-span replacement — the empty-replacement special case). Gating it behind `confirmDestructive` would be inconsistent with `compile:source:` for no safety benefit, and would leave `Workspace changes` permanently non-empty for the common case of "I deleted one dead method," which is exactly the stuck state *Current State* describes today.

### New primitives

```beamtalk
sealed renameTo: aNewName :: Symbol -> Behaviour =>
  @primitive "classRenameTo"

sealed renameSelector: aSelector :: Symbol to: aNewSelector :: Symbol -> Behaviour =>
  @primitive "classRenameSelector"

sealed renameSelector: aSelector :: Symbol to: aNewSelector :: Symbol
    ifAbsent: absentBlock :: Block(T) -> Behaviour | T =>
  @primitive "classRenameSelectorIfAbsent"
```

Both follow `removeSelector:`'s established shape exactly (ADR 0112): sealed class-side methods on `Behaviour`, return the receiver on success for chaining, raise a structured `#beamtalk_error{}` (`selector_not_found`, reusing ADR 0112's kind — not a new one) when the source selector is absent, and the bare form is paired with an `ifAbsent:` escape hatch rather than a boolean return, for the identical reasons ADR 0112 gives (DNU-convention, `at:`/`at:ifAbsent:` idiom, `Behaviour | T` union return). No independent steelman is repeated here for those two shapes — see ADR 0112's Steelman Analysis, which this ADR treats as settled precedent, not a decision to relitigate per-primitive.

**Receiver and side**, for `renameSelector:to:`: same convention as `removeSelector:` — `Counter renameSelector: #a to: #b` touches the instance-side table, `Counter class renameSelector: #a to: #b` touches the class-side table.

```beamtalk
Counter renameSelector: #increment to: #incrementBy
Counter class renameSelector: #ofSize: to: #withCapacity:
Counter renameTo: #Accumulator
```

**A collision with an existing name is refused for both,** loudly, the same way `removeFromSystem` already refuses a name it can't act on — `renameTo: #Existing` when `Existing` is already a loaded class, or `renameSelector: #a to: #b` when `#b` is already locally defined, raises rather than silently overwriting:

```beamtalk
Counter renameTo: #Accumulator
// => error: cannot rename Counter to Accumulator — Accumulator already exists
//    hint: remove or rename the existing class first

Counter renameSelector: #increment to: #decrement
// => error: Counter already defines #decrement locally — refusing to overwrite
//    hint: removeSelector: #decrement first, or choose a different target name
```

### `renameSelector:to:`'s sender rewrite is safe only for `self`/`super` sends — `sendersOf:` alone is not a safe basis for auto-rewriting arbitrary call sites

**A confirmed asymmetry this ADR must design around, not gloss over: `renameTo:`'s site discovery (`referencesTo:`) is keyed by *class name*, a globally unique identifier, so every site it returns genuinely is a reference to the class being renamed. `renameSelector:to:`'s site discovery (`sendersOf:`) is keyed only by *selector name* — `beamtalk_xref_senders` (ADR 0087) is a `Selector -> Sites` bag with no receiver-type field, only a coarse `recv_kind` (`self_recv | super_recv | erlang_ffi | other`) — so `sendersOf: #at:put:` returns every textual send of that selector anywhere in the project, regardless of which class's `at:put:` the sender actually meant to call.** Blindly auto-rewriting every one of those sites, as an earlier draft of this ADR proposed, is not an incompleteness risk (the *missed-reference* category ADR 0112 already accepts) — it is a *false-positive* risk: a rename of `Counter>>at:put:` to `Counter>>setAt:to:` would, unchecked, also rewrite `aDictionary at: k put: v` sends elsewhere in the project that have nothing to do with `Counter`, silently corrupting working, unrelated code. Missing a real reference and mangling an unrelated one are not the same failure class, and this ADR was conflating them.

**The fix uses `recv_kind`, which the xref schema already carries, to split `sendersOf:`'s results into two tiers, not one:**

- **Auto-rewritten (`confirmed` sites):** the definition itself, plus any site with `recv_kind: self_recv` or `recv_kind: super_recv`. These are safe *by Smalltalk semantics, not by inference* — a `self #{oldSelector}` or `super #{oldSelector}` send inside a method can only ever dispatch within the sending object's own class hierarchy, so if the selector matches, the site unambiguously targets the method being renamed (or an override of it), the same way `super`'s target is already unambiguous to chain-walk dispatch (ADR 0032). No type narrowing is needed because the language guarantees it structurally.
- **Reported, not rewritten (`candidate` sites):** every site with `recv_kind: other` (an arbitrary expression receiver — the common "external caller" case, and also the most common shape of a real, intended sender) or `recv_kind: erlang_ffi`. The xref index cannot prove these target `Counter` rather than some unrelated class that happens to implement the same selector, so `renameSelector:to:` does not touch them. They are surfaced the same way ADR 0112's dangling-sender hint already surfaces risk to a human/agent — as a reported count and site list on the primitive's return value and the resulting ChangeLog entry — so the caller can inspect each one and, where it genuinely is a Counter-directed send, patch it manually via the ordinary `compile:source:` path.

This is a real, discovered-in-review scope reduction from "renaming a method automatically fixes every caller" to "renaming a method automatically fixes its own definition and hierarchy-internal calls, and hands you a reviewed list for everything else" — narrower than the ADR's earlier framing, and narrower than what a type-checked language's rename-refactoring tool can offer (see *Prior Art* — Python/TypeScript comparison), but it is the boundary the *existing* xref infrastructure can actually support without corrupting unrelated code. Extending `beamtalk_xref_senders` with inferred-receiver-type narrowing (leaning on ADR 0025's gradual typing where a receiver carries a `:: Counter` annotation) would let a future revision safely promote more `other`-kind sites into the auto-rewritten tier — flagged here as follow-up work, not designed by this ADR. The `rename-class` / `renameTo:` side of this ADR is unaffected: class names have no equivalent ambiguity, so its `sites` list is fully auto-rewritten as originally designed (see *ChangeLog schema extensions*, updated below to reflect the two-tier `rename-method` shape).

### `renameTo:` also rewrites cross-file references — the same mechanism as `renameSelector:to:`, one level up

An earlier draft of this ADR gave `renameTo:` no sender-rewrite mechanism at all — it re-registered the class under its new name and left every *other* file's reference to the old name unaddressed. That does not hold up (see Constraint 4): `Counter new`, `Counter class`, a `:: Counter` type annotation, a `subclass: Counter` superclass reference, and an extension declaration in another file (ADR 0066) all compile to a runtime lookup by name atom (`beamtalk_class_registry:whereis_class/1`), and that atom is baked into the referencing call site exactly as a method selector is baked into a sender. A bare re-registration turns every one of those into a silent, delayed `class_not_found` the next time it executes — precisely the failure mode this ADR already built `renameSelector:to:`'s `sites` mechanism to prevent, one granularity down.

`renameTo:` therefore uses existing, already-shipped infrastructure for site discovery, not a new query invented for this ADR: **`SystemNavigation default referencesTo: aClass` (ADR 0087, BT-2302) already is** the "class → referencing sites" index — a maintained xref table (`beamtalk_xref_references`, doc'd as "class -> sites that reference it") populated at class-load time, sub-millisecond, already covering constructor/message sends, type annotations (including generic parameters like `List(Counter)`), and extension-method references (ADR 0066). An earlier draft of this section proposed inventing a new `referencesOfClass:` query as `sendersOf:`'s class-level sibling — that would have duplicated a capability ADR 0087 already built and shipped, exactly what CLAUDE.md's no-duplicate-implementations rule exists to catch; `renameTo:`'s site discovery calls the same runtime-side function `referencesTo:` already calls (`beamtalk_xref:references_to_bt/1` or its underlying table read), not a parallel mechanism.

**One reference kind `referencesTo:` does *not* cover: superclass declarations.** `referencesTo:`'s own doc comment scopes it to "class-body reference sites" — occurrences inside a method's body or signature — and a class's own declaration header (`Object subclass: #SpecialCounter ... `, naming `Counter` as an ancestor via `superclass:` or the class-hierarchy metadata) is not inside any method body, so it is outside what the xref table indexes. Renaming `Counter` therefore needs a second, separate source for this one reference kind: `beamtalk_class_registry:direct_subclasses/1` (already used by `removeFromSystem`'s subclass-refusal check, BT-785) returns exactly the classes that need their declaration header's superclass reference rewritten — only *direct* subclasses need touching, since a transitive subclass's own declaration names its direct superclass, not `Counter`, and that direct superclass's declaration is unaffected by a rename two levels up. `renameTo:`'s full site list is therefore the union of `referencesTo: aClass` (body/type/extension references) and `direct_subclasses(aClass)` (superclass-declaration references), not one query — this union is exactly what Phase 3's validation spike (see *Phased rollout*) needs to confirm is exhaustive, and it is honestly a more composite mechanism than this ADR's earlier framing ("the class-level analogue of `sendersOf:`") suggested.

**A second, more concrete gap, worse than the generic "dynamic lookup" risk category: the xref index's `references` channel is unconditionally empty for live-patched methods, not merely incomplete for exotic dynamic dispatch.** `beamtalk_xref:build_method_entry/5` — the function every `>>`/`compile:source:` live patch and every sourced extension registration routes through (`beamtalk_object_class:put_method/4`, `beamtalk_extensions:register/5`) — hard-codes `references => []` unconditionally, with its own doc comment explicitly noting why: "there is no runtime 'all references' walker, so class references are left empty for live edits; the class-reference channel is fully populated only at compile time." Concretely: `Counter class >> makeOne => ^Accumulator new` patched live via `>>` produces a `references: []` xref row for `makeOne`, so `referencesTo: Accumulator` will not surface it as a site — not because the reference is exotic or dynamically constructed, but because live-patching is the exact everyday workflow ADR 0082 exists to support, and the reference-indexing half of ADR 0087's xref machinery was never extended to cover it (unlike the *sends* channel `sendersOf:`/`renameSelector:to:` rely on, which `build_method_entry/5` does compute for live patches via `sends_from_source/1`, except for sourceless `unindexed_runtime_fun` entries). This means `renameTo:`'s completeness is measurably *weaker* than `renameSelector:to:`'s for any class with live-patched methods still pending flush — a materially different, and more common, failure mode than the "dynamically constructed lookup" framing below suggests, and one this ADR should name rather than silently generalize away. Phase 3's validation spike must include a live-patched fixture (patch a method via `>>`, then rename a class it references) specifically because the stdlib+examples corpus alone — compiled from disk, never live-patched — would not surface this gap. Closing it for real (extending `beamtalk_xref` with a runtime references walker) is out of scope for this ADR; `renameTo:` ships against the index as it exists today, with this gap documented rather than hidden.

**A related, narrower gap worth naming rather than folding silently into "dynamic lookup": plain string/comment occurrences of a class's name** (an error message, a test assertion string, a doc comment) are invisible to any AST-based reference index by construction and are never rewritten by either `referencesTo:` or `direct_subclasses/1` — a "clean, N files touched" rename can still leave stale prose scattered through `stdlib/test/*.bt` or `docs/`. Neither this nor the live-patch gap above are blockers (see *Alternatives Considered* — refusing on incomplete `sites` was already rejected for `removeSelector:`/`renameSelector:to:` on the same reasoning), but conflating them under one "dynamic lookup" label undersells how much of the residual risk is structural rather than exotic. The dangling-reference risk that remains after combining `referencesTo:` and `direct_subclasses/1` — everything above, plus a genuinely dynamically constructed `Smalltalk at: (aString asSymbol)`-style lookup — is the same *category* of accepted risk ADR 0112 established for `removeSelector:`, but this ADR should not claim it is the same *size* of risk without naming what's actually in it (see *Alternatives Considered*, *Consequences*).

### `Workspace moveClass:to:` — pure file move, no identity change

Referenced by *Undo story* and *Implementation* below but distinct enough from `renameTo:` to need its own definition: `Workspace moveClass: aClass to: aNewPath` relocates a class's `.bt` file without changing the class's name. Unlike `renameTo:`, no cross-file reference needs rewriting — every call site still says `Counter`, only where `Counter.bt` lives on disk changes — so `moveClass:to:` has no `sites` beyond the single file being moved, and reuses only the plain-move half of *Multi-file atomicity*'s Class rename row (the reference-rewrite half is skipped entirely).

```beamtalk
sealed moveClass: aClass :: Behaviour to: aNewPath :: String -> Behaviour =>
  @primitive "workspaceMoveClass"
```

This is a `Workspace`-level operation, not a `Behaviour` primitive, for the same reason `newClass:at:` is (ADR 0082): it's a pure filesystem-organization concern, not a class-protocol message the class itself needs to understand or respond to differently. It produces a `kind: "rename-class"` entry with `old_class == class` (signalling "same identity, different path" to any reader of the schema) and a `sites` list containing only the moved file's own declaration-line entry — never a foreign reference, since none needs rewriting.

**Refusal/flushability** mirrors `renameTo:`'s row in the table below with one difference: a dynamic (`ClassBuilder`) class has no file to move at all, so `moveClass:to:` raises `no_source_file` for it rather than `renameTo:`'s permissive `flushable: false` — moving *nothing* is not the same kind of legitimate in-memory action as patching a dynamic class's body is, since there is no operation for the in-memory step to actually perform.

### Refusal vs flushability, decided per operation, not uniformly

ADR 0112 chose "flushable, not refusal" for `compile:source:`/`removeSelector:` (they install in memory unconditionally against stdlib/dynamic/dependency classes; only the disk write is gated). `removeFromSystem` chose the opposite — hard refusal before any memory mutation. This ADR does not pick one rule for everything; it asks, per primitive, whether the *in-memory* effect alone is a reasonable Smalltalk-style live-debugging action against a class you don't own the source of, the same question ADR 0112 asked and answered differently for method patches versus whole-class teardown:

| Primitive | Stdlib | Dynamic (ClassBuilder) | Dependency | Rationale |
|---|---|---|---|---|
| `removeFromSystem` (unchanged, BT-785) | **Refuse** | Allowed | **Refuse** (module not in project) | Unchanged from today — this ADR does not revisit BT-785's stdlib/dependency refusal, only adds ChangeLog logging to it (see below). |
| `renameTo:` | **Refuse** | Allowed, `flushable: false` (`"dynamic"`) | **Refuse** | The xref index only indexes in-project source, so the `sites` mechanism above can only ever compute a *complete* reference list for a class the project actually owns the callers of — renaming a stdlib or dependency class would silently miss every reference living outside the project (stdlib call sites across every other user's code; a dependency's own internal senders), which is a strictly worse outcome than `compile:source:`'s existing "sealed, others can still see the drift via `Workspace changes`" story. A dynamic class has no external file-based referents the xref index would need to reach beyond what's already indexed (ADR 0038); it is the caller's own construction. |
| `renameSelector:to:` | Allowed, `flushable: false` (`"stdlib"`) | Allowed, `flushable: false` (`"dynamic"`) | Allowed, `flushable: false` (`"dependency:<path>"`) | Same granularity argument ADR 0112 already made for `removeSelector:` — a single-selector operation, not a whole-class identity change; `Integer renameSelector: #printString to: #displayString` is exactly as legitimate a live-debugging move as `Integer compile: #printString source: ...` already is today. Its `sites` list is subject to the identical in-project-only xref limitation, but a stray un-rewritten sender of one renamed *selector* is a narrower blast radius than a whole class silently losing referential integrity project-wide. |

`removeFromSystem`'s stdlib/subclass refusals are untouched by this ADR — only the ChangeLog-logging gap is fixed (see next section). Revisiting BT-785's refusal policy is out of scope; nothing here found a reason to.

### Fixing `removeFromSystem`'s missing ChangeLog entry

**Required fix, not new ground:** `Counter removeFromSystem` must append a `kind: "remove-class"` ChangeLog entry on every successful removal of a flushable class, mirroring the audit-trail-is-unconditional rule ADR 0082 established for every other in-memory mutation ("every in-memory method mutation produces a ChangeEntry. Always."). Today it produces none, which means `Workspace changes` cannot answer "was this class removed, and is that removal reflected on disk?" — the same gap ADR 0082's audit trail exists to close everywhere else. Because `removeFromSystem` already refuses stdlib/dependency classes before acting, the entry it appends is always either `flushable: true` (ordinary project class) or `flushable: false, not_flushable_reason: "dynamic"` (ClassBuilder class, no file to delete) — never `"stdlib"` or `"dependency:..."`, since those cases never reach the append point.

### ChangeLog schema extensions

Extending ADR 0082's open `kind` enum exactly where it said it would (`"remove-method"` already shipped via ADR 0112/BT-3187; `"remove-class"`, `"rename-class"`, `"rename-method"` are new):

```text
%% remove-class — appended by removeFromSystem
{ts, seq, epoch, class, selector: null,
 kind: "remove-class",
 side: null,
 source_ref: null,
 prev_source_ref: "<seq>-prev.bt" | null,     % full class source, for revert:
 sourceFile: "<path>" | null,
 span: null,                                    % whole file, not a byte range
 intent: "durable",
 flushable: bool,
 not_flushable_reason: "dynamic" | null,        % never "stdlib"/"dependency" — refused earlier
 author, author_kind: "human" | "agent"}

%% rename-class — appended by renameTo:
{ts, seq, epoch, class: "<new name>", selector: null,
 kind: "rename-class",
 side: null,
 old_class: "<old name>",
 old_path: "<path>" | null,
 new_path: "<path>" | null,                     % basename derived from new_class, same directory as old_path
 sites: [{sourceFile, span: {start, end}, source_ref, prev_source_ref}, ...],
   %% sites[0] is the class's own declaration line, UNLESS the class is
   %% dynamic (ClassBuilder, no backing file — flushable: false, "dynamic"
   %% below) — a dynamic class has nothing for sites[0] to point at, so its
   %% rename entry has sites[0] = null (in-memory identity change only,
   %% recorded for revert/audit but not a splice target) and sites[1..] are
   %% every current in-project cross-file reference (constructor/message
   %% sends, type annotations, superclass declarations, extension
   %% declarations) found via the xref index at rename time — same shape
   %% and same discovery mechanism as `rename-method`'s `sites` (see
   %% Decision § "renameTo: also rewrites cross-file references")
 source_ref: null, prev_source_ref: null,        % superseded by per-site refs above; no single-file body to record
 sourceFile: null,                                 % ambiguous for a multi-file entry — see sites
 span: null,
 intent: "durable",
 flushable: bool,                                  % true iff every site's file is flushable, same rule as rename-method
 not_flushable_reason: "dynamic" | null,
 author, author_kind}

%% rename-method — appended by renameSelector:to:
{ts, seq, epoch, class, selector: "<new selector>", old_selector: "<old selector>",
 kind: "rename-method",
 side: "instance" | "class",
 sites: [{sourceFile, span: {start, end}, source_ref, prev_source_ref}, ...],
   %% sites[0] is always the definition site; sites[1..] are `self_recv`/
   %% `super_recv` sends found via sendersOf: at rename time — the only
   %% sender shape the xref index can attribute to this class unambiguously
   %% without receiver-type narrowing (see Decision § "renameSelector:to:'s
   %% sender rewrite is safe only for self/super sends"). These are the
   %% ONLY sites `Workspace flush` ever writes for this entry.
 candidate_sites: [{sourceFile, span: {start, end}}, ...],
   %% `other_recv`/`erlang_ffi` sends found via the same sendersOf: query —
   %% reported for human/agent review, never auto-rewritten and never
   %% written by flush. No source_ref/prev_source_ref: nothing here is
   %% ever spliced, so there is no prior/new body to record.
 source_ref: null, prev_source_ref: null,        % superseded by per-site refs above
 sourceFile: null,                                 % ambiguous for a multi-file entry — see sites
 span: null,
 intent: "durable",
 flushable: bool,                                  % true iff every entry in `sites` (not `candidate_sites`) is in a flushable file
 not_flushable_reason: "stdlib" | "dynamic" | "dependency:<path>" | null,
 author, author_kind}
```

`rename-class` and `rename-method` are the two genuinely new shapes — every other kind ADR 0082/0112 defined targets exactly one file; these two target a computed set. For both, `flushable` is `true` only if **every entry in `sites`** (definition + all current confirmed references — for `rename-method`, this explicitly excludes `candidate_sites`, which are never written regardless of flushability) resolves to a flushable file; if even one confirmed reference lives in a dependency or stdlib file, the whole rename entry is `flushable: false` with that reason, because a rename that could only partially reach disk (definition renamed, some confirmed references left pointing at the old name) is worse than not flushing at all — it would silently split the live and on-disk surface for those files. `candidate_sites` entries never gate `flushable` either way, since flush never writes them — a stdlib class being an `other_recv` candidate sender does not block an otherwise-clean rename the way a confirmed stdlib site would. (In practice `rename-class` never reaches the confirmed-site refusal branch for stdlib/dependency — it refuses before installing, per the table above — so `not_flushable_reason` on a `rename-class` entry is always `"dynamic"` or absent; the `flushable`/per-site rule is stated generally here because `rename-method` does reach it.)

### `Workspace flush` — the destructive tier

```beamtalk
Workspace flush
=> flushed 2 methods across 1 file
   skipped: 1 destructive entry (Counter — remove-class) —
     use `Workspace flushIncludingDestructive` to include it

Workspace flushIncludingDestructive
=> flushed 2 methods + 1 removal + 1 rename across 4 files
```

- `Workspace flush` (no argument, existing signature) applies Tier 1 only: patches, `new-class`, and now `remove-method`. Tier 2 entries (`remove-class`, `rename-class`, `rename-method`) are reported in the summary as `skipped: destructive`, distinctly from the existing `skipped: ephemeral` / `skipped: not flushable (...)` reasons — a caller needs to be able to tell "this needs a human/agent decision" apart from "this can never flush."
- `Workspace flushIncludingDestructive` (new, unscoped) additionally applies Tier 2 across the whole pending set — a bare unary selector, not a keyword message, because there is no class/kind argument to attach a `confirmDestructive:` keyword to once the call is unscoped; Smalltalk keyword messages cannot omit an argument the way an "optional parameter" language could. `Workspace flush: aClass confirmDestructive: true` scopes Tier 2 to one class (the class argument gives `confirmDestructive:` a real keyword partner, so this stays an ordinary two-keyword message); `Workspace changes flushKinds: #{#'remove-class'} confirmDestructive: true` scopes to one kind the same way — the existing `flushKinds:` filter combinators (ADR 0082) already compose with a scope, so `confirmDestructive:` is one more independent filter dimension on the keyword forms, not a special case bolted onto each existing form.
- **The destructive tier is never silently on — reaching it always requires either the distinct `flushIncludingDestructive` selector or an explicit `confirmDestructive: true` argument, never a workspace setting or environment variable.** This is deliberate: a config toggle that silently reclassifies future destructive flushes as safe is exactly the kind of "surprise later" a security-relevant default should not create. Each destructive flush call names its own consent, whether by selector or by argument.
- `autoflush: true` (Amendment 1's cockpit default) **never** implies `confirmDestructive: true`. A human clicking "Save" on a live-patched method still autoflushes immediately (Tier 1, unchanged); a human clicking "Remove Class" or "Rename" always surfaces an explicit second gesture, regardless of the autoflush setting — see *Surface* below for what that gesture is per surface. This is the one place autoflush's "one switch, applied uniformly" statement (ADR 0082) gets a second, independent switch layered on top, and it is layered on **on purpose**: autoflush answers "do my edits reach disk without a separate step," `confirmDestructive` answers "do file-destroying edits reach disk without a separate *acknowledgement* step" — two different questions that happen to share a word, "immediately."

### Multi-file atomicity — extending ADR 0082's Phase A/B, not replacing it

ADR 0082's two-phase protocol (Phase A: validate every target, stage every write as `<file>.tmp`; Phase B: rename each `.tmp` into place, sequentially, entries pruned only as each rename succeeds) already handles "write N files, all-or-nothing modulo a documented partial-failure mode." This ADR extends the *staging* step to cover delete and rename, keeping the same two-phase shape:

| Operation | Phase A (stage) | Phase B (commit) |
|---|---|---|
| Patch / new-class (existing) | Write `<file>.tmp` | Rename `<file>.tmp` → `<file>` |
| Method removal (Tier 1) | Write `<file>.tmp` with the span excised | Rename `<file>.tmp` → `<file>` (identical to a patch — this is why Tier 1 needs no new atomicity work) |
| Class removal | Rename `<file>` → `<file>.tmp-delete-<epoch>-<seq>` (same-filesystem rename, POSIX-atomic, trivially reversible) | `unlink <file>.tmp-delete-<epoch>-<seq>` |
| Class rename | Write `<new_path>.tmp` (declaration-line rewritten to the new name; rest of the file byte-identical) **and** `<file>.tmp` per *other* site file with the old-name reference rewritten (same per-site step as method rename, below) | Rename `<new_path>.tmp` → `<new_path>`, `unlink <old_path>`, then rename each site `<file>.tmp` → `<file>`, in seq order |
| Method rename | Write `<file>.tmp` per affected file (definition site + every *confirmed* sender site, spans rewritten — `candidate_sites` are never staged or written) | Rename each `<file>.tmp` → `<file>`, in seq order — same sequential-commit, partial-failure-is-recoverable-via-re-flush shape ADR 0082 already documents for ordinary multi-file flush |

A Phase A failure (a target span no longer resolves — see *External-edit conflicts* below) aborts the whole batch before anything in Phase B runs, exactly as today. A Phase B failure partway through a multi-file rename leaves some files renamed and some not; the per-file status report (already part of ADR 0082's flush summary) tells the caller which, and re-issuing the same destructive flush call (`flushIncludingDestructive`, or the scoped `flush: aClass confirmDestructive: true` form) retries only what's left — the staged `.tmp-delete-*` files for not-yet-committed deletions are still present on disk (nothing was lost), and already-committed unlinks/renames are not retried because their ChangeEntries were already pruned.

**Class removal's staged-rename step is the closest thing this ADR has to a tombstone, and it is intentionally ephemeral, not persistent** — see *Steelman Analysis*.

**This table covers the *disk* half of atomicity only. The *in-memory* half — rewriting confirmed sites across N separate class gen_servers before any flush happens at all — is a real, separate correctness question this ADR does not fully design, and downstream implementation must not treat it as solved by analogy to the table above.** Each site's recompile-and-hot-reload goes through its own class gen_server (the same per-class-generation mechanism ADR 0112's `remove_method/3` already uses), and OTP has no cross-process transaction primitive spanning them — if rewriting confirmed site 5 of 10 fails partway through the in-memory step (a concurrent independent edit to that file's class, a compile error introduced by an interleaving patch), the rename is left half-applied *in memory*, before flush is even in the picture: some classes now send the new selector, others still send the old one, and (per ADR 0082's existing "cannot roll back a hot-reloaded module once live actors may hold references to it" precedent) there is no clean rollback. Downstream implementation needs an explicit answer here — most plausibly, validate every confirmed site's compile-ahead-of-mutation (mirroring flush's own Phase A "stage everything, validate everything, then commit" shape, but at the in-memory recompile step rather than the disk-write step) so a failure aborts before any class is actually re-installed — but this ADR does not specify that mechanism, and it is a materially different risk than anything the *disk*-side Multi-file atomicity table above addresses. Flagged as a required design decision for Phase 3's implementer, not resolved here.

### Undo story

`Workspace changes revert:` (ADR 0082) extends to all three new kinds, and the extension is symmetric with `prev_source_ref`'s existing role:

| Kind | `revert:` behaviour |
|---|---|
| `remove-method` | Re-installs `prev_source_ref` at the recorded selector/side — the recorded prior body is exactly what `compile:source:` needs, so revert is a patch back to the pre-removal method. Already implied by the schema (ADR 0112 recorded `prev_source_ref` on removal specifically for this); this ADR is the first thing that actually exercises it. |
| `remove-class` | Recompiles and reinstalls the whole class from `prev_source_ref` (the full pre-removal source, captured at hook time by `removeFromSystem`'s new logging step) via the same `Workspace newClass:at:`-shaped install path `new-class` revert already uses (ADR 0082, BT-2664) — reusing that path rather than inventing a second whole-class-install mechanism. |
| `rename-class` | Restores the class's identity/path (`old_class`/`old_path` — no `prev_source_ref` needed for this half, matching `new-class`'s "add-removal needs no prior body" precedent) **and** re-splices every entry in the recorded `sites` list back to its own `prev_source_ref`, directly — the identical reasoning `rename-method` uses below, not the exemption an earlier draft of this row claimed. This is a restore against the entry's own recorded `sites`, **not** a fresh call to the public `renameTo:` primitive (which would re-run xref discovery against the class's *current*, post-rename state and could silently compute a different site list than the original rename touched, if any referencing file was independently edited in between — the same risk `rename-method`'s row below is written to avoid). |
| `rename-method` | Restores every entry in the recorded `sites` list back to its own `prev_source_ref`, directly against those recorded locations — not a fresh call to the public `renameSelector:to:` primitive, for the same reason: a sender's surrounding code may have changed between the rename and the revert in ways a blind re-rename (re-discovering senders via xref) wouldn't reproduce byte-for-byte. |

**Once flushed, `revert:` degrades to "best-effort, pre-flush semantics only"** for the same reason ADR 0082 already documents for ordinary flushed patches — the ChangeEntry is pruned on successful flush, so post-flush undo is git's job (for humans, per Amendment 1) or a fresh corrective operation (for agents, who can re-run `newClass:at:` from the same `prev_source_ref` snapshot if they kept it — the ChangeLog's own audit/archive retains it per ADR 0082's rotation policy even after pruning from the active view). This ADR does not add a third undo mechanism beyond "revert before flush" and "git/re-create after flush" — see *Steelman Analysis*, tombstone question.

### External-edit conflicts

Reuses ADR 0082's `(mtime, content-hash)` snapshot-and-compare mechanism verbatim, extended to the two new failure shapes destructive operations introduce beyond "content changed":

| Conflict | Detection | Resolution |
|---|---|---|
| Target file's content changed since the entry was logged (patch, `remove-method`) | Existing mechanism, unchanged | Existing choices: `flush:force`, `changes clear`, `changes diff:` |
| Target file for a `remove-class` or a `rename-class`/`rename-method` definition site was already deleted externally | `stat` fails at Phase A | Surfaces as `already gone — nothing to remove` (for `remove-class`, a soft success: the entry is pruned, the outcome the user wanted already holds) or `source file relocated or deleted` (for a rename's definition site, ADR 0082's existing relocation conflict kind, unchanged) |
| Target *path* for a `rename-class`/`rename-method` site already exists externally (someone else created a file at the new name first) | `stat` succeeds unexpectedly at Phase A for what should be a fresh path | New conflict kind, `target path collision` — refuses the whole batch (Phase A abort), same "abort, nothing committed" shape as any other Phase A failure |
| A `rename-class` or `rename-method` reference site's span no longer resolves (the site was independently edited since the rename was computed) | Existing byte-span resolution, per-site | That one site fails Phase A, aborting the whole rename entry (not a partial rename — see *Consequences*) |

### Reproducible-build guarantee

Unaffected by construction: `renameTo:`/`removeFromSystem` refuse stdlib/dependency classes before any ChangeEntry exists; `renameSelector:to:`'s per-site `flushable` check means a rename touching even one stdlib/dependency sender file never reaches Phase A for *any* of its sites. Flush still never writes into the stdlib tree or a dependency cache — the same guarantee ADR 0082/0112 already state, unmodified.

### Surface

Per ADR 0082's principle, every surface constructs one of the Beamtalk expressions above and submits via the existing `evaluate` op — no new workspace-side dispatcher op. What differs by surface, per Amendment 1, is **what "confirm" means**:

| Surface | Audience | Confirmation gesture |
|---|---|---|
| REPL | Human (or scripted) | `:remove-class <Class>` prompts `y/N` at the terminal before constructing `Counter removeFromSystem` **and then**, if the class was flushable, a second prompt before running the follow-up `:flush-destructive` (a distinct meta-command, mirroring the `:flush`/`:flush <Class>` pair with a `:flush-destructive`/`:flush-destructive <Class>` pair rather than a flag on `:flush` — the REPL meta-command layer has no `--flag` precedent, only positional args) — or the human runs `:flush` (Tier 1 only) and later `:flush-destructive` explicitly. Two prompts, matching two genuinely separate decisions (remove from memory vs. delete from disk). |
| MCP | Agent | `remove_class`, `rename_class`, `rename_method` tools construct the memory-mutating expression only — they do **not** implicitly flush. A distinct `flush` tool call with an explicit `confirm_destructive: true` argument is required to reach disk, mirroring `try_method` → `save_method`'s existing two-step promotion idiom (ADR 0082): the *first* call is exploratory/reversible-via-revert, the *second* is the one that actually commits. No interactive dialog exists for MCP, so the tool schema's required boolean argument **is** the confirmation — an agent cannot flush a destructive entry by accident because the parameter has no default. |
| LSP | Editor (VSCode etc.) | `workspace/executeCommand: flush` gains an optional `confirmDestructive` argument; the VSCode extension surfaces a native modal ("This will delete `foo.bt` and 2 other files — Continue?") before sending it, listing every affected path from `Workspace changes` (already queryable pre-flush). |
| Browser | Human, cockpit | A "Remove Class" / "Rename" action performs the memory-mutating call immediately (matching `autoflush: true`'s existing "the memory step is not gated" behaviour for ordinary patches) but the resulting dirty indicator for that entry renders with a distinct "destructive — needs confirmation" affordance instead of silently participating in the autoflush write, requiring one explicit click ("Delete file" / "Rename file") to actually call `confirmDestructive: true`. This is the browser's analogue of the REPL's second prompt — same two-decision shape, native-to-cockpit affordance instead of a terminal prompt. |

`docs/development/surface-parity.md` gains four rows (`removeFromSystem`'s logging fix needs no new row — it is the same expression as today, just with an audit-trail side effect) and a note that `confirmDestructive` is a parity-preserving *argument*, not a surface-specific rule: every surface can express "flush without destructive entries" and "flush including them," they just gate the second one differently because a modal dialog and a required tool-schema argument are the same *shape* of gate (an explicit, un-defaultable extra step) expressed in each surface's native idiom.

## Prior Art

### Pharo / Squeak Smalltalk — Refactoring Browser and `.changes`

Pharo's Refactoring Engine (`RBRenameClassRefactoring`, `RBRenameMethodRefactoring`, etc.) computes every affected reference *before* applying anything, previews the full change set to the user, and applies it as one atomic transaction across the image — rename in Pharo has never been "rewrite the definition and hope callers notice," it has always been "find every sender first, rewrite all of them together." Method/class removal in Pharo (`Behavior>>#removeSelector:`, `SystemDictionary>>#removeClassNamed:`) is comparatively blunt by contrast — no automatic "are there senders?" gate, matching ADR 0112's own choice not to block `removeSelector:` on dangling senders.

**Adopted:** the "compute every affected site before touching anything, apply as one transaction" shape for both `renameTo:` and `renameSelector:to:` — this is exactly the multi-site `sites` list and Phase A validate-everything-before-writing-anything design above.
**Adapted:** Pharo's rename preview is an interactive image-browser step with no disk/memory distinction (there is no flush). This ADR's `confirmDestructive` argument is the same *intent* (don't let the change happen without the human seeing what it touches) reshaped for a system where memory and disk are already two separate steps (ADR 0082) — the preview is `Workspace changes` (or its per-site diff) queried *before* the confirming flush call, not a bespoke dialog Pharo's image model doesn't need.
**Rejected:** Pharo's removal bluntness *for class removal specifically* — this ADR gates `remove-class`'s disk step behind `confirmDestructive`, where Pharo's `removeClassNamed:` just does it. The difference is disk-vs-memory: Pharo's removal is memory-only the same instant it happens (an image save is a separate, much coarser gesture); ours makes an irrecoverable filesystem change unless gated.

### Git — `rm` and `mv` as distinct verbs from `add`/`commit`

Git's index model treats a delete or rename as a first-class staged operation (`git rm`, `git mv`) distinct from an ordinary content edit (`git add` after editing in place) — both still require a subsequent `git commit` to become durable, and both are trivially recoverable pre-commit (`git checkout`) and recoverable-with-effort post-commit (`git revert`, reflog). The "two verbs, one commit step" shape is close to the design here: Tier 1 (edit) and Tier 2 (destroy/move) are different *kinds* of pending change, but both still funnel through one `flush`/`commit` gesture rather than each having its own independent commit path.

**Adopted:** distinguishing destructive changes as their own category within one staged-change model, rather than either (a) making every change equally "scary" (git doesn't require extra confirmation for `git commit` just because an `rm` is staged) or (b) making destructive changes an entirely separate workflow with their own commit step. `confirmDestructive` is closer to `git commit --no-verify`-style explicit intent than to a second commit command.
**Rejected:** git's actual UX for the *default* case — `git rm`/`git mv` require no extra confirmation flag at all; the safety net is entirely "it's just a commit, and commits are cheap to undo." This ADR does not rely on git-recoverability alone for the *flush-time* gate (see Steelman — "isn't git enough?") because MCP agents and REPL scripts don't necessarily commit between every flush, and because a destructive flush can happen against files that were never committed in the first place.

### LSP — `workspace/applyEdit` with `DeleteFile` / `RenameFile` / `CreateFile`

The LSP spec's `WorkspaceEdit.documentChanges` array supports typed resource operations — `CreateFile`, `RenameFile`, `DeleteFile` — alongside ordinary `TextDocumentEdit`s, specifically so a server-initiated refactor can tell the client "this isn't a content edit, this is a file-system operation" and let the client apply its own UX for that distinction (VSCode shows a different confirmation for a workspace-wide rename than for a single edit). This is the *exact* mechanism ADR 0082 left unused — flush today emits one edit-shaped event per file regardless of what actually happened to it.

**Adopted wholesale:** flush must emit `DeleteFile` for `remove-class`, `RenameFile` for `rename-class`, and (for `rename-method`) a `TextDocumentEdit` per affected site plus, if the rename also implies a class rename in the same batch, the `RenameFile` for that. `Workspace newClass:at:` flush should *also* switch from the generic `Change` shape to `CreateFile` — a pre-existing gap this ADR's LSP work closes as a side effect, since the typed-operation machinery has to exist anyway for `DeleteFile`/`RenameFile`.
**Not adapted, nothing to depart from:** the spec's shape maps directly; there is no Beamtalk-specific wrinkle here beyond needing to build it (nothing existed before).

### Newspeak / Erlang / Elixir

Neither offers relevant prior art beyond what ADR 0082/0112 already drew from them (Newspeak's image has no file-delete concept to speak of; Erlang/Elixir modules are all-or-nothing load/purge with no rename primitive at all — `code:purge/1` deletes, nothing renames). No new adoption or rejection beyond what those ADRs already recorded.

### Python / TypeScript — statically-indexed vs. textual rename tooling

This ADR's central risk (a rename can only rewrite references its index can see; dynamically constructed lookups are invisible and become dangling) is not Beamtalk-specific — it is the exact fault line that separates rename tooling quality across the mainstream-language landscape, and both sides of that fault line are directly relevant precedent. **TypeScript's "Rename Symbol"** (via `tsserver`, the same LSP machinery this ADR's `DeleteFile`/`RenameFile` work integrates with) is built on the compiler's own semantic reference graph — a rename that misses a reference is a compiler bug, not an accepted risk, because the type-checked reference graph is exhaustive by construction for anything the compiler can see (`obj[computedKey]`-style dynamic access is TypeScript's own equivalent dangling-reference gap, and it produces the identical class of silent breakage). **Python's rename tooling** (`rope`, PyCharm's refactor-rename) has no compiler-verified reference graph to lean on — it is AST/text-pattern-based, and every Python refactoring tool's documentation carries some version of the same warning this ADR gives `sites`: `getattr(obj, "method_name")`, monkey-patching, and dynamically imported modules are invisible to the refactor and will silently break.

**Adopted:** the xref-index-driven `sites` mechanism is architecturally TypeScript's approach (a maintained, queryable reference graph feeding the rename, not an ad-hoc grep) — Beamtalk already has this graph for other purposes (`sendersOf:`, `whichClassIncludesSelector:`), so this ADR extends an existing asset rather than building AST-pattern-matching from scratch the way `rope` had to.
**Adapted:** Beamtalk cannot get TypeScript's *soundness guarantee* (a rename that compiles is a rename that's complete) because, like Python, Beamtalk is dynamically dispatched — `perform:`, `Smalltalk at:`, and any string-built selector are invisible to xref exactly as `getattr` is invisible to `rope`. This ADR's posture is therefore Python's, not TypeScript's: best-effort against a real index, with an accepted, documented gap for dynamic access — the same conclusion ADR 0112 already reached for `removeSelector:`'s dangling senders, now confirmed as the correct posture (not a Beamtalk-specific shortcut) by cross-checking against how the mainstream dynamic-language tooling ecosystem has already converged on the identical trade-off.
**Rejected:** neither TypeScript's "refuse to compile until every reference is fixed" gate (Beamtalk has no compile step that could enforce it project-wide before a rename takes effect) nor Python tooling's common fallback of a dry-run-only mode with no automatic rewrite at all (this ADR's *Alternatives Considered* — "block on references outside the batch" — covers the equivalent ground and was rejected for the same reason ADR 0112 already gave).

## User Impact

### Newcomer (from VSCode / Python / JS)

- "Delete File" and "Rename Symbol" already exist as concepts in every editor a newcomer has used — VSCode's own file-delete asks "Move to Trash?" and its rename-symbol feature previews every affected file before applying. `confirmDestructive`'s LSP-surfaced modal matches that expectation directly; nothing new to learn.
- The two-gesture shape (memory-mutate now, disk-confirm separately) is more surprising than VSCode's single-gesture delete — mitigated by making the *first* gesture ("Remove Class" in the browser) still work as expected for the in-session experience (the class is gone from the running app immediately), and only the *disk* consequence needs the second click.
- Discoverability: `Counter respondsTo: #renameTo:` and ordinary tab-completion on `Behaviour` surface `renameTo:`/`renameSelector:to:` the same way any other class-protocol method is discovered — no separate registry to learn about. Before confirming a destructive flush, `Workspace changes` (pre-flush, ADR 0082) already lists every pending entry including the new `remove-class`/`rename-*` kinds, so a newcomer can inspect exactly what `flushIncludingDestructive` is about to do before running it, without needing to know the ChangeLog's internal schema.

### Smalltalk developer

- `renameTo:`/`renameSelector:to:` read as exactly the kind of message-send-based class-protocol operation Smalltalk trains developers to expect (mirrors `removeSelector:`'s reception in ADR 0112).
- The Refactoring-Browser-style "compute references, apply together" behaviour of `renameTo:`/`renameSelector:to:` is the single most Smalltalk-native piece of this ADR — though a Pharo developer used to the Refactoring Browser's "every sender" guarantee would be surprised, and should be told plainly, that `renameSelector:to:` only auto-applies to `self`/`super` sends and reports the rest as `candidate_sites` for the reason given in *Consequences* — this is narrower than Pharo's own tool, not equivalent to it.
- The two-phase memory/disk split for something as immediate-feeling as "rename this method" is the one place this ADR asks a Smalltalk developer to hold two mental models at once (their rename already took effect; the file hasn't caught up yet) — same tension ADR 0082 already introduced for ordinary patches, not a new one.

### Erlang/BEAM developer

- Nothing here introduces a new BEAM-level mechanism: class removal is `code:purge` + registry cleanup (already shipped, BT-785); rename recompiles a bounded set of modules through the existing hot-reload pipeline already used for `removeSelector:`'s recompile-based mechanism (ADR 0112 Implementation). No new OTP pattern.
- A production release node never sees any of this — same "no workspace, no ChangeLog, no flush" guarantee ADR 0082/0112 already give; this ADR adds no new release-build code path.

### Production operator

- The `remove-class`/`rename-*` audit gap this ADR closes (today, `removeFromSystem` leaves *no record* it happened) is itself an operator-relevant fix independent of the flush design — "was a class removed from this running node, by whom, when" currently has no answer at all.
- `confirmDestructive` being a call-site argument rather than a workspace setting means an operator auditing a production incident can see, in the ChangeLog's `author`/`author_kind` metadata plus the fact that the entry *did* flush, that someone deliberately chose to delete/rename a file — not that a background setting silently permitted it.

### Tooling developer (LSP/MCP/browser)

- The `DeleteFile`/`RenameFile`/`CreateFile` LSP work is the first time this codebase's flush path uses the spec's typed resource operations instead of a generic edit — a reusable capability for any future refactor beyond this ADR's three flows.
- MCP's `remove_class`/`rename_class`/`rename_method` tools plus a `confirm_destructive` argument on `flush` extend the existing tool surface with no new dispatch mechanism — same "typed wrapper over `evaluate`" shape every prior MCP tool already uses.

## Steelman Analysis

### Confirm-by-default (`confirmDestructive` required) vs. unconfirmed (destructive entries flush like anything else)

- 🧑‍💻 **Newcomer:** "Requiring a special argument for delete/rename is inconsistent with how `Workspace flush` already works for everything else — I already learned that flush just... flushes. A second, different-shaped flush call for some entries is a second thing to learn."
- 🎩 **Smalltalk purist:** "Smalltalk's whole ethos is that the running system doesn't second-guess you — `removeFromSystem` already doesn't ask twice, and ADR 0112 explicitly rejected sealing-based refusals for `removeSelector:` in favour of installing unconditionally. A confirmation gate at flush time reintroduces exactly the kind of paternalism ADR 0112 argued against for the in-memory step — just moved one layer over."
- ⚙️ **BEAM veteran:** "An extra required argument is one more thing every caller — including scripted/automated flush calls in CI or a deploy hook — has to remember to pass, and 'forgot the flag' bugs are exactly as real as 'forgot to check' bugs."
- 🏭 **Operator:** "git already recovers from a bad delete or rename as long as it was committed. A confirmation gate that only protects uncommitted work is protecting the wrong thing — teach people to commit often, don't build a second safety net for the same problem git already solves."
- 🎨 **Language designer:** "Fewer branches in the flush state machine is a real virtue. `confirmDestructive` is a special case the design otherwise doesn't need — every other ADR 0082/0112 decision found a way to unify destructive and non-destructive handling under one rule (flushability) rather than adding a second gate."
- **Why confirm-by-default wins anyway:** every argument above is real, and the decisive rebuttal is the same one Amendment 1 already established for a different axis — **git recoverability is a human, cockpit-surface property, not a universal one.** MCP agents do not necessarily commit between flushes (ADR 0082 Amendment 1 explicitly puts MCP in the *pre-flush*, ChangeLog-first layer specifically because agents batch and iterate before crossing the flush seam at all); a script or CI hook calling `Workspace flush` today, unmodified, would silently start deleting/renaming files the moment this ADR ships new entry kinds, with no code change to that caller signalling the new risk. `confirmDestructive` being a required, no-default argument is the mechanism that makes *upgrading to this ADR* safe for every existing caller — the alternative (destructive entries flush like anything else) is a breaking, silent behaviour change to every unmodified `Workspace flush` call in the ecosystem, not a neutral simplification. ADR 0112's "flushability, not refusal" precedent this argument leans on was about the *in-memory* step, which is exactly as unconfirmed here as it was there (`removeSelector:`, `renameTo:`, `renameSelector:to:` all install immediately, no gate) — the gate this ADR adds is purely at the disk-write step, a boundary ADR 0112 never actually addressed (it explicitly deferred flush entirely).

### Tombstone-first (leave a `.bt.deleted` / `.bt.orig` marker on disk) vs. immediate-delete (git is the only durable record)

- 🧑‍💻 **Newcomer:** "A visible tombstone file means I can see what got deleted just by looking at the directory listing — I don't need to know git archaeology (`git log --diff-filter=D`) to find it."
- 🎩 **Smalltalk purist:** "Pharo's `.changes` file is, functionally, exactly this — a durable, append-only record that survives independent of the image being saved. A tombstone is the natural Smalltalk-native answer, and this codebase already has the `changes/` subdirectory as precedent for 'durable state that isn't the source tree itself.'"
- ⚙️ **BEAM veteran:** "A tombstone is trivial to implement (write one more file) and gives crash-safety for free — if the process dies between delete and ChangeLog-prune, the tombstone is still sitting there as evidence, where a bare `unlink` leaves nothing."
- 🏭 **Operator:** "Tombstones are greppable/`find`-able without needing the workspace running at all — useful for an operator doing forensic cleanup on a node that's already down."
- 🎨 **Language designer:** "A tombstone makes the on-disk state self-describing: `git status`/`ls` alone tells the whole story, no need to cross-reference a separate ChangeLog file to understand why a `.bt` file vanished."
- **Why immediate-delete (no persistent tombstone) wins:** the decisive problem is exactly the one ADR 0082's Alternative F (Shadow-file overlay) was rejected for — **a `.bt.deleted` marker is a second source of truth about what exists**, and this codebase already has two durable records of "this class/method was removed and here's its prior body": git (once committed) and the ChangeLog's own `changes/sources/` archive (ADR 0082's rotation policy already keeps pruned/archived source bodies around, independent of whether the live file still exists). A third record — a tombstone file sitting in the actual source tree — creates exactly the ambiguity ADR 0082 fought hard to avoid for shadow-overlay patches: does `ls src/` show the tombstone as "this file still kind of exists," does `bt fmt`/the compiler/LSP have to learn to skip `.bt.deleted` files, does a fresh `git clone` (which never ran the delete, only sees the committed result) end up with orphaned tombstones nothing ever cleans up? The staged-rename-then-unlink mechanism in *Multi-file atomicity* above gives the crash-safety win the BEAM veteran wants (a crash mid-delete leaves a recoverable `.tmp-delete-*` file, not a silent loss) **without** leaving anything behind after a *successful* delete — crash-safety and permanence are separable, and this design takes the first without the second. The newcomer/operator "just look at the filesystem" argument is answered by `Workspace changes` (pre-flush) and `git log`/`git show` (post-flush) each being the right tool for their respective half of the timeline, matching Amendment 1's existing division of labour rather than inventing a third view that duplicates both.

### Separate command per destructive kind (e.g., `Workspace flushRemovals`, `Workspace flushRenames`) vs. one unified `confirmDestructive` mechanism

- 🧑‍💻 **Newcomer:** "Named commands are self-documenting — `flushRemovals` tells me exactly what it's going to do without needing to know what's pending first."
- 🎩 **Smalltalk purist:** "Pharo's Refactoring Engine genuinely does have distinct entry points per refactoring kind (`RBRenameClassRefactoring` vs `RBRemoveMethodRefactoring` are different classes with different `execute` protocols) — mirroring that distinction here would be staying closer to the prior art this ADR itself cites."
- ⚙️ **BEAM veteran:** "Separate commands are easier to reason about independently and easier to add telemetry/rate-limiting to per kind if one destructive category turns out riskier in practice than another."
- 🏭 **Operator:** "A named command in an audit log (`flushRemovals` called) is more legible at a glance than `flushIncludingDestructive` plus having to cross-reference which entries were pending to know what actually happened."
- 🎨 **Language designer:** "One command with a boolean is a smaller, more composable surface, but it conflates 'I want to flush everything destructive' with 'I want to flush *this specific* destructive thing' — separate commands let each kind evolve its own parameters independently later without polluting a shared signature."
- **Why unified wins:** `flushKinds:` already exists (ADR 0082) as the general filter-by-kind mechanism, and it already composes with a scope (`aClass`) — adding `confirmDestructive` as one more filter dimension on the *same* mechanism (`Workspace changes flushKinds: #{#'remove-class'} confirmDestructive: true`) gets the operator's "legible, specific" want and the language designer's "each kind can carry its own semantics" want for free, without a second command family to keep in sync with the first as new kinds are added later (exactly the kind of duplicate-vocabulary risk CLAUDE.md's no-duplicate-implementations rule flags — a `flushRemovals`/`flushRenames` family would re-derive `flushKinds:`'s filtering logic under new names). The purist's Pharo-precedent argument is real but describes Pharo's *in-memory execute* step, which this ADR already gives distinct primitives for (`renameTo:`, `renameSelector:to:`, `removeFromSystem`) — the disagreement is only about the *flush* step, which ADR 0082 already unified across every existing kind, and there is no new argument here for un-unifying it just for the newest three.

### Tension points

- **Newcomer/BEAM-veteran "simplicity" vs. operator/agent "safety" on `confirmDestructive`:** the strongest real tension. Resolved the same direction ADR 0082 resolved patch-vs-write-through — safety wins for the *default*, and the cost is paid once per destructive flush call, not per ordinary edit. Unlike Alternative B in ADR 0082 (rejected write-through), this is not asking every caller to pay the cost on every operation — Tier 1 stays exactly as frictionless as it is today.
- **Purist "Refactoring-Browser-style separate commands" vs. designer "one unified filter mechanism":** resolved in favour of the unified mechanism because `flushKinds:` already exists and already solves the composability problem the separate-command instinct is reaching for — see above.
- **"Git is enough" vs. "a workspace-native safety gate is still needed":** the whole `confirmDestructive` design rests on this tension resolving toward the second position, and it does so specifically because of Amendment 1's agent/human split, not because git-recoverability is weak in general — for the *cockpit* audience alone, the BEAM-veteran/operator argument ("git already handles this") would be much stronger, and a future revision could reasonably relax `confirmDestructive` to an autoflush-tracked default for the cockpit surface specifically once real usage data exists (mirroring how Amendment 1 itself was a usage-driven refinement of the original ADR 0082 design, not a day-one decision).

## Alternatives Considered

### Alternative: do nothing — leave destructive-flush UX undesigned

Unlike ADR 0112's equivalent "do nothing" alternative (where a real workaround existed — overwriting a method in place), there is no workaround here at all for two of the three flows: `removeFromSystem` already has no flush story and none is emerging on its own, and rename has no primitive to even attempt a workaround with. The third flow (`remove-method`) technically "does nothing" today by construction — `Workspace flush` silently excludes those entries — but that is not a stable resting state, it is an already-shipped bug surface (*Current State*): every `removeSelector:` call already produces a ChangeLog entry that can never be satisfied, growing without bound in a long-lived workspace. Rejected for the same structural reason ADR 0112 rejected it: ADR 0082 explicitly named this ADR's three flows as deferred-not-abandoned work, BT-2192 exists specifically to make that deferral good, and "do nothing" does not avoid a design decision here — it leaves an already-committed-to gap unfilled and lets the `remove-method` bug compound.

### Alternative: no confirmation tier — destructive entries flush like any other durable+flushable entry

See Steelman above. Rejected as a silent, breaking behaviour change for every existing unmodified `Workspace flush` caller once this ADR's new entry kinds start appearing in a workspace's ChangeLog.

### Alternative: persistent on-disk tombstone (`<file>.bt.deleted`) instead of (or in addition to) `confirmDestructive`

See Steelman above. Rejected as a third, redundant source of truth about what currently exists, echoing ADR 0082's Alternative F (Shadow-file overlay) rejection reasoning. The staged-rename-then-unlink step already gives equivalent crash-safety without the permanence.

### Alternative: separate `flushRemovals` / `flushRenames` commands instead of a unified `confirmDestructive` mechanism

See Steelman above. Rejected in favour of extending the existing `flushKinds:` filter mechanism, avoiding a second, parallel vocabulary for "which entries to apply."

### Alternative: block `renameTo:`/`renameSelector:to:` on references outside the current in-project rename batch (never leave a partial rename)

Considered for the case where a rename's `sites` list, computed via the xref index, might miss a reference that exists in a file the xref index hasn't indexed yet (a freshly-created, not-yet-compiled file) or a dynamically-constructed lookup (`aClass perform: (aString asSymbol)`, `Smalltalk at: aSymbol`) that no static xref can see. Rejected as a hard block, for both primitives: ADR 0112 already accepted this exact risk category for `removeSelector:` ("dangling senders... not prevented, but the tooling to surface them already exists") and gave a considered reason not to block on it — the same reasoning applies to both rename primitives without a new argument. `renameTo:`/`renameSelector:to:` instead surface the reference count found (mirroring ADR 0112's hint text) so the caller has visibility, without refusing an action ADR 0112 already decided is the caller's call to make.

### Alternative: language-level rename primitive omitted; rename is LSP/tooling-only (like a bare text find-and-replace the editor performs, with no `Behaviour` message)

Rejected for the identical reason ADR 0112 rejected a workspace-only `removeSelector:` — it would violate ADR 0082's "every tool op is a structured invocation of a Beamtalk expression" principle, and every surface (MCP, LSP, REPL, browser) would end up separately re-implementing "find senders, rewrite them" logic against the runtime's xref index instead of sharing one primitive, precisely the duplication CLAUDE.md's no-duplicate-implementations rule warns against.

### Alternative: keep the old name registered as a forwarding alias instead of eagerly rewriting every site

Since class/selector references resolve by name at dispatch time rather than through a baked-in pid (Constraint 4), a cheaper-looking fix exists: on `renameTo:`/`renameSelector:to:`, keep the *old* name dispatchable — routed to the new implementation — until sources are caught up, rather than eagerly rewriting sites at all. This is not a hypothetical for this codebase: `beamtalk_alias_xref.erl` (ADR 0108, named union type aliases) already exists precisely because "a `::` annotation naming a type alias is not a message send" needed its own lightweight index rather than forcing `beamtalk_xref` to model it — i.e., alias-as-first-class-mechanism has direct precedent here. A redirect would sidestep two problems this review surfaced: it makes the in-memory multi-class-gen-server sequencing question (what does a live actor observe if site 5 of 10 fails mid-rewrite? — see *Multi-file atomicity*, which only specifies the *disk* half) moot, because no sender needs to change atomically with the rename — old callers keep working through the alias regardless of ordering; and it makes `renameSelector:to:`'s `other_recv`-site gap (previous section) far less urgent, since an unrewritten sender still dispatches correctly via the alias rather than failing.

**Rejected for v1, not dismissed as unsound:** a permanent alias is exactly the `undef_method`-adjacent "keep answering under the old name forever" shape ADR 0112 explicitly declined to build for `removeSelector:` ("a tombstone value... is left as a genuinely separate primitive to design from scratch"), and a *temporary* alias needs its own lifecycle question this ADR would then have to design from scratch — when does the alias expire, does an un-flushed rename's alias survive workspace restart, does `Workspace changes revert:` need to also unregister the alias, does a second rename of the same class stack aliases. ADR 0108's `beamtalk_alias_xref` is a *type*-alias mechanism (a compile-time/type-checking concern); reusing its *shape* for a *dispatch*-time class-identity alias is a materially different runtime commitment (every `whereis_class` lookup would need an alias-table fallback check, a dispatch-hot-path cost every other primitive in ADR 0082/0112 was careful to avoid introducing). Given the eager-rewrite design this ADR ships already reduces to the safe self/super-only subset for methods (previous section) and is fully safe for classes (name-keyed, no ambiguity), the incremental safety a redirect would buy is real but narrower than it first appears — flagged here as the right design to revisit if Phase 3/4's real usage shows the `candidate_sites` manual-followup burden is too high, not adopted now.

## Consequences

### Positive

- Closes a real, already-observable gap: `Workspace changes` today either contains `remove-method` entries that can never flush (permanently stuck) or is silently missing `remove-class` entries entirely (`removeFromSystem` doesn't log). Both are fixed.
- `Behaviour` gains a complete patch/create/remove/rename set (`compile:source:`, `newClass:at:`, `removeSelector:`, `renameTo:`, `renameSelector:to:`) with consistent receiver, error, and ChangeLog conventions across all five — the "closes the gap" pattern ADR 0112 already established for its own trio extends cleanly.
- Reuses, rather than reinvents, four separate pieces of existing infrastructure: ADR 0082's two-phase flush protocol (extended, not replaced), ADR 0112's recompile-based method-removal mechanism (the same machinery both rename primitives' per-site rewrite generalizes further, one shared mechanism rather than two), the xref index's existing `sendersOf:` query (already built for ADR 0112's dangling-sender hint, now load-bearing for `renameSelector:to:`'s `sites`), and ADR 0087's already-shipped `SystemNavigation>>referencesTo:` (its "class → referencing sites" index doing the equivalent job for `renameTo:`, combined with the existing `beamtalk_class_registry:direct_subclasses/1` for the one reference kind `referencesTo:` doesn't cover — see *renameTo: also rewrites cross-file references*).
- `confirmDestructive` closes a real, silent-upgrade-risk gap for every unmodified `Workspace flush` caller, without adding any friction to the Tier-1 path this ADR leaves untouched.

### Negative

- **`renameTo:` and `renameSelector:to:` are the most implementation-heavy primitives this codebase has added via an ADR to date** — both require recompiling not just the target definition (as removal already does) but every current in-project reference, computed transactionally, with a `sites` list that has no precedent in the existing schema shape (every prior kind, before this ADR, targeted exactly one file). This is real complexity duplicated across two primitives, not a documentation-only extension — an earlier draft of this ADR under-scoped `renameTo:` specifically by omitting this mechanism for it (see Constraint 4 and the *renameTo: also rewrites cross-file references* subsection), which is worth flagging here so a future reviewer knows the parity was deliberate, not incidental.
- **`renameSelector:to:` auto-rewrites less than a first reading of "rewrite every sender" suggests, and this is a deliberate, review-driven correction, not an oversight left in place.** Because `sendersOf:` is selector-name-keyed with no receiver-type narrowing (see *`renameSelector:to:`'s sender rewrite is safe only for `self`/`super` sends*), only `self_recv`/`super_recv` sites are auto-rewritten; every `other_recv` site — the common "external caller" shape, and the one a user renaming a method most wants fixed automatically — is reported as a `candidate_sites` entry and left for manual follow-up. A caller expecting Pharo/TypeScript-grade "rename fixes every call site" behaviour will find this narrower in practice, especially for widely-shared selector names (`size`, `at:put:`, `printOn:`) where `candidate_sites` will be long and mostly irrelevant noise from unrelated classes. This is the correct trade-off given the xref index's current capabilities (the alternative — auto-rewriting `other_recv` sites too — silently corrupts unrelated code, which is strictly worse), but it is a real capability gap relative to what a newcomer coming from a statically-typed IDE's rename-symbol feature will expect, and closing it requires new receiver-type-narrowed xref infrastructure this ADR does not build.
- **A rename can only ever rewrite references the xref index can see.** Dynamically constructed lookups (`perform:`, `Smalltalk at:`, string-built selectors/names) and references in files that haven't been compiled/indexed yet are invisible to `sites` and become dangling exactly the way ADR 0112 already accepted for `removeSelector:` — this ADR does not raise that bar, but a rename's blast radius (across N files) makes an invisible dangling reference more likely to matter than a single-selector removal's, and a *class* rename's blast radius (every constructor send, type annotation, and subclass declaration project-wide) is larger again than a method rename's.
- **`confirmDestructive` is one more thing every caller across four surfaces has to learn**, and — as the Steelman concedes — is a real, if justified, complication relative to ADR 0082's existing single-mode `flush`.
- **Two additional stdlib/dependency-refusal precedents to keep straight** (`renameTo:`/`moveClass:to:` refuse like `removeFromSystem`; `renameSelector:to:` allows-with-flushable-false like `removeSelector:`/`compile:source:`) — a future contributor adding a sixth `Behaviour` primitive has to re-derive which precedent applies rather than there being one uniform rule; this ADR's *Refusal vs flushability* table is the reference, but the asymmetry itself is a documented cost, not a free simplification.
- **A `rename-class` or `rename-method` entry that partially fails at Phase A (one reference site's span no longer resolves) aborts the entire multi-file rename**, per *External-edit conflicts* — this can be more disruptive than a single-file patch conflict, since the caller has to resolve one stale site before *any* of the rename (including the definition itself) can flush, even if every other site is fine.

### Neutral

- `removeFromSystem`'s stdlib/dependency/subclass refusals are unchanged — this ADR only adds logging to an already-decided operation, not new policy.
- No bulk rename primitive (rename N selectors in one call) ships — same "loop over the existing primitive" answer ADR 0112 gave for a bulk remove-all-methods primitive, for the same "no acceptance criterion calls for it" reason.
- `Workspace flush`'s existing single-argument form remains the common case and is textually unchanged for every caller that never touches a destructive entry — this ADR is purely additive to the flush surface for those callers.
- **`renameTo:` does not put already-instantiated objects of the renamed class at risk.** `#beamtalk_object{}` carries its class as a direct pid (`ClassPid = erlang:element(4, Self)`, the same field `classRemoveFromSystem`'s own implementation reads), established at instantiation time — an existing instance's dispatch is not a name lookup through `beamtalk_class_registry` on every send, so re-registering the class under a new name does not affect any object that already holds that pid. This is the class-rename analogue of ADR 0112's explicit "already-running actor instances see the change immediately, with no drain" statement for `removeSelector:`, and it resolves in the *safer* direction: only *other* classes' compiled references to the old *name* (the `sites` this ADR rewrites) are at risk, not instances of the renamed class itself.

### DDD Model Impact

- **Compilation context** gains the multi-site rewrite step both `renameTo:` and `renameSelector:to:` share (rewrite N spans across N files, each independently re-parsed and re-validated) — an extension of the existing byte-span resolver (ADR 0082), not a new subsystem, and one mechanism serving both primitives rather than two.
- **Workspace context** owns the new `beamtalk_workspace_flush` Tier-2 staging logic (rename-to-tmp-then-unlink for delete; write-then-rename-then-unlink-old for class rename) and the `confirmDestructive` filter dimension on `flush`/`flushKinds:` — same module boundaries ADR 0082 already established, extended rather than duplicated.
- **Runtime context** gains `classRenameTo`/`classRenameSelector`/`classRenameSelectorIfAbsent`/`workspaceMoveClass` primitives alongside the existing `classRemoveSelector*` family in `beamtalk_behaviour_intrinsics.erl`, wires `renameTo:`'s site discovery to the already-shipped `referencesTo:`/`direct_subclasses/1` pair rather than a new query, and adds the ChangeLog-append call `removeFromSystem` is currently missing.
- **Language Service context** gains the `DeleteFile`/`RenameFile`/`CreateFile` typed `workspace/applyEdit` resource operations (*Prior Art* / *Surface*) — unlike ADR 0082, which needed no Language Service change because it only ever consumed the *existing* generic `applyEdit` bridge, this ADR extends that bridge itself, so it is a genuine (if narrow) Language Service context change, not an absence of one.

## Implementation

*(For downstream implementation work — this ADR does not implement any of the below.)*

### Affected components

| Layer | Change |
|---|---|
| `runtime/apps/beamtalk_runtime/src/beamtalk_behaviour_intrinsics.erl` | New `classRenameTo/2`, `classRenameSelector/3`, `classRenameSelectorIfAbsent/4` primitives, modelled directly on the existing `classRemoveSelector`/`classRemoveSelectorIfAbsent` functions in the same module. `classRemoveFromSystemByName/1` gains a ChangeLog-append call at its existing success point (`publish_class_removed/2` call site), capturing the class's full current source as `prev_source_ref` *before* the removal proceeds (mirrors the existing "read+parse before mutate" ordering `compile:source:`'s patch hook already uses). |
| `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_interface_primitives.erl` (or a new sibling module) | New `workspaceMoveClass/2` backing `Workspace moveClass:to:`, modelled on the existing `newClass:at:` primitive in the same module — single-file move, no site discovery. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_eval.erl` / `beamtalk_repl_loader.erl` | One shared multi-site rewrite mechanism, generalizing ADR 0112's `remove_method/3` (a sibling `rewrite_sites/4` or equivalent) — reuse, not two parallel mechanisms, per CLAUDE.md's no-duplicate-implementations rule. `renameSelector:to:` calls it with `sendersOf:`'s `self_recv`/`super_recv`-filtered results (`other_recv`/`erlang_ffi` results go to `candidate_sites` instead, never rewritten — see Decision) as the site-discovery step; `renameTo:` calls the *same* function with the union of `SystemNavigation>>referencesTo:` (ADR 0087, BT-2302 — already shipped, no new xref query needed) and `beamtalk_class_registry:direct_subclasses/1` (superclass-declaration sites `referencesTo:` doesn't cover). The rewrite/staging half of the mechanism does not care which query produced its `sites` list, but each rewritten site's own class needs the same `purge_compiler_cache`/xref-reindex step `beamtalk_class_lifecycle` already applies to the *renamed* class (BT-3105/BT-3107) — a sender file's compiler cache entry is exactly as stale after its call site changes as the renamed class's own cache entry is, and the existing single-class-keyed purge does not automatically reach it without this being wired in explicitly. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_changelog.erl` | New `kind`s (`'remove-class'`, `'rename-class'`, `'rename-method'`), new fields (`old_class`, `old_path`, `new_path`, `sites`) on the entry record, extending the existing open-schema pattern — `sites` is now shared by both rename kinds, not `rename-method`-specific. `target_key/1` (ADR 0112, BT-3187's `(class, selector, side)` fix) needs a variant for both multi-site shapes — likely keying shadow-detection per-site rather than per-entry, since two independent renames could touch overlapping reference files. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_flush.erl` | Remove the current unconditional `exclude_remove_method/1` filtering and replace with tier classification (`entry_tier(Entry) -> tier1 \| tier2`); `do_flush/1` gains a `confirmDestructive` parameter threaded through `filter_entries/2`; Phase A/B staging extended per *Multi-file atomicity* above — the delete/rename staging steps are new, everything else (grouping, shadow-duplicate detection, per-file status reporting) is reused unmodified. |
| `stdlib/src/Behaviour.bt` | `renameTo:`, `renameSelector:to:`, `renameSelector:to:ifAbsent:` — three new sealed methods, same pattern as `removeSelector:`/`removeSelector:ifAbsent:`. |
| `stdlib/src/Workspace.bt` | New unscoped `flushIncludingDestructive` and two-keyword `flush:confirmDestructive:` variant on the existing `flush`/`flush:` facade methods; `flushKinds:confirmDestructive:` on `ChangeLog`; `moveClass:to:` for the path-only-move case (no identity change), per its own subsection above. |
| `crates/beamtalk-lsp/src/runtime.rs` | `FlushEvent` needs to carry per-file *operation* (`Change`/`Create`/`Delete`/`Rename{from}`), not just a flat path list — a breaking change to the existing struct's shape, needed regardless of whether this ADR's new kinds are in play, since `new-class` flush already deserves `CreateFile` and doesn't get it today. |
| `crates/beamtalk-lsp/src/server.rs` | `workspace/executeCommand: flush` gains a `confirmDestructive` argument; emits `DeleteFile`/`RenameFile`/`CreateFile` resource operations per the extended `FlushEvent`. |
| `crates/beamtalk-mcp/src/server.rs` | New tools: `remove_class` (wraps `removeFromSystem`), `rename_class` (wraps `renameTo:`), `rename_method` (wraps `renameSelector:to:`); `flush` tool gains a required-when-applicable `confirm_destructive` boolean. |
| `crates/beamtalk-cli/src/commands/repl/mod.rs` | New meta-commands `:remove-class`, `:rename-class`, `:rename-method`, each with the two-prompt shape from *Surface* above; new `:flush-destructive` / `:flush-destructive <Class>` pair alongside the existing `:flush` / `:flush <Class>` pair. |
| `runtime/apps/beamtalk_workspace/priv/static/workspace.js` | "Remove Class" / "Rename" browser actions with the distinct destructive-dirty-indicator affordance from *Surface* above. |
| `docs/development/surface-parity.md` | Four new expression-backed rows; a note on `confirmDestructive` as a parity-preserving argument (see *Surface*). |
| `docs/beamtalk-language-features.md` | Document `renameTo:`/`renameSelector:to:` alongside the existing `removeSelector:`/`removeFromSystem` sections. |

### Phased rollout

| Phase | Scope | Effort | Tests |
|---|---|---|---|
| **1** | `removeFromSystem` ChangeLog-logging fix (`kind: "remove-class"`) — the smallest, most independently-shippable piece, and the one closing an audit gap that exists today with zero new UX to design. | S | EUnit: entry appears with correct `flushable`/`prev_source_ref`; BUnit: `Workspace changes` shows it. |
| **2** | `Workspace flush` tiering: classify `remove-method`/`remove-class` as Tier 1/Tier 2 respectively, implement `flushIncludingDestructive` + `confirmDestructive:` filter dimension, implement class-removal staged-delete (rename-to-tmp, unlink). Unblocks the already-stuck `remove-method` entries as a side effect (Tier 1, no new gate needed for them). | M | EUnit: staged-delete crash-safety (kill between rename and unlink); BUnit: `Workspace flush` reports `skipped: destructive`, `flushIncludingDestructive` applies it. |
| **3** | **Opens with a validation spike**, in ADR 0082 Phase 0's spirit: run `referencesTo:` + `direct_subclasses/1` against the full stdlib + examples corpus **and against a live-patched fixture** (a class with a `>>`-patched method referencing another class, exercising the `references => []`-for-live-patches gap — see *renameTo: also rewrites cross-file references*) and confirm the combined site list matches a hand-audited sample, *before* wiring it into any primitive — this is the load-bearing new assumption this ADR introduces (ADR 0082's byte-span splice is already proven; multi-site *discovery*, and specifically its behaviour against live-patched code, is not). If the spike holds, the phase proceeds to build the shared multi-site rewrite mechanism (transactional in-memory rewrite of definition + sites, `sites`-shaped ChangeLog entry, multi-file flush) plus `renameTo:` as its first consumer: class-rename flush (file move, declaration-line rewrite, reference-site rewrite), `Workspace moveClass:to:` for the path-only case. Largest phase — this is where the mechanism gets built, not just wired up. | L | Corpus round-trip / reference-discovery accuracy tests (the spike, including the live-patch fixture) before any other Phase 3 work starts; EUnit: reference rewrite correctness against a small fixture graph (class with 3 in-project references across 2 files) + rename-to-tmp-then-final-rename atomicity; BUnit: `Counter renameTo: #Accumulator` + flush produces the moved file, updated declaration, and rewritten references, with no dangling `class_not_found`. |
| **4** | `renameSelector:to:`/`ifAbsent:` — reuses Phase 3's mechanism with the existing `sendersOf:` query as its site-discovery step instead of `referencesTo:`/`direct_subclasses/1`. Depends on Phase 3, not the reverse; smaller than Phase 3 because the mechanism itself is already built. | M | BUnit: end-to-end method rename + flush + verify no dangling `does_not_understand`; regression check that Phase 3's fixture graph still passes through the shared code path. |
| **5** | `revert:` extensions for all three new kinds (Undo story table above). | M | BUnit: revert of each kind, pre-flush and (where applicable) documented as unsupported post-flush. |
| **6** | LSP `DeleteFile`/`RenameFile`/`CreateFile` typed resource operations (`FlushEvent` restructuring); MCP tools; REPL meta-commands; browser actions. Surface-parity audit. | M | LSP command tests; MCP integration tests; browser e2e for the two-gesture destructive flow; surface-parity drift check passes. |

Total: ~M-L across 6 phases. Phase 3 (the shared rewrite mechanism, built via `renameTo:`) is the load-bearing risk — if it proves unreliable against real code (partial rewrites, xref gaps), the design may need to fall back to a narrower v1 for *both* rename primitives (rewrite the definition only, leave references as a `Workspace changes` follow-up list for the caller to apply manually) before committing to the full transactional shape. Building the mechanism once, in the phase with the smaller blast radius per mistake (a class-rename fixture is easier to reason about than a method-rename one with overlapping selector names), before Phase 4 reuses it, is a deliberate risk-ordering choice.

## Migration Path

No user code changes required — every new surface (`renameTo:`, `renameSelector:to:`, `moveClass:to:`, `flushIncludingDestructive`, `flush:confirmDestructive:`) is additive, and `removeFromSystem`'s call signature is unchanged (it now logs, but the caller sees the same `nil` return).

**One real behaviour change for already-shipped code, worth calling out explicitly rather than leaving implicit:** ADR 0112/BT-3187 already ships `removeSelector:`, and any workspace that has been calling it has `"remove-method"` entries sitting in its ChangeLog today — *permanently* pending, since `Workspace flush` currently excludes them unconditionally (see *Current State*). Once Phase 2 of this ADR lands, the *next* ordinary `Workspace flush` call (Tier 1, no `flushIncludingDestructive` needed) will apply every such backlog entry, splicing the removed method's byte span out of its source file for real, for the first time. This is the intended fix, not a side effect to guard against — the entries were already durable, already flushable, and already represented a user's explicit `removeSelector:` call; they were simply stuck. But it does mean a long-running workspace upgraded to a build containing this ADR's Phase 2 can see disk writes on its *next* flush that a workspace administrator did not request *at flush time* — the request was made earlier, when `removeSelector:` was originally called. Operators with long-lived workspaces should be aware that upgrading and then running `Workspace flush` may write more than the immediately-preceding session's edits. No opt-out is provided (splicing dead text out of a still-existing file is Tier 1 by this ADR's own classification, see *Why two tiers, not three*) — an operator who wants to inspect the backlog first can do so pre-upgrade via `Workspace changes select: [:e | e kind = #'remove-method']`.

For ADR 0046 (VSCode sidebar) and ADR 0085 (editor live-image representation): no migration — both consume `workspace/applyEdit`, which continues to fire per touched file; the `DeleteFile`/`RenameFile`/`CreateFile` typed-operation upgrade (Phase 6) is additive precision, not a contract change existing consumers depend on differently.

## References
- Related issues: BT-2192 (this ADR), BT-2191 / BT-3183 (ADR 0112 — method-level removal, the direct predecessor this ADR extends into flush), BT-3187 (added `remove-method`'s `side` field and the flush shadow-key fix this ADR's `rename-method` shadow-key work extends further), BT-785 (`removeFromSystem`, gaining ChangeLog logging here), BT-3105 (single class-removal teardown path reused unmodified), BT-2664 / BT-2663 / BT-2665 (ADR 0082's `revert:` mechanisms for `new-class`/add/class-side, generalized here for `remove-class`/`rename-*`)
- Related ADRs: ADR 0082 (Method-Level Edit and Save — the flush/ChangeLog/two-phase-atomicity/Amendment-1 foundation this entire ADR builds on), ADR 0112 (Method-Level Removal Language Primitive — the direct predecessor and the primitive-design template this ADR's `renameTo:`/`renameSelector:to:` follow), ADR 0032 (Early Class Protocol — chain-walk dispatch, and specifically the *limit* of its "no cache to invalidate" guarantee once senders carry baked-in selector text), ADR 0066 (Open Class Extension Methods — extension-method attribution rules this ADR's rename/remove-class logging reuses unmodified from ADR 0082/0112), ADR 0038 (Subclass/ClassBuilder Protocol — dynamic-class flushability precedent)
- Documentation: `docs/beamtalk-language-features.md`, `docs/development/surface-parity.md`, `docs/development/architecture-principles.md` § Duplication & the Shared-Leaf-Module Pattern
- LSP spec: `workspace/applyEdit` with `CreateFile`/`DeleteFile`/`RenameFile` operations, <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_applyEdit>
