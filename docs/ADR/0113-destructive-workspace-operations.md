# ADR 0113: Destructive Workspace Operations — File Deletion and Rename in Flush

## Status
Proposed

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

**Add two new sealed `Behaviour` primitives — `renameTo:` (class rename) and `renameSelector:to:` / `renameSelector:to:ifAbsent:` (method rename) — alongside a ChangeLog logging fix to the existing `removeFromSystem`. Both rename primitives use one shared mechanism: compute every in-project reference to the old name via the xref index, and rewrite the definition plus every found reference transactionally, as one ChangeLog entry carrying a `sites` list — `renameTo:` at class-name granularity, `renameSelector:to:` at selector granularity. Extend `Workspace flush` with a second, explicit tier: ordinary `Workspace flush` applies every entry that only *edits* an existing file (patch, new-class, and now method removal, since excising a span leaves the file in place); a new `Workspace flush: confirmDestructive: true` (and its `flush: aClass confirmDestructive: true` / `flushKinds:` variants) is required to apply any entry that deletes or moves a file (class removal, class rename). This tiering — not autoflush, not a tombstone file — is the safety mechanism, and it applies uniformly regardless of the `autoflush` setting.**

### Why two tiers, not three

The Linear issue's three flows collapse into two risk classes once you ask "does flushing this entry destroy or relocate a file, or only its contents?":

| Flow | What flush does | File survives? | Tier |
|---|---|---|---|
| Method removal | Excise the recorded byte span from the (still-existing) source file | Yes | 1 — ordinary flush |
| Class removal | Delete the `.bt` file | No | 2 — `confirmDestructive` |
| Class rename | Move (and possibly rewrite the declaration line of) the `.bt` file | Moved, not destroyed, but the old path is gone | 2 — `confirmDestructive` |
| Method rename | Rewrite the target file's span **and every sender's span, in every file that calls it** | Yes, but N files are touched that the caller didn't directly ask to edit | 2 — `confirmDestructive` |

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

### `renameTo:` also rewrites cross-file references — the same mechanism as `renameSelector:to:`, one level up

An earlier draft of this ADR gave `renameTo:` no sender-rewrite mechanism at all — it re-registered the class under its new name and left every *other* file's reference to the old name unaddressed. That does not hold up (see Constraint 4): `Counter new`, `Counter class`, a `:: Counter` type annotation, a `subclass: Counter` superclass reference, and an extension declaration in another file (ADR 0066) all compile to a runtime lookup by name atom (`beamtalk_class_registry:whereis_class/1`), and that atom is baked into the referencing call site exactly as a method selector is baked into a sender. A bare re-registration turns every one of those into a silent, delayed `class_not_found` the next time it executes — precisely the failure mode this ADR already built `renameSelector:to:`'s `sites` mechanism to prevent, one granularity down.

`renameTo:` therefore uses the identical mechanism, not a second one: at rename time, it queries the xref index for every in-project reference to the old class name (the class-level analogue of `sendersOf:` — constructor/message sends, type annotations, superclass declarations, extension declarations), and rewrites each one transactionally alongside the class's own re-registration and (if flushable) file move. The result is a single `rename-class` entry whose `sites` list carries every touched location — the exact shape `rename-method` already defines, reused rather than duplicated (see *ChangeLog schema extensions*). The dangling-reference risk this leaves — a reference the xref index cannot see (a dynamically constructed `Smalltalk at: (aString asSymbol)`-style lookup, or a sender in a file that hasn't been indexed yet) — is the same, already-accepted risk category ADR 0112 established for `removeSelector:` and this ADR already accepts for `renameSelector:to:`, not a new one (see *Alternatives Considered*).

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
   %% sites[0] is always the definition site; sites[1..] are current senders
   %% found via the xref index at rename time (see Implementation)
 source_ref: null, prev_source_ref: null,        % superseded by per-site refs above
 sourceFile: null,                                 % ambiguous for a multi-file entry — see sites
 span: null,
 intent: "durable",
 flushable: bool,                                  % true iff every site's file is flushable
 not_flushable_reason: "stdlib" | "dynamic" | "dependency:<path>" | null,
 author, author_kind}
```

`rename-class` and `rename-method` are the two genuinely new shapes — every other kind ADR 0082/0112 defined targets exactly one file; these two target a computed set. For both, `flushable` is `true` only if **every** site (definition + all current in-project references) resolves to a flushable file; if even one reference lives in a dependency or stdlib file, the whole rename entry is `flushable: false` with that reason, because a rename that could only partially reach disk (definition renamed, some references left pointing at the old name) is worse than not flushing at all — it would silently split the live and on-disk surface for those files. (In practice `rename-class` never reaches this branch for stdlib/dependency — it refuses before installing, per the table above — so `not_flushable_reason` on a `rename-class` entry is always `"dynamic"` or absent; the `flushable`/per-site rule is stated generally here because `rename-method` does reach it.)

### `Workspace flush` — the destructive tier

```beamtalk
Workspace flush
=> flushed 2 methods across 1 file
   skipped: 1 destructive entry (Counter — remove-class) —
     use `Workspace flush: confirmDestructive: true` to include it

Workspace flush: confirmDestructive: true
=> flushed 2 methods + 1 removal + 1 rename across 4 files
```

- `Workspace flush` (no argument, existing signature) applies Tier 1 only: patches, `new-class`, and now `remove-method`. Tier 2 entries (`remove-class`, `rename-class`, `rename-method`) are reported in the summary as `skipped: destructive`, distinctly from the existing `skipped: ephemeral` / `skipped: not flushable (...)` reasons — a caller needs to be able to tell "this needs a human/agent decision" apart from "this can never flush."
- `Workspace flush: confirmDestructive: true` (new keyword-argument form) additionally applies Tier 2 for the same scope the base call would have covered. `Workspace flush: aClass confirmDestructive: true` scopes to one class; `Workspace changes flushKinds: #{#'remove-class'} confirmDestructive: true` scopes to one kind — the existing `flushKinds:` filter combinators (ADR 0082) already compose with a scope, so `confirmDestructive` is one more independent filter dimension, not a special case bolted onto each existing form.
- **`confirmDestructive: true` is not read from a workspace setting or environment variable — it is a literal argument the caller passes at the call site, every time.** This is deliberate: a config toggle that silently reclassifies future destructive flushes as safe is exactly the kind of "surprise later" a security-relevant default should not create. Each destructive flush call names its own consent.
- `autoflush: true` (Amendment 1's cockpit default) **never** implies `confirmDestructive: true`. A human clicking "Save" on a live-patched method still autoflushes immediately (Tier 1, unchanged); a human clicking "Remove Class" or "Rename" always surfaces an explicit second gesture, regardless of the autoflush setting — see *Surface* below for what that gesture is per surface. This is the one place autoflush's "one switch, applied uniformly" statement (ADR 0082) gets a second, independent switch layered on top, and it is layered on **on purpose**: autoflush answers "do my edits reach disk without a separate step," `confirmDestructive` answers "do file-destroying edits reach disk without a separate *acknowledgement* step" — two different questions that happen to share a word, "immediately."

### Multi-file atomicity — extending ADR 0082's Phase A/B, not replacing it

ADR 0082's two-phase protocol (Phase A: validate every target, stage every write as `<file>.tmp`; Phase B: rename each `.tmp` into place, sequentially, entries pruned only as each rename succeeds) already handles "write N files, all-or-nothing modulo a documented partial-failure mode." This ADR extends the *staging* step to cover delete and rename, keeping the same two-phase shape:

| Operation | Phase A (stage) | Phase B (commit) |
|---|---|---|
| Patch / new-class (existing) | Write `<file>.tmp` | Rename `<file>.tmp` → `<file>` |
| Method removal (Tier 1) | Write `<file>.tmp` with the span excised | Rename `<file>.tmp` → `<file>` (identical to a patch — this is why Tier 1 needs no new atomicity work) |
| Class removal | Rename `<file>` → `<file>.tmp-delete-<epoch>-<seq>` (same-filesystem rename, POSIX-atomic, trivially reversible) | `unlink <file>.tmp-delete-<epoch>-<seq>` |
| Class rename | Write `<new_path>.tmp` (declaration-line rewritten to the new name; rest of the file byte-identical) **and** `<file>.tmp` per *other* site file with the old-name reference rewritten (same per-site step as method rename, below) | Rename `<new_path>.tmp` → `<new_path>`, `unlink <old_path>`, then rename each site `<file>.tmp` → `<file>`, in seq order |
| Method rename | Write `<file>.tmp` per affected file (definition site + every sender site, spans rewritten) | Rename each `<file>.tmp` → `<file>`, in seq order — same sequential-commit, partial-failure-is-recoverable-via-re-flush shape ADR 0082 already documents for ordinary multi-file flush |

A Phase A failure (a target span no longer resolves — see *External-edit conflicts* below) aborts the whole batch before anything in Phase B runs, exactly as today. A Phase B failure partway through a multi-file rename leaves some files renamed and some not; the per-file status report (already part of ADR 0082's flush summary) tells the caller which, and re-flushing the same `confirmDestructive: true` call retries only what's left — the staged `.tmp-delete-*` files for not-yet-committed deletions are still present on disk (nothing was lost), and already-committed unlinks/renames are not retried because their ChangeEntries were already pruned.

**Class removal's staged-rename step is the closest thing this ADR has to a tombstone, and it is intentionally ephemeral, not persistent** — see *Steelman Analysis*.

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
| REPL | Human (or scripted) | `:remove-class <Class>` prompts `y/N` at the terminal before constructing `Counter removeFromSystem` **and then**, if the class was flushable, a second prompt before appending `confirmDestructive: true` to the follow-up `:flush` — or the human runs `:flush` (Tier 1 only) and later `:flush --confirm-destructive` explicitly. Two prompts, matching two genuinely separate decisions (remove from memory vs. delete from disk). |
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

## User Impact

### Newcomer (from VSCode / Python / JS)

- "Delete File" and "Rename Symbol" already exist as concepts in every editor a newcomer has used — VSCode's own file-delete asks "Move to Trash?" and its rename-symbol feature previews every affected file before applying. `confirmDestructive`'s LSP-surfaced modal matches that expectation directly; nothing new to learn.
- The two-gesture shape (memory-mutate now, disk-confirm separately) is more surprising than VSCode's single-gesture delete — mitigated by making the *first* gesture ("Remove Class" in the browser) still work as expected for the in-session experience (the class is gone from the running app immediately), and only the *disk* consequence needs the second click.

### Smalltalk developer

- `renameTo:`/`renameSelector:to:` read as exactly the kind of message-send-based class-protocol operation Smalltalk trains developers to expect (mirrors `removeSelector:`'s reception in ADR 0112).
- The Refactoring-Browser-style "compute every reference, apply together" behaviour of `renameTo:`/`renameSelector:to:` is the single most Smalltalk-native piece of this ADR — a Pharo developer would be surprised if either worked any *other* way.
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

### Separate command per destructive kind (e.g., `Workspace flushRemovals`, `Workspace flushRenames`) vs. one unified `flush: confirmDestructive:`

- 🧑‍💻 **Newcomer:** "Named commands are self-documenting — `flushRemovals` tells me exactly what it's going to do without needing to know what's pending first."
- 🎩 **Smalltalk purist:** "Pharo's Refactoring Engine genuinely does have distinct entry points per refactoring kind (`RBRenameClassRefactoring` vs `RBRemoveMethodRefactoring` are different classes with different `execute` protocols) — mirroring that distinction here would be staying closer to the prior art this ADR itself cites."
- ⚙️ **BEAM veteran:** "Separate commands are easier to reason about independently and easier to add telemetry/rate-limiting to per kind if one destructive category turns out riskier in practice than another."
- 🏭 **Operator:** "A named command in an audit log (`flushRemovals` called) is more legible at a glance than `flush: confirmDestructive: true` plus having to cross-reference which entries were pending to know what actually happened."
- 🎨 **Language designer:** "One command with a boolean is a smaller, more composable surface, but it conflates 'I want to flush everything destructive' with 'I want to flush *this specific* destructive thing' — separate commands let each kind evolve its own parameters independently later without polluting a shared signature."
- **Why unified wins:** `flushKinds:` already exists (ADR 0082) as the general filter-by-kind mechanism, and it already composes with a scope (`aClass`) — adding `confirmDestructive` as one more filter dimension on the *same* mechanism (`Workspace changes flushKinds: #{#'remove-class'} confirmDestructive: true`) gets the operator's "legible, specific" want and the language designer's "each kind can carry its own semantics" want for free, without a second command family to keep in sync with the first as new kinds are added later (exactly the kind of duplicate-vocabulary risk CLAUDE.md's no-duplicate-implementations rule flags — a `flushRemovals`/`flushRenames` family would re-derive `flushKinds:`'s filtering logic under new names). The purist's Pharo-precedent argument is real but describes Pharo's *in-memory execute* step, which this ADR already gives distinct primitives for (`renameTo:`, `renameSelector:to:`, `removeFromSystem`) — the disagreement is only about the *flush* step, which ADR 0082 already unified across every existing kind, and there is no new argument here for un-unifying it just for the newest three.

### Tension points

- **Newcomer/BEAM-veteran "simplicity" vs. operator/agent "safety" on `confirmDestructive`:** the strongest real tension. Resolved the same direction ADR 0082 resolved patch-vs-write-through — safety wins for the *default*, and the cost is paid once per destructive flush call, not per ordinary edit. Unlike Alternative B in ADR 0082 (rejected write-through), this is not asking every caller to pay the cost on every operation — Tier 1 stays exactly as frictionless as it is today.
- **Purist "Refactoring-Browser-style separate commands" vs. designer "one unified filter mechanism":** resolved in favour of the unified mechanism because `flushKinds:` already exists and already solves the composability problem the separate-command instinct is reaching for — see above.
- **"Git is enough" vs. "a workspace-native safety gate is still needed":** the whole `confirmDestructive` design rests on this tension resolving toward the second position, and it does so specifically because of Amendment 1's agent/human split, not because git-recoverability is weak in general — for the *cockpit* audience alone, the BEAM-veteran/operator argument ("git already handles this") would be much stronger, and a future revision could reasonably relax `confirmDestructive` to an autoflush-tracked default for the cockpit surface specifically once real usage data exists (mirroring how Amendment 1 itself was a usage-driven refinement of the original ADR 0082 design, not a day-one decision).

## Alternatives Considered

### Alternative: no confirmation tier — destructive entries flush like any other durable+flushable entry

See Steelman above. Rejected as a silent, breaking behaviour change for every existing unmodified `Workspace flush` caller once this ADR's new entry kinds start appearing in a workspace's ChangeLog.

### Alternative: persistent on-disk tombstone (`<file>.bt.deleted`) instead of (or in addition to) `confirmDestructive`

See Steelman above. Rejected as a third, redundant source of truth about what currently exists, echoing ADR 0082's Alternative F (Shadow-file overlay) rejection reasoning. The staged-rename-then-unlink step already gives equivalent crash-safety without the permanence.

### Alternative: separate `flushRemovals` / `flushRenames` commands instead of `flush: confirmDestructive:`

See Steelman above. Rejected in favour of extending the existing `flushKinds:` filter mechanism, avoiding a second, parallel vocabulary for "which entries to apply."

### Alternative: block `renameTo:`/`renameSelector:to:` on references outside the current in-project rename batch (never leave a partial rename)

Considered for the case where a rename's `sites` list, computed via the xref index, might miss a reference that exists in a file the xref index hasn't indexed yet (a freshly-created, not-yet-compiled file) or a dynamically-constructed lookup (`aClass perform: (aString asSymbol)`, `Smalltalk at: aSymbol`) that no static xref can see. Rejected as a hard block, for both primitives: ADR 0112 already accepted this exact risk category for `removeSelector:` ("dangling senders... not prevented, but the tooling to surface them already exists") and gave a considered reason not to block on it — the same reasoning applies to both rename primitives without a new argument. `renameTo:`/`renameSelector:to:` instead surface the reference count found (mirroring ADR 0112's hint text) so the caller has visibility, without refusing an action ADR 0112 already decided is the caller's call to make.

### Alternative: language-level rename primitive omitted; rename is LSP/tooling-only (like a bare text find-and-replace the editor performs, with no `Behaviour` message)

Rejected for the identical reason ADR 0112 rejected a workspace-only `removeSelector:` — it would violate ADR 0082's "every tool op is a structured invocation of a Beamtalk expression" principle, and every surface (MCP, LSP, REPL, browser) would end up separately re-implementing "find senders, rewrite them" logic against the runtime's xref index instead of sharing one primitive, precisely the duplication CLAUDE.md's no-duplicate-implementations rule warns against.

## Consequences

### Positive

- Closes a real, already-observable gap: `Workspace changes` today either contains `remove-method` entries that can never flush (permanently stuck) or is silently missing `remove-class` entries entirely (`removeFromSystem` doesn't log). Both are fixed.
- `Behaviour` gains a complete patch/create/remove/rename set (`compile:source:`, `newClass:at:`, `removeSelector:`, `renameTo:`, `renameSelector:to:`) with consistent receiver, error, and ChangeLog conventions across all five — the "closes the gap" pattern ADR 0112 already established for its own trio extends cleanly.
- Reuses, rather than reinvents, three separate pieces of existing infrastructure: ADR 0082's two-phase flush protocol (extended, not replaced), ADR 0112's recompile-based method-removal mechanism (the same machinery both rename primitives' per-site rewrite generalizes further, one shared mechanism rather than two), and the xref index (its existing `sendersOf:` query, already built for ADR 0112's dangling-sender hint, now load-bearing for `renameSelector:to:`'s `sites`; its new `referencesOfClass:` sibling doing the same job for `renameTo:`).
- `confirmDestructive` closes a real, silent-upgrade-risk gap for every unmodified `Workspace flush` caller, without adding any friction to the Tier-1 path this ADR leaves untouched.

### Negative

- **`renameTo:` and `renameSelector:to:` are the most implementation-heavy primitives this codebase has added via an ADR to date** — both require recompiling not just the target definition (as removal already does) but every current in-project reference, computed transactionally, with a `sites` list that has no precedent in the existing schema shape (every prior kind, before this ADR, targeted exactly one file). This is real complexity duplicated across two primitives, not a documentation-only extension — an earlier draft of this ADR under-scoped `renameTo:` specifically by omitting this mechanism for it (see Constraint 4 and the *renameTo: also rewrites cross-file references* subsection), which is worth flagging here so a future reviewer knows the parity was deliberate, not incidental.
- **A rename can only ever rewrite references the xref index can see.** Dynamically constructed lookups (`perform:`, `Smalltalk at:`, string-built selectors/names) and references in files that haven't been compiled/indexed yet are invisible to `sites` and become dangling exactly the way ADR 0112 already accepted for `removeSelector:` — this ADR does not raise that bar, but a rename's blast radius (across N files) makes an invisible dangling reference more likely to matter than a single-selector removal's, and a *class* rename's blast radius (every constructor send, type annotation, and subclass declaration project-wide) is larger again than a method rename's.
- **`confirmDestructive` is one more thing every caller across four surfaces has to learn**, and — as the Steelman concedes — is a real, if justified, complication relative to ADR 0082's existing single-mode `flush`.
- **Two additional stdlib/dependency-refusal precedents to keep straight** (`renameTo:`/`moveClass:to:` refuse like `removeFromSystem`; `renameSelector:to:` allows-with-flushable-false like `removeSelector:`/`compile:source:`) — a future contributor adding a sixth `Behaviour` primitive has to re-derive which precedent applies rather than there being one uniform rule; this ADR's *Refusal vs flushability* table is the reference, but the asymmetry itself is a documented cost, not a free simplification.
- **A `rename-class` or `rename-method` entry that partially fails at Phase A (one reference site's span no longer resolves) aborts the entire multi-file rename**, per *External-edit conflicts* — this can be more disruptive than a single-file patch conflict, since the caller has to resolve one stale site before *any* of the rename (including the definition itself) can flush, even if every other site is fine.

### Neutral

- `removeFromSystem`'s stdlib/dependency/subclass refusals are unchanged — this ADR only adds logging to an already-decided operation, not new policy.
- No bulk rename primitive (rename N selectors in one call) ships — same "loop over the existing primitive" answer ADR 0112 gave for a bulk remove-all-methods primitive, for the same "no acceptance criterion calls for it" reason.
- `Workspace flush`'s existing single-argument form remains the common case and is textually unchanged for every caller that never touches a destructive entry — this ADR is purely additive to the flush surface for those callers.

### DDD Model Impact

- **Compilation context** gains the multi-site rewrite step both `renameTo:` and `renameSelector:to:` share (rewrite N spans across N files, each independently re-parsed and re-validated) — an extension of the existing byte-span resolver (ADR 0082), not a new subsystem, and one mechanism serving both primitives rather than two.
- **Workspace context** owns the new `beamtalk_workspace_flush` Tier-2 staging logic (rename-to-tmp-then-unlink for delete; write-then-rename-then-unlink-old for class rename) and the `confirmDestructive` filter dimension on `flush`/`flushKinds:` — same module boundaries ADR 0082 already established, extended rather than duplicated.
- **Runtime context** gains `classRenameTo`/`classRenameSelector`/`classRenameSelectorIfAbsent`/`workspaceMoveClass` primitives alongside the existing `classRemoveSelector*` family in `beamtalk_behaviour_intrinsics.erl`, plus the new `referencesOfClass:`-shaped xref query `renameTo:` needs (a class-name-reference analogue of the existing `sendersOf:`), and the ChangeLog-append call `removeFromSystem` is currently missing.
- **No language-service changes beyond the LSP typed-resource-operation work** already covered under *Prior Art* / *Surface*.

## Implementation

*(For downstream implementation work — this ADR does not implement any of the below.)*

### Affected components

| Layer | Change |
|---|---|
| `runtime/apps/beamtalk_runtime/src/beamtalk_behaviour_intrinsics.erl` | New `classRenameTo/2`, `classRenameSelector/3`, `classRenameSelectorIfAbsent/4` primitives, modelled directly on the existing `classRemoveSelector`/`classRemoveSelectorIfAbsent` functions in the same module. `classRemoveFromSystemByName/1` gains a ChangeLog-append call at its existing success point (`publish_class_removed/2` call site), capturing the class's full current source as `prev_source_ref` *before* the removal proceeds (mirrors the existing "read+parse before mutate" ordering `compile:source:`'s patch hook already uses). |
| `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_interface_primitives.erl` (or a new sibling module) | New `workspaceMoveClass/2` backing `Workspace moveClass:to:`, modelled on the existing `newClass:at:` primitive in the same module — single-file move, no site discovery. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_eval.erl` / `beamtalk_repl_loader.erl` | One shared multi-site rewrite mechanism, generalizing ADR 0112's `remove_method/3` (a sibling `rewrite_sites/4` or equivalent) — reuse, not two parallel mechanisms, per CLAUDE.md's no-duplicate-implementations rule. `renameSelector:to:` calls it with the xref `sendersOf:` query (already exists, used today for ADR 0112's dangling-sender hint) as the site-discovery step; `renameTo:` calls the *same* function with a new xref query — call it `referencesOfClass:` — that generalizes `sendersOf:` from "selector sends" to "class-name references" (constructor/message sends, type annotations, superclass declarations, extension declarations). The rewrite/staging half of the mechanism does not care which query produced its `sites` list. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_changelog.erl` | New `kind`s (`'remove-class'`, `'rename-class'`, `'rename-method'`), new fields (`old_class`, `old_path`, `new_path`, `sites`) on the entry record, extending the existing open-schema pattern — `sites` is now shared by both rename kinds, not `rename-method`-specific. `target_key/1` (ADR 0112, BT-3187's `(class, selector, side)` fix) needs a variant for both multi-site shapes — likely keying shadow-detection per-site rather than per-entry, since two independent renames could touch overlapping reference files. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_flush.erl` | Remove the current unconditional `exclude_remove_method/1` filtering and replace with tier classification (`entry_tier(Entry) -> tier1 \| tier2`); `do_flush/1` gains a `confirmDestructive` parameter threaded through `filter_entries/2`; Phase A/B staging extended per *Multi-file atomicity* above — the delete/rename staging steps are new, everything else (grouping, shadow-duplicate detection, per-file status reporting) is reused unmodified. |
| `stdlib/src/Behaviour.bt` | `renameTo:`, `renameSelector:to:`, `renameSelector:to:ifAbsent:` — three new sealed methods, same pattern as `removeSelector:`/`removeSelector:ifAbsent:`. |
| `stdlib/src/Workspace.bt` | `flush: confirmDestructive:` keyword variant on the existing `flush`/`flush:` facade methods; `flushKinds:confirmDestructive:` on `ChangeLog`; `moveClass:to:` for the path-only-move case (no identity change), per its own subsection above. |
| `crates/beamtalk-lsp/src/runtime.rs` | `FlushEvent` needs to carry per-file *operation* (`Change`/`Create`/`Delete`/`Rename{from}`), not just a flat path list — a breaking change to the existing struct's shape, needed regardless of whether this ADR's new kinds are in play, since `new-class` flush already deserves `CreateFile` and doesn't get it today. |
| `crates/beamtalk-lsp/src/server.rs` | `workspace/executeCommand: flush` gains a `confirmDestructive` argument; emits `DeleteFile`/`RenameFile`/`CreateFile` resource operations per the extended `FlushEvent`. |
| `crates/beamtalk-mcp/src/server.rs` | New tools: `remove_class` (wraps `removeFromSystem`), `rename_class` (wraps `renameTo:`), `rename_method` (wraps `renameSelector:to:`); `flush` tool gains a required-when-applicable `confirm_destructive` boolean. |
| `crates/beamtalk-cli/src/commands/repl/mod.rs` | New meta-commands `:remove-class`, `:rename-class`, `:rename-method`, each with the two-prompt shape from *Surface* above; `:flush` gains a `--confirm-destructive` flag. |
| `runtime/apps/beamtalk_workspace/priv/static/workspace.js` | "Remove Class" / "Rename" browser actions with the distinct destructive-dirty-indicator affordance from *Surface* above. |
| `docs/development/surface-parity.md` | Four new expression-backed rows; a note on `confirmDestructive` as a parity-preserving argument (see *Surface*). |
| `docs/beamtalk-language-features.md` | Document `renameTo:`/`renameSelector:to:` alongside the existing `removeSelector:`/`removeFromSystem` sections. |

### Phased rollout

| Phase | Scope | Effort | Tests |
|---|---|---|---|
| **1** | `removeFromSystem` ChangeLog-logging fix (`kind: "remove-class"`) — the smallest, most independently-shippable piece, and the one closing an audit gap that exists today with zero new UX to design. | S | EUnit: entry appears with correct `flushable`/`prev_source_ref`; BUnit: `Workspace changes` shows it. |
| **2** | `Workspace flush` tiering: classify `remove-method`/`remove-class` as Tier 1/Tier 2 respectively, implement `confirmDestructive` filter dimension, implement class-removal staged-delete (rename-to-tmp, unlink). Unblocks the already-stuck `remove-method` entries as a side effect (Tier 1, no new gate needed for them). | M | EUnit: staged-delete crash-safety (kill between rename and unlink); BUnit: `Workspace flush` reports `skipped: destructive`, `flush: confirmDestructive: true` applies it. |
| **3** | The shared multi-site rewrite mechanism (xref-driven site discovery, transactional in-memory rewrite of definition + sites, `sites`-shaped ChangeLog entry, multi-file flush) plus `renameTo:` as its first consumer: class-name reference discovery (`referencesOfClass:`), class-rename flush (file move, declaration-line rewrite, reference-site rewrite), `Workspace moveClass:to:` for the path-only case. Largest phase — this is where the mechanism gets built, not just wired up. | L | EUnit: reference rewrite correctness against a small fixture graph (class with 3 in-project references across 2 files) + rename-to-tmp-then-final-rename atomicity; BUnit: `Counter renameTo: #Accumulator` + flush produces the moved file, updated declaration, and rewritten references, with no dangling `class_not_found`. |
| **4** | `renameSelector:to:`/`ifAbsent:` — reuses Phase 3's mechanism with the existing `sendersOf:` query as its site-discovery step instead of `referencesOfClass:`. Depends on Phase 3, not the reverse; smaller than Phase 3 because the mechanism itself is already built. | M | BUnit: end-to-end method rename + flush + verify no dangling `does_not_understand`; regression check that Phase 3's fixture graph still passes through the shared code path. |
| **5** | `revert:` extensions for all three new kinds (Undo story table above). | M | BUnit: revert of each kind, pre-flush and (where applicable) documented as unsupported post-flush. |
| **6** | LSP `DeleteFile`/`RenameFile`/`CreateFile` typed resource operations (`FlushEvent` restructuring); MCP tools; REPL meta-commands; browser actions. Surface-parity audit. | M | LSP command tests; MCP integration tests; browser e2e for the two-gesture destructive flow; surface-parity drift check passes. |

Total: ~M-L across 6 phases. Phase 3 (the shared rewrite mechanism, built via `renameTo:`) is the load-bearing risk — if it proves unreliable against real code (partial rewrites, xref gaps), the design may need to fall back to a narrower v1 for *both* rename primitives (rewrite the definition only, leave references as a `Workspace changes` follow-up list for the caller to apply manually) before committing to the full transactional shape. Building the mechanism once, in the phase with the smaller blast radius per mistake (a class-rename fixture is easier to reason about than a method-rename one with overlapping selector names), before Phase 4 reuses it, is a deliberate risk-ordering choice.

## References
- Related issues: BT-2192 (this ADR), BT-2191 / BT-3183 (ADR 0112 — method-level removal, the direct predecessor this ADR extends into flush), BT-3187 (added `remove-method`'s `side` field and the flush shadow-key fix this ADR's `rename-method` shadow-key work extends further), BT-785 (`removeFromSystem`, gaining ChangeLog logging here), BT-3105 (single class-removal teardown path reused unmodified), BT-2664 / BT-2663 / BT-2665 (ADR 0082's `revert:` mechanisms for `new-class`/add/class-side, generalized here for `remove-class`/`rename-*`)
- Related ADRs: ADR 0082 (Method-Level Edit and Save — the flush/ChangeLog/two-phase-atomicity/Amendment-1 foundation this entire ADR builds on), ADR 0112 (Method-Level Removal Language Primitive — the direct predecessor and the primitive-design template this ADR's `renameTo:`/`renameSelector:to:` follow), ADR 0032 (Early Class Protocol — chain-walk dispatch, and specifically the *limit* of its "no cache to invalidate" guarantee once senders carry baked-in selector text), ADR 0066 (Open Class Extension Methods — extension-method attribution rules this ADR's rename/remove-class logging reuses unmodified from ADR 0082/0112), ADR 0038 (Subclass/ClassBuilder Protocol — dynamic-class flushability precedent)
- Documentation: `docs/beamtalk-language-features.md`, `docs/development/surface-parity.md`, `docs/development/architecture-principles.md` § Duplication & the Shared-Leaf-Module Pattern
- LSP spec: `workspace/applyEdit` with `CreateFile`/`DeleteFile`/`RenameFile` operations, <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_applyEdit>
