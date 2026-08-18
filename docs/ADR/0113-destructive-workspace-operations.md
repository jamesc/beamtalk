# ADR 0113: Destructive Workspace Operations — File Deletion in Flush

## Status
Proposed (2026-08-18)

## Context

### Problem

ADR 0082 (Method-Level Edit and Save in the Live Workspace) shipped a full patch/create story — `Behaviour compile:source:` / `tryCompile:source:`, `Workspace newClass:at:`, `Workspace flush` — and explicitly deferred anything that deletes a file:

> **Class-level removal flush UX** — `aClass removeFromSystem` already exists (BT-785) for memory removal. What it should mean to *flush* a class removal — deleting a `.bt` file from disk — is irreversibly destructive and wants its own UX...

ADR 0112 (Method-Level Removal Language Primitive) then shipped `Behaviour removeSelector:` / `removeSelector:ifAbsent:` — the in-memory half of method removal — but drew its own scope boundary at the same line:

> **What `Workspace flush` does with a `"remove-method"` entry is explicitly out of scope for this ADR.** Splicing dead text out of a live `.bt` file is a destructive disk operation... it deserves the same confirmation/tombstone/undo design ADR 0082 already deferred class-level removal-flush to BT-2192 for.

This ADR is that design, narrowed to the two flows that flush *deletions*. (BT-2192 originally also scoped in class/method rename; that turned out to carry real, unresolved design risk — an xref index gap for live-patched code, a selector-name-global-scope correctness problem in naive auto-rewrite, an undesigned in-memory cross-gen-server atomicity question — that has nothing to do with deletion. Rename is split out to its own follow-up, BT-3204, so this ADR can ship the small, low-risk half on its own. See *Migration Path* for how the split was decided.)

Two flows share one root cause — flush turning a durable in-memory change into an on-disk file mutation that cannot be undone by re-running the operation, the way a patch's byte-span replacement can:

1. **Method removal flushed to disk.** `Counter removeSelector: #increment` already installs in memory and logs a `kind: "remove-method"` ChangeLog entry (ADR 0112, BT-3187). `Workspace flush` today explicitly excludes these entries from being spliced (see *Current State*) — they sit in `Workspace changes` forever, never applied, never reported as skipped-for-a-real-reason.
2. **Class removal flushed to disk.** `Counter removeFromSystem` (BT-785) already removes the class from memory, purges every derived registry (BT-3105), and returns — but it does not append a ChangeLog entry at all today. There is no record that a removal happened, and no way for `flush` to know a `.bt` file should be deleted.

### Current State

| Concern | Today |
|---|---|
| `Counter removeSelector: #sel` | Installs in memory, logs `kind: "remove-method"` with `span`, `prev_source_ref`, `side`, `flushable` (ADR 0112, BT-3187) |
| `Workspace flush` handling of `"remove-method"` entries | **Explicitly excluded before splicing** (`beamtalk_workspace_flush:exclude_remove_method/1`) — the entry survives shadowing (so a stale patch to the same selector doesn't wrongly resurrect it) but is never written to disk and never marked flushed. It stays in `Workspace changes` indefinitely with no path forward. |
| `Counter removeFromSystem` | Removes class from memory, purges xref/extensions/protocol/compiler-cache/`class_sources` (BT-3105). **Does not append a ChangeLog entry.** No record exists that the removal happened; `Workspace changes` shows nothing. |
| LSP `workspace/applyEdit` | Runtime emits one `Change`-shaped edit per touched file on flush (`FlushEvent { files: Vec<String> }`, `crates/beamtalk-lsp/src/runtime.rs`) — no `DeleteFile` operation is ever constructed. |
| Undo (`Workspace changes revert:`) | Handles patch-modify, patch-add, and `new-class` (ADR 0082). Untested/undesigned for `remove-method` (though the schema already carries `prev_source_ref` for it), and has no case for `remove-class`. |
| `Workspace flush` confirmation | Single mode: writes every `intent: durable AND flushable: true` entry, no distinction by how destructive the write is. |

### Constraints

1. **ADR 0082's two-phase flush protocol** (Phase A: validate + stage every write; Phase B: commit renames in sequence) is the existing atomicity mechanism for multi-file flush. Whatever this ADR adds must compose with it, not replace it — CLAUDE.md's no-duplicate-implementations rule leaves no other option once a working two-phase primitive exists.
2. **ADR 0082 Amendment 1** split the workspace into two audiences with different default postures: the LiveView cockpit is **human, git-first, `autoflush: true` by default**; MCP is **agent, ChangeLog-first, `autoflush: false` always**. Any confirmation UX this ADR designs has to work for both — a synchronous "are you sure?" dialog makes sense for a human clicking a button and is meaningless for a programmatic MCP call.
3. **ADR 0112's flushability precedent.** `compile:source:` and `removeSelector:` both install unconditionally in memory and vary only `flushable`/`not_flushable_reason` for stdlib/dynamic/dependency classes ("flushability, not refusal"). `removeFromSystem` (BT-785) instead hard-refuses stdlib and subclassed classes *before* installing. This ADR doesn't revisit that choice for `removeFromSystem` — it only adds the missing ChangeLog logging to it.
4. **Reproducible-build guarantee** (ADR 0082, ADR 0112): flush must never write into the stdlib source tree or a dependency's cache, exactly as it doesn't today for patches or method removal.
5. **Surface parity** (`docs/development/surface-parity.md`, ADR 0082's "every tool op is a structured invocation of a Beamtalk expression"). Whatever this ADR adds must be reachable identically from REPL, MCP, LSP, and browser.

## Decision

**Fix the ChangeLog logging gap in the existing `removeFromSystem`, and extend `Workspace flush` with a second, explicit tier: ordinary `Workspace flush` applies every entry that only *edits* an existing file (patch, new-class, and now method removal, since excising a span leaves the file in place); a new unscoped `Workspace flushIncludingDestructive` selector (plus the keyword-argument `flush: aClass confirmDestructive: true` / `flushKinds: aSet confirmDestructive: true` variants, where the class/kind argument gives the second keyword a real partner) is required to apply an entry that deletes a file (class removal). This tiering — not autoflush, not a tombstone file — is the safety mechanism, and it applies uniformly regardless of the `autoflush` setting.** No new language primitive is added — `removeSelector:` (ADR 0112) and `removeFromSystem` (BT-785) already exist; this ADR is entirely about what `flush` does with the ChangeLog entries they already produce (or, for `removeFromSystem`, should produce).

### Why a confirmation tier at all

The Linear issue's two flows split into different risk classes once you ask "does flushing this entry destroy a file, or only edit its contents?":

| Flow | What flush does | File survives? | Tier |
|---|---|---|---|
| Method removal | Excise the recorded byte span from the (still-existing) source file | Yes | 1 — ordinary flush |
| Class removal | Delete the `.bt` file | No | 2 — `confirmDestructive` |

Method removal reads, at first glance, like it belongs with class removal — both stem from a `remove*` primitive. But the actual risk a confirmation gate exists to catch is "flush is about to make a file disappear," and splicing dead text out of an existing file is mechanically identical to what a normal patch already does unconfirmed today (byte-span replacement — the empty-replacement special case). Gating it behind `confirmDestructive` would be inconsistent with `compile:source:` for no safety benefit, and would leave `Workspace changes` permanently non-empty for the common case of "I deleted one dead method," which is exactly the stuck state *Current State* describes today.

### Fixing `removeFromSystem`'s missing ChangeLog entry

**Required fix, not new ground:** `Counter removeFromSystem` must append a `kind: "remove-class"` ChangeLog entry on every successful removal of a flushable class, mirroring the audit-trail-is-unconditional rule ADR 0082 established for every other in-memory mutation ("every in-memory method mutation produces a ChangeEntry. Always."). Today it produces none, which means `Workspace changes` cannot answer "was this class removed, and is that removal reflected on disk?" — the same gap ADR 0082's audit trail exists to close everywhere else. `removeFromSystem` already refuses stdlib/dependency classes before acting (BT-785, unchanged by this ADR), so the entry it appends is always either `flushable: true` (ordinary project class) or `flushable: false, not_flushable_reason: "dynamic"` (ClassBuilder class, no file to delete) — never `"stdlib"` or `"dependency:..."`, since those cases never reach the append point.

### ChangeLog schema extension

Extending ADR 0082's open `kind` enum exactly where it said it would (`"remove-method"` already shipped via ADR 0112/BT-3187; `"remove-class"` is new):

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
```

Every other field on this entry matches ADR 0082's established shape exactly — no new concepts, just a new `kind` value and the whole-file (`span: null`) framing `new-class` already established for a non-byte-range target.

### `Workspace flush` — the destructive tier

```beamtalk
Workspace flush
=> flushed 2 methods across 1 file
   skipped: 1 destructive entry (Counter — remove-class) —
     use `Workspace flushIncludingDestructive` to include it

Workspace flushIncludingDestructive
=> flushed 2 methods + 1 removal across 2 files
```

- `Workspace flush` (no argument, existing signature) applies Tier 1 only: patches, `new-class`, and now `remove-method`. Tier 2 entries (`remove-class`) are reported in the summary as `skipped: destructive`, distinctly from the existing `skipped: ephemeral` / `skipped: not flushable (...)` reasons — a caller needs to be able to tell "this needs a human/agent decision" apart from "this can never flush."
- `Workspace flushIncludingDestructive` (new, unscoped) additionally applies Tier 2 across the whole pending set — a bare unary selector, not a keyword message, because there is no class/kind argument to attach a `confirmDestructive:` keyword to once the call is unscoped; Smalltalk keyword messages cannot omit an argument the way an "optional parameter" language could. `Workspace flush: aClass confirmDestructive: true` scopes Tier 2 to one class (the class argument gives `confirmDestructive:` a real keyword partner, so this stays an ordinary two-keyword message); `Workspace changes flushKinds: #{#'remove-class'} confirmDestructive: true` scopes to one kind the same way — the existing `flushKinds:` filter combinators (ADR 0082) already compose with a scope, so `confirmDestructive:` is one more independent filter dimension on the keyword forms, not a special case bolted onto each existing form.
- **The destructive tier is never silently on — reaching it always requires either the distinct `flushIncludingDestructive` selector or an explicit `confirmDestructive: true` argument, never a workspace setting or environment variable.** This is deliberate: a config toggle that silently reclassifies future destructive flushes as safe is exactly the kind of "surprise later" a security-relevant default should not create. Each destructive flush call names its own consent, whether by selector or by argument.
- `autoflush: true` (Amendment 1's cockpit default) **never** implies `confirmDestructive: true`. A human clicking "Save" on a live-patched method still autoflushes immediately (Tier 1, unchanged); a human clicking "Remove Class" always surfaces an explicit second gesture, regardless of the autoflush setting — see *Surface* below for what that gesture is per surface. This is the one place autoflush's "one switch, applied uniformly" statement (ADR 0082) gets a second, independent switch layered on top, and it is layered on **on purpose**: autoflush answers "do my edits reach disk without a separate step," `confirmDestructive` answers "do file-destroying edits reach disk without a separate *acknowledgement* step" — two different questions that happen to share a word, "immediately."

### Delete atomicity — extending ADR 0082's Phase A/B, not replacing it

ADR 0082's two-phase protocol (Phase A: validate every target, stage every write as `<file>.tmp`; Phase B: rename each `.tmp` into place, entries pruned only as each rename succeeds) already handles "write N files, all-or-nothing modulo a documented partial-failure mode." This ADR extends the *staging* step to cover delete, keeping the same two-phase shape:

| Operation | Phase A (stage) | Phase B (commit) |
|---|---|---|
| Patch / new-class (existing) | Write `<file>.tmp` | Rename `<file>.tmp` → `<file>` |
| Method removal (Tier 1) | Write `<file>.tmp` with the span excised | Rename `<file>.tmp` → `<file>` (identical to a patch — this is why Tier 1 needs no new atomicity work) |
| Class removal (Tier 2) | Rename `<file>` → `<file>.tmp-delete-<epoch>-<seq>` (same-filesystem rename, POSIX-atomic, trivially reversible) | `unlink <file>.tmp-delete-<epoch>-<seq>` |

Because `remove-class` targets exactly one file, this ADR needs none of ADR 0082's *multi*-file sequencing complexity — a single staged rename, then a single unlink. A crash between the two leaves a recoverable `.tmp-delete-*` file on disk (nothing lost); a re-flush finishes the unlink. **This staged-rename step is the closest thing this ADR has to a tombstone, and it is intentionally ephemeral, not persistent** — see *Steelman Analysis*.

### Undo story

`Workspace changes revert:` (ADR 0082) extends to both kinds, symmetric with `prev_source_ref`'s existing role:

| Kind | `revert:` behaviour |
|---|---|
| `remove-method` | Re-installs `prev_source_ref` at the recorded selector/side — the recorded prior body is exactly what `compile:source:` needs, so revert is a patch back to the pre-removal method. Already implied by the schema (ADR 0112 recorded `prev_source_ref` on removal specifically for this); this ADR is the first thing that actually exercises it. |
| `remove-class` | Recompiles and reinstalls the whole class from `prev_source_ref` (the full pre-removal source, captured at hook time by `removeFromSystem`'s new logging step) via the same `Workspace newClass:at:`-shaped install path `new-class` revert already uses (ADR 0082, BT-2664) — reusing that path rather than inventing a second whole-class-install mechanism. |

**Once flushed, `revert:` degrades to "best-effort, pre-flush semantics only"** for the same reason ADR 0082 already documents for ordinary flushed patches — the ChangeEntry is pruned on successful flush, so post-flush undo is git's job (for humans, per Amendment 1) or a fresh corrective operation (for agents, who can re-run `newClass:at:` from the same `prev_source_ref` snapshot if they kept it — the ChangeLog's own audit/archive retains it per ADR 0082's rotation policy even after pruning from the active view). This ADR does not add a third undo mechanism beyond "revert before flush" and "git/re-create after flush" — see *Steelman Analysis*, tombstone question.

### External-edit conflicts

Reuses ADR 0082's `(mtime, content-hash)` snapshot-and-compare mechanism verbatim, extended to the one new failure shape deletion introduces beyond "content changed":

| Conflict | Detection | Resolution |
|---|---|---|
| Target file's content changed since the entry was logged (patch, `remove-method`) | Existing mechanism, unchanged | Existing choices: `flush:force`, `changes clear`, `changes diff:` |
| Target file for a `remove-class` was already deleted externally | `stat` fails at Phase A | Surfaces as `already gone — nothing to remove`, a soft success: the entry is pruned, the outcome the user wanted already holds |

### Reproducible-build guarantee

Unaffected by construction: `removeFromSystem` refuses stdlib/dependency classes before any ChangeEntry exists, so a `remove-class` entry is never `flushable` against protected source. Flush still never writes into the stdlib tree or a dependency cache — the same guarantee ADR 0082/0112 already state, unmodified.

### Surface

Per ADR 0082's principle, every surface constructs one of the Beamtalk expressions above and submits via the existing `evaluate` op — no new workspace-side dispatcher op. What differs by surface, per Amendment 1, is **what "confirm" means**:

| Surface | Audience | Confirmation gesture |
|---|---|---|
| REPL | Human (or scripted) | `:remove-class <Class>` prompts `y/N` at the terminal before constructing `Counter removeFromSystem` **and then**, if the class was flushable, a second prompt before running the follow-up `:flush-destructive` (a distinct meta-command, mirroring the `:flush`/`:flush <Class>` pair with a `:flush-destructive`/`:flush-destructive <Class>` pair rather than a flag on `:flush` — the REPL meta-command layer has no `--flag` precedent, only positional args) — or the human runs `:flush` (Tier 1 only) and later `:flush-destructive` explicitly. Two prompts, matching two genuinely separate decisions (remove from memory vs. delete from disk). |
| MCP | Agent | The `remove_class` tool constructs the memory-mutating expression only — it does **not** implicitly flush. A distinct `flush` tool call with an explicit `confirm_destructive: true` argument is required to reach disk, mirroring `try_method` → `save_method`'s existing two-step promotion idiom (ADR 0082): the *first* call is exploratory/reversible-via-revert, the *second* is the one that actually commits. No interactive dialog exists for MCP, so the tool schema's required boolean argument **is** the confirmation — an agent cannot flush a destructive entry by accident because the parameter has no default. |
| LSP | Editor (VSCode etc.) | `workspace/executeCommand: flush` gains an optional `confirmDestructive` argument; the VSCode extension surfaces a native modal ("This will delete `foo.bt` — Continue?") before sending it, listing the affected path from `Workspace changes` (already queryable pre-flush). |
| Browser | Human, cockpit | A "Remove Class" action performs the memory-mutating call immediately (matching `autoflush: true`'s existing "the memory step is not gated" behaviour for ordinary patches) but the resulting dirty indicator for that entry renders with a distinct "destructive — needs confirmation" affordance instead of silently participating in the autoflush write, requiring one explicit click ("Delete file") to actually call `confirmDestructive: true`. This is the browser's analogue of the REPL's second prompt — same two-decision shape, native-to-cockpit affordance instead of a terminal prompt. |

`docs/development/surface-parity.md` gains one row (`removeFromSystem`'s logging fix needs no new row — it is the same expression as today, just with an audit-trail side effect) and a note that `confirmDestructive` is a parity-preserving *argument*, not a surface-specific rule: every surface can express "flush without destructive entries" and "flush including them," they just gate the second one differently because a modal dialog and a required tool-schema argument are the same *shape* of gate (an explicit, un-defaultable extra step) expressed in each surface's native idiom.

## Prior Art

### Pharo / Squeak Smalltalk

Method/class removal in Pharo (`Behavior>>#removeSelector:`, `SystemDictionary>>#removeClassNamed:`) is blunt — no automatic confirmation gate, matching ADR 0112's own choice not to block `removeSelector:` on dangling senders. Pharo's `.changes` file is the canonical durable-log reference: every method edit appends a chunk to the changes file before the image even commits the change, which is what makes "save in place" tolerable elsewhere in Pharo's model.

**Adopted:** nothing new beyond what ADR 0082/0112 already adopted from Pharo's `.changes` model.
**Rejected:** Pharo's removal bluntness *for class removal specifically* — this ADR gates `remove-class`'s disk step behind `confirmDestructive`, where Pharo's `removeClassNamed:` just does it. The difference is disk-vs-memory: Pharo's removal is memory-only the same instant it happens (an image save is a separate, much coarser gesture); ours makes an irrecoverable filesystem change unless gated.

### Git — `rm` as a distinct staged verb from `add`/`commit`

Git's index model treats a delete as a first-class staged operation (`git rm`) distinct from an ordinary content edit (`git add` after editing in place) — both still require a subsequent `git commit` to become durable, and both are trivially recoverable pre-commit (`git checkout`) and recoverable-with-effort post-commit (`git revert`, reflog). The "distinct verb, one commit step" shape is close to the design here: Tier 1 (edit) and Tier 2 (destroy) are different *kinds* of pending change, but both still funnel through one `flush`/`commit` gesture.

**Adopted:** distinguishing destructive changes as their own category within one staged-change model, rather than either (a) making every change equally "scary" (git doesn't require extra confirmation for `git commit` just because an `rm` is staged) or (b) making destructive changes an entirely separate workflow with their own commit step. `confirmDestructive` is closer to `git commit --no-verify`-style explicit intent than to a second commit command.
**Rejected:** git's actual UX for the *default* case — `git rm` requires no extra confirmation flag at all; the safety net is entirely "it's just a commit, and commits are cheap to undo." This ADR does not rely on git-recoverability alone for the *flush-time* gate (see Steelman — "isn't git enough?") because MCP agents and REPL scripts don't necessarily commit between every flush, and because a destructive flush can happen against files that were never committed in the first place.

### LSP — `workspace/applyEdit` with `DeleteFile`

The LSP spec's `WorkspaceEdit.documentChanges` array supports typed resource operations — `CreateFile`, `RenameFile`, `DeleteFile` — alongside ordinary `TextDocumentEdit`s, specifically so a server-initiated change can tell the client "this isn't a content edit, this is a file-system operation" and let the client apply its own UX for that distinction. This is the mechanism ADR 0082 left unused — flush today emits one edit-shaped event per file regardless of what actually happened to it.

**Adopted:** flush must emit `DeleteFile` for `remove-class`. `Workspace newClass:at:` flush should *also* switch from the generic `Change` shape to `CreateFile` — a pre-existing gap this ADR's LSP work closes as a side effect, since the typed-operation machinery has to exist anyway for `DeleteFile`.
**Not adapted, nothing to depart from:** the spec's shape maps directly for a single-file delete; there is no Beamtalk-specific wrinkle here beyond needing to build it (nothing existed before).

### Erlang / Elixir

Erlang/Elixir modules are all-or-nothing load/purge (`code:purge/1` unloads a whole module; there is no "delete part of a module"). This is expected: Beamtalk's method-level granularity is a language-level abstraction the class gen_server provides *on top of* whole-module BEAM loading, not something the BEAM itself offers. No new prior art to adopt or reject beyond what ADR 0082/0112 already recorded.

## User Impact

### Newcomer (from VSCode / Python / JS)

- "Delete File" already exists as a concept in every editor a newcomer has used — VSCode's own file-delete asks "Move to Trash?". `confirmDestructive`'s LSP-surfaced modal matches that expectation directly; nothing new to learn.
- The two-gesture shape (memory-mutate now, disk-confirm separately) is more surprising than VSCode's single-gesture delete — mitigated by making the *first* gesture ("Remove Class" in the browser) still work as expected for the in-session experience (the class is gone from the running app immediately), and only the *disk* consequence needs the second click.
- Discoverability: `Counter respondsTo: #removeFromSystem` and ordinary tab-completion surface it the same way any other class-protocol method is discovered. Before confirming a destructive flush, `Workspace changes` (pre-flush, ADR 0082) already lists every pending entry including the new `remove-class` kind, so a newcomer can inspect exactly what `flushIncludingDestructive` is about to do before running it.

### Smalltalk developer

- The two-phase memory/disk split for something as immediate-feeling as "remove this class" is the one place this ADR asks a Smalltalk developer to hold two mental models at once (their removal already took effect; the file hasn't caught up yet) — same tension ADR 0082 already introduced for ordinary patches, not a new one.
- `Workspace changes` browser maps directly onto Pharo's ChangeLog browser, now showing `remove-class`/`remove-method` entries alongside patches.

### Erlang/BEAM developer

- Nothing here introduces a new BEAM-level mechanism: class removal is `code:purge` + registry cleanup (already shipped, BT-785); this ADR only adds ChangeLog logging and a staged-delete flush step on top.
- A production release node never sees any of this — same "no workspace, no ChangeLog, no flush" guarantee ADR 0082/0112 already give; this ADR adds no new release-build code path.

### Production operator

- The `remove-class` audit gap this ADR closes (today, `removeFromSystem` leaves *no record* it happened) is itself an operator-relevant fix independent of the flush design — "was a class removed from this running node, by whom, when" currently has no answer at all.
- `confirmDestructive` being a call-site argument rather than a workspace setting means an operator auditing a production incident can see, in the ChangeLog's `author`/`author_kind` metadata plus the fact that the entry *did* flush, that someone deliberately chose to delete a file — not that a background setting silently permitted it.

### Tooling developer (LSP/MCP/browser)

- The `DeleteFile`/`CreateFile` LSP work is the first time this codebase's flush path uses the spec's typed resource operations instead of a generic edit — a reusable capability for any future refactor.
- MCP's `remove_class` tool plus a `confirm_destructive` argument on `flush` extends the existing tool surface with no new dispatch mechanism — same "typed wrapper over `evaluate`" shape every prior MCP tool already uses.

## Steelman Analysis

### Confirm-by-default (`confirmDestructive` required) vs. unconfirmed (destructive entries flush like anything else)

- 🧑‍💻 **Newcomer:** "Requiring a special selector for delete is inconsistent with how `Workspace flush` already works for everything else — I already learned that flush just... flushes. A second, different-shaped flush call for one entry kind is a second thing to learn."
- 🎩 **Smalltalk purist:** "Smalltalk's whole ethos is that the running system doesn't second-guess you — `removeFromSystem` already doesn't ask twice, and ADR 0112 explicitly rejected sealing-based refusals for `removeSelector:` in favour of installing unconditionally. A confirmation gate at flush time reintroduces exactly the kind of paternalism ADR 0112 argued against for the in-memory step — just moved one layer over."
- ⚙️ **BEAM veteran:** "An extra required selector/argument is one more thing every caller — including scripted/automated flush calls in CI or a deploy hook — has to remember to use, and 'forgot to switch to the destructive call' bugs are exactly as real as 'forgot to check' bugs."
- 🏭 **Operator:** "git already recovers from a bad delete as long as it was committed. A confirmation gate that only protects uncommitted work is protecting the wrong thing — teach people to commit often, don't build a second safety net for the same problem git already solves."
- 🎨 **Language designer:** "Fewer branches in the flush state machine is a real virtue. `confirmDestructive` is a special case the design otherwise doesn't need — every other ADR 0082/0112 decision found a way to unify destructive and non-destructive handling under one rule (flushability) rather than adding a second gate."
- **Why confirm-by-default wins anyway:** every argument above is real, and the decisive rebuttal is the same one Amendment 1 already established for a different axis — **git recoverability is a human, cockpit-surface property, not a universal one.** MCP agents do not necessarily commit between flushes (ADR 0082 Amendment 1 explicitly puts MCP in the *pre-flush*, ChangeLog-first layer specifically because agents batch and iterate before crossing the flush seam at all); a script or CI hook calling `Workspace flush` today, unmodified, would silently start deleting files the moment this ADR ships the new entry kind, with no code change to that caller signalling the new risk. `confirmDestructive` being a required, no-default gate is the mechanism that makes *upgrading to this ADR* safe for every existing caller — the alternative (destructive entries flush like anything else) is a breaking, silent behaviour change to every unmodified `Workspace flush` call in the ecosystem, not a neutral simplification. ADR 0112's "flushability, not refusal" precedent this argument leans on was about the *in-memory* step, which is exactly as unconfirmed here as it was there (`removeFromSystem` installs immediately, no gate) — the gate this ADR adds is purely at the disk-write step, a boundary ADR 0112 never actually addressed (it explicitly deferred flush entirely).

### Tombstone-first (leave a `.bt.deleted` marker on disk) vs. immediate-delete (git is the only durable record)

- 🧑‍💻 **Newcomer:** "A visible tombstone file means I can see what got deleted just by looking at the directory listing — I don't need to know git archaeology (`git log --diff-filter=D`) to find it."
- 🎩 **Smalltalk purist:** "Pharo's `.changes` file is, functionally, exactly this — a durable, append-only record that survives independent of the image being saved. A tombstone is the natural Smalltalk-native answer, and this codebase already has the `changes/` subdirectory as precedent for 'durable state that isn't the source tree itself.'"
- ⚙️ **BEAM veteran:** "A tombstone is trivial to implement (write one more file) and gives crash-safety for free — if the process dies between delete and ChangeLog-prune, the tombstone is still sitting there as evidence, where a bare `unlink` leaves nothing."
- 🏭 **Operator:** "Tombstones are greppable/`find`-able without needing the workspace running at all — useful for an operator doing forensic cleanup on a node that's already down."
- 🎨 **Language designer:** "A tombstone makes the on-disk state self-describing: `git status`/`ls` alone tells the whole story, no need to cross-reference a separate ChangeLog file to understand why a `.bt` file vanished."
- **Why immediate-delete (no persistent tombstone) wins:** the decisive problem is exactly the one ADR 0082's Alternative F (Shadow-file overlay) was rejected for — **a `.bt.deleted` marker is a second source of truth about what exists**, and this codebase already has two durable records of "this class was removed and here's its prior body": git (once committed) and the ChangeLog's own `changes/sources/` archive (ADR 0082's rotation policy already keeps pruned/archived source bodies around, independent of whether the live file still exists). A third record — a tombstone file sitting in the actual source tree — creates exactly the ambiguity ADR 0082 fought hard to avoid for shadow-overlay patches: does `ls src/` show the tombstone as "this file still kind of exists," does `bt fmt`/the compiler/LSP have to learn to skip `.bt.deleted` files, does a fresh `git clone` (which never ran the delete, only sees the committed result) end up with orphaned tombstones nothing ever cleans up? The staged-rename-then-unlink mechanism in *Delete atomicity* above gives the crash-safety win the BEAM veteran wants (a crash mid-delete leaves a recoverable `.tmp-delete-*` file, not a silent loss) **without** leaving anything behind after a *successful* delete — crash-safety and permanence are separable, and this design takes the first without the second. The newcomer/operator "just look at the filesystem" argument is answered by `Workspace changes` (pre-flush) and `git log`/`git show` (post-flush) each being the right tool for their respective half of the timeline, matching Amendment 1's existing division of labour rather than inventing a third view that duplicates both.

### Separate command per destructive kind (e.g., `Workspace flushRemovals`) vs. one unified `confirmDestructive` mechanism

- 🧑‍💻 **Newcomer:** "A named command is self-documenting — `flushRemovals` tells me exactly what it's going to do without needing to know what's pending first."
- 🎩 **Smalltalk purist:** "Pharo's own tooling genuinely does have distinct entry points per kind of operation — mirroring that distinction here would be staying closer to established Smalltalk convention."
- ⚙️ **BEAM veteran:** "A separate command is easier to reason about independently and easier to add telemetry/rate-limiting to if this destructive category turns out riskier in practice than expected."
- 🏭 **Operator:** "A named command in an audit log (`flushRemovals` called) is more legible at a glance than `flushIncludingDestructive` plus having to cross-reference which entries were pending to know what actually happened."
- 🎨 **Language designer:** "A generic `confirmDestructive` mechanism is smaller today, with exactly one kind behind it — but it is future-proofed for BT-3204 (rename) to slot into the same gate later without a new mechanism, which a single-purpose `flushRemovals` name would not naturally extend to."
- **Why unified wins:** `flushKinds:` already exists (ADR 0082) as the general filter-by-kind mechanism, and it already composes with a scope (`aClass`) — adding `confirmDestructive` as one more filter dimension on the *same* mechanism (`Workspace changes flushKinds: #{#'remove-class'} confirmDestructive: true`) gets the operator's "legible, specific" want for free, without a second command family to keep in sync with the first as new destructive kinds are added later (exactly the kind of duplicate-vocabulary risk CLAUDE.md's no-duplicate-implementations rule flags). The purist's Pharo-precedent argument describes Pharo's *in-memory execute* step (`removeFromSystem`/`removeSelector:` already are distinct, named primitives) — the disagreement here is only about the *flush* step, which ADR 0082 already unified across every existing kind, and there is no new argument for un-unifying it just for this one.

### Tension points

- **Newcomer/BEAM-veteran "simplicity" vs. operator/agent "safety" on `confirmDestructive`:** the strongest real tension. Resolved the same direction ADR 0082 resolved patch-vs-write-through — safety wins for the *default*, and the cost is paid once per destructive flush call, not per ordinary edit. Unlike Alternative B in ADR 0082 (rejected write-through), this is not asking every caller to pay the cost on every operation — Tier 1 stays exactly as frictionless as it is today.
- **"Git is enough" vs. "a workspace-native safety gate is still needed":** the whole `confirmDestructive` design rests on this tension resolving toward the second position, and it does so specifically because of Amendment 1's agent/human split, not because git-recoverability is weak in general — for the *cockpit* audience alone, the BEAM-veteran/operator argument ("git already handles this") would be much stronger, and a future revision could reasonably relax `confirmDestructive` to an autoflush-tracked default for the cockpit surface specifically once real usage data exists (mirroring how Amendment 1 itself was a usage-driven refinement of the original ADR 0082 design, not a day-one decision).

## Alternatives Considered

### Alternative: do nothing — leave destructive-flush UX undesigned

There is no workaround for `remove-class` today at all — `removeFromSystem` has no flush story and none is emerging on its own. `remove-method` technically "does nothing" today by construction — `Workspace flush` silently excludes those entries — but that is not a stable resting state, it is an already-shipped bug surface (*Current State*): every `removeSelector:` call already produces a ChangeLog entry that can never be satisfied, growing without bound in a long-lived workspace. Rejected: ADR 0082 explicitly named this ADR's flows as deferred-not-abandoned work, BT-2192 exists specifically to make that deferral good, and "do nothing" does not avoid a design decision here — it leaves an already-committed-to gap unfilled and lets the `remove-method` bug compound.

### Alternative: no confirmation tier — destructive entries flush like any other durable+flushable entry

See Steelman above. Rejected as a silent, breaking behaviour change for every existing unmodified `Workspace flush` caller once the `remove-class` kind starts appearing in a workspace's ChangeLog.

### Alternative: persistent on-disk tombstone (`<file>.bt.deleted`) instead of (or in addition to) `confirmDestructive`

See Steelman above. Rejected as a third, redundant source of truth about what currently exists, echoing ADR 0082's Alternative F (Shadow-file overlay) rejection reasoning. The staged-rename-then-unlink step already gives equivalent crash-safety without the permanence.

### Alternative: separate `flushRemovals` command instead of `flush: confirmDestructive:`

See Steelman above. Rejected in favour of extending the existing `flushKinds:` filter mechanism, avoiding a second, parallel vocabulary for "which entries to apply" — and leaving room for BT-3204 to extend the same `confirmDestructive` gate to rename kinds later without inventing a new mechanism.

## Consequences

### Positive

- Closes a real, already-observable gap: `Workspace changes` today either contains `remove-method` entries that can never flush (permanently stuck) or is silently missing `remove-class` entries entirely (`removeFromSystem` doesn't log). Both are fixed.
- Reuses, rather than reinvents, ADR 0082's two-phase flush protocol (extended only at the staging step, not restructured) and ADR 0112's `prev_source_ref`-backed revert precedent.
- `confirmDestructive` closes a real, silent-upgrade-risk gap for every unmodified `Workspace flush` caller, without adding any friction to the Tier-1 path this ADR leaves untouched.
- Establishes the `confirmDestructive` tiering mechanism BT-3204 (rename) can extend directly rather than redesign — the split was chosen specifically so this small, low-risk half doesn't wait on rename's harder open questions.

### Negative

- **`confirmDestructive` is one more thing every caller across four surfaces has to learn**, and — as the Steelman concedes — is a real, if justified, complication relative to ADR 0082's existing single-mode `flush`.
- A `remove-class` entry is single-file, so this ADR's atomicity story is simpler than a true multi-file destructive operation would need — this is a genuine scope limitation, not a design choice that generalizes for free; BT-3204's rename ADR will need real multi-file atomicity design of its own; this ADR's Phase A/B extension cannot be assumed to cover it by analogy.

### Neutral

- `removeFromSystem`'s stdlib/dependency/subclass refusals are unchanged — this ADR only adds logging to an already-decided operation, not new policy.
- `Workspace flush`'s existing single-argument form remains the common case and is textually unchanged for every caller that never touches a `remove-class` entry — this ADR is purely additive to the flush surface for those callers.

### DDD Model Impact

- **Workspace context** owns the new `beamtalk_workspace_flush` Tier-2 staging logic (rename-to-tmp-then-unlink for delete) and the `confirmDestructive` filter dimension on `flush`/`flushKinds:` — same module boundaries ADR 0082 already established, extended rather than duplicated.
- **Runtime context** gains one ChangeLog-append call inside `classRemoveFromSystemByName/1` (`beamtalk_behaviour_intrinsics.erl`) — no new primitive.
- **Language Service context** gains the `DeleteFile`/`CreateFile` typed `workspace/applyEdit` resource operations — unlike ADR 0082, which needed no Language Service change because it only ever consumed the *existing* generic `applyEdit` bridge, this ADR extends that bridge itself, so it is a genuine (if narrow) Language Service context change.

## Implementation

*(For downstream implementation work — this ADR does not implement any of the below.)*

### Affected components

| Layer | Change |
|---|---|
| `runtime/apps/beamtalk_runtime/src/beamtalk_behaviour_intrinsics.erl` | `classRemoveFromSystemByName/1` gains a ChangeLog-append call at its existing success point (`publish_class_removed/2` call site), capturing the class's full current source as `prev_source_ref` *before* the removal proceeds (mirrors the existing "read+parse before mutate" ordering `compile:source:`'s patch hook already uses). |
| `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_changelog.erl` | New `kind: 'remove-class'` on the entry record, extending the existing open-schema pattern — no new fields beyond what `new-class`/`remove-method` already established. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_flush.erl` | Remove the current unconditional `exclude_remove_method/1` filtering and replace with tier classification (`entry_tier(Entry) -> tier1 \| tier2`); `do_flush/1` gains a `confirmDestructive` parameter threaded through `filter_entries/2`; single-file staged-delete added per *Delete atomicity* above — everything else (grouping, shadow-duplicate detection, per-file status reporting) is reused unmodified. |
| `stdlib/src/Workspace.bt` | New unscoped `flushIncludingDestructive` and two-keyword `flush:confirmDestructive:` variant on the existing `flush`/`flush:` facade methods; `flushKinds:confirmDestructive:` on `ChangeLog`. |
| `crates/beamtalk-lsp/src/runtime.rs` | `FlushEvent` needs to carry per-file *operation* (`Change`/`Create`/`Delete`), not just a flat path list — a breaking change to the existing struct's shape, needed regardless of this ADR, since `new-class` flush already deserves `CreateFile` and doesn't get it today. |
| `crates/beamtalk-lsp/src/server.rs` | `workspace/executeCommand: flush` gains a `confirmDestructive` argument; emits `DeleteFile`/`CreateFile` resource operations per the extended `FlushEvent`. |
| `crates/beamtalk-mcp/src/server.rs` | New tool: `remove_class` (wraps `removeFromSystem`); `flush` tool gains a required-when-applicable `confirm_destructive` boolean. |
| `crates/beamtalk-cli/src/commands/repl/mod.rs` | New meta-command `:remove-class`, with the two-prompt shape from *Surface* above; new `:flush-destructive` / `:flush-destructive <Class>` pair alongside the existing `:flush` / `:flush <Class>` pair. |
| `runtime/apps/beamtalk_workspace/priv/static/workspace.js` | "Remove Class" browser action with the distinct destructive-dirty-indicator affordance from *Surface* above. |
| `docs/development/surface-parity.md` | One new expression-backed row; a note on `confirmDestructive` as a parity-preserving argument (see *Surface*). |
| `docs/beamtalk-language-features.md` | Document `Workspace flushIncludingDestructive` alongside the existing `removeFromSystem`/`removeSelector:` sections. |

### Phased rollout

| Phase | Scope | Effort | Tests |
|---|---|---|---|
| **1** | `removeFromSystem` ChangeLog-logging fix (`kind: "remove-class"`) — the smallest, most independently-shippable piece, and the one closing an audit gap that exists today with zero new UX to design. | S | EUnit: entry appears with correct `flushable`/`prev_source_ref`; BUnit: `Workspace changes` shows it. |
| **2** | `Workspace flush` tiering: classify `remove-method`/`remove-class` as Tier 1/Tier 2 respectively, implement `flushIncludingDestructive` + `confirmDestructive:` filter dimension, implement single-file class-removal staged-delete (rename-to-tmp, unlink). Unblocks the already-stuck `remove-method` entries as a side effect (Tier 1, no new gate needed for them). | M | EUnit: staged-delete crash-safety (kill between rename and unlink); BUnit: `Workspace flush` reports `skipped: destructive`, `flushIncludingDestructive` applies it. |
| **3** | `revert:` extensions for both kinds (Undo story table above). | S | BUnit: revert of each kind, pre-flush and (where applicable) documented as unsupported post-flush. |
| **4** | LSP `DeleteFile`/`CreateFile` typed resource operations (`FlushEvent` restructuring); MCP tool; REPL meta-commands; browser action. Surface-parity audit. | M | LSP command tests; MCP integration tests; browser e2e for the two-gesture destructive flow; surface-parity drift check passes. |

Total: ~S-M across 4 phases — no phase carries the kind of unvalidated, load-bearing risk BT-3204's rename design does, because every mechanism here (single-file byte-span splice, single-file staged delete, `prev_source_ref`-backed revert) is a direct reuse of infrastructure ADR 0082/0112 already shipped and proved.

## Migration Path

No user code changes required — every new surface (`flushIncludingDestructive`, `flush:confirmDestructive:`) is additive, and `removeFromSystem`'s call signature is unchanged (it now logs, but the caller sees the same `nil` return).

**One real behaviour change for already-shipped code, worth calling out explicitly rather than leaving implicit:** ADR 0112/BT-3187 already ships `removeSelector:`, and any workspace that has been calling it has `"remove-method"` entries sitting in its ChangeLog today — *permanently* pending, since `Workspace flush` currently excludes them unconditionally (see *Current State*). Once Phase 2 of this ADR lands, the *next* ordinary `Workspace flush` call (Tier 1, no `flushIncludingDestructive` needed) will apply every such backlog entry, splicing the removed method's byte span out of its source file for real, for the first time. This is the intended fix, not a side effect to guard against — the entries were already durable, already flushable, and already represented a user's explicit `removeSelector:` call; they were simply stuck. But it does mean a long-running workspace upgraded to a build containing this ADR's Phase 2 can see disk writes on its *next* flush that a workspace administrator did not request *at flush time* — the request was made earlier, when `removeSelector:` was originally called. Operators with long-lived workspaces should be aware that upgrading and then running `Workspace flush` may write more than the immediately-preceding session's edits. No opt-out is provided (splicing dead text out of a still-existing file is Tier 1 by this ADR's own classification, see *Why a confirmation tier at all*) — an operator who wants to inspect the backlog first can do so pre-upgrade via `Workspace changes select: [:e | e kind = #'remove-method']`.

**Scope split from the original bundled draft.** This ADR originally also designed `renameTo:`/`renameSelector:to:` (class/method rename). During review, rename turned out to carry real, unresolved design risk unrelated to deletion — an xref index gap for live-patched code (`beamtalk_xref:build_method_entry/5` hard-codes `references => []` for any live-patched method), a correctness problem in naive sender-rewriting (`sendersOf:` is selector-name-keyed with no receiver-type narrowing, so blindly rewriting every "sender" would corrupt unrelated code sharing a selector name), and an undesigned in-memory cross-gen-server atomicity question. None of these affect deletion. Rather than hold the small, safe half hostage to the harder open questions, rename was split out to BT-3204 as its own follow-up ADR, and this ADR was narrowed to file deletion only. No functionality is lost by the split — nothing in the original draft's deletion design changed, only rename-specific content moved out.

For ADR 0046 (VSCode sidebar): no migration — it consumes `workspace/applyEdit`, which continues to fire per touched file; the `DeleteFile`/`CreateFile` typed-operation upgrade is additive precision, not a contract change existing consumers depend on differently.

## References
- Related issues: BT-2192 (this ADR), BT-2191 / BT-3183 (ADR 0112 — method-level removal, the direct predecessor this ADR extends into flush), BT-3187 (added `remove-method`'s `side` field and the flush shadow-key fix), BT-785 (`removeFromSystem`, gaining ChangeLog logging here), BT-3105 (single class-removal teardown path reused unmodified), BT-2664 (ADR 0082's `revert:` mechanism for `new-class`, generalized here for `remove-class`), BT-3204 (the split-out follow-up ADR for class/method rename)
- Related ADRs: ADR 0082 (Method-Level Edit and Save — the flush/ChangeLog/two-phase-atomicity/Amendment-1 foundation this entire ADR builds on), ADR 0112 (Method-Level Removal Language Primitive — the direct predecessor whose `remove-method` kind this ADR finally unblocks)
- Documentation: `docs/beamtalk-language-features.md`, `docs/development/surface-parity.md`
- LSP spec: `workspace/applyEdit` with `CreateFile`/`DeleteFile` operations, <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_applyEdit>
