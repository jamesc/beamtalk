%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_flush).

%%% **DDD Context:** Workspace Context

-moduledoc """
`Workspace flush` and `Workspace flush:` implementation (ADR 0082 Phase 2;
destructive tiering per ADR 0113 Phase 2).

Writes pending ChangeLog entries to disk via trivia-preserving byte-span splice
(no AST reprint), atomically (`<file>.tmp` → atomic rename), with external-edit
conflict detection and post-write pruning of the affected entries from the
active view.

## Tiers (ADR 0113)

Every flushable entry classifies into one of two tiers (`entry_tier/1`):

  - **Tier 1** — edits a still-existing file: patches (`instance`/`class`),
    `'new-class'`, and `'remove-method'` (excising a recorded span leaves the
    file in place, mechanically identical to a patch). Applied by ordinary
    `flush/0` / `flush/1` / `flush_kinds/1` with no gate.
  - **Tier 2** — destroys a file: `'remove-class'`; moves one: `'rename-class'`
    (ADR 0114); or rewrites a method's confirmed call sites across
    files in place, no file moved: `'rename-method'` (ADR 0114).
    Only applied when the caller passes `ConfirmDestructive = true`
    (`flush/2`, `flush_kinds/2`) or calls the unscoped
    `flush_including_destructive/0`. Never silently reached — no workspace
    setting or environment variable can imply it. A pending Tier 2 entry left
    out of the applied set is reported in the summary's `skipped` field with
    `reason => <<"destructive">>`.

## Selection

`flush/0` picks every ChangeEntry that is durable, flushable, active (current
epoch, not orphaned), and not already flushed. `flush/1` adds a filter:

  - a class name (atom or binary, e.g. `Counter`) selects entries for that class
  - a selector Symbol (e.g. `#'new-class'`) selects entries of that kind
  - a Beamtalk Dictionary `#{ #file => "..." }` (Symbol-keyed) selects entries
    with that `sourceFile`

Multiple ChangeEntries against the *same* `(class, selector)` are shadowed: only
the most recent (highest `seq`) is applied to disk. The shadowed entries are
also marked flushed so the audit log shows them as resolved.

## Splice

For each flushable method entry: read the on-disk file, verify the bytes at
the recorded `span` are byte-identical to the recorded `prev_source` (external-
edit detection), then replace those bytes with the patched body. For a single
file with multiple entries, splices are applied in *descending* span order so
each splice leaves earlier byte offsets untouched.

For `kind: #'new-class'` entries: the `targetPath` must not exist on disk; the
full source is written.

## Atomicity (single-file)

Per file: write to `<file>.tmp` then `file:rename/2` to `<file>`. `rename/2` is
atomic on POSIX, so a crash leaves either the old or the new file on disk —
never partial. The ChangeEntries for that file are marked flushed only *after*
the rename returns ok.

`<file>.tmp` throughout this doc is shorthand: the real staging name is
unique per write, `<file>.bt-staging.<unique>.tmp` (`beamtalk_workspace_file:write_temp/2`,
BT-4683). Flush runs in its caller's process, so two flushes (an autoflush and
a manual `Workspace flush`, say) can stage the same file at once; a shared
fixed name would let one rename the other's half-written body into place. A
crash between Phase A and Phase B can therefore leave a stray
`<file>.bt-staging.<unique>.tmp` behind instead of one that the next attempt overwrites.
It is never read back. The next successful commit to `<file>` deletes any such
leftover more than an hour old (`beamtalk_workspace_file:sweep_stale_temps/2`);
a younger one may belong to a flush still in flight, so it is left alone. The
sweep matches only the `.bt-staging.` marker, so it never deletes another
tool's `<file>.<digits>.tmp` from the user's source tree.

## Atomicity (multi-file)

Two-phase per flush operation (ADR 0082, *Multi-file atomicity*):

  - **Phase A**: for every target file, read disk, validate span + prev_source,
    compute the new body, write the `<file>.tmp`. If *any* file fails Phase A,
    abort: clean up every `<file>.tmp` written so far, leave the live log
    untouched, and return `{ok, Summary}` with the per-file failures reported
    in the `conflicts` field of the summary (so the REPL can render them
    without an exception). Hard runtime errors (e.g. I/O write failure on
    `<file>.tmp`) still come back as `{error, #beamtalk_error{}}`. **No rename
    happened.**
  - **Phase B**: rename each `<file>.tmp` → `<file>` in sequence. POSIX
    guarantees the rename is atomic but the OS may still surface I/O errors;
    a Phase B failure leaves a mixed state where successfully-renamed files
    have their entries pruned and reported as completed, while failed files
    retain their entries for retry. The user gets a per-file status report.

This is the strongest atomicity achievable without a filesystem transaction.
It ensures the failure mode is *recoverable via re-flush*, never silent data
loss.

## Atomicity (class removal — ADR 0113)

A `'remove-class'` entry targets exactly one file, so it needs none of the
multi-file sequencing above — a single staged rename, then a single unlink,
extending the same Phase A (stage) / Phase B (commit) shape patches already
use:

  - **Phase A**: same-filesystem rename `<file>` → `<file>.tmp-delete-<epoch>-
    <seq>` (POSIX-atomic; `epoch`/`seq` are the entry's own identity, so a
    later flush attempt can recognise its own staged file). A crash here
    before the rename returns leaves the original file untouched (nothing to
    clean up); a crash *after* leaves the staged `.tmp-delete-*` file, which
    the next flush attempt recognises and resumes at Phase B.
  - **Phase B**: `file:delete/1` the staged `.tmp-delete-<epoch>-<seq>` file.
    A crash between Phase A and Phase B leaves a recoverable `.tmp-delete-*`
    file on disk (nothing lost) — a re-flush finishes the unlink.

**Disambiguating "already deleted externally" from "my own prior attempt
already staged this."** Phase A's `stat <file>` can fail with `enoent` for two
different reasons: (a) *this* entry's own earlier flush attempt already
completed the rename and crashed before the unlink — recognised by finding
`<file>.tmp-delete-<epoch>-<seq>` under this entry's own `epoch`/`seq`, in
which case Phase A resumes at the already-staged file and Phase B finishes
the unlink normally; or (b) something else deleted `<file>` externally, with
no matching staged file — surfaced as a soft success (`already gone — nothing
to remove`, ADR 0113's external-edit-conflict table), and the entry is pruned
without any further disk I/O (`op = noop` in `prepare_remove_class/3`).

An *aborted* flush (a Phase A conflict on some other file in the same batch)
undoes only the rename *this* attempt performed — renaming the staged file
back to `<file>` — never the unlink, and never a stage left behind by a
different (earlier, crashed) attempt, which stays exactly as found for a
future flush to resume.

## Atomicity (class rename — ADR 0114)

A `'rename-class'` entry is the first Tier-2 kind that genuinely spans more
than one file: `sites[0]` is the class's own declaration line (in the file
being moved, `old_path` -> `new_path`); `sites[1..]` are every other
in-project file with a rewritten reference (`entry_old_path/1`,
`entry_new_path/1`, `entry_sites/1`, `beamtalk_workspace_changelog:site()`).
It is only ever flushable when `entry_flushable/1` is true, which the
schema guarantees means `old_path`/`new_path` are real paths and every site
resolves to a flushable file (a dynamic class's rename is `flushable: false`
and never reaches this code, via `flushable_pending/0`) — so this module
never has to special-case a `sites[0] = null` dynamic-class shape.

| Phase A (stage) | Phase B (commit) |
|---|---|
| Read `old_path`, splice the declaration span (and any *other* site whose own file happens to equal `old_path` — a class that references its own name inside one of its own methods, folded into the SAME write since the file is moving as a unit), write `<new_path>.tmp`; write `<file>.tmp` per *other* site file, each an ordinary splice | Rename `<new_path>.tmp` → `<new_path>`, `unlink <old_path>`, then rename each remaining site `<file>.tmp` → `<file>` |

Phase A never touches `old_path` itself — only reads it — so a crash
anywhere during Phase A leaves the on-disk tree exactly as flush found it
(mirrors ordinary patch/new-class Phase A, not `remove-class`'s stage-rename,
which mutates `old_path` immediately). This is deliberately simpler than
`remove-class`'s epoch/seq-keyed staged-file disambiguation: a rename's own
"did a prior attempt already finish this" signal is `new_path` itself, not a
separate marker file.

**Crash-recovery design decision.** Three states `old_path`/`new_path` can be
in when Phase A runs, and what each means:

  - **`old_path` exists** (the ordinary case, including a retry after ANY
    earlier partial attempt that got no further than writing `.tmp` files):
    read `old_path`, resolve every site's splice against it (see the
    idempotent check below). If `new_path` does not exist yet, write
    `<new_path>.tmp` — the ordinary case. If `new_path` ALSO already exists
    (review round 4 on PR #3526 — `old_path` still being present here means
    THIS attempt's own Phase B has not yet renamed `<new_path>.tmp` into
    place, so a pre-existing `new_path` can only be an unrelated file that
    happens to occupy the derived target, or — the one legitimate case — an
    EARLIER attempt's Phase B rename that already succeeded but whose
    subsequent unlink of `old_path` then failed, per `commit/1`'s `op =
    move` doc), `check_rename_target_safe/3` compares `new_path`'s current
    bytes against the FULL post-splice body Phase A just computed — an exact
    match means this is that legitimate resume (harmless to overwrite with
    the same bytes, or skip), anything else is a hard, unresolvable conflict
    (`reason => <<"rename_target_exists">>`), mirroring `prepare_new_class/3`'s
    `target_exists` guard for the identical reason. `old_path` is never
    mutated by Phase A, only read, so a legitimate retry is always safe and
    idempotent either way.
  - **`old_path` gone, `new_path` present**: since Phase A never touches
    `old_path` and Phase B's own ordering renames `<new_path>.tmp` into place
    *before* unlinking `old_path`, the ONLY way `old_path` can be gone is a
    Phase B commit that already completed the move (in this attempt or an
    earlier, crashed one whose `mark_flushed` never landed — the same
    `flush_marker_failed` shape ordinary patches already document). Phase A
    checks whether `new_path`'s current bytes already carry every move-group
    site's recorded new text at its expected offset
    (`all_units_already_applied/2`); if so, the move is treated as already
    done (`op = move_noop`, no I/O) rather than an error — this is what lets
    "re-issuing the same destructive flush call retries only what's left"
    hold for the multi-file case, since the class's OTHER sites may still be
    unwritten. If the bytes do not match, this is a genuine, unresolvable
    conflict (`reason => <<"rename_target_mismatch">>`) — something else put
    unrelated content at `new_path`.
  - **Both gone**: nothing to recover from — `reason =>
    <<"source_missing">>`, a hard conflict rather than a soft success (unlike
    `remove-class`'s "already gone" case, a rename's desired end state is
    "the class exists at `new_path`", which does not hold here and cannot be
    reconstructed).

**Idempotent per-site splice check.** Every site's splice (the declaration
line and every other reference) uses a three-way check, not the ordinary
two-way external-edit check: the recorded span either still holds the prior
text (apply normally), already holds the new text (a previously-committed
site from a partially-failed attempt — treated as a no-op, not a conflict),
or neither (a genuine external edit). Without the middle case, re-flushing
after a partial Phase B failure would see an already-correctly-rewritten
site's `prev_source` permanently mismatch and report it as `external_edit`
forever, defeating the "retry only what's left" guarantee.

**Per-entry, not per-file, flushed marking.** A `'rename-class'` entry is
only marked flushed once *every* file it touches (the move's own target
plus every other rewritten site file) has committed in the SAME Phase B
pass — `multi_site_entry_fully_committed/3` (shared with
`'rename-method'`). A Phase B failure partway through
leaves the entry pending in its entirety (even though some of its files are
now correctly on disk); the per-file `files`/`conflicts` summary still
reports exactly which files committed, and the idempotent check above means
re-flushing the same entry only re-writes the files that did not.

**A `'rename-class'` entry sharing a target file with an ordinary pending
patch, or with another rename-class entry's own target files** in the same
flush batch surfaces as a `mixed_rename_and_ordinary_edit` (respectively
`mixed_rename_and_rename_edit`) conflict, aborting the whole batch — the
same defensive posture `remove-class`'s `mixed_remove_class_and_splice`
guard already takes for the identical reason (reordering "write" and "move
away" within one Phase A/B pass is undesigned). Today only the ordinary-vs-
rename shape is reachable (`classRenameTo/2`'s own collision refusal
prevents two co-pending entries from targeting the same class name), but
`resolve_new_path/1` documents a future `new_path`-supplying producer that
would not have that same guard, and `derive_new_path/3`'s two derivation
styles could coincide for two unrelated class names — so
`check_no_rename_file_collisions/2` guards the rename-vs-rename shape too
(`rename_entry_rename_collisions/1`), rather than leaving it to the day a
`.tmp` write silently clobbers another rename's in-flight one.

**Renaming the SAME class twice before ever flushing.**
`classRenameTo/2` computes a rename entry's `old_path` from the class's
*compiled* `beamtalk_source` attribute, which is only refreshed by a flush
COMMIT (the post-commit refresh above) — so `Foo renameTo: #Bar` followed,
with no intervening flush, by `Bar renameTo: #Baz` produces two entries that
both compute `old_path = foo.bt`, genuinely sharing a file and tripping the
rename-vs-rename guard just described. This is a same-class rename CHAIN,
not two unrelated classes colliding, and chain-collapsing it into a single
effective rename was deliberately rejected (too much atomicity risk for a
UX nicety — see the design decision) in favour of keeping the existing
abort-cleanly behaviour but reporting it accurately:
`rename_entry_rename_collisions/1` detects this specific shape (one entry's
`old_class` traces back to the other's `class`) and reports
`same_class_rename_chain_needs_flush` instead of the generic
`mixed_rename_and_rename_edit`, so the message says a flush is needed
between renames of the same class rather than implying an unrelated-class
collision. This is a documented, intentional limitation, not a bug: flush
between renames of the same class.

**Post-commit source-attribute refresh.** A class's
compiled BEAM module embeds its own source path as a `beamtalk_source`
module attribute at compile time (`beamtalk_reflection:
source_file_from_module/1`; `Behaviour>>sourceFile`) — this is exactly what
`beamtalk_class_refactor:classRenameTo/2`'s *next* invocation on this
same class reads to compute its own `old_path`. Flush moves the file on
disk but never recompiled the class, so that attribute went stale the
moment the move committed: a second `renameTo:` on the same class, flushed
afterwards, would compute `old_path` from the now-deleted pre-move path and
could never resolve it (`reason => <<"source_missing">>`) even though the
file's real, current content sits at `new_path` the entire time — an
entirely ordinary "rename, flush, rename again" sequence, not a race. Once
a `move`/`move_noop` commits successfully, `maybe_reload_renamed_class_
source/1` best-effort reloads the class from `new_path`
(`beamtalk_repl_loader:reload_class_file/2` — the same tested machinery
`Counter reload`/`:reload` already uses) purely to refresh this bookkeeping
attribute for a possible future rename; the class's in-memory identity and
behaviour are already correct at this point (`classRenameTo/2`'s own job).
A reload failure (no compiler available, a bare runtime, or any
other error) is logged via `?LOG_WARNING` and never fails the
already-successful flush — see that function's own doc for the full
rationale.

## Atomicity (method rename — ADR 0114)

A `'rename-method'` entry is also genuinely multi-file (`sites[0]` is the
definition, `sites[1..]` are every *confirmed* self/super sender site —
`candidate_sites` are reported for human/agent review only and are never
staged, written, or otherwise touched by flush) but, unlike class rename, it
has no file-move component: renaming a selector never changes which file a
method's class lives in, so every one of its sites — including the
definition — is an ordinary in-place splice against the file it already sits
in. This means method rename needs none of class rename's move-specific
machinery (`old_path`/`new_path`, staged-rename crash recovery, the
`already gone` disambiguation) — it reuses the exact same generic
`{site(), OwnerEntry}` splice/grouping machinery class rename's own
*other*-site references already go through (`group_units_by_file/1`,
`prepare_rename_site_group/2`, `apply_site_units/2`), just seeded from
*every* site instead of only the non-declaration ones.

| Phase A (stage) | Phase B (commit) |
|---|---|
| For every confirmed site across every pending `'rename-method'` entry, group by `sourceFile` (sites from different entries landing in the same file merge into one splice, exactly like `group_by_file/1` already does for ordinary Tier-1 entries), read each file, splice every site's `span` against its recorded `source_ref`/`prev_source_ref`, write `<file>.tmp` | Rename each `<file>.tmp` → `<file>`, in sequence — same sequential-commit, partial-failure-is-recoverable-via-re-flush shape ordinary multi-file flush already documents above |

A Phase A failure on any one file (a confirmed site's recorded span no
longer resolves against current disk content) aborts the whole batch before
any file is renamed, cleaning up every `.tmp` already written — mirrors the
"any conflict aborts the whole flush" rule for ordinary multi-file flush and
class rename alike. Like `'rename-class'`, a `'rename-method'` entry is only
counted as fully flushed (and only then marked flushed in the ChangeLog)
once *every* file among its confirmed sites has committed in the *same*
Phase B pass (`multi_site_entry_fully_committed/3`) — a Phase B failure
partway through leaves the whole entry pending even though some of its files
already landed correctly; re-flushing retries only what didn't.

A `'rename-method'` entry sharing a target file with a pending ordinary
patch, or with a pending `'rename-class'` entry's own touched files, aborts
the whole flush (`mixed_rename_method_and_pending_edit`) — the same
defensive posture `'rename-class'`'s own collision guards take, for the
identical reason (reordering two different splice mechanisms against one
file within a single Phase A/B pass is undesigned). Two `'rename-method'`
entries sharing a file are *not* a collision: their sites simply merge into
one splice via `group_units_by_file/1`, the same way multiple ordinary
patches against one file already merge via `group_by_file/1`.

## Conflict detection

External-edit conflict: the recorded `prev_source` does not byte-match the
current contents of the recorded span in the on-disk file. Surfaced as an
entry in the `conflicts` field of the summary (`reason: "external_edit"`)
rather than as an Erlang exception, so the caller can decide whether to
re-flush after reconciling, discard via `Workspace changes clear`, or inspect
the diff. The ChangeEntries for that file remain pending.

New-class conflict: `targetPath` already exists on disk. Surfaced as
`reason: "target_exists"` in `conflicts` with the same recoverable semantics.

A whole-flush Phase A failure (any single file's preparation produced a
conflict) aborts *all* file writes: no tmp file is renamed, no entry is
marked flushed. A Phase B failure leaves a mixed state — files renamed before
the failure are flushed; the failing file and any later ones report
`reason: "rename_failed"` and stay pending for retry.

## Return value

A summary map (the value `Workspace flush` returns to the REPL):

```
#{
  '$beamtalk_class' => 'FlushResult',
  flushed => N,                         %% durable entries written
  files => [<<"path1">>, ...],          %% files touched (written or removed)
  newClasses => M,                      %% subset of `flushed` for new-class entries
  removedClasses => R,                  %% subset of `flushed` for remove-class entries
  skipped => [#{seq, class, reason}],   %% e.g. reason => <<"destructive">> (ADR 0113)
  conflicts => [#{file, reason, seqs}]  %% Phase A / Phase B conflicts
}
```

A success path returns `flushed > 0` (or `0` when there is nothing to flush),
zero conflicts, an empty `skipped` list, and `files` reflecting the touched
set. A pending `'remove-class'` entry left out of the applied set because
`ConfirmDestructive` was not given contributes an entry to `skipped` instead
(`reason => <<"destructive">>`) rather than being silently dropped.
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include("beamtalk_workspace_flush.hrl").

-export([
    flush/0,
    flush/1,
    %% ADR 0113 Phase 2: `flush/2` threads the `confirmDestructive` filter
    %% dimension through the existing `flush/1` filter; `flush_including_destructive/0`
    %% is the unscoped bare-unary form (no filter argument to attach a
    %% `confirmDestructive:` keyword to).
    flush/2,
    flush_including_destructive/0,
    flush_kinds/1,
    flush_kinds/2
]).

%% Exported for tests.
-export([
    splice/3,
    group_by_file/1,
    complete_flush/5,
    filter_shadowed_by_survivor/2,
    renamed_target_keys/1,
    announce_flush_completed/2,
    %% ADR 0112: exercises the `(class, selector, side)` shadow-key
    %% fix directly — the shadow-key logic is exercised without going
    %% through prepare_splice/2.
    shadow_duplicates/1,
    %% ADR 0113 Phase 2: tier classification, exported for direct unit
    %% coverage independent of a full flush round-trip.
    entry_tier/1,
    %% ADR 0113 Phase 2: lets a crash-recovery test construct the exact
    %% staged path a real flush attempt would have produced, without
    %% duplicating the naming format in the test module.
    delete_staging_path/3,
    %% ADR 0114 Phase 2: rename-class multi-file staging, exported
    %% for direct unit coverage independent of a full flush round-trip.
    %% (`resolve_new_path/1` lives in `beamtalk_workspace_flush_rename`.)
    is_rename_class_entry/1,
    %% ADR 0114 Phase 3: rename-method multi-file staging, exported
    %% for direct unit coverage independent of a full flush round-trip.
    is_rename_method_entry/1
]).

-type filter() ::
    any
    | {class, binary()}
    | {selector, atom()}
    | {file, binary()}
    | {kinds, [atom()], [atom()]}.

%%% ----------------------------------------------------------------------------
%%% Public API
%%% ----------------------------------------------------------------------------

-doc """
Flush all pending durable+flushable Tier 1 ChangeEntries (ADR 0082 Phase 2;
Tier 1/2 split per ADR 0113 Phase 2).

Tier 1 only — patches, `'new-class'`, and `'remove-method'`. A pending Tier 2
(`'remove-class'`) entry is left pending and reported in the summary's
`skipped` field (`reason => <<"destructive">>`); use
`flush_including_destructive/0` to also apply it.

Returns the summary map described in the module docs. Never raises on a normal
conflict — conflicts are reported in the `conflicts` field so the caller can
decide what to do (re-flush after editing, discard via `Workspace changes
clear`, inspect via `Workspace diff:`). Hard runtime errors (e.g. the changelog
server is not running) come back as `{error, #beamtalk_error{}}`.
""".
-spec flush() -> {ok, map()} | {error, #beamtalk_error{}}.
flush() ->
    do_flush(any, false).

-doc """
Flush only the Tier 1 ChangeEntries that match `Filter` (ADR 0082 Phase 2).

`Filter` is one of:

  - a Beamtalk class object (a `#beamtalk_object{}` whose class atom ends in
    `" class"`) — filter by the class's display name
  - a class name atom (`'Counter'`) or binary (`<<"Counter">>`) — filter by name
  - a selector atom, including `'new-class'` — filter by entry kind/selector
  - a Beamtalk Dictionary `#{ #file => "..." }` (Symbol-keyed) — filter by
    `sourceFile`

Anything else surfaces a structured error. Equivalent to `flush(Filter, false)`
— a matching Tier 2 entry is reported in `skipped`, not applied; use `flush/2`
with `ConfirmDestructive = true` to scope the destructive tier to `Filter`.
""".
-spec flush(term()) -> {ok, map()} | {error, #beamtalk_error{}}.
flush(Filter) ->
    flush(Filter, false).

-doc """
Flush the ChangeEntries that match `Filter`, additionally applying Tier 2
(`'remove-class'`) entries within that scope when `ConfirmDestructive` is
`true` (ADR 0113 Phase 2).

Backs `Workspace flush: aClass confirmDestructive: true` and
`Workspace flush: #{ #file => "..." } confirmDestructive: true` — the class/
kind/file argument gives `confirmDestructive:` a real keyword partner, so this
stays an ordinary two-keyword message. `ConfirmDestructive` must be a literal
Boolean — never read from a workspace setting or environment variable
(ADR 0113, "the destructive tier is never silently on").
""".
-spec flush(term(), boolean()) -> {ok, map()} | {error, #beamtalk_error{}}.
flush(Filter, ConfirmDestructive) when is_boolean(ConfirmDestructive) ->
    case normalise_filter(Filter) of
        {ok, F} -> do_flush(F, ConfirmDestructive);
        {error, _} = Err -> Err
    end;
flush(_Filter, _Other) ->
    {error,
        filter_error(<<"flush:confirmDestructive: expects a Boolean for confirmDestructive:">>)}.

-doc """
Flush every pending durable+flushable ChangeEntry, Tier 1 *and* Tier 2
(ADR 0113 Phase 2).

The unscoped destructive-flush entry point — a bare unary selector (backing
`Workspace flushIncludingDestructive`), not a keyword message, since there is
no class/kind/file argument to attach a `confirmDestructive:` keyword to once
the call has no scope. Equivalent to applying both `flush/0`'s Tier 1 set and
every pending Tier 2 (`'remove-class'`) entry in one pass.
""".
-spec flush_including_destructive() -> {ok, map()} | {error, #beamtalk_error{}}.
flush_including_destructive() ->
    do_flush(any, true).

-doc """
Flush only the Tier 1 ChangeEntries whose kind or author_kind is in `Kinds`
(ADR 0082 Phase 4).

`Kinds` is a list of Symbols (atoms). Each symbol classifies as either an
**entry kind** (`instance`, `class`, `'new-class'`) or an **author kind**
(`human`, `agent`):

  - If at least one entry-kind symbol is present, an entry's `kind` must be in
    that set.
  - If at least one author-kind symbol is present, the entry's `author_kind`
    must be in that set.
  - An entry must satisfy **all** non-empty constraint sets (entry-kind AND
    author-kind, when both are provided), so the caller can flush, e.g., "all
    agent-authored new-class entries" with `[agent, 'new-class']`.

Empty `Kinds` is rejected with a structured error (use `flush/0` to flush
everything). Unknown symbols are rejected with a structured error so a typo
fails loudly rather than silently flushing the wrong set. Equivalent to
`flush_kinds(Kinds, false)` — a matching Tier 2 entry (`kind => 'remove-class'`)
is reported in `skipped`, not applied; use `flush_kinds/2` to also apply it.

Returns the same `FlushResult` summary as `flush/0` / `flush/1`.
""".
-spec flush_kinds([atom()]) -> {ok, map()} | {error, #beamtalk_error{}}.
flush_kinds(Kinds) ->
    flush_kinds(Kinds, false).

-doc """
Flush the ChangeEntries whose kind or author_kind is in `Kinds`, additionally
applying Tier 2 (`'remove-class'`) entries within that scope when
`ConfirmDestructive` is `true` (ADR 0113 Phase 2).

Backs `Workspace changes flushKinds: aSet confirmDestructive: true` —
`confirmDestructive` composes as one more independent filter dimension on the
existing `flushKinds:` mechanism, not a special case. See `flush_kinds/1` for
the `Kinds` contract.
""".
-spec flush_kinds([atom()], boolean()) -> {ok, map()} | {error, #beamtalk_error{}}.
flush_kinds(Kinds, ConfirmDestructive) when is_list(Kinds), is_boolean(ConfirmDestructive) ->
    case classify_kinds(Kinds) of
        {ok, EntryKinds, AuthorKinds} ->
            do_flush({kinds, EntryKinds, AuthorKinds}, ConfirmDestructive);
        {error, _} = Err ->
            Err
    end;
flush_kinds(Kinds, _Other) when is_list(Kinds) ->
    {error,
        filter_error(
            <<"flushKinds:confirmDestructive: expects a Boolean for confirmDestructive:">>
        )};
flush_kinds(_Other, _ConfirmDestructive) ->
    {error,
        filter_error(
            <<"flushKinds: expects a List or Set of kind Symbols (e.g. #instance, #agent)">>
        )}.

%% Classify each symbol in `Kinds' as an entry-kind or an author-kind. Unknown
%% symbols are rejected — surface as a structured error so the caller sees the
%% typo rather than silently flushing the wrong set. Empty `Kinds' is also
%% rejected (use `flush/0' to flush everything).
-spec classify_kinds([atom()]) -> {ok, [atom()], [atom()]} | {error, #beamtalk_error{}}.
classify_kinds([]) ->
    {error,
        filter_error(<<
            "flushKinds: requires at least one kind Symbol; use Workspace flush to flush "
            "every pending durable change"
        >>)};
classify_kinds(Kinds) ->
    classify_kinds(Kinds, [], [], []).

classify_kinds([], EKs, AKs, []) ->
    {ok, lists:usort(EKs), lists:usort(AKs)};
classify_kinds([], _EKs, _AKs, Unknowns) ->
    {error, unknown_kind_error(lists:reverse(Unknowns))};
classify_kinds([K | Rest], EKs, AKs, Unknowns) when is_atom(K) ->
    case classify_kind(K) of
        entry -> classify_kinds(Rest, [K | EKs], AKs, Unknowns);
        author -> classify_kinds(Rest, EKs, [K | AKs], Unknowns);
        unknown -> classify_kinds(Rest, EKs, AKs, [K | Unknowns])
    end;
classify_kinds([Other | _], _EKs, _AKs, _Unknowns) ->
    {error,
        filter_error(
            iolist_to_binary([
                <<"flushKinds: expects Symbol elements, got: ">>,
                io_lib:format("~p", [Other])
            ])
        )}.

-spec classify_kind(atom()) -> entry | author | unknown.
classify_kind(human) ->
    author;
classify_kind(agent) ->
    author;
classify_kind(K) ->
    case lists:member(K, beamtalk_workspace_kinds:persisted()) of
        true -> entry;
        false -> unknown
    end.

-spec unknown_kind_error([atom()]) -> #beamtalk_error{}.
unknown_kind_error(Unknowns) ->
    Joined = lists:join(<<", ">>, [atom_to_binary(K, utf8) || K <- Unknowns]),
    Allowed = lists:join(
        <<", ">>, [
            beamtalk_workspace_kinds:filter_symbol(K)
         || K <- beamtalk_workspace_kinds:persisted()
        ]
    ),
    filter_error(
        iolist_to_binary([
            <<"flushKinds: unrecognised kind symbol(s): ">>,
            Joined,
            <<". Allowed: ">>,
            Allowed,
            <<" (entry kinds); #human, #agent (author kinds)">>
        ])
    ).

%%% ----------------------------------------------------------------------------
%%% Filter normalisation
%%% ----------------------------------------------------------------------------

%% The caller-supplied filter arrives in whatever shape the FFI surfaces it as.
%% Pin down the supported shapes here so the rest of the pipeline can pattern
%% match on a small closed set.
-spec normalise_filter(term()) -> {ok, filter()} | {error, #beamtalk_error{}}.
normalise_filter(#beamtalk_object{class = ClassNameAtom}) when is_atom(ClassNameAtom) ->
    %% `Counter` evaluated at the REPL is a class *object* whose internal class
    %% atom is `'Counter class'`. Strip the suffix so the filter matches the
    %% entry's `class` field (the unsuffixed display name).
    case beamtalk_class_registry:is_class_name(ClassNameAtom) of
        true ->
            {ok, {class, beamtalk_class_registry:class_display_name(ClassNameAtom)}};
        false ->
            {error,
                filter_error(
                    <<"flush: argument is an Object but not a Class; pass a class, a Symbol, or a Dictionary">>
                )}
    end;
normalise_filter(ClassName) when is_atom(ClassName) ->
    %% Distinguish `#new-class` / `#'new-class'` (a selector filter) from class
    %% names. Class names canonically start with uppercase; selectors lowercase.
    case is_class_name_atom(ClassName) of
        true -> {ok, {class, atom_to_binary(ClassName, utf8)}};
        false -> {ok, {selector, ClassName}}
    end;
normalise_filter(Bin) when is_binary(Bin) ->
    {ok, {class, Bin}};
normalise_filter(Map) when is_map(Map) ->
    case maps:get(file, Map, undefined) of
        %% BT-4173: a String file arrives tagged — unwrap to bytes.
        {beamtalk_string, Tagged} when is_binary(Tagged) ->
            {ok, {file, Tagged}};
        File when is_binary(File) ->
            {ok, {file, File}};
        File when is_list(File) ->
            {ok, {file, list_to_binary(File)}};
        undefined ->
            {error, filter_error(<<"flush: dictionary filter must include a `file` key">>)}
    end;
normalise_filter(_Other) ->
    {error,
        filter_error(<<
            "flush: expects a Class, a Symbol (e.g. #'new-class'), or a Dictionary "
            "#{ #file => \"...\" } (Symbol-keyed)"
        >>)}.

%% Heuristic: class names start uppercase (PascalCase), selectors lowercase.
%% Matches Beamtalk's naming convention; selectors that happen to start
%% uppercase would be rare and the caller can always pass an explicit
%% `#{ #file => ... }` (Symbol-keyed) dictionary instead.
-spec is_class_name_atom(atom()) -> boolean().
is_class_name_atom(Atom) ->
    case atom_to_binary(Atom, utf8) of
        <<C, _/binary>> when C >= $A, C =< $Z -> true;
        _ -> false
    end.

%%% ----------------------------------------------------------------------------
%%% Core flush
%%% ----------------------------------------------------------------------------

-spec do_flush(filter(), boolean()) -> {ok, map()} | {error, #beamtalk_error{}}.
do_flush(Filter, ConfirmDestructive) ->
    Pending = filter_entries(beamtalk_workspace_changelog:flushable_pending(), Filter),
    %% `flushable_pending` already excludes non-flushable entries — the
    %% `skipped` field of the response is documentation of what would be
    %% skipped if non-flushable durable entries were in scope, which only
    %% happens when the caller passes a filter that selects them explicitly.
    %% Non-flushable entries are simply not in `flushable_pending`, so they
    %% never populate `skipped`; the field's other populated reason (ADR
    %% 0113) is `"destructive"` — see `run_flush/2`.
    case Pending of
        [] ->
            {ok, empty_summary()};
        _ ->
            run_flush(Pending, ConfirmDestructive)
    end.

-spec filter_entries([term()], filter()) -> [term()].
filter_entries(Entries, any) ->
    Entries;
filter_entries(Entries, {class, ClassBin}) ->
    [E || E <- Entries, beamtalk_workspace_changelog:entry_class(E) =:= ClassBin];
filter_entries(Entries, {selector, Sel}) when
    Sel =:= 'new-class';
    Sel =:= 'remove-method';
    Sel =:= 'remove-class';
    Sel =:= 'rename-class';
    Sel =:= 'rename-method'
->
    %% `'new-class'`/`'remove-class'`/`'rename-class'` entries carry
    %% selector: null; a `'remove-method'`/`'rename-method'` entry carries
    %% the real removed/new selector (e.g. #foo), not null. Either way,
    %% filtering on the bare marker atom is meant to mean "all removals/
    %% additions/renames of this kind", not a literal selector match —
    %% matching on entry_selector/1 would return nothing for the
    %% null-selector kinds and would only ever match a method coincidentally
    %% named e.g. #'remove-method' for the others, so kind-matching is what
    %% this filter form is actually for. Unlike `instance`/`class` (also
    %% valid kind() values, but ambiguous with real getter selectors of the
    %% same name), none of these five collides with a plausible method
    %% selector, so redirecting them to kind-matching is unambiguous.
    [E || E <- Entries, beamtalk_workspace_changelog:entry_kind(E) =:= Sel];
filter_entries(Entries, {selector, Sel}) ->
    SelBin = atom_to_binary(Sel, utf8),
    [E || E <- Entries, beamtalk_workspace_changelog:entry_selector(E) =:= SelBin];
filter_entries(Entries, {file, FileBin}) ->
    [E || E <- Entries, beamtalk_workspace_changelog:entry_source_file(E) =:= FileBin];
filter_entries(Entries, {kinds, EntryKinds, AuthorKinds}) ->
    [E || E <- Entries, entry_matches_kinds(E, EntryKinds, AuthorKinds)].

-spec entry_matches_kinds(term(), [atom()], [atom()]) -> boolean().
entry_matches_kinds(E, EntryKinds, AuthorKinds) ->
    matches_set(EntryKinds, beamtalk_workspace_changelog:entry_kind(E)) andalso
        matches_set(AuthorKinds, beamtalk_workspace_changelog:entry_author_kind(E)).

%% An empty constraint set means "no filter on this dimension" — accept any
%% value. Otherwise require membership.
-spec matches_set([atom()], atom()) -> boolean().
matches_set([], _Value) -> true;
matches_set(Set, Value) -> lists:member(Value, Set).

-spec run_flush([term()], boolean()) -> {ok, map()} | {error, #beamtalk_error{}}.
run_flush(Pending, ConfirmDestructive) ->
    %% Shadow duplicates: for each (class, selector, side) keep only the
    %% highest-seq entry as the "applied" one. Shadowed entries are also
    %% marked flushed afterwards (their target was reached by a later entry).
    %%
    %% `Pending` here includes every kind — `'remove-method'` and
    %% `'remove-class'` entries on purpose — so a removal correctly shadows
    %% any older, now-stale patch (or creation) targeting the same key.
    %% Tiering (ADR 0113) is applied *after* shadowing has decided
    %% survivorship, exactly as `'remove-method'` exclusion used to be
    %% applied post-shadow (ADR 0112): a `(class, selector, side)`
    %% target's survivor is whichever entry has the highest seq regardless of
    %% tier, so a stale, older patch never gets spliced back to disk just
    %% because its newer survivor (a removal) was withheld this round.
    {Applied0, Shadowed} = shadow_duplicates(Pending),
    {Tier1, Tier2} = lists:partition(fun(E) -> entry_tier(E) =:= tier1 end, Applied0),
    {ToApply, SkippedTier2} =
        case ConfirmDestructive of
            true -> {Applied0, []};
            false -> {Tier1, Tier2}
        end,
    Skipped = [skipped_entry(E) || E <- SkippedTier2],
    %% ADR 0114: a `'rename-class'`/`'rename-method'`
    %% entry's top-level `sourceFile` is always null (schema: ambiguous for a
    %% multi-file entry) so neither can ever land in `group_by_file/1`'s
    %% grouping — both are pulled out up front and staged through their own
    %% multi-file pipelines instead, then merged back into one Phase A /
    %% Phase B pass with the ordinary entries.
    {RenameClassEntries, Rest1} = lists:partition(fun is_rename_class_entry/1, ToApply),
    {RenameMethodEntries, OrdinaryEntries} = lists:partition(fun is_rename_method_entry/1, Rest1),
    OrdinaryGroups = group_by_file(OrdinaryEntries),
    case
        beamtalk_workspace_flush_rename:check_no_rename_file_collisions(
            RenameClassEntries, OrdinaryGroups
        )
    of
        {conflict, Conflicts0} ->
            {ok, conflict_summary(Conflicts0, Skipped)};
        ok ->
            %% A `'rename-method'` entry must not race a pending
            %% ordinary patch OR a pending `'rename-class'` entry over the
            %% same file — see the moduledoc's "Atomicity (method rename)"
            %% section.
            OtherFiles = sets:union(
                sets:from_list([F || {F, _} <- OrdinaryGroups], [{version, 2}]),
                sets:from_list(
                    lists:flatmap(
                        fun beamtalk_workspace_flush_rename:rename_entry_all_files/1,
                        RenameClassEntries
                    ),
                    [{version, 2}]
                )
            ),
            case
                beamtalk_workspace_flush_rename:check_no_rename_method_file_collisions(
                    RenameMethodEntries, OtherFiles, OrdinaryGroups
                )
            of
                {conflict, Conflicts1} ->
                    {ok, conflict_summary(Conflicts1, Skipped)};
                ok ->
                    Combined = combine_phase_a(
                        combine_phase_a(
                            phase_a(OrdinaryGroups),
                            beamtalk_workspace_flush_rename:phase_a_renames(RenameClassEntries)
                        ),
                        beamtalk_workspace_flush_rename:phase_a_rename_methods(RenameMethodEntries)
                    ),
                    case Combined of
                        {ok, Prepared} ->
                            RenameExpectedFiles = maps:merge(
                                beamtalk_workspace_flush_rename:rename_expected_files_map(
                                    RenameClassEntries
                                ),
                                beamtalk_workspace_flush_rename:rename_method_expected_files_map(
                                    RenameMethodEntries
                                )
                            ),
                            phase_b(Prepared, Shadowed, Skipped, RenameExpectedFiles);
                        {error, _} = Err ->
                            Err;
                        {conflict, Conflicts} ->
                            {ok, conflict_summary(Conflicts, Skipped)}
                    end
            end
    end.

-spec is_rename_class_entry(term()) -> boolean().
is_rename_class_entry(E) ->
    beamtalk_workspace_kinds:rename(beamtalk_workspace_changelog:entry_kind(E)) =:= class.

-spec is_rename_method_entry(term()) -> boolean().
is_rename_method_entry(E) ->
    beamtalk_workspace_kinds:rename(beamtalk_workspace_changelog:entry_kind(E)) =:= method.

%%% ----------------------------------------------------------------------------
%%% Tiering (ADR 0113 Phase 2)
%%% ----------------------------------------------------------------------------

-doc """
Classify a ChangeEntry into flush's destructive-confirmation tier.

Tier 1 — edits a still-existing file (`instance`, `class`, `'new-class'`,
`'remove-method'`) — applies under ordinary `flush/0` / `flush/1` /
`flush_kinds/1` with no gate. Tier 2 — destroys a file (`'remove-class'`),
moves one (`'rename-class'`, ADR 0114), or rewrites a method's
confirmed call sites across files (`'rename-method'`, ADR 0114) —
only applies when the caller passes `ConfirmDestructive = true`.
""".
-spec entry_tier(term()) -> tier1 | tier2.
entry_tier(E) ->
    beamtalk_workspace_kinds:tier(beamtalk_workspace_changelog:entry_kind(E)).

%% The `skipped` entry shape for a Tier 2 entry left pending because
%% `ConfirmDestructive` was not given — distinct from the (currently unused,
%% forward-compat) `"ephemeral"` / `"not flushable (...)"` reasons the module
%% doc's `Return value` section documents.
-spec skipped_entry(term()) -> map().
skipped_entry(E) ->
    #{
        seq => beamtalk_workspace_changelog:entry_seq(E),
        class => beamtalk_workspace_changelog:entry_class(E),
        reason => <<"destructive">>
    }.

%%% ----------------------------------------------------------------------------
%%% Shadowing
%%% ----------------------------------------------------------------------------

%% Keep only the most-recent entry for each (class, selector, side) target.
%% Older entries with the same target are returned as `Shadowed` and will be
%% marked flushed at the end (their patch is already on disk via the newer
%% entry).
-spec shadow_duplicates([term()]) -> {[term()], [term()]}.
shadow_duplicates(Entries) ->
    %% Sort newest-first so we encounter the survivor of each target before
    %% any of its shadows.
    Sorted = lists:sort(
        fun(A, B) ->
            beamtalk_workspace_changelog:entry_seq(A) > beamtalk_workspace_changelog:entry_seq(B)
        end,
        Entries
    ),
    {Applied, Shadowed, _Seen} = lists:foldl(
        fun(E, {ApplAcc, ShadAcc, Seen}) ->
            Key = target_key(E),
            case sets:is_element(Key, Seen) of
                true -> {ApplAcc, [E | ShadAcc], Seen};
                false -> {[E | ApplAcc], ShadAcc, sets:add_element(Key, Seen)}
            end
        end,
        {[], [], sets:new([{version, 2}])},
        Sorted
    ),
    %% Restore append order on Applied so a single file's splices come back
    %% in seq order; downstream `splice_file` sorts them properly anyway, but
    %% the ordered form makes the summary deterministic.
    {
        lists:sort(
            fun(A, B) ->
                beamtalk_workspace_changelog:entry_seq(A) <
                    beamtalk_workspace_changelog:entry_seq(B)
            end,
            Applied
        ),
        Shadowed
    }.

%% The shadow key is `beamtalk_workspace_changelog:shadow_key/1` — the single
%% definition shared with the pending-changes view (BT-4859), so what the
%% view shows and what flush writes cannot drift. See that function for the
%% `(class, selector, side)` (ADR 0112) / whole-class-by-kind rules.
-spec target_key(term()) -> term().
target_key(E) ->
    beamtalk_workspace_changelog:shadow_key(E).

%%% ----------------------------------------------------------------------------
%%% Grouping
%%% ----------------------------------------------------------------------------

-doc """
Group entries by `sourceFile`. Returns a list of `{File, [Entry, ...]}` pairs
in append (seq-ascending) order within each file. Exported for tests.
""".
-spec group_by_file([term()]) -> [{binary(), [term()]}].
group_by_file(Entries) ->
    %% Preserve seq order within each file so downstream code can rely on it.
    Sorted = lists:sort(
        fun(A, B) ->
            beamtalk_workspace_changelog:entry_seq(A) < beamtalk_workspace_changelog:entry_seq(B)
        end,
        Entries
    ),
    lists:foldr(
        fun(E, Acc) ->
            File = beamtalk_workspace_changelog:entry_source_file(E),
            case lists:keyfind(File, 1, Acc) of
                {File, Es} -> lists:keyreplace(File, 1, Acc, {File, [E | Es]});
                false -> [{File, [E]} | Acc]
            end
        end,
        [],
        Sorted
    ).

%%% ----------------------------------------------------------------------------
%%% Phase A: read + validate + write all <file>.tmp
%%% ----------------------------------------------------------------------------

-spec phase_a([{binary(), [term()]}]) ->
    {ok, [#prepared{}]} | {error, #beamtalk_error{}} | {conflict, [map()]}.
phase_a(Groups) ->
    phase_a_loop(Groups, [], []).

phase_a_loop([], Prepared, []) ->
    {ok, lists:reverse(Prepared)};
phase_a_loop([], Prepared, Conflicts) ->
    %% Any Phase A conflict aborts the whole flush — clean up tmps already
    %% written (a partial commit is *worse* than a conflict report).
    beamtalk_workspace_flush_support:cleanup_tmps(Prepared),
    {conflict, lists:reverse(Conflicts)};
phase_a_loop([{File, Entries} | Rest], Prepared, Conflicts) ->
    case prepare_file(File, Entries) of
        {ok, Rec} ->
            phase_a_loop(Rest, [Rec | Prepared], Conflicts);
        {conflict, ConflictMap} ->
            phase_a_loop(Rest, Prepared, [ConflictMap | Conflicts]);
        {error, _} = Err ->
            beamtalk_workspace_flush_support:cleanup_tmps(Prepared),
            Err
    end.

%% Decide between a new-class write (single entry, span=undefined) and a
%% method-splice write (one or more entries with spans). Both end with a
%% <file>.tmp ready to be renamed in Phase B.
%%
%% Additional `compile:source:` patches against a not-yet-flushed new class
%% surface as non-flushable in the loader (the targetPath does not exist on
%% disk yet, so the install hook cannot resolve a span and downgrades the
%% entry). They are therefore not in `flushable_pending` and never reach this
%% function. After a successful flush of the new-class entry, the file exists
%% on disk and subsequent patches resolve cleanly. We still handle a stray
%% mixed group defensively below.
-spec prepare_file(binary(), [term()]) ->
    {ok, #prepared{}} | {conflict, map()} | {error, #beamtalk_error{}}.
prepare_file(File, [Entry] = Entries) ->
    case beamtalk_workspace_changelog:entry_kind(Entry) of
        'new-class' -> prepare_new_class(File, Entries, Entry);
        'remove-class' -> prepare_remove_class(File, Entries, Entry);
        _ -> beamtalk_workspace_flush_sites:prepare_splice(File, Entries)
    end;
prepare_file(File, Entries) ->
    case lists:any(fun is_new_class_entry/1, Entries) of
        true ->
            %% Defensive: a new-class entry mixed with siblings should not occur
            %% because subsequent patches against a not-yet-flushed new class
            %% are non-flushable (see comment on prepare_file/2). Surface as a
            %% conflict rather than silently producing an inconsistent file.
            {conflict,
                beamtalk_workspace_flush_support:conflict_map(
                    File,
                    <<"mixed_new_class_and_splice">>,
                    Entries,
                    <<
                        "Cannot flush a new-class entry alongside other patches in the "
                        "same operation; flush the new-class entry first, then re-flush "
                        "to apply method patches against the newly created file"
                    >>
                )};
        false ->
            case lists:any(fun is_remove_class_entry/1, Entries) of
                true ->
                    %% Defensive, ADR 0113: a `'remove-class'` entry shares no
                    %% target_key with a method-level entry (its own key is
                    %% `(class, undefined, undefined)`), so an un-shadowed
                    %% patch against the same class can survive shadowing
                    %% alongside it and land in the same file group (e.g. an
                    %% unflushed method patch, then `removeFromSystem`).
                    %% Applying the patch is pointless (the file is about to
                    %% be deleted) and reordering write-then-delete within one
                    %% Phase A/B pass is undesigned — surface as a conflict,
                    %% same defensive shape as the new-class case above.
                    {conflict,
                        beamtalk_workspace_flush_support:conflict_map(
                            File,
                            <<"mixed_remove_class_and_splice">>,
                            Entries,
                            <<
                                "Cannot flush a remove-class entry alongside other pending "
                                "patches against the same class in the same operation; flush "
                                "or discard the other pending entries first, then re-flush "
                                "the removal"
                            >>
                        )};
                false ->
                    beamtalk_workspace_flush_sites:prepare_splice(File, Entries)
            end
    end.

-spec is_new_class_entry(term()) -> boolean().
is_new_class_entry(E) ->
    beamtalk_workspace_changelog:entry_kind(E) =:= 'new-class'.

-spec is_remove_class_entry(term()) -> boolean().
is_remove_class_entry(E) ->
    beamtalk_workspace_changelog:entry_kind(E) =:= 'remove-class'.

%%% ----------------------------------------------------------------------------
%%% New-class write
%%% ----------------------------------------------------------------------------

prepare_new_class(File, Entries, Entry) ->
    AbsPath = binary_to_list(File),
    %% Use file:read_file_info/1 (not filelib:is_regular/1) so any existing
    %% filesystem entry — directory, symlink, unreadable path — is caught
    %% up front as target_exists. Otherwise a directory at the target would
    %% slip past Phase A and fail later with an opaque rename_failed conflict.
    %% Mirrors the same validation in beamtalk_repl_loader_newclass:validate_target_path/1.
    case file:read_file_info(AbsPath) of
        {error, enoent} ->
            case beamtalk_workspace_changelog:read_source_body(Entry) of
                {ok, Body} ->
                    case beamtalk_workspace_flush_support:write_tmp(AbsPath, Body) of
                        {ok, Tmp} ->
                            {ok, #prepared{
                                file = File,
                                tmp = Tmp,
                                entries = Entries,
                                pre_existing = false
                            }};
                        {error, _} = Err ->
                            beamtalk_workspace_flush_support:wrap_io_error(Err, File)
                    end;
                {error, Reason} ->
                    {error, beamtalk_workspace_flush_support:source_body_error(File, Reason)}
            end;
        _Other ->
            %% Any existing filesystem entry (regular file, directory, symlink)
            %% blocks new-class; also treat unreadable paths (eacces, etc.) as
            %% existing rather than silently overwriting. `_Other` is either
            %% `{ok, FileInfo}` or `{error, Reason}` where Reason is something
            %% other than enoent (e.g. eacces, eloop).
            {conflict,
                beamtalk_workspace_flush_support:conflict_map(
                    File, <<"target_exists">>, Entries, <<
                        "newClass:at: target already exists on disk; choose a different path "
                        "or clear the pending entry"
                    >>
                )}
    end.

%%% ----------------------------------------------------------------------------
%%% Class removal (Tier 2) — staged delete (ADR 0113)
%%% ----------------------------------------------------------------------------

-doc """
Stage a `'remove-class'` entry's Phase A step: same-filesystem rename
`<file>` → `<file>.tmp-delete-<epoch>-<seq>` (POSIX-atomic).

Three outcomes, matching the ADR's *Delete atomicity* / *External-edit
conflicts* tables:

  - The target file exists: rename it to the staged path. Phase B unlinks the
    staged file to commit the delete.
  - The target file is absent AND a staged file matching this entry's own
    `epoch`/`seq` already exists: a prior flush attempt already completed
    Phase A and crashed before Phase B. Resume there — Phase B finishes the
    unlink — rather than re-attempting a rename against an already-absent
    source.
  - The target file is absent AND no matching staged file exists: something
    else deleted it externally. Soft success (`op = noop`) — Phase B performs
    no I/O and the entry is still marked flushed, since the outcome the user
    wanted (the file is gone) already holds.

An I/O error at any step (rename failure, unreadable path) is a hard error,
mirroring `write_tmp/2`'s failures for the write path.
""".
-spec prepare_remove_class(binary(), [term()], term()) ->
    {ok, #prepared{}} | {error, #beamtalk_error{}}.
prepare_remove_class(File, Entries, Entry) ->
    AbsPath = binary_to_list(File),
    StagedPath = delete_staging_path(
        AbsPath,
        beamtalk_workspace_changelog:entry_epoch(Entry),
        beamtalk_workspace_changelog:entry_seq(Entry)
    ),
    case file:read_file_info(AbsPath) of
        {ok, _Info} ->
            case file:rename(AbsPath, StagedPath) of
                ok ->
                    {ok, #prepared{
                        file = File,
                        tmp = StagedPath,
                        entries = Entries,
                        pre_existing = true,
                        op = delete
                    }};
                {error, Reason} ->
                    beamtalk_workspace_flush_support:wrap_io_error({error, Reason}, File)
            end;
        {error, enoent} ->
            resolve_missing_remove_class_target(File, Entries, StagedPath);
        {error, Reason} ->
            beamtalk_workspace_flush_support:wrap_io_error({error, Reason}, File)
    end.

%% Disambiguate a missing target file (ADR 0113, "Disambiguating a missing
%% <file> from a recoverable mid-delete crash"): a staged file matching this
%% entry's own epoch/seq means a prior flush attempt already renamed it and
%% crashed before the unlink — resume at Phase B. No matching staged file
%% means something else deleted the file externally — a soft success.
-spec resolve_missing_remove_class_target(binary(), [term()], string()) ->
    {ok, #prepared{}}.
resolve_missing_remove_class_target(File, Entries, StagedPath) ->
    case file:read_file_info(StagedPath) of
        {ok, _Info} ->
            {ok, #prepared{
                file = File,
                tmp = StagedPath,
                entries = Entries,
                pre_existing = false,
                op = delete
            }};
        {error, _} ->
            %% Already gone — nothing to remove. A soft success: no further
            %% disk I/O, the entry is still marked flushed in Phase B.
            {ok, #prepared{
                file = File,
                tmp = StagedPath,
                entries = Entries,
                pre_existing = false,
                op = noop
            }}
    end.

%% Same-filesystem staging path for a class-removal delete (ADR 0113), keyed
%% on the entry's own epoch/seq so a resumed flush can recognise its own
%% prior staging attempt.
-spec delete_staging_path(string(), non_neg_integer(), non_neg_integer()) -> string().
delete_staging_path(AbsPath, Epoch, Seq) ->
    AbsPath ++ ".tmp-delete-" ++ integer_to_list(Epoch) ++ "-" ++ integer_to_list(Seq).

%% Combine the ordinary-groups and rename-classes Phase A results into one
%% `{ok, [#prepared{}]} | {error, _} | {conflict, [map()]}`, matching
%% `phase_a_loop/3`'s own "any conflict/error aborts the whole batch, cleaning
%% up every tmp already written" rule across BOTH pipelines — a Phase A
%% conflict discovered while preparing ordinary files must clean up
%% successfully-staged rename `.tmp`s too, and vice versa. Each side already
%% cleans up its OWN tmps internally on an internal conflict/error
%% (`phase_a_loop/3`, `phase_a_renames_loop/3`), so only the OTHER side's
%% tmps need cleaning up here.
-spec combine_phase_a(
    {ok, [#prepared{}]} | {error, term()} | {conflict, [map()]},
    {ok, [#prepared{}]} | {error, term()} | {conflict, [map()]}
) -> {ok, [#prepared{}]} | {error, term()} | {conflict, [map()]}.
combine_phase_a({ok, P1}, {ok, P2}) ->
    {ok, P1 ++ P2};
combine_phase_a({error, _} = Err, Other) ->
    cleanup_other_prepared(Other),
    Err;
combine_phase_a(Other, {error, _} = Err) ->
    cleanup_other_prepared(Other),
    Err;
combine_phase_a({conflict, C1}, {conflict, C2}) ->
    {conflict, C1 ++ C2};
combine_phase_a({conflict, C1}, {ok, P2}) ->
    beamtalk_workspace_flush_support:cleanup_tmps(P2),
    {conflict, C1};
combine_phase_a({ok, P1}, {conflict, C2}) ->
    beamtalk_workspace_flush_support:cleanup_tmps(P1),
    {conflict, C2}.

-spec cleanup_other_prepared({ok, [#prepared{}]} | term()) -> ok.
cleanup_other_prepared({ok, P}) -> beamtalk_workspace_flush_support:cleanup_tmps(P);
cleanup_other_prepared(_) -> ok.

-doc """
Splice `Replacement` into `Body` at the byte range `{Start, End}` (half-open).
Delegates to `beamtalk_workspace_span:splice/3`; kept exported for tests.
""".
-spec splice(binary(), {non_neg_integer(), non_neg_integer()}, binary()) -> binary().
splice(Body, Span, Replacement) ->
    beamtalk_workspace_span:splice(Body, Span, Replacement).

%%% ----------------------------------------------------------------------------
%%% Phase B: commit (rename into place, or unlink a staged delete)
%%% ----------------------------------------------------------------------------

%% Sequential commit. On the first failure: stop, mark every already-committed
%% file's entries as flushed, and surface a per-file status report. Earlier
%% successes are *not* rolled back — POSIX rename/unlink are one-way and the
%% on-disk state is the new authoritative source for those files. `Skipped`
%% (ADR 0113: pending Tier 2 entries left out of this flush) passes straight
%% through to the summary — Phase B does nothing with it but report it.
-spec phase_b([#prepared{}], [term()], [map()], #{non_neg_integer() => sets:set(binary())}) ->
    {ok, map()}.
phase_b(Prepared, Shadowed, Skipped, RenameExpectedFiles) ->
    phase_b_loop(Prepared, Shadowed, [], [], Skipped, RenameExpectedFiles).

phase_b_loop([], Shadowed, Committed, Failed, Skipped, RenameExpectedFiles) ->
    Files = lists:reverse([P#prepared.file || P <- Committed]),
    CommittedFilesSet = sets:from_list(Files, [{version, 2}]),
    CommittedEntries = lists:flatten([P#prepared.entries || P <- Committed]),
    %% ADR 0114: a `'rename-class'` entry spans multiple
    %% files (the move target plus every other rewritten site) and a
    %% `'rename-method'` entry does too (the definition plus every confirmed
    %% sender site), so appearing in `CommittedEntries` at all (from ONE of
    %% its files) is not enough — every file either kind touches must have
    %% committed in THIS pass. A non-multi-site entry trivially satisfies
    %% this (every other kind targets one file).
    FullyCommitted = [
        E
     || E <- CommittedEntries,
        beamtalk_workspace_flush_sites:multi_site_entry_fully_committed(
            E, RenameExpectedFiles, CommittedFilesSet
        )
    ],
    %% Only mark shadowed entries whose survivor (same class+selector) was
    %% actually committed in this Phase B. When Phase B aborts mid-loop, a
    %% shadowed entry whose survivor never made it to disk must stay pending
    %% — otherwise we silently lose the change from the active view while it
    %% is not on disk. See Copilot review on PR #2325.
    SurvivorKeys = renamed_target_keys(FullyCommitted),
    AppliedShadowed = filter_shadowed_by_survivor(Shadowed, SurvivorKeys),
    EntriesToMark = FullyCommitted ++ AppliedShadowed,
    Seqs = lists:usort([beamtalk_workspace_changelog:entry_seq(E) || E <- EntriesToMark]),
    complete_flush(Files, lists:reverse(Committed), Failed, Seqs, Skipped);
phase_b_loop([P | Rest], Shadowed, Committed, Failed, Skipped, RenameExpectedFiles) ->
    case commit(P) of
        ok ->
            maybe_reload_renamed_class_source(P),
            phase_b_loop(Rest, Shadowed, [P | Committed], Failed, Skipped, RenameExpectedFiles);
        {error, Reason} ->
            ?LOG_ERROR(
                "Workspace flush: Phase B commit failed",
                #{
                    file => P#prepared.file,
                    op => P#prepared.op,
                    reason => Reason,
                    domain => [beamtalk, runtime]
                }
            ),
            %% A Phase B failure: stop, but keep the already-committed files.
            FailedHere = #{
                file => P#prepared.file,
                reason => <<"rename_failed">>,
                seqs => [beamtalk_workspace_flush_support:seq(E) || E <- P#prepared.entries],
                detail => atom_to_binary(Reason, utf8)
            },
            %% Clean up any unattempted tmps so we do not leave stale files
            %% behind for the next flush to trip over.
            beamtalk_workspace_flush_support:cleanup_tmps([P | Rest]),
            phase_b_loop(
                [], Shadowed, Committed, [FailedHere | Failed], Skipped, RenameExpectedFiles
            )
    end.

%% Phase B's commit action, dispatched by `P#prepared.op`:
%%   - `write`  — rename `tmp` (the freshly-written `<file>.tmp`) into `file`.
%%   - `delete` — unlink the staged `<file>.tmp-delete-<epoch>-<seq>`
%%     (ADR 0113): `file` is already absent from its original location by this
%%     point (Phase A's stage-rename already happened, this attempt's or an
%%     earlier one's), so this is the operation that finally makes the
%%     removal durable.
%%   - `noop`   — the external-edit soft success (already gone); no I/O.
%%   - `move`   — rename `tmp` into `file` (= `new_path`) THEN unlink
%%     `old_file` (ADR 0114): in THAT order, so a crash or failure
%%     between the two never loses the file's content — `old_file` is only
%%     ever removed once its content is durably present at the new name. If
%%     the unlink itself fails, `old_file` simply lingers (still untouched,
%%     still matching its recorded `prev_source`) and the next flush attempt
%%     harmlessly re-derives and re-stages `new_path` before retrying
%%     the unlink — see the moduledoc's "Atomicity (class rename)" section.
%%   - `move_noop` — the move already completed in an earlier, crashed or
%%     marker-failed attempt; no I/O.
-spec commit(#prepared{}) -> ok | {error, term()}.
commit(#prepared{op = write, tmp = Tmp, file = File}) ->
    after_rename(file:rename(Tmp, binary_to_list(File)), File);
commit(#prepared{op = delete, tmp = Tmp}) ->
    file:delete(Tmp);
commit(#prepared{op = noop}) ->
    ok;
commit(#prepared{op = move, tmp = Tmp, file = NewPath, old_file = OldPath}) ->
    case after_rename(file:rename(Tmp, binary_to_list(NewPath)), NewPath) of
        ok -> file:delete(binary_to_list(OldPath));
        {error, _} = Err -> Err
    end;
commit(#prepared{op = move_noop}) ->
    ok.

%% After a successful staging rename into `File`, sweep stale staging files a
%% crashed earlier attempt left next to it (see the moduledoc's "Atomicity
%% (single-file)" section). Best effort: never turns a commit into a failure.
-spec after_rename(ok | {error, term()}, binary()) -> ok | {error, term()}.
after_rename(ok, File) ->
    _ = beamtalk_workspace_file:sweep_stale_temps(binary_to_list(File)),
    ok;
after_rename({error, _} = Err, _File) ->
    Err.

-doc """
Best-effort post-commit source-attribute refresh (ADR 0114 follow-up). See the moduledoc's "Post-commit source-attribute
refresh" section for the full "why" — in short: `classRenameTo/2`'s NEXT
invocation on this same class reads the class's compiled BEAM module's
`beamtalk_source` attribute to compute ITS `old_path`, and that attribute is
only ever refreshed by *recompiling* the class — which an ordinary flush
commit (a plain `file:rename/2` + `file:delete/1`, no compile step) never
does. Left unrefreshed, a second `renameTo:` + flush on the same class would
compute a stale, now-deleted `old_path` and could never resolve it.

Gated on `op =:= move orelse op =:= move_noop` (design point 3: only a
committed rename-class move needs this — every other `op` either isn't a
rename at all, or (a rename's OTHER-site splices, `prepare_rename_site_group/2`)
targets a file that was never the renamed class's own declaration file) AND
`entry_kind(Entry) =:= 'rename-class'` (defensive — today every `move`/
`move_noop` `#prepared{}` is always produced by `prepare_rename_move/2` for
exactly one `'rename-class'` entry, but this function's contract should not
silently misfire if that ever changes).

Deliberately best-effort (design point 1): the class's in-memory identity
and behaviour are ALREADY correct at this point — `classRenameTo/2`
is what made the rename real; this reload only refreshes a
bookkeeping attribute for a POSSIBLE FUTURE rename. A failure here (no
compiler available, a bare runtime, a transient error) must never fail the
already-successful flush that files are durably written under — surfaced
via `?LOG_WARNING` instead (never silently swallowed, per design point 1's
"clear signal in the logs" requirement) so it is diagnosable without
threatening what already succeeded.

`reload_class_file/2` (`beamtalk_repl_loader`, exported and already used by
`Counter reload`/`:reload Counter`) is reused rather than any new machinery
(the ADR 0114 review's own recommended direction): it reads `new_path` from
disk, compiles it through the ordinary pipeline, and installs the result —
which is precisely what embeds the correct (new) `beamtalk_source` module
attribute, closing the staleness gap at its root. It deliberately does NOT
emit a `'class-def'` ChangeLog entry (see its own doc) so this never creates
a fresh pending entry for a subsequent flush to trip over, and it does not
touch the ChangeLog at all — this call runs inside `phase_b_loop/6`'s
per-item recursion, right after each successful `commit/1` (so BEFORE
`complete_flush/5`, which only runs once every `#prepared{}` has been
processed), but since it never touches the ChangeLog either way, nothing
here can race `mark_flushed/1` regardless of that ordering.
""".
-spec maybe_reload_renamed_class_source(#prepared{}) -> ok.
maybe_reload_renamed_class_source(#prepared{op = Op, file = NewPath, entries = [Entry | _]}) when
    Op =:= move; Op =:= move_noop
->
    case beamtalk_workspace_changelog:entry_kind(Entry) of
        'rename-class' ->
            reload_renamed_class_source(NewPath, beamtalk_workspace_changelog:entry_class(Entry));
        _ ->
            ok
    end;
maybe_reload_renamed_class_source(_Prepared) ->
    ok.

-spec reload_renamed_class_source(binary(), binary()) -> ok.
reload_renamed_class_source(NewPath, NewClassBin) ->
    ExpectedClassName =
        case beamtalk_repl_errors:safe_to_existing_atom(NewClassBin) of
            {ok, Atom} -> Atom;
            {error, _} -> undefined
        end,
    try beamtalk_repl_loader:reload_class_file(binary_to_list(NewPath), ExpectedClassName) of
        {ok, _ClassNames} ->
            ok;
        {error, Reason} ->
            ?LOG_WARNING(
                "Workspace flush: best-effort reload of a just-renamed class's source "
                "attribute failed. The rename itself already succeeded (the class's "
                "identity and file move are both correct); only the class's compiled "
                "beamtalk_source bookkeeping attribute may still point at its pre-rename "
                "path until the class is next reloaded/recompiled — a subsequent rename "
                "of this same class may need its old_path corrected manually before it "
                "can flush",
                #{
                    path => NewPath,
                    class => NewClassBin,
                    reason => Reason,
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Workspace flush: best-effort reload of a just-renamed class's source "
                "attribute crashed. The rename itself already succeeded; see "
                "reload_class_file/2's own failure mode above for what this leaves stale",
                #{
                    path => NewPath,
                    class => NewClassBin,
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    end.

%% Build a set of {Class, Selector} target keys from the entries whose file
%% was renamed in Phase B. Used to gate which shadowed entries can be marked
%% flushed — a shadowed entry whose survivor never reached disk must stay
%% pending.
-spec renamed_target_keys([term()]) -> sets:set().
renamed_target_keys(Entries) ->
    lists:foldl(
        fun(E, Acc) -> sets:add_element(target_key(E), Acc) end,
        sets:new([{version, 2}]),
        Entries
    ).

-spec filter_shadowed_by_survivor([term()], sets:set()) -> [term()].
filter_shadowed_by_survivor(Shadowed, SurvivorKeys) ->
    [E || E <- Shadowed, sets:is_element(target_key(E), SurvivorKeys)].

%% Centralise the post-rename completion path: mark flushed seqs in the
%% ChangeLog, build the summary, and surface a marker failure as an
%% additional conflict in the summary so the caller can react. Files have
%% already been renamed at this point — we never return a hard `{error, _}`
%% here because on-disk state has moved forward and the caller still needs
%% to see which files were written.
-spec complete_flush([binary()], [#prepared{}], [map()], [non_neg_integer()], [map()]) ->
    {ok, map()}.
complete_flush(Files, Renamed, Failed, Seqs, Skipped) ->
    %% ADR 0082 Phase 3: broadcast flush completion so LSP clients
    %% can emit `workspace/applyEdit` for each touched file. Fire BEFORE
    %% `mark_flushed/1` so editor refresh fires reliably in the mixed-success
    %% case where renames succeeded but the ChangeLog server is unreachable —
    %% the files are already on disk at this point and the editor needs to
    %% realign regardless of marker outcome.
    %%
    %% The typed `FlushCompleted` announcement on the SystemAnnouncer
    %% bus is now the sole flush-completion push source (the legacy
    %% `beamtalk_flush_events` broadcast was retired). Fire-and-forget: the
    %% announcer swallows missing-bus errors so flush never fails on a downstream
    %% subscriber issue.
    %%
    %% Alongside the flat `Files` list, also announce each file's
    %% per-entry operation kind (`file_kind_map/1`) so a `CreateFile`-vs-patch
    %% consumer (the LSP) no longer has to infer "freshly created" from
    %% filesystem existence — see `announce_flush_completed/2`.
    announce_flush_completed(Files, [file_kind_map(P) || P <- Renamed]),
    %% Catch any failure mode from the ChangeLog server — explicit {error, _}
    %% returns *or* gen_server crashes (the call exits with noproc/timeout
    %% when the server is unreachable). Files have already been written so
    %% returning a hard error tuple would lose the success report; we surface
    %% the failure as a conflict-shaped entry on the summary instead.
    MarkResult =
        try
            beamtalk_workspace_changelog:mark_flushed(Seqs)
        catch
            exit:ExitReason -> {error, {changelog_unreachable, ExitReason}}
        end,
    case MarkResult of
        ok ->
            {ok, success_summary(Files, Renamed, Failed, Skipped, Seqs)};
        {error, Reason} ->
            ?LOG_ERROR(
                "Workspace flush: mark_flushed failed after successful rename",
                #{
                    reason => Reason,
                    files => Files,
                    seqs => Seqs,
                    domain => [beamtalk, runtime]
                }
            ),
            MarkerFailure = #{
                file => <<"<changelog>">>,
                reason => <<"flush_marker_failed">>,
                seqs => Seqs,
                detail => iolist_to_binary([
                    <<
                        "Files were written to disk but the ChangeLog could not "
                        "mark the entries as flushed; they still appear in "
                        "`Workspace changes` and will conflict on re-flush. "
                        "Detail: "
                    >>,
                    io_lib:format("~p", [Reason])
                ])
            },
            {ok, success_summary(Files, Renamed, [MarkerFailure | Failed], Skipped, Seqs)}
    end.

%% Announce `FlushCompleted` on the `SystemAnnouncer` bus after a flush has
%% written files to disk (ADR 0093 §2). `files` carries the absolute
%% binary paths, matching the legacy `{flush_completed, Files}` broadcast.
%%
%% `FileKinds` (ADR 0113 LSP follow-up) is the per-file companion:
%% one `#{file => Path, kind => Kind}` map per entry in `Files`, where `Kind`
%% is `beamtalk_workspace_changelog:entry_kind/1`'s own enum value verbatim
%% (`'new-class'`, `'remove-class'`, `instance`, `class`, `'remove-method'`,
%% `'rename-class'`, `'rename-method'` (ADR 0114), `unknown`) —
%% forwarded as-is rather than collapsed into a new taxonomy, so a consumer
%% buckets it itself (the LSP: `'new-class'` -> `CreateFile`, `'remove-class'`
%% -> `DeleteFile`, `'rename-class'` with an `oldFile` -> `RenameFile`,
%% `'rename-method'` -> a `TextDocumentEdit` per confirmed site, anything
%% else -> an ordinary patch). `file_kind_map/1` builds each entry from the
%% Phase B `#prepared{}` record, adding `oldFile` for the one
%% `op = move` record a `'rename-class'` flush produces.
%%
%% Skipped for an empty file list (parity with `on_files_flushed/1`). Guarded
%% by a `whereis` check (the announcements worker may be absent on a minimal
%% runtime) and wrapped in try/catch: announcing is a best-effort observability
%% side effect and must never fail the flush.
-spec announce_flush_completed([binary()], [map()]) -> ok.
announce_flush_completed([], _FileKinds) ->
    ok;
announce_flush_completed(Files, FileKinds) ->
    case erlang:whereis(beamtalk_announcements) of
        undefined ->
            ok;
        _Pid ->
            try
                beamtalk_announcements:system_announce('FlushCompleted', #{
                    files => Files,
                    fileKinds => FileKinds
                })
            catch
                _:_ -> ok
            end
    end,
    ok.

%% The `#{file => Path, kind => Kind}` wire entry for one committed
%% `#prepared{}` record. A `'new-class'`/`'remove-class'` file
%% group is always single-entry (`prepare_file/2`'s mixing guards forbid
%% combining either with sibling patches); a splice group may hold several
%% entries but they are always `instance`/`class`/`'remove-method'` — never
%% mixed with `'new-class'`/`'remove-class'` — so any entry in the group
%% yields the same LSP-relevant bucket ("an ordinary patch"). Taking the
%% first entry's `entry_kind/1` is therefore always representative, not just
%% a convenient default.
%%
%% ADR 0114 LSP follow-up: `op = move`'s `old_file` is also
%% forwarded as `oldFile` — the ONLY signal on the wire that distinguishes
%% "this `'rename-class'`-kind file IS the moved declaration file" (needs a
%% `RenameFile` resource operation) from "this `'rename-class'`-kind file is
%% an ordinary same-batch reference rewrite in a file that didn't move"
%% (needs an ordinary patch) — both share the same `kind` (`entry_kind/1`
%% reports the owning entry's kind for every file it touches, not a
%% per-file distinction). `op = move_noop` has no `old_file` (the move
%% already completed in an earlier attempt — see the moduledoc's
%% "Atomicity (class rename)" section) so it correctly degrades to the
%% no-`oldFile` / ordinary-patch shape rather than re-emitting a `RenameFile`
%% for an `old_path` that is already gone.
-spec file_kind_map(#prepared{}) -> map().
file_kind_map(#prepared{file = File, entries = [Entry | _], old_file = undefined}) ->
    #{file => File, kind => beamtalk_workspace_changelog:entry_kind(Entry)};
file_kind_map(#prepared{file = File, entries = [Entry | _], old_file = OldFile}) ->
    #{file => File, kind => beamtalk_workspace_changelog:entry_kind(Entry), oldFile => OldFile}.

%%% ----------------------------------------------------------------------------
%%% Summary
%%% ----------------------------------------------------------------------------

-spec empty_summary() -> map().
empty_summary() ->
    base_summary(0, [], 0, 0, [], []).

-doc """
`Flushed`/`newClasses`/`removedClasses` are counted over `Seqs` — the seqs
`complete_flush/5` actually asked `mark_flushed/1` to mark, i.e. exactly
the entries `phase_b_loop/6` decided are fully committed — rather than
naively flattening `Committed`'s `.entries` fields. For every existing
single-file kind these two sets always coincide (each entry belongs to
exactly one `#prepared{}` record) so this is a no-op change in behaviour;
for a `'rename-class'` entry (ADR 0114) the SAME entry legitimately
appears in `Committed` once per file it touches (the move plus every other
rewritten site), and must count once, not once per file — and not at all if
`phase_b_loop/6` excluded it for not having ALL of its files committed yet.
""".
-spec success_summary([binary()], [#prepared{}], [map()], [map()], [non_neg_integer()]) -> map().
success_summary(Files, Committed, Failed, Skipped, Seqs) ->
    SeqSet = sets:from_list(Seqs, [{version, 2}]),
    AllEntries = lists:usort(
        fun(A, B) ->
            beamtalk_workspace_changelog:entry_seq(A) =< beamtalk_workspace_changelog:entry_seq(B)
        end,
        [
            E
         || E <- lists:flatten([P#prepared.entries || P <- Committed]),
            sets:is_element(beamtalk_workspace_changelog:entry_seq(E), SeqSet)
        ]
    ),
    Flushed = length(AllEntries),
    NewClassCount = length([E || E <- AllEntries, is_new_class_entry(E)]),
    RemovedClassCount = length([E || E <- AllEntries, is_remove_class_entry(E)]),
    base_summary(Flushed, Files, NewClassCount, RemovedClassCount, Skipped, Failed).

-spec conflict_summary([map()], [map()]) -> map().
conflict_summary(Conflicts, Skipped) ->
    base_summary(0, [], 0, 0, Skipped, Conflicts).

-spec base_summary(
    non_neg_integer(), [binary()], non_neg_integer(), non_neg_integer(), [map()], [map()]
) -> map().
base_summary(Flushed, Files, NewClasses, RemovedClasses, Skipped, Conflicts) ->
    #{
        '$beamtalk_class' => 'FlushResult',
        flushed => Flushed,
        files => Files,
        newClasses => NewClasses,
        removedClasses => RemovedClasses,
        skipped => Skipped,
        conflicts => Conflicts
    }.

%%% ----------------------------------------------------------------------------
%%% Errors
%%% ----------------------------------------------------------------------------

-spec filter_error(binary()) -> #beamtalk_error{}.
filter_error(Message) ->
    Err0 = beamtalk_error:new(type_error, 'Workspace'),
    Err1 = beamtalk_error:with_selector(Err0, 'flush:'),
    beamtalk_error:with_message(Err1, Message).
