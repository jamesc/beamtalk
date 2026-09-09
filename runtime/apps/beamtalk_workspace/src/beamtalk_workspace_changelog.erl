%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_changelog).
-behaviour(gen_server).

%%% **DDD Context:** Workspace Context

-moduledoc """
Append-only ChangeLog for live in-memory method mutations (ADR 0082 Phase 1).

The ChangeLog is the workspace-local record of every in-memory class/method
mutation made via the live-edit path (`Counter >> sel`, `compile:source:`,
`tryCompile:source:`, `Workspace newClass:at:`). It is both the *dirty-state*
tracker — "what has the running workspace changed relative to disk?" — and the
*undo* store. It lives in the **Workspace context** (not the REPL) because it is
consumed cross-surface by REPL, MCP, LSP, and the browser IDE.

This module owns the gen_server that serialises log appends (and, in a later
phase, flush-start reads). Live state is held in an ETS table for fast,
concurrent reads; durable state is a two-part on-disk layout per the ADR:

```
<workspace>/changes/
  changes.jsonl              % one compact JSON object per ChangeEntry
  sources/
    000142-source.bt        % the patched method body (source_ref)
    000142-prev.bt          % the prior on-disk body (prev_source_ref), if any
  archive/
    changes-<ts>.jsonl.gz    % rotated metadata segment
    sources-<ts>.tar.gz      % rotated source bodies
```

The metadata line stays small (well under ~300 chars) regardless of method
size because the bodies live in `sources/` as plain `.bt` files — `cat`, `less`,
`bt fmt`, `diff`, and syntax highlighting all work on them without escaping.

### ChangeEntry schema

Each `changes.jsonl` line is a JSON object with these fields (ADR 0082,
*ChangeLog format*):

| Field                  | Type                                                      | Notes |
|------------------------|-------------------------------------------------------------|-------|
| `ts`                   | integer (ms since epoch)                                     | append time |
| `seq`                  | integer                                                      | monotonic per workspace |
| `epoch`                | integer                                                      | bumped each workspace start |
| `class`                | string                                                       | e.g. `"Counter"` |
| `selector`             | string \| null                                              | null for `new-class` |
| `kind`                 | `"instance"`\|`"class"`\|`"new-class"`\|`"remove-method"`\|`"remove-class"`\|`"rename-class"`\|`"rename-method"` | open enum |
| `side`                 | `"instance"`\|`"class"`\|null                               | ADR 0112: explicit only for `"remove-method"`/`"rename-method"` (ADR 0114, BT-3269); legacy `"instance"`/`"class"`-kind entries derive it from `kind` (`entry_side/1`); always null for `"remove-class"`/`"rename-class"` (BT-3206/BT-3269 — no method-level target) |
| `source_ref`           | string \| null                                              | null for `"remove-method"`/`"remove-class"`/`"rename-class"`/`"rename-method"` (nothing replaces the deleted text; a multi-site entry's per-site bodies live under `sites` instead) |
| `prev_source_ref`      | string \| null                                              | null for `new-class`; the removed class's full prior source for `"remove-class"` (BT-3206); null for `"rename-class"`/`"rename-method"` (superseded by `sites`) |
| `sourceFile`           | string \| null                                              | null for stdlib/dynamic; also null for `"rename-class"`/`"rename-method"` (ambiguous for a multi-file entry — see `sites`) |
| `span`                 | `{start,end}` \| null                                       | null for `new-class` and `"remove-class"` (BT-3206 — no byte range within a whole-file removal); the excised span for `"remove-method"` (BT-2192's future flush-excise step); null for `"rename-class"`/`"rename-method"` (see `sites`) |
| `old_class`            | string \| null                                              | ADR 0114 (BT-3269): `"rename-class"`-only — the pre-rename class name |
| `old_selector`         | string \| null                                              | ADR 0114 (BT-3269): `"rename-method"`-only — the pre-rename selector (`selector` holds the new one) |
| `old_path`/`new_path`  | string \| null                                              | ADR 0114 (BT-3269): `"rename-class"`-only — the file path before/after a rename that also moves the backing file; null for a dynamic class |
| `sites`                | `[{sourceFile,span,source_ref,prev_source_ref}\|null,...] \| null` | ADR 0114 (BT-3269): `"rename-class"`/`"rename-method"`-only — `sites[0]` is the definition/declaration site (`null` only for a dynamic class with no backing file), `sites[1..]` are every other rewritten reference |
| `candidate_sites`      | `[{sourceFile,span},...] \| null`                           | ADR 0114 (BT-3269): `"rename-method"`-only — reported, never auto-rewritten senders; no `source_ref`/`prev_source_ref` since nothing here is ever spliced |
| `intent`               | `"durable"`\|`"ephemeral"`                                  | |
| `flushable`            | boolean                                                      | true iff in-project source; for `"rename-class"`/`"rename-method"`, true iff every entry in `sites` (never `candidate_sites`) resolves to a flushable file |
| `not_flushable_reason` | string \| null                                              | `"stdlib"`/`"dynamic"`/`"dependency:<path>"`/`"extension"`; `"rename-class"` is `"dynamic"`\|null only (ADR 0114 refuses stdlib/dependency before any entry exists); `"rename-method"` is `"stdlib"`\|`"dynamic"`\|`"dependency:<path>"`\|null — `"extension"` is not reachable there either (ADR 0114 § ChangeLog schema) |
| `author`               | string                                                       | session/tool id |
| `author_kind`          | `"human"`\|`"agent"`                                        | audit metadata |

### Restart semantics

The on-disk log survives workspace restart; the in-memory BEAM module state does
not. On startup the gen_server reads `changes.jsonl`, assigns a **fresh epoch**
(prior `max(epoch)` + 1), and tags every pre-existing entry as belonging to a
prior epoch. An entry is additionally tagged `orphan` when its recorded
`prev_source` no longer matches the current on-disk content of `sourceFile` (the
disk advanced — via VSCode/git/another flush — while the workspace was down).
Prior-epoch and orphan entries are excluded from the active dirty view; they
remain in the log for audit.

### Scope (Phase 1)

This module implements the gen_server, the append API, the two-part persistence,
restart epoch/orphan tagging, and the bounded ring with archive rotation. The
install hook that *emits* entries, `Workspace flush`, and the `change_log.bt`
stdlib facade are later phases (BT-2280 epic). In run mode (no workspace, no
`workspace_id`) the gen_server keeps state in ETS only and never touches disk —
release nodes do not start a workspace, so this code is a no-op there.
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Public API
-export([
    start_link/1,
    append/1,
    entries/0,
    active_entries/0,
    flushable_pending/0,
    mark_flushed/1,
    size/0,
    epoch/0,
    clear/0,
    find_revert_target/2,
    find_revert_target/3,
    %% ADR 0114 (BT-3270): per-site body persistence for the shared
    %% multi-site rewrite mechanism — see this function's doc.
    store_site_body/1
]).

%% Beamtalk FFI surface (ADR 0082 Phase 1, BT-2284). These build the data the
%% `change_log.bt` / `change_entry.bt` value objects wrap: each entry becomes a
%% `$beamtalk_class`-tagged map and `dirtyMethods/0` returns the per-class set
%% of dirty selectors. The FFI dispatches on the Beamtalk selector verbatim, so
%% these entry points are named in camelCase (`changeLog`, `dirtyMethods`) to
%% match the selectors used in `change_log.bt` / `workspace_interface.bt`. Called
%% via `(Erlang beamtalk_workspace_changelog) ...` from the compiled stdlib.
-export([
    changeLog/0,
    dirtyMethods/0,
    change_entries/0
]).

%% Accessors on the opaque entry type (used by callers and tests).
-export([
    entry_seq/1,
    entry_epoch/1,
    entry_class/1,
    entry_selector/1,
    entry_kind/1,
    known_entry_kinds/0,
    entry_side/1,
    entry_intent/1,
    entry_flushable/1,
    entry_not_flushable_reason/1,
    entry_flushed/1,
    entry_author_kind/1,
    entry_is_orphan/1,
    entry_is_prior_epoch/1,
    entry_source_file/1,
    entry_span/1,
    entry_source_ref/1,
    entry_prev_source_ref/1,
    read_source_body/1,
    read_prev_source_body/1,
    %% ADR 0114 (BT-3271): reads a `site()`'s own ref directly, for
    %% `beamtalk_workspace_flush`'s multi-site rename-class splice — see this
    %% function's own doc for why it cannot reuse `read_source_body/1`/
    %% `read_prev_source_body/1` verbatim.
    read_site_body/1,
    %% ADR 0114 (BT-3269).
    entry_old_class/1,
    entry_old_selector/1,
    entry_old_path/1,
    entry_new_path/1,
    entry_sites/1,
    entry_candidate_sites/1
]).

%% ADR 0114 (BT-3269): shadow-detection and flushability helpers for the
%% multi-site `'rename-class'`/`'rename-method'` kinds.
-export([
    target_key/1,
    sites_flushable/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Exported for tests only.
-export([changes_dir/1, entry_to_json/1, entry_from_json/1, body_delta/2, read_source_file/1]).

-define(ETS_TABLE, beamtalk_changelog_entries).
%% Bounded ring: keep at most this many entries on disk before rotating older
%% segments into archive/ (ADR 0082, "ChangeLog growth").
-define(MAX_ENTRIES, 1000).

%%% ----------------------------------------------------------------------------
%%% Types
%%% ----------------------------------------------------------------------------

%% `kind` is an open enum (ADR 0082): newer writers may add values this beam does
%% not know. Decoding maps any unrecognised value to `unknown` so history is
%% preserved across versions rather than dropped. `'remove-method'` is ADR
%% 0112's method-removal kind (BT-3187). `'class-def'` is ADR 0082's
%% extension for redefining an *existing* class's whole definition (BT-3248) —
%% the cockpit `:def` tab's "Compile" action, as opposed to `'new-class'`
%% (a brand-new class created via `newClass:at:`). `'rename-class'`/
%% `'rename-method'` are ADR 0114's `renameTo:`/`renameSelector:to:` kinds
%% (BT-3269) — the first two kinds whose rewrite spans a *set* of files
%% (`sites`/`candidate_sites`) rather than one, see those fields' docs below.
-type kind() ::
    instance
    | class
    | 'new-class'
    | 'class-def'
    | 'remove-method'
    | 'remove-class'
    | 'rename-class'
    | 'rename-method'
    | unknown.
%% ADR 0112: which method table a `'remove-method'` entry targets. Stored
%% explicitly only for that kind — legacy `instance`/`class`-kind patch
%% entries derive their side from `kind` itself (`entry_side/1`), so the field
%% is additive, not a breaking schema change (ADR 0112 § ChangeLog interaction).
%% ADR 0114 (BT-3269): `'rename-method'` stores `side` the same explicit way;
%% `'rename-class'` always has `side = undefined` (null) — a class identity
%% change has no method-table side.
-type side() :: instance | class.
-type intent() :: durable | ephemeral | unknown.
-type author_kind() :: human | agent | unknown.
-type span() :: #{start := non_neg_integer(), 'end' := non_neg_integer()}.

%% ADR 0114 (BT-3269): one rewritten reference location in a `'rename-class'`/
%% `'rename-method'` entry's `sites` list. `source_ref`/`prev_source_ref` name
%% the recorded pre/post rewrite bodies exactly like the top-level fields do
%% for a single-file kind (undefined for a site not yet populated with a
%% recorded body — the site-discovery/rewrite mechanism itself is BT-3270,
%% out of scope here). A bare `undefined` in place of a `site()` map (rather
%% than a map with `source_file = undefined`) is the ADR's documented
%% `sites[0] = null` case: a dynamic (ClassBuilder) class being renamed has no
%% backing file for its own declaration site to point at.
-type site() :: #{
    source_file := binary() | undefined,
    span := span() | undefined,
    source_ref := binary() | undefined,
    prev_source_ref := binary() | undefined
}.

%% ADR 0114 (BT-3269): one reported-but-never-rewritten sender in a
%% `'rename-method'` entry's `candidate_sites` list. No `source_ref`/
%% `prev_source_ref` — nothing here is ever spliced, so there is no prior/new
%% body to record (ADR 0114 § ChangeLog schema).
-type candidate_site() :: #{
    source_file := binary(),
    span := span()
}.

%% A ChangeEntry as stored in memory. Bodies are not kept in the record —
%% only the `source_ref` / `prev_source_ref` filenames — so the ETS footprint
%% stays small regardless of method size. The bodies live as files in sources/.
-record(entry, {
    seq :: non_neg_integer(),
    ts :: integer(),
    epoch :: non_neg_integer(),
    class :: binary(),
    selector :: binary() | undefined,
    kind :: kind(),
    %% `undefined` for every entry except a `'remove-method'` one — read via
    %% `entry_side/1`, never this field directly.
    side :: side() | undefined,
    %% `undefined` for a `'remove-method'` entry (ADR 0112: nothing replaces
    %% the deleted text, so there is no new body to store).
    source_ref :: binary() | undefined,
    prev_source_ref :: binary() | undefined,
    source_file :: binary() | undefined,
    span :: span() | undefined,
    %% ADR 0114 (BT-3269): `'rename-class'`-only — the pre-rename class name.
    %% `undefined` for every other kind.
    old_class :: binary() | undefined,
    %% ADR 0114 (BT-3269): `'rename-method'`-only — the pre-rename selector
    %% (`selector` itself holds the *new* selector, mirroring how `class`
    %% holds the *new* name for `'rename-class'`). `undefined` for every
    %% other kind.
    old_selector :: binary() | undefined,
    %% ADR 0114 (BT-3269): `'rename-class'`-only — the file path before/after
    %% a rename that also moves the backing file (or a pure `Workspace
    %% moveClass:to:` move). `undefined` for a dynamic class (no backing
    %% file) and for every other kind.
    old_path :: binary() | undefined,
    new_path :: binary() | undefined,
    %% ADR 0114 (BT-3269): `'rename-class'`/`'rename-method'`-only — the
    %% multi-site shape neither field above can express: `sites[0]` is always
    %% the definition/declaration site, `sites[1..]` are every other rewritten
    %% reference. `undefined` (not `[]`) for every other kind, matching the
    %% "undefined means not applicable" convention every other optional field
    %% here already follows.
    sites :: [site() | undefined] | undefined,
    %% ADR 0114 (BT-3269): `'rename-method'`-only — the reported, never
    %% auto-rewritten candidate sender list. `undefined` for every other kind
    %% (including `'rename-class'`, which has no candidate tier).
    candidate_sites :: [candidate_site()] | undefined,
    intent :: intent(),
    flushable :: boolean(),
    not_flushable_reason :: binary() | undefined,
    author :: binary(),
    author_kind :: author_kind(),
    %% True once a `Workspace flush` has written this entry's patch to disk
    %% (ADR 0082 Phase 2) — OR, for a `'remove-class'` entry specifically,
    %% once `Workspace changes revert:` has undone it (ADR 0113, BT-3208), OR,
    %% the same way, for a `'rename-class'`/`'rename-method'` entry once its
    %% own `revert:` has undone it (ADR 0114, BT-3274): no disk write
    %% happened, but the entry's effect is equally resolved and must equally
    %% drop out of the active/pending view. Persisted so the
    %% entry stays excluded from the active view across workspace restarts:
    %% history is preserved in the log for audit, but the entry is no longer
    %% considered "dirty". Don't read this field alone as "this reached disk"
    %% — for that, check the entry's `kind` too.
    flushed = false :: boolean(),
    %% Derived, in-memory only — not persisted (recomputed on restart).
    prior_epoch = false :: boolean(),
    orphan = false :: boolean()
}).

-opaque entry() :: #entry{}.

%% Input map accepted by append/1. Bodies (`source`, `prev_source`) are passed
%% in full; the gen_server writes them to sources/ and stores only the refs.
%% `source` is optional (ADR 0112): a `'remove-method'` entry has no new body
%% to store — omitting it leaves `source_ref` (and therefore the on-disk
%% `sources/<seq>-source.bt` file) absent, matching the ADR's `source_ref:
%% null` schema bullet.
-type append_input() :: #{
    class := binary(),
    kind := kind(),
    intent := intent(),
    flushable := boolean(),
    author := binary(),
    author_kind := author_kind(),
    selector => binary() | undefined,
    side => side(),
    source => binary() | undefined,
    prev_source => binary() | undefined,
    source_file => binary() | undefined,
    span => span() | undefined,
    not_flushable_reason => binary() | undefined,
    %% ADR 0114 (BT-3269): see the matching `#entry{}` fields' docs above.
    old_class => binary() | undefined,
    old_selector => binary() | undefined,
    old_path => binary() | undefined,
    new_path => binary() | undefined,
    sites => [site() | undefined] | undefined,
    candidate_sites => [candidate_site()] | undefined
}.

-export_type([
    entry/0,
    append_input/0,
    kind/0,
    side/0,
    intent/0,
    author_kind/0,
    span/0,
    site/0,
    candidate_site/0
]).

-record(state, {
    %% Absolute path to <workspace>/changes, or undefined in run mode
    %% (no workspace_id) — in run mode everything stays in ETS only.
    changes_dir :: string() | undefined,
    %% Path to changes.jsonl (undefined in run mode).
    log_path :: string() | undefined,
    %% Next sequence number to assign (monotonic across restarts).
    next_seq :: non_neg_integer(),
    %% Epoch for entries appended in this session (bumped each start).
    epoch :: non_neg_integer()
}).

%%% ----------------------------------------------------------------------------
%%% Public API
%%% ----------------------------------------------------------------------------

-doc """
Start the ChangeLog gen_server, registered locally under the module name.

`Config` is a map; the only field consulted in Phase 1 is `workspace_id`
(binary). When absent or `undefined` the server runs in *memory-only* mode
(run mode / tests with no workspace): ETS holds the live entries and nothing is
written to disk. When present, durable state lives under
`<home>/.beamtalk/workspaces/<workspace_id>/changes/`.
""".
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

-doc """
Append a ChangeEntry to the log.

Writes the source bodies to `sources/` and the metadata line to `changes.jsonl`
crash-safely (bodies first, then the metadata line that references them, so a
crash never leaves a metadata line pointing at a missing body), assigns the next
sequence number, and inserts the entry into ETS. Returns the assigned `seq`.

The append is serialised by the gen_server, so concurrent callers cannot
interleave sequence numbers or partial writes.
""".
-spec append(append_input()) -> {ok, non_neg_integer()} | {error, #beamtalk_error{}}.
append(Input) when is_map(Input) ->
    gen_server:call(?MODULE, {append, Input}).

-doc """
Return all entries (including prior-epoch and orphan), oldest first.

Returns `[]` when the ChangeLog server has not been started (the ETS table is
absent), so callers on a node without a workspace do not crash.

`?ETS_TABLE` is an `ordered_set` keyed by the monotonic `seq`, so table
traversal already yields ascending key order — no per-call sort needed.
""".
-spec entries() -> [entry()].
entries() ->
    case ets:info(?ETS_TABLE, id) of
        undefined ->
            [];
        _ ->
            [E || {_Seq, E} <- ets:tab2list(?ETS_TABLE)]
    end.

-doc """
Return only *active* entries — those from the current epoch that are not
orphaned and have not been flushed to disk. This is the dirty-state view that
`Workspace changes` is backed by; entries already written to disk by
`Workspace flush` (ADR 0082 Phase 2) drop out of this view but stay in the log
for audit.
""".
-spec active_entries() -> [entry()].
active_entries() ->
    [E || E <- entries(), is_active(E)].

%% Whether an entry is part of the active dirty view: current epoch, not
%% orphaned, and not yet flushed to disk. Shared by `active_entries/0` and the
%% shadow computation (`survivor_seqs/1`) so both agree on what "active" means.
-spec is_active(#entry{}) -> boolean().
is_active(#entry{prior_epoch = Prior, orphan = Orphan, flushed = Flushed}) ->
    (not Prior) andalso (not Orphan) andalso (not Flushed).

-doc """
Return the entries that are candidates for `Workspace flush` (ADR 0082 Phase 2):
durable intent, flushable, not yet flushed, and active (current epoch, not
orphaned). Ordered by sequence number ascending so the caller can apply them
in append order.
""".
-spec flushable_pending() -> [entry()].
flushable_pending() ->
    [
        E
     || E <- active_entries(),
        E#entry.intent =:= durable,
        E#entry.flushable
    ].

-doc """
Mark the entries with the given sequence numbers as flushed.

Called by `Workspace flush` after successfully writing their patches to disk.
The entries stay in the on-disk log (audit history is preserved) but drop out
of the active view. The on-disk metadata segment is rewritten so the flushed
flag survives workspace restart.

Returns `ok` on success and `{error, Reason}` if rewriting the on-disk log
segment fails (disk full, permissions, etc.); callers should log and continue
rather than retry, since the in-memory entries' flushed flag is only flipped
after a successful rewrite. Idempotent: passing the empty list or seqs not in
the log is a successful no-op (defensive callers can pass the full pending set
without checking emptiness first).
""".
-spec mark_flushed([non_neg_integer()]) -> ok | {error, term()}.
mark_flushed([]) ->
    ok;
mark_flushed(Seqs) when is_list(Seqs) ->
    gen_server:call(?MODULE, {mark_flushed, Seqs}).

-doc """
Total number of entries in the log (all epochs).

Returns `0` when the ChangeLog server has not been started (the ETS table is
absent).
""".
-spec size() -> non_neg_integer().
size() ->
    case ets:info(?ETS_TABLE, size) of
        undefined -> 0;
        N -> N
    end.

-doc "The epoch assigned to entries appended in this session.".
-spec epoch() -> non_neg_integer().
epoch() ->
    gen_server:call(?MODULE, epoch).

-doc """
Discard all entries from the in-memory log and truncate the on-disk metadata
segment. Source bodies in `sources/` are left in place (they are cheap and a
later `revert:`/audit flow may still want them; rotation prunes them). Used by
`Workspace changes clear` (ADR 0082 Phase 4) and by tests.
""".
-spec clear() -> ok.
clear() ->
    gen_server:call(?MODULE, clear).

-doc """
Find the most recent active ChangeEntry for `(Class, Selector)` and return its
recorded prior source body (ADR 0082 Phase 4, BT-2290; add/new-class/class-side
extensions BT-2663/BT-2664/BT-2665; `remove-class` extension ADR 0113, BT-3208).

Used by `Workspace changes revert: aMethod` to look up the pre-patch state that
must be restored. Returns:

  - `{ok, PrevBody, Entry}` when an active entry for the target exists *and* its
    prior body can be recovered (from a recorded `prev_source_ref`, or from the
    current on-disk body) — the typical method-*modify* revert path. The entry's
    `kind` (`instance`/`class`) tells the caller which side to re-install on. This
    is also the path a `'remove-method'` entry reaches: its `prev_source_ref` is
    the removed method's pre-removal body, so re-installing it (on the entry's
    recorded `side`) is exactly what undoes the removal (ADR 0112, BT-3187).
  - `{remove, Entry}` when the most recent active entry is an *addition*: a
    brand-new method whose selector did not exist before the patch (no recorded
    prior body AND the selector is absent from the on-disk source / the class has
    no on-disk source), or a `new-class` entry. The pre-patch state was "this did
    not exist", so revert is a removal rather than a body re-install.
  - `{reinstall_class, PrevBody, Entry}` when the most recent active entry is a
    `'remove-class'` (ADR 0113, BT-3208): the pre-removal state was "this class
    existed", recorded as the whole-file `prev_source_ref` `capture_class_removal_
    snapshot/1` captures before teardown (falling back to a direct read of the
    entry's own `sourceFile` — `recover_class_prev_from_disk/1` — when the
    recorded body itself is unreadable, e.g. pruned by ChangeLog rotation).
    Revert recompiles and reinstalls the whole class from `PrevBody`, not a
    single-method patch.
  - `{revert_rename, Entry}` when the most recent active entry is a
    `'rename-class'`/`'rename-method'` (ADR 0114, BT-3274): a multi-site
    target, not a single prior body — the caller (`beamtalk_repl_loader:
    revert_rename_sites/1`) rewrites every one of `Entry`'s own `sites` back
    to its own recorded `prev_source_ref`, against that site's own recorded
    location, rather than a single `PrevBody` this function could return.
  - `{error, no_entry}` when no active entry targets `(Class, Selector)`
    (nothing to revert: either never patched, or already reverted/flushed).
  - `{error, no_prev_source}` when the most recent entry is a *modify* whose
    prior body is genuinely unrecoverable (the recorded body is missing AND the
    on-disk source exists but the span can no longer be resolved — e.g. the file
    advanced under us), or a `'remove-class'` entry whose prior body cannot be
    recovered from either the ChangeLog's recorded copy or the entry's own
    `sourceFile` (no `sourceFile` recorded at all — a dynamically-defined class
    — or the file itself can no longer be read). We must not silently delete/skip
    a method or class that existed before, so this stays a loud error rather than
    a removal or a no-op.

`Class` is the unsuffixed display name as a binary; `Selector` is an atom or a
binary (an atom is converted to a binary so the comparison matches the entry's
recorded selector). A `new-class` entry has `selector = undefined`; pass the
new-class selector placeholder atom `'new-class'` (or the binary `<<"new-class">>`)
to reach it — `find_revert_target(Class, 'new-class')` resolves the class's
new-class entry and yields a `{remove, Entry}` outcome. A `'remove-class'` entry
also has `selector = undefined` (ADR 0113: "no method-level target"), so the same
placeholder reaches it too — whichever of a class's `new-class`/`remove-class`
entries has the higher `seq` wins, matching the general highest-seq-candidate rule.

`find_revert_target/2` matches candidates on `(Class, Selector)` only — the
highest-seq active candidate wins regardless of side. Use `find_revert_target/3`
with an explicit `Side` (ADR 0112, BT-3187) when the caller knows which side it
means to revert: same-selector instance/class-side entries (e.g. an instance-side
patch and a later class-side `'remove-method'` entry for the same selector name)
are otherwise indistinguishable by `(Class, Selector)` alone, and the wrong one —
whichever has the higher `seq` — would be selected.
""".
-spec find_revert_target(binary(), atom() | binary()) ->
    {ok, binary(), entry()}
    | {remove, entry()}
    | {reinstall_class, binary(), entry()}
    | {revert_rename, entry()}
    | {error, no_entry | no_prev_source}.
find_revert_target(Class, Selector) ->
    find_revert_target(Class, Selector, undefined).

-doc """
Like `find_revert_target/2`, but restricts candidates to the given `Side`
(`instance` | `class`) when it is not `undefined` (ADR 0112, BT-3187). Side is
resolved per-entry via `entry_side/1`, so it matches both a `'remove-method'`
entry's explicit `side` field and a legacy `instance`/`class`-kind patch's
`kind`-derived side. Passing `undefined` reproduces `find_revert_target/2`'s
side-agnostic behavior (used by callers, such as `revert_method/2`'s
`(Class, Selector)`-only surface, that have no side information to give).
""".
-spec find_revert_target(binary(), atom() | binary(), side() | undefined) ->
    {ok, binary(), entry()}
    | {remove, entry()}
    | {reinstall_class, binary(), entry()}
    | {revert_rename, entry()}
    | {error, no_entry | no_prev_source}.
find_revert_target(Class, Selector, Side) when is_binary(Class) ->
    SelectorBin = revert_selector_binary(Selector),
    %% A `new-class` entry records `selector = undefined`; callers reach it with
    %% the `new-class` placeholder selector, which we map back to `undefined` so
    %% the candidate filter matches the stored value.
    MatchSelector = match_selector(SelectorBin),
    Candidates = lists:filter(
        fun(E) ->
            E#entry.class =:= Class andalso
                E#entry.selector =:= MatchSelector andalso
                (Side =:= undefined orelse entry_side(E) =:= Side) andalso
                (not E#entry.prior_epoch) andalso
                (not E#entry.orphan) andalso
                (not E#entry.flushed)
        end,
        entries()
    ),
    case lists:reverse(lists:keysort(#entry.seq, Candidates)) of
        [] ->
            {error, no_entry};
        [#entry{kind = 'new-class'} = Entry | _] ->
            %% Reverting a new-class creation removes the class (BT-2664).
            {remove, Entry};
        [#entry{kind = 'remove-class'} = Entry | _] ->
            %% Reverting a class removal recompiles and reinstalls the whole
            %% class from its recorded prior source (ADR 0113, BT-3208). If
            %% the recorded `prev_source_ref` body is unreadable (ChangeLog
            %% rotation pruned `sources/`, or a rare fs race), fall back to
            %% the class's own on-disk file — mirroring the modify path's
            %% `recover_prev_from_disk/1`, but simpler: a class removal has no
            %% byte span to resolve, so a direct whole-file read suffices, and
            %% it IS the pre-removal body whenever the removal itself was
            %% never flushed (the only case reachable here — a flushed entry
            %% is inactive and never becomes a candidate above).
            case read_prev_source_body(Entry) of
                {ok, Body} -> {reinstall_class, Body, Entry};
                {error, _} -> recover_class_prev_from_disk(Entry)
            end;
        [#entry{kind = Kind} = Entry | _] when
            Kind =:= 'rename-class'; Kind =:= 'rename-method'
        ->
            %% ADR 0114 (BT-3274): a multi-site target — the caller rewrites
            %% every one of `Entry`'s own `sites` back to its own recorded
            %% `prev_source_ref`, so there is no single `PrevBody` for this
            %% function to resolve the way the modify/reinstall-class arms
            %% above do.
            {revert_rename, Entry};
        [#entry{prev_source_ref = undefined} = Entry | _] ->
            %% No recorded prior body. Either the method existed on disk before
            %% the patch (a modify, whose unflushed disk body IS its pre-patch
            %% body — resilience for entries predating source attribution), or it
            %% is a brand-new method (an *add*, whose pre-patch state is "absent"
            %% → revert = removal, BT-2663). recover_prev_from_disk/1 tells them
            %% apart.
            recover_prev_from_disk(Entry);
        [Entry | _] ->
            case read_prev_source_body(Entry) of
                {ok, Body} -> {ok, Body, Entry};
                {error, _} -> recover_prev_from_disk(Entry)
            end
    end.

%% Map a revert selector binary to the value stored on the matching entry: the
%% `new-class` placeholder resolves to `undefined` (a new-class entry stores no
%% selector); any other selector matches verbatim.
-spec match_selector(binary()) -> binary() | undefined.
match_selector(<<"new-class">>) -> undefined;
match_selector(SelectorBin) -> SelectorBin.

%% Reconstruct a method's pre-patch state from its on-disk source file when no
%% `prev_source' was recorded. Distinguishes a *modify* (the selector exists on
%% disk → return its current body as the body to re-install) from an *add* (the
%% selector is absent on disk, or the class has no on-disk source → revert is a
%% removal). Returns:
%%
%%   - `{ok, Body, Entry}'  — modify: re-install the recovered prior body.
%%   - `{remove, Entry}'    — add: remove the just-added method (BT-2663).
%%   - `{error, no_prev_source}' — modify whose prior body is genuinely
%%     unrecoverable: the file can't be read, the span no longer resolves, or
%%     the selector is absent but the entry recorded a `prev_source_ref' (so it
%%     was definitively a modify, not an add); never silently delete it.
%%
%% Invariant + limit: the on-disk body returned for a modify is the true
%% pre-patch body only while the entry is unflushed AND the file has not been
%% edited externally (VSCode/git) since the patch. The normal-flow entries that
%% *do* record `prev_source' (BT-2553 follow-up) don't reach here; this is a
%% best-effort fallback. A later flush still runs its own byte-span/prev_source
%% conflict check before writing.
-spec recover_prev_from_disk(entry()) ->
    {ok, binary(), entry()} | {remove, entry()} | {error, no_prev_source}.
recover_prev_from_disk(
    #entry{source_file = File, class = Class, selector = Selector} = Entry
) when
    is_binary(File), is_binary(Selector)
->
    %% `resolve_method_span/4` only accepts `instance` | `class` for `Side`; the
    %% entry's raw `kind` can be `'remove-method'` (ADR 0112, BT-3187), which
    %% would always fail with `bad_argument` here. `entry_side/1` normalises
    %% both the legacy `instance`/`class`-kind shape and the explicit `side`
    %% field a `'remove-method'` entry carries.
    Side = entry_side(Entry),
    case file:read_file(File) of
        {ok, DiskSource} ->
            case beamtalk_compiler:resolve_method_span(DiskSource, Class, Selector, Side) of
                {ok, _Span, PrevBody} ->
                    %% Selector present on disk → a modify; re-install the body.
                    {ok, PrevBody, Entry};
                {error, selector_not_found, _} ->
                    %% The file exists but the selector is absent. Disambiguate
                    %% add-vs-modify by whether the entry recorded a prior body:
                    case Entry#entry.prev_source_ref of
                        undefined ->
                            %% No recorded prior body AND absent on disk → a
                            %% brand-new method added live; its pre-patch state
                            %% is "absent", so revert removes it (BT-2663).
                            %%
                            %% Residual ambiguity (accepted, unavoidable): a
                            %% pre-BT-2553 *modify* entry also carries no
                            %% prev_source_ref, so if its method was externally
                            %% removed from the file (git restore / editor
                            %% revert) AFTER the live patch, we cannot tell it
                            %% apart from an add and will treat revert as a
                            %% removal. Normal-flow entries record prev_source_ref
                            %% (BT-2553) and never reach this branch.
                            {remove, Entry};
                        _Ref ->
                            %% The entry DID record a prev_source_ref — it is
                            %% definitively a *modify*; we only reached the disk
                            %% probe because the body file was unreadable
                            %% (rotation/cleanup/fs issue). The selector being
                            %% absent on disk now does NOT make it an add — never
                            %% silently delete a method the user only modified.
                            {error, no_prev_source}
                    end;
                _ ->
                    %% Any other resolution failure (ambiguous / parse error /
                    %% file advanced) leaves the prior body genuinely
                    %% unrecoverable — refuse loudly rather than delete.
                    {error, no_prev_source}
            end;
        {error, _} ->
            %% The recorded source file can no longer be read (deleted, moved,
            %% permissions). A modify whose prior body cannot be recovered must
            %% NOT be silently deleted — surface a loud error (BT-2663 AC).
            {error, no_prev_source}
    end;
recover_prev_from_disk(_Entry) ->
    %% No on-disk source attribution (dynamic / source-less class), or any other
    %% shape we cannot resolve against disk. We cannot positively distinguish an
    %% *add* from a *modify* here — there is no `prev_source` and no file to probe
    %% for the selector — so refuse loudly rather than risk deleting a method that
    %% existed before the patch (BT-2663 AC: "never a silent delete"). Positive
    %% add evidence only comes from `selector_not_found` against a readable source
    %% file (handled above), which is the new-method-on-a-project-class case the
    %% LiveView add-revert flow produces.
    {error, no_prev_source}.

-doc """
Fallback for a `'remove-class'` entry (ADR 0113, BT-3208) whose recorded
`prev_source_ref` body could not be read via `read_prev_source_body/1` — the
ChangeLog's bounded ring (`?MAX_ENTRIES`) rotated `sources/` before the
revert happened, or a rare fs race. Unlike a method's
`recover_prev_from_disk/1`, there is no span to resolve: the class's own
recorded `sourceFile`, read whole-file, IS the pre-removal body whenever the
removal itself was never flushed — the only case that reaches
`find_revert_target/3`'s `'remove-class'` branch at all (a flushed entry is
inactive and never becomes a candidate).

Returns `{error, no_prev_source}` — never a silent no-op — when the entry has
no recorded `sourceFile` (a dynamically-defined class, nothing on disk to
begin with) or the file can no longer be read (deleted/moved/permissions).
""".
-spec recover_class_prev_from_disk(entry()) ->
    {reinstall_class, binary(), entry()} | {error, no_prev_source}.
recover_class_prev_from_disk(#entry{source_file = File} = Entry) when is_binary(File) ->
    case file:read_file(File) of
        {ok, Body} -> {reinstall_class, Body, Entry};
        {error, _} -> {error, no_prev_source}
    end;
recover_class_prev_from_disk(_Entry) ->
    {error, no_prev_source}.

%% Normalise the selector argument: callers may pass an atom or a binary.
-spec revert_selector_binary(atom() | binary()) -> binary().
revert_selector_binary(Sel) when is_binary(Sel) -> Sel;
revert_selector_binary(Sel) when is_atom(Sel) -> atom_to_binary(Sel, utf8).

%%% ----------------------------------------------------------------------------
%%% Beamtalk FFI surface (ADR 0082 Phase 1, BT-2284)
%%% ----------------------------------------------------------------------------
%%% These functions translate the opaque `#entry{}` records into the
%%% `$beamtalk_class`-tagged maps that the `change_log.bt` / `change_entry.bt`
%%% value objects wrap. `change_entries/0` returns *every* entry (the
%%% `ChangeLog` object holds the full set so `select:` can still reach
%%% prior-epoch / orphan entries); the active/dirty filtering lives on the
%%% Beamtalk side using the per-entry `active` and `shadowed` flags (the default
%%% pending view is active-and-not-shadowed, collapsing repeated patches/reverts
%%% of one method to its latest entry). `dirtyMethods/0` is computed here because
%%% it groups *active* entries by class into Beamtalk `Set` values (the Set
%%% already collapses duplicate selectors, so it needs no shadow filter).

-doc """
Return the workspace ChangeLog as a `ChangeLog` value-object map.

The map is tagged `'$beamtalk_class' => 'ChangeLog'` and carries the full set
of entries (as `ChangeEntry` maps) under `entries`, so the `change_log.bt`
object can apply the active-vs-full filtering in Beamtalk. This is what
`Workspace changes` returns. Called via
`(Erlang beamtalk_workspace_changelog) changeLog`.
""".
%% The `'$beamtalk_class' := 'ChangeLog'` tag lets the type checker (via
%% beamtalk_spec_reader) infer this FFI result as the `ChangeLog` Beamtalk
%% class rather than a bare `Dictionary`, matching `WorkspaceInterface>>changes`
%% declared `-> ChangeLog` return type. Mirrors `beamtalk_ets:t()`.
-spec changeLog() -> #{'$beamtalk_class' := 'ChangeLog', entries := [map()]}.
changeLog() ->
    #{
        '$beamtalk_class' => 'ChangeLog',
        entries => change_entries()
    }.

-doc """
Return every ChangeLog entry as a `ChangeEntry` value-object map, oldest first.

Each map is tagged `'$beamtalk_class' => 'ChangeEntry'` so the runtime
dispatches the instance methods defined in `change_entry.bt`. The full set is
returned (including prior-epoch and orphan entries) so `ChangeLog select:` can
still reach them; the default collection views filter on the per-entry `active`
flag in Beamtalk. Internal helper for `changeLog/0`; not used by the stdlib API
(`change_log.bt` only ever calls `changeLog`) — exported for tests/helpers.
""".
-spec change_entries() -> [map()].
change_entries() ->
    All = entries(),
    Survivors = survivor_seqs(All),
    [entry_to_value(E, Survivors) || E <- All].

%% For each `(class, selector, side)` target, the seq of the most-recent
%% *active* entry — the "survivor" that `Workspace flush` would apply and that
%% the pending-changes view shows. Every other active entry for the same
%% target is *shadowed*: an older patch (or a patch superseded by a revert,
%% since a revert is itself a patch — ADR 0082 "Undo") that newer state
%% replaced. Mirrors `beamtalk_workspace_flush:target_key/1` /
%% `shadow_duplicates/1` so the displayed dirty set matches what flush
%% actually writes. Inactive entries (prior-epoch / orphan / flushed) never
%% survive and never shadow.
%%
%% Keyed on `(class, selector, side)`, not just `(class, selector)` (ADR 0112's
%% required fix to ADR 0082's shipped shadow key): an instance-side patch of
%% `Counter >> #foo` and a class-side `Counter class removeSelector: #foo`
%% share `(class, selector)` but target different method tables and must not
%% shadow each other.
-spec survivor_seqs([#entry{}]) -> #{shadow_key() => non_neg_integer()}.
survivor_seqs(Entries) ->
    lists:foldl(
        fun(E, Acc) ->
            case is_active(E) of
                false ->
                    Acc;
                true ->
                    Key = shadow_key(E),
                    case Acc of
                        #{Key := Max} when Max >= E#entry.seq -> Acc;
                        _ -> Acc#{Key => E#entry.seq}
                    end
            end
        end,
        #{},
        Entries
    ).

%% The `(class, selector, side)` shadow-key tuple for `E` — shared by
%% `survivor_seqs/1` and `entry_to_value/2` so both agree on what shadows what.
%%
%% Whole-class-level entries (`'new-class'`, `'class-def'`, `'remove-class'`)
%% carry no `selector`, so without a tie-breaker they would all collide on
%% the same `(class, undefined, undefined)` key. Concretely: a `'class-def'`
%% redefinition (BT-3248, always `flushable: false`) of a class whose
%% `'new-class'` creation (flushable, still pending) has not yet been
%% flushed would win the shadow slot by seq and mark the `'new-class'` entry
%% `shadowed`, hiding the entry `Workspace flush` actually acts on from the
%% CHANGES dock's pending view — the dock would show only the non-flushable
%% edit, silently misrepresenting what flush is about to write. Keying on
%% `kind` too for selector-less entries keeps `'new-class'`/`'class-def'`/
%% `'remove-class'` in separate shadow buckets so each stays independently
%% visible.
-type shadow_key() ::
    {binary(), binary() | undefined, side() | undefined} | {binary(), undefined, kind()}.
-spec shadow_key(#entry{}) -> shadow_key().
shadow_key(#entry{selector = undefined} = E) ->
    {E#entry.class, undefined, E#entry.kind};
shadow_key(E) ->
    {E#entry.class, E#entry.selector, entry_side(E)}.

%%% ----------------------------------------------------------------------------
%%% Per-site shadow-detection key (ADR 0114, BT-3269)
%%% ----------------------------------------------------------------------------

-doc """
Per-site shadow-detection keys for `Entry` (ADR 0114, BT-3269).

Every existing kind targets exactly one file, so `shadow_key/1`'s single
tuple already identifies "what does this entry patch" unambiguously — that
is exactly the "does a newer edit replace this entry for the same
`(class, selector, side)` (or whole-class) target" dirty-view question
`survivor_seqs/1` answers. A `'rename-class'`/`'rename-method'` entry's
`sites` list breaks the one-entry-one-target assumption underneath that
question: renaming `Counter` might rewrite a reference inside `widget.bt`,
and an *unrelated*, independently issued rename of `Widget` might also
rewrite a (different) reference inside that very same file. Whole-entry
keying cannot express "does a newer edit's rewritten location overlap this
older edit's rewritten location" — that question needs a key per rewritten
location, not per entry.

Returns one key per site for `'rename-class'`/`'rename-method'` — a `null`
site (the dynamic-class case, ADR 0114 § ChangeLog schema) is skipped, since
there is no location to key — and a single-element list wrapping
`shadow_key/1`'s existing tuple for every other kind, so a caller folding
over "every target this entry touches" needs no kind-specific branch.
`candidate_sites` are never included: they are never rewritten (ADR 0114),
so they have no shadow-detection role.

Each site's key also carries the entry's own rename identity —
`class`/`old_class` for `'rename-class'`; `class`/`selector`/`old_selector`/
`side` for `'rename-method'` — alongside the site's own `sourceFile`/`span`,
so two independent renames that happen to touch the same file are never
conflated into a false shadow relationship purely because they share a
file: only a genuinely repeated edit of the *same* rename at the *same*
location collides.

Exported for the future multi-site rewrite mechanism (BT-3270) and for
tests. Not yet wired into `survivor_seqs/1`/the `ChangeEntry` `shadowed`
flag — `'rename-class'`/`'rename-method'` flush and dirty-view integration
is out of scope for BT-3269 (schema only).
""".
-spec target_key(entry()) -> [term()].
target_key(#entry{kind = 'rename-class'} = E) ->
    [
        {'rename-class', E#entry.class, E#entry.old_class, site_location(S)}
     || S <- sites_or_empty(E), S =/= undefined
    ];
target_key(#entry{kind = 'rename-method'} = E) ->
    [
        {'rename-method', E#entry.class, E#entry.selector, E#entry.old_selector, entry_side(E),
            site_location(S)}
     || S <- sites_or_empty(E), S =/= undefined
    ];
target_key(E) ->
    [shadow_key(E)].

-spec sites_or_empty(#entry{}) -> [site() | undefined].
sites_or_empty(#entry{sites = undefined}) -> [];
sites_or_empty(#entry{sites = Sites}) -> Sites.

-spec site_location(site()) -> {binary() | undefined, span() | undefined}.
site_location(#{source_file := SourceFile, span := Span}) ->
    {SourceFile, Span}.

-doc """
Fold per-site flushability verdicts into the single `flushable`/
`not_flushable_reason` pair a `'rename-class'`/`'rename-method'` ChangeEntry
records (ADR 0114 § ChangeLog schema): `true` iff every entry in `sites` is
flushable, `false` with the first non-flushable site's reason otherwise.
`candidate_sites` are never consulted — flush never writes them, so a
candidate site being e.g. a stdlib sender must never block an otherwise
clean rename (ADR 0114 explicitly calls this out: "a single incidental
stdlib candidate sender would leave the whole rename stuck forever").

Callers classify each site themselves before folding — reusing
`beamtalk_repl_loader:classify_source_file/1`/`no_source_reason/1` exactly
as the single-file kinds already do (ADR 0112), rather than this module
re-deriving that classification, which would duplicate it: this module has
no class-registry access, and a bare `sourceFile` cannot distinguish
"stdlib" from "dynamic" the way a class name can (`no_source_reason/1`
needs `code:which/1` on the class's own module).

Order matters only for which reason is reported when several sites
disagree: the first non-flushable entry in `Classifications` wins, so
callers should classify sites in the same order they appear in `sites`
(definition/declaration site first) for a deterministic, sites[0]-first
reason.
""".
-spec sites_flushable([flushable | {not_flushable, binary()}]) ->
    {boolean(), binary() | undefined}.
sites_flushable(Classifications) ->
    case
        lists:filtermap(
            fun
                (flushable) -> false;
                ({not_flushable, Reason}) -> {true, Reason}
            end,
            Classifications
        )
    of
        [] -> {true, undefined};
        [Reason | _] -> {false, Reason}
    end.

-doc """
Return the dirty methods derived from the *active* entries as a Beamtalk
Dictionary `#{ClassSymbol => Set(selectorSymbol)}`.

Only active entries (current epoch, not orphaned) contribute, matching the ADR's
"dirty state" semantics. New-class entries (no selector) are recorded under the
class with the placeholder selector `#'new-class'`, mirroring the ADR REPL example
`DoubleCounterTest -> #'new-class'`. Called via
`(Erlang beamtalk_workspace_changelog) dirtyMethods`.
""".
-spec dirtyMethods() -> #{atom() => map()}.
dirtyMethods() ->
    Active = active_entries(),
    Grouped = lists:foldl(
        fun(E, Acc) ->
            ClassSym = binary_to_atom(E#entry.class, utf8),
            Sel = dirty_selector(E),
            Existing = maps:get(ClassSym, Acc, []),
            Acc#{ClassSym => [Sel | Existing]}
        end,
        #{},
        Active
    ),
    maps:map(fun(_Class, Selectors) -> beamtalk_set:from_list(Selectors) end, Grouped).

%% The selector recorded for the dirty-methods view. Method patches use their
%% own selector; new-class entries (selector = undefined) use the `#new-class`
%% placeholder so the per-class entry is still visible. A `'class-def'` entry
%% (redefinition of an *existing* class's whole definition, BT-3248) also
%% carries no selector — it gets its own `#'class-def'` placeholder rather
%% than reusing `#new-class`, so the dirty view does not misreport a
%% redefinition as a brand-new class. `'rename-class'` (ADR 0114, BT-3269)
%% gets the same treatment for the same reason — it must not be confused
%% with either a brand-new class or a whole-definition redefinition.
-spec dirty_selector(#entry{}) -> atom().
dirty_selector(#entry{kind = 'class-def', selector = undefined}) -> 'class-def';
dirty_selector(#entry{kind = 'rename-class', selector = undefined}) -> 'rename-class';
dirty_selector(#entry{selector = undefined}) -> 'new-class';
dirty_selector(#entry{selector = Sel}) -> binary_to_atom(Sel, utf8).

%% Build a `ChangeEntry` value-object map from an `#entry{}` record. Field keys
%% match the `field:` declarations in `change_entry.bt`; `self.field` reads them.
%% Atoms (selector, kind, intent, authorKind) are surfaced as Beamtalk Symbols;
%% the derived `active` flag is `true` iff the entry is current-epoch, not an
%% orphan, and not flushed (the default dirty view). The derived `shadowed` flag
%% is `true` iff the entry is active but a *newer* active entry exists for the
%% same `(class, selector, side)` target — an older patch superseded by a
%% later patch/revert. `Survivors` maps each `(class, selector, side)` shadow
%% key (`shadow_key/1`) to its surviving (highest) seq.
-spec entry_to_value(#entry{}, map()) -> map().
entry_to_value(#entry{} = E, Survivors) ->
    Active = is_active(E),
    Shadowed =
        Active andalso
            maps:get(shadow_key(E), Survivors, E#entry.seq) =/= E#entry.seq,
    %% Only the live pending candidates (active, not shadowed) are diffed against
    %% disk — that bounds the file-read + parse work to the dirty set, and the
    %% rest are excluded from the pending view anyway.
    {Clean, Diff} =
        case Active andalso not Shadowed of
            true -> method_delta(E);
            false -> {false, undefined}
        end,
    #{
        '$beamtalk_class' => 'ChangeEntry',
        seq => E#entry.seq,
        className => binary_to_atom(E#entry.class, utf8),
        selector => selector_symbol(E#entry.selector),
        kind => E#entry.kind,
        side => side_symbol(entry_side(E)),
        intent => E#entry.intent,
        flushable => E#entry.flushable,
        authorKind => E#entry.author_kind,
        sourceFile => source_file_value(E#entry.source_file),
        orphan => E#entry.orphan,
        priorEpoch => E#entry.prior_epoch,
        flushed => E#entry.flushed,
        active => Active,
        shadowed => Shadowed,
        clean => Clean,
        diff => diff_value(Diff),
        %% ADR 0114 (BT-3269/BT-3284): `undefined` for every kind except
        %% `'rename-class'` (`oldClass`) / `'rename-method'` (`oldSelector`)
        %% — `selector_symbol/1` is a generic binary()|undefined -> atom()|nil
        %% converter, reused here rather than duplicated for the class-name
        %% case (it never inspects *which* field it was called for).
        oldClass => selector_symbol(E#entry.old_class),
        oldSelector => selector_symbol(E#entry.old_selector)
    }.

%% nil for "no diff" (clean / not computable), so Beamtalk reads it as the nil
%% object; otherwise the unified-diff binary.
-spec diff_value(binary() | undefined) -> binary() | nil.
diff_value(undefined) -> nil;
diff_value(Diff) when is_binary(Diff) -> Diff.

%% Compute the net delta of a pending entry against the current on-disk body
%% (ADR 0082 Phase 5+, BT-2575): `{Clean, Diff}` where `Clean` is true iff the
%% installed in-memory body matches disk (so the entry has been reverted back to
%% its on-disk state and should drop out of the pending view), and `Diff` is the
%% on-disk → in-memory unified diff (or `undefined` when clean or not
%% computable). Best-effort: any failure (no workspace, unreadable file, parse
%% error, non-method entry) degrades to `{false, undefined}` — the entry stays
%% visible as pending without a diff, never crashing the listing.
-spec method_delta(#entry{}) -> {boolean(), binary() | undefined}.
method_delta(#entry{kind = 'new-class'} = E) ->
    %% A new class has no on-disk counterpart until flush — always pending,
    %% rendered as an all-added diff of the full class source.
    case read_source_body(E) of
        {ok, MemBody} -> {false, beamtalk_workspace_diff:unified(<<>>, MemBody)};
        _ -> {false, undefined}
    end;
method_delta(#entry{kind = Kind, selector = Selector, source_file = File} = E) when
    (Kind =:= instance orelse Kind =:= class), is_binary(Selector), is_binary(File)
->
    try
        case {read_source_body(E), file:read_file(File)} of
            {{ok, MemBody}, {ok, DiskSource}} ->
                body_delta(disk_method_body(DiskSource, E#entry.class, Selector, Kind), MemBody);
            _ ->
                {false, undefined}
        end
    catch
        _:_ -> {false, undefined}
    end;
method_delta(#entry{kind = 'class-def', source_file = File} = E) when is_binary(File) ->
    %% BT-3248: same disk-vs-memory delta as an instance/class-kind method
    %% patch above, just resolved at whole-class granularity
    %% (`resolve_class_span/2` instead of `resolve_method_span/4`) — a
    %% redefinition of an *existing* class always has a prior on-disk body to
    %% diff against (unlike `'new-class'`, which never does).
    try
        case {read_source_body(E), file:read_file(File)} of
            {{ok, MemBody}, {ok, DiskSource}} ->
                body_delta(disk_class_body(DiskSource, E#entry.class), MemBody);
            _ ->
                {false, undefined}
        end
    catch
        _:_ -> {false, undefined}
    end;
method_delta(_E) ->
    {false, undefined}.

%% The method's current body on disk. A selector absent from the file is a
%% brand-new method added live — its prior body is empty (the whole patch is an
%% addition). Any other resolution error throws so the caller degrades to "no
%% diff" rather than reporting a misleading clean/dirty verdict.
-spec disk_method_body(binary(), binary(), binary(), instance | class) -> binary().
disk_method_body(DiskSource, Class, Selector, Kind) ->
    case beamtalk_compiler:resolve_method_span(DiskSource, Class, Selector, Kind) of
        {ok, _Span, Body} -> Body;
        {error, selector_not_found, _} -> <<>>;
        {error, _Reason, _Msg} -> throw(span_unresolved)
    end.

%% The class's current whole-definition body on disk (BT-3248). Unlike a
%% method's `selector_not_found` case, a `'class-def'` entry only ever exists
%% for a class this ChangeLog already knows had a prior tracked source (see
%% `beamtalk_repl_loader:emit_class_def_entries/3`'s doc), so the class is
%% expected to resolve on disk; any resolution failure throws so the caller
%% degrades to "no diff" rather than reporting a misleading verdict.
-spec disk_class_body(binary(), binary()) -> binary().
disk_class_body(DiskSource, Class) ->
    case beamtalk_compiler:resolve_class_span(DiskSource, Class) of
        {ok, _Span, Body} -> Body;
        {error, _Reason, _Msg} -> throw(span_unresolved)
    end.

-doc """
Compare an on-disk method body with the installed in-memory body and return
`{Clean, Diff}` (ADR 0082, BT-2575). Both sides are normalised first —
trailing whitespace trimmed and the common leading indentation stripped — so the
comparison and diff are on *content*, not layout. This is deliberate (per the
"whitespace-only reformat vs real change" criterion): the on-disk span is
file-indented and doc-inclusive (BT-2577) while the stored body is the compiler's
canonical column-0 form, so without the dedent every doc-commented method would
read as dirty with an indentation-noise diff. `Clean = true` (no diff) when the
normalised bodies are equal — the revert-to-disk case that drops out of the
pending view. Exported for tests. (BT-2584 will make a single representation
flow end-to-end and retire this normalisation.)
""".
-spec body_delta(binary(), binary()) -> {boolean(), binary() | undefined}.
body_delta(DiskBody, MemBody) ->
    Disk = normalize_body(DiskBody),
    Mem = normalize_body(MemBody),
    case Disk =:= Mem of
        true -> {true, undefined};
        false -> {false, beamtalk_workspace_diff:unified(Disk, Mem)}
    end.

%% Trim trailing whitespace and strip the common leading indentation shared by
%% all non-blank lines, so a file-indented body and a column-0 body compare on
%% content. Blank lines collapse to empty.
-spec normalize_body(binary()) -> binary().
normalize_body(Bin) ->
    Lines = binary:split(Bin, <<"\n">>, [global]),
    Indent = common_indent(Lines, infinity),
    Dedented = [strip_indent(Indent, Line) || Line <- Lines],
    rstrip(iolist_to_binary(lists:join(<<"\n">>, Dedented))).

-spec rstrip(binary()) -> binary().
rstrip(Bin) ->
    %% string:trim/2 returns chardata; force a binary so the `=:=` compares bytes.
    unicode:characters_to_binary(string:trim(Bin, trailing)).

%% The least leading-whitespace width across non-blank lines (blank lines ignored).
-spec common_indent([binary()], non_neg_integer() | infinity) -> non_neg_integer().
common_indent([], infinity) ->
    0;
common_indent([], Acc) ->
    Acc;
common_indent([Line | Rest], Acc) ->
    case is_blank_line(Line) of
        true -> common_indent(Rest, Acc);
        false -> common_indent(Rest, min_indent(Acc, ws_width(Line)))
    end.

-spec min_indent(non_neg_integer() | infinity, non_neg_integer()) -> non_neg_integer().
min_indent(infinity, Width) -> Width;
min_indent(Acc, Width) -> min(Acc, Width).

%% Drop up to `N` leading whitespace bytes from `Line` (a blank line becomes empty).
-spec strip_indent(non_neg_integer(), binary()) -> binary().
strip_indent(_N, Line) when Line =:= <<>> ->
    <<>>;
strip_indent(N, Line) ->
    Drop = min(N, ws_width(Line)),
    binary:part(Line, Drop, byte_size(Line) - Drop).

%% Count the leading run of spaces/tabs.
-spec ws_width(binary()) -> non_neg_integer().
ws_width(Line) -> ws_width(Line, 0).

ws_width(<<C, Rest/binary>>, N) when C =:= $\s; C =:= $\t -> ws_width(Rest, N + 1);
ws_width(_Line, N) -> N.

-spec is_blank_line(binary()) -> boolean().
is_blank_line(Line) -> ws_width(Line) =:= byte_size(Line).

-spec selector_symbol(binary() | undefined) -> atom() | nil.
selector_symbol(undefined) -> nil;
selector_symbol(Sel) -> binary_to_atom(Sel, utf8).

%% `side` is already an atom (`instance` | `class`, a legal Beamtalk Symbol) —
%% unlike `selector_symbol/1` there is no binary to convert, only the
%% nil-mapping for "no side" (`'new-class'` / `unknown` entries).
-spec side_symbol(side() | undefined) -> side() | nil.
side_symbol(undefined) -> nil;
side_symbol(Side) -> Side.

-spec source_file_value(binary() | undefined) -> binary() | nil.
source_file_value(undefined) -> nil;
source_file_value(File) -> File.

%%% ----------------------------------------------------------------------------
%%% Entry accessors
%%% ----------------------------------------------------------------------------
%%% The entry() type is opaque; these accessors are the supported way to read a
%%% ChangeEntry's fields (consumed by later phases and by tests).

-spec entry_seq(entry()) -> non_neg_integer().
entry_seq(#entry{seq = V}) -> V.

-doc """
The workspace epoch `Entry` was appended in (ADR 0113, BT-3207).

Used, together with `entry_seq/1`, to name the same-filesystem staging path
for a `'remove-class'` entry's Phase A rename (`<file>.tmp-delete-<epoch>-<seq>`)
so a resumed flush can tell "this entry's own prior attempt already staged the
delete and crashed before the unlink" apart from "something else deleted the
file externally" — see `beamtalk_workspace_flush`'s delete-atomicity docs.
""".
-spec entry_epoch(entry()) -> non_neg_integer().
entry_epoch(#entry{epoch = V}) -> V.

-spec entry_class(entry()) -> binary().
entry_class(#entry{class = V}) -> V.

-spec entry_selector(entry()) -> binary() | undefined.
entry_selector(#entry{selector = V}) -> V.

-spec entry_kind(entry()) -> kind().
entry_kind(#entry{kind = V}) -> V.

-doc """
Every atom `kind()` admits, exactly matching that type's literal union above
— the single runtime-introspectable source of truth for the wire-string
conformance corpus (`runtime/apps/beamtalk_workspace/test/fixtures/
flush_file_kind_wire_corpus.json`, BT-3275) that pins the Rust LSP consumer's
`FlushFileKind::from_wire` (`crates/beamtalk-lsp/src/runtime.rs`) to this
module's `kind()` domain: `beamtalk_workspace_changelog_tests` asserts this
list's `atom_to_binary` image equals the corpus's wire-string set exactly (so
adding, removing, or renaming a `kind()` alternative without updating this
function — the two are directly adjacent, unlike the corpus in a different
app's test tree — fails a fast, obvious Erlang test), and the Rust side
asserts `from_wire` against the same corpus in
`FlushFileKind::from_wire`'s own test module. Neither side hand-derives the
other's expected values — both are pinned to the shared file.
""".
-spec known_entry_kinds() -> [kind()].
known_entry_kinds() ->
    [
        instance,
        class,
        'new-class',
        'class-def',
        'remove-method',
        'remove-class',
        'rename-class',
        'rename-method',
        unknown
    ].

-doc """
The side (`instance` | `class`) a method-shaped entry targets, or `undefined`
for an entry with no side (`'new-class'`, `unknown`) (ADR 0112, BT-3187).

The supported way to read side — never pattern-match `#entry.side` or
`entry_kind/1` directly for this. A `'remove-method'` entry stores `side`
explicitly (`kind` alone can no longer double as the side discriminator once
it is spent distinguishing removal from patch); a legacy `instance`/`class`
-kind patch entry has no stored `side` — it derives one from its own `kind`,
exactly as `kind` always doubled for side before this ADR. This single
accessor is what both the flush shadow-key (`beamtalk_workspace_flush:
target_key/1`) and `revert:`'s side-resolution
(`beamtalk_workspace_interface_primitives:revert_side/1`) key/resolve on,
per ADR 0112's required fix to ADR 0082's `(class, selector)`-only shadow key.
""".
-spec entry_side(entry()) -> side() | undefined.
entry_side(#entry{side = Side}) when Side =/= undefined -> Side;
entry_side(#entry{kind = instance}) -> instance;
entry_side(#entry{kind = class}) -> class;
entry_side(#entry{}) -> undefined.

-spec entry_intent(entry()) -> intent().
entry_intent(#entry{intent = V}) -> V.

-spec entry_flushable(entry()) -> boolean().
entry_flushable(#entry{flushable = V}) -> V.

-spec entry_not_flushable_reason(entry()) -> binary() | undefined.
entry_not_flushable_reason(#entry{not_flushable_reason = V}) -> V.

-spec entry_author_kind(entry()) -> author_kind().
entry_author_kind(#entry{author_kind = V}) -> V.

-spec entry_is_orphan(entry()) -> boolean().
entry_is_orphan(#entry{orphan = V}) -> V.

-spec entry_is_prior_epoch(entry()) -> boolean().
entry_is_prior_epoch(#entry{prior_epoch = V}) -> V.

-spec entry_flushed(entry()) -> boolean().
entry_flushed(#entry{flushed = V}) -> V.

-spec entry_source_file(entry()) -> binary() | undefined.
entry_source_file(#entry{source_file = V}) -> V.

-spec entry_span(entry()) -> span() | undefined.
entry_span(#entry{span = V}) -> V.

-spec entry_source_ref(entry()) -> binary() | undefined.
entry_source_ref(#entry{source_ref = V}) -> V.

-spec entry_prev_source_ref(entry()) -> binary() | undefined.
entry_prev_source_ref(#entry{prev_source_ref = V}) -> V.

-doc "The pre-rename class name for a `'rename-class'` entry (ADR 0114, BT-3269); `undefined` otherwise.".
-spec entry_old_class(entry()) -> binary() | undefined.
entry_old_class(#entry{old_class = V}) -> V.

-doc "The pre-rename selector for a `'rename-method'` entry (ADR 0114, BT-3269); `undefined` otherwise.".
-spec entry_old_selector(entry()) -> binary() | undefined.
entry_old_selector(#entry{old_selector = V}) -> V.

-doc "The pre-rename file path for a `'rename-class'` entry (ADR 0114, BT-3269); `undefined` otherwise.".
-spec entry_old_path(entry()) -> binary() | undefined.
entry_old_path(#entry{old_path = V}) -> V.

-doc "The post-rename file path for a `'rename-class'` entry (ADR 0114, BT-3269); `undefined` otherwise.".
-spec entry_new_path(entry()) -> binary() | undefined.
entry_new_path(#entry{new_path = V}) -> V.

-doc """
The multi-site rewrite list for a `'rename-class'`/`'rename-method'` entry
(ADR 0114, BT-3269); `undefined` otherwise. `sites[0]` is always the
definition/declaration site; a bare `undefined` element (rather than a
`site()` map) is the dynamic-class "no declaration site" case.
""".
-spec entry_sites(entry()) -> [site() | undefined] | undefined.
entry_sites(#entry{sites = V}) -> V.

-doc """
The reported-but-never-rewritten candidate sender list for a
`'rename-method'` entry (ADR 0114, BT-3269); `undefined` otherwise
(including for `'rename-class'`, which has no candidate tier).
""".
-spec entry_candidate_sites(entry()) -> [candidate_site()] | undefined.
entry_candidate_sites(#entry{candidate_sites = V}) -> V.

-doc """
Read the patched method body (or full new-class source) recorded for `Entry`
from `<workspace>/changes/sources/<source_ref>.bt`.

Returns `{ok, Body}` or `{error, Reason}`. Used by `Workspace flush` (ADR 0082
Phase 2) to splice the patched body back into the on-disk file. In run mode (no
workspace_id, no `changes/` dir) returns `{error, no_workspace}`. A
`'remove-method'` entry (ADR 0112) has no `source_ref` — there is no new body,
the operation only deletes text — so this returns `{error, no_source}` rather
than crashing on the missing filename.
""".
-spec read_source_body(entry()) -> {ok, binary()} | {error, term()}.
read_source_body(#entry{source_ref = undefined}) ->
    {error, no_source};
read_source_body(#entry{source_ref = Ref}) ->
    read_source_file(Ref).

-doc """
Read the recorded prior on-disk body for `Entry` from
`<workspace>/changes/sources/<prev_source_ref>.bt`.

Returns `{ok, Body}` or `{error, Reason}`. New-class entries (no
`prev_source_ref`) return `{error, no_prev_source}`. Used by `Workspace flush`
to detect external edits before splicing.
""".
-spec read_prev_source_body(entry()) -> {ok, binary()} | {error, term()}.
read_prev_source_body(#entry{prev_source_ref = undefined}) ->
    {error, no_prev_source};
read_prev_source_body(#entry{prev_source_ref = Ref}) ->
    read_source_file(Ref).

-spec read_source_file(binary()) -> {ok, binary()} | {error, term()}.
read_source_file(Ref) ->
    case gen_server:call(?MODULE, get_sources_dir) of
        {ok, SourcesDir} ->
            Path = filename:join(SourcesDir, binary_to_list(Ref)),
            file:read_file(Path);
        undefined ->
            {error, no_workspace}
    end.

-doc """
Read one rewrite site's own recorded body (its `source_ref` or
`prev_source_ref`) from `<workspace>/changes/sources/<ref>.bt` (ADR 0114,
BT-3271).

Generalizes `read_source_body/1`/`read_prev_source_body/1` to a `site()`'s
own per-site ref, rather than the OWNING ENTRY's top-level `source_ref`/
`prev_source_ref` — which are always `undefined` for a `'rename-class'`/
`'rename-method'` entry (the per-site bodies live under `sites` instead, see
the ChangeEntry schema doc's `source_ref`/`sites` rows). `beamtalk_workspace_
flush`'s multi-site rename-class splice needs to read a specific site's ref
directly, so this takes the raw `binary() | undefined` ref rather than an
`entry()` — reuses `read_source_file/1` verbatim (CLAUDE.md's no-duplicate-
implementations rule) rather than re-deriving the `sources/` path lookup.

Returns `{error, no_source}` for `undefined` — mirroring `read_source_body/1`'s
handling of an absent top-level `source_ref` — rather than crashing on a
missing filename: a site legitimately has an `undefined` `source_ref` or
`prev_source_ref` (e.g. a site discovered but never itself given a body ref).
""".
-spec read_site_body(binary() | undefined) -> {ok, binary()} | {error, term()}.
read_site_body(undefined) ->
    {error, no_source};
read_site_body(Ref) when is_binary(Ref) ->
    read_source_file(Ref).

-doc """
Persist one rewrite site's body to the ChangeLog's `sources/` directory and
return its ref filename, for building a `'rename-class'`/`'rename-method'`
entry's `sites` list (ADR 0114, BT-3270).

Generalizes the single-body persistence `do_append/2` already performs for a
whole entry's `source`/`prev_source` (`write_optional_source/3`) to the
N-body case a multi-site rewrite needs — one call per site per body (a site
typically has both a `source` and a `prev_source` body). Callers write every
site's bodies via this function BEFORE calling `append/1`: a `site()`'s
`source_ref`/`prev_source_ref` fields are already-persisted refs by the time
`append/1`'s `append_input()` map is built, unlike the top-level `source`/
`prev_source` keys, which name raw bodies `do_append/2` persists itself
against the entry's own `seq` — not yet assigned at the point a caller is
still assembling a multi-site `sites` list.

Returns `undefined` for `undefined` (nothing to write — mirrors
`write_optional_source/3`'s no-op for an absent body) and in run mode (no
workspace; nothing durable to persist a ref against, and every reader of a
`site()`'s refs already tolerates an absent one). Returns `{error, Reason}`
on a write failure — callers should log and drop to `undefined` rather than
fail the whole rewrite, mirroring how a ChangeLog write failure elsewhere is
already best-effort from its caller's point of view.
""".
-spec store_site_body(binary() | undefined) -> {ok, binary()} | undefined | {error, term()}.
store_site_body(undefined) ->
    undefined;
store_site_body(Body) when is_binary(Body) ->
    case gen_server:call(?MODULE, get_sources_dir) of
        undefined ->
            undefined;
        {ok, SourcesDir} ->
            Ref = site_body_ref_filename(),
            Path = filename:join(SourcesDir, binary_to_list(Ref)),
            case write_file_atomic(Path, Body) of
                ok -> {ok, Ref};
                {error, _} = Err -> Err
            end
    end.

%% sources/site-<unique>.bt — a rewrite site's body ref filename. Unlike
%% `source_ref_filename/2` (keyed by the owning entry's own `seq`, assigned
%% only once `do_append/2` runs), a site's body must be written BEFORE the
%% entry's `seq` is known (see `store_site_body/1`'s doc) — `erlang:unique_
%% integer/1` gives a monotonic, collision-free name with no such ordering
%% dependency.
-spec site_body_ref_filename() -> binary().
site_body_ref_filename() ->
    Unique = erlang:unique_integer([positive, monotonic]),
    iolist_to_binary([<<"site-">>, integer_to_binary(Unique), <<".bt">>]).

%%% ----------------------------------------------------------------------------
%%% gen_server callbacks
%%% ----------------------------------------------------------------------------

init(Config) ->
    %% Inherit the runtime logging domain for every log call from this process.
    beamtalk_logging_config:set_domain(runtime),
    WorkspaceId = maps:get(workspace_id, Config, undefined),
    ChangesDir = changes_dir(WorkspaceId),
    ensure_ets(),
    State0 = #state{
        changes_dir = ChangesDir,
        log_path = log_path(ChangesDir),
        next_seq = 0,
        epoch = 0
    },
    State1 = load_from_disk(State0),
    %% A persisted log may already exceed MAX_ENTRIES (it was written by an older
    %% build, hand-edited, or restored from backup). The ring bound is otherwise
    %% only enforced on append, so trim the overflow now rather than letting a
    %% large log linger until the next mutation.
    State = maybe_rotate(State1),
    {ok, State}.

handle_call({append, Input}, _From, State) ->
    case do_append(Input, State) of
        {ok, Seq, State1} ->
            {reply, {ok, Seq}, State1};
        {error, _Reason} = Err ->
            {reply, Err, State}
    end;
handle_call(epoch, _From, State) ->
    {reply, State#state.epoch, State};
handle_call({mark_flushed, Seqs}, _From, State) ->
    Reply = do_mark_flushed(Seqs, State),
    {reply, Reply, State};
handle_call(get_sources_dir, _From, State) ->
    case State#state.changes_dir of
        undefined ->
            {reply, undefined, State};
        Dir ->
            {reply, {ok, filename:join(Dir, "sources")}, State}
    end;
handle_call(clear, _From, State) ->
    ets:delete_all_objects(?ETS_TABLE),
    truncate_log(State),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ----------------------------------------------------------------------------
%%% Append
%%% ----------------------------------------------------------------------------

-spec do_append(append_input(), #state{}) ->
    {ok, non_neg_integer(), #state{}} | {error, #beamtalk_error{}}.
do_append(Input, State) ->
    Seq = State#state.next_seq,
    Source = maps:get(source, Input, undefined),
    %% ADR 0112: `source` is optional — a `'remove-method'` entry has no new
    %% body, so it gets no `source_ref` (and therefore no on-disk source file;
    %% mirrors how `prev_source_ref` already stays `undefined` when there is
    %% no `prev_source`).
    SourceRef =
        case Source of
            undefined -> undefined;
            _ -> source_ref_filename(Seq, source)
        end,
    PrevSource = maps:get(prev_source, Input, undefined),
    PrevSourceRef =
        case PrevSource of
            undefined -> undefined;
            _ -> source_ref_filename(Seq, prev)
        end,
    Entry = #entry{
        seq = Seq,
        ts = erlang:system_time(millisecond),
        epoch = State#state.epoch,
        class = maps:get(class, Input),
        selector = maps:get(selector, Input, undefined),
        kind = maps:get(kind, Input),
        side = maps:get(side, Input, undefined),
        source_ref = SourceRef,
        prev_source_ref = PrevSourceRef,
        source_file = maps:get(source_file, Input, undefined),
        span = maps:get(span, Input, undefined),
        old_class = maps:get(old_class, Input, undefined),
        old_selector = maps:get(old_selector, Input, undefined),
        old_path = maps:get(old_path, Input, undefined),
        new_path = maps:get(new_path, Input, undefined),
        sites = maps:get(sites, Input, undefined),
        candidate_sites = maps:get(candidate_sites, Input, undefined),
        intent = maps:get(intent, Input),
        flushable = maps:get(flushable, Input),
        not_flushable_reason = maps:get(not_flushable_reason, Input, undefined),
        author = maps:get(author, Input),
        author_kind = maps:get(author_kind, Input)
    },
    case persist_append(Entry, Source, PrevSource, State) of
        ok ->
            ets:insert(?ETS_TABLE, {Seq, Entry}),
            State1 = State#state{next_seq = Seq + 1},
            State2 = maybe_rotate(State1),
            {ok, Seq, State2};
        {error, Reason} ->
            {error, append_error(Reason)}
    end.

%% Mark each given seq's entry as flushed. Updates ETS in-place and rewrites the
%% on-disk log so the flag survives restart. Unknown seqs are silently skipped
%% (idempotent: callers can pass the full pending set without checking each
%% entry first). The whole-log rewrite is atomic via temp+rename so a crash
%% mid-update never truncates the log; a crash *before* the rewrite leaves the
%% in-memory ETS still showing entries as flushed while disk does not. To avoid
%% diverging the live view from disk, we only flip the ETS flag *after* the
%% log rewrite returns ok.
-spec do_mark_flushed([non_neg_integer()], #state{}) -> ok | {error, term()}.
do_mark_flushed(Seqs, State) ->
    SeqSet = sets:from_list(Seqs, [{version, 2}]),
    All = entries(),
    Updated = lists:map(
        fun(E) ->
            case sets:is_element(E#entry.seq, SeqSet) of
                true -> E#entry{flushed = true};
                false -> E
            end
        end,
        All
    ),
    case rewrite_log(Updated, State) of
        ok ->
            lists:foreach(fun(E) -> ets:insert(?ETS_TABLE, {E#entry.seq, E}) end, Updated),
            ok;
        {error, _} = Err ->
            Err
    end.

%% Crash-safe persistence ordering: write the body files first (atomically via
%% temp+rename), then append the metadata line. A crash between the two leaves
%% orphaned body files (harmless — pruned on rotation) but never a metadata line
%% pointing at a missing body. In run mode (no changes_dir) this is a no-op and
%% the entry lives in ETS only.
-spec persist_append(#entry{}, binary() | undefined, binary() | undefined, #state{}) ->
    ok | {error, term()}.
persist_append(_Entry, _Source, _PrevSource, #state{changes_dir = undefined}) ->
    ok;
persist_append(Entry, Source, PrevSource, State) ->
    SourcesDir = filename:join(State#state.changes_dir, "sources"),
    case filelib:ensure_path(SourcesDir) of
        ok ->
            case write_optional_source(SourcesDir, Entry#entry.source_ref, Source) of
                ok ->
                    case
                        write_optional_source(SourcesDir, Entry#entry.prev_source_ref, PrevSource)
                    of
                        ok -> append_metadata_line(Entry, State);
                        Err -> Err
                    end;
                Err ->
                    Err
            end;
        {error, Reason} ->
            {error, {ensure_path, SourcesDir, Reason}}
    end.

%% Write `Body` to `sources/<Ref>` when `Ref` is present. `Ref` is `undefined`
%% for a `'remove-method'` entry's `source_ref` (no new body — ADR 0112) and,
%% independently, for `prev_source_ref` on any entry with no recorded prior
%% body (e.g. a `new-class` entry, or a patch that added a brand-new method) —
%% both are legitimate no-ops, not persistence failures.
-spec write_optional_source(string(), binary() | undefined, binary() | undefined) ->
    ok | {error, term()}.
write_optional_source(_SourcesDir, undefined, _Body) ->
    ok;
write_optional_source(SourcesDir, Ref, Body) ->
    Path = filename:join(SourcesDir, binary_to_list(Ref)),
    write_file_atomic(Path, Body).

-spec append_metadata_line(#entry{}, #state{}) -> ok | {error, term()}.
append_metadata_line(Entry, State) ->
    Line = [entry_to_json(Entry), $\n],
    case file:write_file(State#state.log_path, Line, [append]) of
        ok -> ok;
        {error, Reason} -> {error, {write_log, Reason}}
    end.

%%% ----------------------------------------------------------------------------
%%% Load on startup (restart semantics)
%%% ----------------------------------------------------------------------------

%% Read changes.jsonl, rebuild ETS, assign a fresh epoch, and tag every
%% pre-existing entry as prior-epoch + (if prev_source no longer matches disk)
%% orphan. In run mode (no changes_dir) there is nothing on disk — start clean.
-spec load_from_disk(#state{}) -> #state{}.
load_from_disk(#state{changes_dir = undefined} = State) ->
    State#state{next_seq = 0, epoch = 0};
load_from_disk(State) ->
    LogPath = State#state.log_path,
    case file:read_file(LogPath) of
        {ok, Bin} ->
            Entries = parse_log(Bin),
            PriorEpochMax = max_epoch(Entries),
            NextSeq = max_seq(Entries) + 1,
            FreshEpoch = PriorEpochMax + 1,
            SourcesDir = filename:join(State#state.changes_dir, "sources"),
            Tagged = [tag_prior(E, SourcesDir) || E <- Entries],
            lists:foreach(fun(E) -> ets:insert(?ETS_TABLE, {E#entry.seq, E}) end, Tagged),
            State#state{next_seq = NextSeq, epoch = FreshEpoch};
        {error, enoent} ->
            State#state{next_seq = 0, epoch = 1};
        {error, Reason} ->
            ?LOG_WARNING("Failed to read ChangeLog at ~ts: ~p", [LogPath, Reason]),
            State#state{next_seq = 0, epoch = 1}
    end.

%% Pre-existing entry → prior epoch; orphan iff its recorded prev_source no
%% longer matches the current on-disk content of its sourceFile.
-spec tag_prior(#entry{}, string()) -> #entry{}.
tag_prior(Entry, SourcesDir) ->
    Entry#entry{prior_epoch = true, orphan = is_orphan(Entry, SourcesDir)}.

-spec is_orphan(#entry{}, string()) -> boolean().
is_orphan(#entry{source_file = undefined}, _SourcesDir) ->
    %% Non-flushable (stdlib/dynamic) entries have no disk file to compare —
    %% their memory state is gone on restart, but they are not "orphaned"
    %% against disk content. Excluded from the active view via prior_epoch.
    false;
is_orphan(#entry{prev_source_ref = undefined}, _SourcesDir) ->
    %% new-class entries: the file should not have existed at append time. If it
    %% exists now, the active view excludes it via prior_epoch regardless; we do
    %% not mark new-class entries orphan here (relocation/conflict is Phase 2).
    false;
is_orphan(#entry{source_file = SourceFile, span = Span, prev_source_ref = PrevRef}, SourcesDir) ->
    PrevPath = filename:join(SourcesDir, binary_to_list(PrevRef)),
    case {file:read_file(binary_to_list(SourceFile)), file:read_file(PrevPath)} of
        {{ok, DiskBin}, {ok, PrevBin}} ->
            not span_matches(DiskBin, Span, PrevBin);
        _ ->
            %% Source file or recorded prev body unreadable → treat as orphaned:
            %% the patch can no longer be safely reconciled against disk.
            true
    end.

%% The recorded prev_source must still be byte-identical to the bytes currently
%% occupying the recorded span in the on-disk file. If the span is out of range
%% or the bytes differ, the disk advanced under us — orphan.
-spec span_matches(binary(), span() | undefined, binary()) -> boolean().
span_matches(_DiskBin, undefined, _PrevBin) ->
    false;
span_matches(DiskBin, #{start := Start, 'end' := End}, PrevBin) when
    is_integer(Start), is_integer(End), End >= Start, End =< byte_size(DiskBin)
->
    binary:part(DiskBin, Start, End - Start) =:= PrevBin;
span_matches(_DiskBin, _Span, _PrevBin) ->
    false.

-spec parse_log(binary()) -> [#entry{}].
parse_log(Bin) ->
    Lines = binary:split(Bin, <<"\n">>, [global, trim_all]),
    lists:filtermap(
        fun(Line) ->
            try
                {true, entry_from_json(Line)}
            catch
                Class:Reason ->
                    ?LOG_WARNING("Skipping malformed ChangeLog line: ~p:~p", [Class, Reason]),
                    false
            end
        end,
        Lines
    ).

-spec max_seq([#entry{}]) -> integer().
max_seq([]) -> -1;
max_seq(Entries) -> lists:max([E#entry.seq || E <- Entries]).

-spec max_epoch([#entry{}]) -> integer().
max_epoch([]) -> 0;
max_epoch(Entries) -> lists:max([E#entry.epoch || E <- Entries]).

%%% ----------------------------------------------------------------------------
%%% Bounded ring + archive rotation
%%% ----------------------------------------------------------------------------

%% When the on-disk log exceeds MAX_ENTRIES, archive the oldest segment
%% (metadata as a gzipped .jsonl, the referenced source bodies as a gzipped tar)
%% and drop those entries from the live log + ETS. Human and agent entries are
%% pruned on equal footing — only the ring bound applies.
%%
%% Rotation is transactional: the live ETS and changes.jsonl are mutated ONLY
%% after the archive segment is written AND the trimmed log is rewritten, both
%% successfully. If archiving or the rewrite fails (disk full, permissions,
%% tar/gzip error) the existing ETS + log are left untouched and the error is
%% logged — a failed rotation must never lose history or leave disk inconsistent.
-spec maybe_rotate(#state{}) -> #state{}.
maybe_rotate(#state{changes_dir = undefined} = State) ->
    State;
maybe_rotate(State) ->
    All = entries(),
    case length(All) > ?MAX_ENTRIES of
        false ->
            State;
        true ->
            Overflow = length(All) - ?MAX_ENTRIES,
            {ToArchive, ToKeep} = lists:split(Overflow, All),
            rotate_transactional(ToArchive, ToKeep, State)
    end.

%% Perform the rotation only if every disk step succeeds. Order:
%%   1. archive the overflow segment (metadata + sources) to archive/
%%   2. rewrite changes.jsonl with exactly the retained entries
%% Both are crash-safe (atomic temp+rename). Only once both succeed do we prune
%% the archived source bodies and swap ETS to the retained set. On any failure we
%% return State unchanged (ETS and the live log keep all entries) and log it.
-spec rotate_transactional([#entry{}], [#entry{}], #state{}) -> #state{}.
rotate_transactional(ToArchive, ToKeep, State) ->
    case archive_segment(ToArchive, State) of
        {ok, ArchivedMembers} ->
            case rewrite_log(ToKeep, State) of
                ok ->
                    %% Both disk steps committed — now it is safe to drop the
                    %% archived body files and swap ETS to the retained set.
                    prune_source_members(ArchivedMembers),
                    ets:delete_all_objects(?ETS_TABLE),
                    lists:foreach(
                        fun(E) -> ets:insert(?ETS_TABLE, {E#entry.seq, E}) end, ToKeep
                    ),
                    State;
                {error, Reason} ->
                    %% Archive succeeded but the live-log rewrite failed. Leave
                    %% ETS + log untouched; the (harmless) extra archive segment
                    %% will be superseded on the next successful rotation.
                    ?LOG_ERROR(
                        "ChangeLog rotation aborted: failed to rewrite live log",
                        #{reason => Reason, domain => [beamtalk, runtime]}
                    ),
                    State
            end;
        {error, Reason} ->
            ?LOG_ERROR(
                "ChangeLog rotation aborted: failed to archive overflow segment",
                #{reason => Reason, domain => [beamtalk, runtime]}
            ),
            State
    end.

%% Archive the overflow segment. Returns `{ok, Members}` (the source-body files
%% that were tarred, so the caller can delete them after the whole rotation
%% commits) or `{error, Reason}` if any disk step fails. Source bodies are NOT
%% deleted here — deletion is deferred until rewrite_log/2 also succeeds.
-spec archive_segment([#entry{}], #state{}) ->
    {ok, [{string(), string()}]} | {error, term()}.
archive_segment(Entries, State) ->
    ArchiveDir = filename:join(State#state.changes_dir, "archive"),
    Ts = archive_suffix(),
    case filelib:ensure_path(ArchiveDir) of
        ok ->
            case archive_metadata(Entries, ArchiveDir, Ts) of
                ok ->
                    archive_sources(Entries, ArchiveDir, Ts, State);
                {error, _} = Err ->
                    Err
            end;
        {error, Reason} ->
            {error, {ensure_path, ArchiveDir, Reason}}
    end.

%% Unique, monotonic, collision-free archive filename suffix. A millisecond
%% timestamp can still collide when two rotations land in the same millisecond
%% (e.g. a single overflowing batch), so we append a strictly-increasing unique
%% integer. Format: "<ms>-<unique>".
-spec archive_suffix() -> string().
archive_suffix() ->
    Ms = integer_to_list(erlang:system_time(millisecond)),
    Unique = integer_to_list(erlang:unique_integer([positive, monotonic])),
    Ms ++ "-" ++ Unique.

-spec archive_metadata([#entry{}], string(), string()) -> ok | {error, term()}.
archive_metadata(Entries, ArchiveDir, Ts) ->
    Path = filename:join(ArchiveDir, "changes-" ++ Ts ++ ".jsonl.gz"),
    Lines = [[entry_to_json(E), $\n] || E <- Entries],
    Gz = zlib:gzip(iolist_to_binary(Lines)),
    case write_file_atomic(Path, Gz) of
        ok ->
            ok;
        {error, Reason} ->
            {error, {archive_metadata, Path, Reason}}
    end.

%% Tar the referenced source bodies into archive/. Returns `{ok, Members}` with
%% the body files that were archived (deleted later, once the rotation commits)
%% or `{error, Reason}`. An empty member set is a successful no-op.
-spec archive_sources([#entry{}], string(), string(), #state{}) ->
    {ok, [{string(), string()}]} | {error, term()}.
archive_sources(Entries, ArchiveDir, Ts, State) ->
    SourcesDir = filename:join(State#state.changes_dir, "sources"),
    Refs = source_refs(Entries),
    Members = collect_source_members(Refs, SourcesDir),
    Path = filename:join(ArchiveDir, "sources-" ++ Ts ++ ".tar.gz"),
    case Members of
        [] ->
            {ok, []};
        _ ->
            case erl_tar:create(Path, Members, [compressed]) of
                ok ->
                    {ok, Members};
                {error, Reason} ->
                    {error, {archive_sources, Path, Reason}}
            end
    end.

%% Delete the source-body files that were safely archived. Called only after the
%% whole rotation has committed (archive + log rewrite both succeeded).
-spec prune_source_members([{string(), string()}]) -> ok.
prune_source_members(Members) ->
    lists:foreach(fun({_Name, AbsPath}) -> _ = file:delete(AbsPath) end, Members),
    ok.

-spec source_refs([#entry{}]) -> [binary()].
source_refs(Entries) ->
    lists:flatten([refs_of(E) || E <- Entries]).

%% Both refs are optional (`source_ref` is `undefined` for a `'remove-method'`
%% entry; `prev_source_ref` is `undefined` for e.g. a `new-class` entry or a
%% brand-new-method patch) — only the ones actually present are archived.
-spec refs_of(#entry{}) -> [binary()].
refs_of(#entry{source_ref = SR, prev_source_ref = PR}) ->
    [Ref || Ref <- [SR, PR], Ref =/= undefined].

%% Build erl_tar member list {NameInArchive, AbsolutePath} for refs that exist.
-spec collect_source_members([binary()], string()) -> [{string(), string()}].
collect_source_members(Refs, SourcesDir) ->
    lists:filtermap(
        fun(Ref) ->
            Name = binary_to_list(Ref),
            Abs = filename:join(SourcesDir, Name),
            case filelib:is_regular(Abs) of
                true -> {true, {Name, Abs}};
                false -> false
            end
        end,
        Refs
    ).

%%% ----------------------------------------------------------------------------
%%% On-disk helpers
%%% ----------------------------------------------------------------------------

%% Rewrite changes.jsonl from scratch with exactly Entries (used after rotation
%% and never on the hot append path). Atomic temp+rename so a crash mid-rewrite
%% cannot truncate the live log.
-spec rewrite_log([#entry{}], #state{}) -> ok | {error, term()}.
rewrite_log(_Entries, #state{log_path = undefined}) ->
    ok;
rewrite_log(Entries, State) ->
    Lines = [[entry_to_json(E), $\n] || E <- Entries],
    write_file_atomic(State#state.log_path, iolist_to_binary(Lines)).

-spec truncate_log(#state{}) -> ok.
truncate_log(#state{log_path = undefined}) ->
    ok;
truncate_log(State) ->
    _ = write_file_atomic(State#state.log_path, <<>>),
    ok.

%% Write Data to Path via a sibling temp file + atomic rename so readers never
%% observe a partially written file.
-spec write_file_atomic(string(), iodata()) -> ok | {error, term()}.
write_file_atomic(Path, Data) ->
    _ = filelib:ensure_dir(Path),
    Tmp = Path ++ ".tmp",
    case file:write_file(Tmp, Data) of
        ok ->
            case file:rename(Tmp, Path) of
                ok ->
                    ok;
                {error, Reason} ->
                    _ = file:delete(Tmp),
                    {error, {rename, Reason}}
            end;
        {error, Reason} ->
            {error, {write, Reason}}
    end.

%% sources/<seq6>-source.bt / sources/<seq6>-prev.bt
-spec source_ref_filename(non_neg_integer(), source | prev) -> binary().
source_ref_filename(Seq, Which) ->
    Padded = io_lib:format("~6..0b", [Seq]),
    Suffix =
        case Which of
            source -> "-source.bt";
            prev -> "-prev.bt"
        end,
    iolist_to_binary([Padded, Suffix]).

-doc """
Return the absolute `changes/` directory for a workspace, or `undefined` when
there is no workspace (run mode). Mirrors `beamtalk_workspace_meta`'s path
resolution: `<home>/.beamtalk/workspaces/<id>/changes`, falling back to the OS
user-cache dir when HOME/USERPROFILE is unset. Exported for tests.
""".
-spec changes_dir(binary() | undefined) -> string() | undefined.
changes_dir(undefined) ->
    undefined;
changes_dir(WorkspaceId) when is_binary(WorkspaceId) ->
    Base =
        case beamtalk_platform:home_dir() of
            false -> filename:basedir(user_cache, "beamtalk");
            Home -> filename:join(Home, ".beamtalk")
        end,
    filename:join([Base, "workspaces", binary_to_list(WorkspaceId), "changes"]).

-spec log_path(string() | undefined) -> string() | undefined.
log_path(undefined) -> undefined;
log_path(ChangesDir) -> filename:join(ChangesDir, "changes.jsonl").

-spec ensure_ets() -> ok.
ensure_ets() ->
    case ets:whereis(?ETS_TABLE) of
        undefined ->
            ets:new(?ETS_TABLE, [named_table, public, ordered_set, {read_concurrency, true}]);
        _ ->
            ets:delete_all_objects(?ETS_TABLE)
    end,
    ok.

%%% ----------------------------------------------------------------------------
%%% JSON (de)serialisation
%%% ----------------------------------------------------------------------------

-doc """
Encode a ChangeEntry to a compact JSON binary (one `changes.jsonl` line, no
trailing newline). Exported for tests. The derived in-memory flags
(`prior_epoch`, `orphan`) are not persisted — they are recomputed on restart.
""".
-spec entry_to_json(entry()) -> binary().
entry_to_json(#entry{} = E) ->
    Map = #{
        <<"ts">> => E#entry.ts,
        <<"seq">> => E#entry.seq,
        <<"epoch">> => E#entry.epoch,
        <<"class">> => E#entry.class,
        <<"selector">> => null_or(E#entry.selector),
        <<"kind">> => atom_to_binary(E#entry.kind, utf8),
        %% ADR 0112: `undefined` for every legacy `instance`/`class`-kind entry
        %% (side is derived from `kind` for those, never persisted — additive
        %% field, ADR 0112 § ChangeLog interaction) — only a `'remove-method'`
        %% entry stores this.
        <<"side">> => side_json(E#entry.side),
        <<"source_ref">> => null_or(E#entry.source_ref),
        <<"prev_source_ref">> => null_or(E#entry.prev_source_ref),
        <<"sourceFile">> => null_or(E#entry.source_file),
        <<"span">> => span_to_json(E#entry.span),
        %% ADR 0114 (BT-3269).
        <<"old_class">> => null_or(E#entry.old_class),
        <<"old_selector">> => null_or(E#entry.old_selector),
        <<"old_path">> => null_or(E#entry.old_path),
        <<"new_path">> => null_or(E#entry.new_path),
        <<"sites">> => sites_to_json(E#entry.sites),
        <<"candidate_sites">> => candidate_sites_to_json(E#entry.candidate_sites),
        <<"intent">> => atom_to_binary(E#entry.intent, utf8),
        <<"flushable">> => E#entry.flushable,
        <<"not_flushable_reason">> => null_or(E#entry.not_flushable_reason),
        <<"author">> => E#entry.author,
        <<"author_kind">> => atom_to_binary(E#entry.author_kind, utf8),
        <<"flushed">> => E#entry.flushed
    },
    iolist_to_binary(json:encode(Map)).

-doc "Decode a `changes.jsonl` line into a ChangeEntry record. Exported for tests.".
-spec entry_from_json(binary()) -> entry().
entry_from_json(Line) ->
    Map = json:decode(Line),
    #entry{
        ts = maps:get(<<"ts">>, Map),
        seq = maps:get(<<"seq">>, Map),
        epoch = maps:get(<<"epoch">>, Map),
        class = maps:get(<<"class">>, Map),
        selector = from_null(maps:get(<<"selector">>, Map, null)),
        kind = decode_kind(maps:get(<<"kind">>, Map)),
        %% Absent in every metadata line written before this ADR (`side` is a
        %% new field) — `decode_side/1`'s `null`/missing clause already yields
        %% `undefined`, which is exactly correct for those legacy lines: their
        %% side is derived from `kind` at read time via `entry_side/1`.
        side = decode_side(maps:get(<<"side">>, Map, null)),
        source_ref = from_null(maps:get(<<"source_ref">>, Map, null)),
        prev_source_ref = from_null(maps:get(<<"prev_source_ref">>, Map, null)),
        source_file = from_null(maps:get(<<"sourceFile">>, Map, null)),
        span = span_from_json(maps:get(<<"span">>, Map, null)),
        %% ADR 0114 (BT-3269): absent in every metadata line written before
        %% this ADR — `maps:get/3`'s `null` default decodes to `undefined`
        %% via the same helpers used for every other pre-existing optional
        %% field, so legacy lines round-trip unchanged.
        old_class = from_null(maps:get(<<"old_class">>, Map, null)),
        old_selector = from_null(maps:get(<<"old_selector">>, Map, null)),
        old_path = from_null(maps:get(<<"old_path">>, Map, null)),
        new_path = from_null(maps:get(<<"new_path">>, Map, null)),
        sites = sites_from_json(maps:get(<<"sites">>, Map, null)),
        candidate_sites = candidate_sites_from_json(maps:get(<<"candidate_sites">>, Map, null)),
        intent = decode_intent(maps:get(<<"intent">>, Map)),
        flushable = maps:get(<<"flushable">>, Map),
        not_flushable_reason = from_null(maps:get(<<"not_flushable_reason">>, Map, null)),
        author = maps:get(<<"author">>, Map),
        author_kind = decode_author_kind(maps:get(<<"author_kind">>, Map)),
        %% `flushed` was added in Phase 2; entries written by an earlier build
        %% will not have this field — default to false so they re-appear as
        %% pending on first restart (the correct conservative outcome — they
        %% never made it to disk).
        flushed = maps:get(<<"flushed">>, Map, false)
    }.

%% Enum decoders use an explicit allowlist with a safe `unknown` fallback rather
%% than binary_to_existing_atom/2. A value written by a newer build (kind is an
%% open enum) — or a corrupt closed-enum field — would otherwise throw and cause
%% parse_log/1 to silently drop the whole line, losing history. Mapping to
%% `unknown` keeps the entry; it is excluded from the active view via prior_epoch
%% on restart regardless.
-spec decode_kind(binary()) -> kind().
decode_kind(<<"instance">>) ->
    instance;
decode_kind(<<"class">>) ->
    class;
decode_kind(<<"new-class">>) ->
    'new-class';
decode_kind(<<"class-def">>) ->
    'class-def';
decode_kind(<<"remove-method">>) ->
    'remove-method';
decode_kind(<<"remove-class">>) ->
    'remove-class';
decode_kind(<<"rename-class">>) ->
    'rename-class';
decode_kind(<<"rename-method">>) ->
    'rename-method';
decode_kind(Other) ->
    log_unknown_enum(kind, Other),
    unknown.

-spec decode_intent(binary()) -> intent().
decode_intent(<<"durable">>) ->
    durable;
decode_intent(<<"ephemeral">>) ->
    ephemeral;
decode_intent(Other) ->
    log_unknown_enum(intent, Other),
    unknown.

-spec decode_author_kind(binary()) -> author_kind().
decode_author_kind(<<"human">>) ->
    human;
decode_author_kind(<<"agent">>) ->
    agent;
decode_author_kind(Other) ->
    log_unknown_enum(author_kind, Other),
    unknown.

-spec log_unknown_enum(atom(), term()) -> ok.
log_unknown_enum(Field, Value) ->
    ?LOG_WARNING(
        "Unknown ChangeLog enum value; preserving entry as 'unknown'",
        #{field => Field, value => Value, domain => [beamtalk, runtime]}
    ).

%% `side` is a small closed set (`instance` | `class`); an unrecognised value
%% (corruption, a future writer using a value this build doesn't know) is
%% mapped to `undefined` rather than kept verbatim — `entry_side/1` already
%% treats `undefined` as "derive from kind", the same safe fallback a
%% `'remove-method'` entry with a genuinely lost side would need anyway.
-spec side_json(side() | undefined) -> binary() | null.
side_json(undefined) -> null;
side_json(Side) -> atom_to_binary(Side, utf8).

%% `null`/missing is the normal shape for every legacy `instance`/`class`-kind
%% entry (they never wrote a `side` field at all) — that maps to `undefined`
%% silently, same as the sibling decoders' `unknown` fallback but without the
%% warning, since it is not corruption. Any *other* unrecognised binary,
%% though, means an on-disk value this build doesn't know — mirror
%% `decode_kind/1`, `decode_intent/1`, `decode_author_kind/1` and log it via
%% `log_unknown_enum/2` before falling back to `undefined`, so corruption is
%% distinguishable from the ordinary absent-legacy-field case.
-spec decode_side(binary() | null) -> side() | undefined.
decode_side(null) ->
    undefined;
decode_side(<<"instance">>) ->
    instance;
decode_side(<<"class">>) ->
    class;
decode_side(Other) ->
    log_unknown_enum(side, Other),
    undefined.

-spec span_to_json(span() | undefined) -> map() | null.
span_to_json(undefined) -> null;
span_to_json(#{start := Start, 'end' := End}) -> #{<<"start">> => Start, <<"end">> => End}.

-spec span_from_json(map() | null) -> span() | undefined.
span_from_json(null) -> undefined;
span_from_json(#{<<"start">> := Start, <<"end">> := End}) -> #{start => Start, 'end' => End}.

%% ADR 0114 (BT-3269): `sites`/`candidate_sites` (de)serialisation. A whole
%% `sites` list of `null` (rather than an empty array) matches every other
%% optional field's "absent means not applicable" convention — only
%% `'rename-class'`/`'rename-method'` entries ever populate it. An individual
%% `null` *element* inside a present list is the dynamic-class "no
%% declaration site" case (ADR 0114 § ChangeLog schema), distinct from the
%% whole-field absence.
-spec site_to_json(site() | undefined) -> map() | null.
site_to_json(undefined) ->
    null;
site_to_json(#{
    source_file := SourceFile, span := Span, source_ref := SourceRef, prev_source_ref := PrevRef
}) ->
    #{
        <<"sourceFile">> => null_or(SourceFile),
        <<"span">> => span_to_json(Span),
        <<"source_ref">> => null_or(SourceRef),
        <<"prev_source_ref">> => null_or(PrevRef)
    }.

-spec site_from_json(map() | null) -> site() | undefined.
site_from_json(null) ->
    undefined;
site_from_json(Map) when is_map(Map) ->
    #{
        source_file => from_null(maps:get(<<"sourceFile">>, Map, null)),
        span => span_from_json(maps:get(<<"span">>, Map, null)),
        source_ref => from_null(maps:get(<<"source_ref">>, Map, null)),
        prev_source_ref => from_null(maps:get(<<"prev_source_ref">>, Map, null))
    }.

-spec sites_to_json([site() | undefined] | undefined) -> [map() | null] | null.
sites_to_json(undefined) -> null;
sites_to_json(Sites) -> [site_to_json(S) || S <- Sites].

-spec sites_from_json([map() | null] | null) -> [site() | undefined] | undefined.
sites_from_json(null) -> undefined;
sites_from_json(List) when is_list(List) -> [site_from_json(S) || S <- List].

-spec candidate_site_to_json(candidate_site()) -> map().
candidate_site_to_json(#{source_file := SourceFile, span := Span}) ->
    #{<<"sourceFile">> => SourceFile, <<"span">> => span_to_json(Span)}.

%% Unlike `site_from_json/1`, a `candidate_site()` has no optional-field case
%% to default (every candidate site always names a real sourceFile/span) —
%% so a missing/invalid key is genuinely malformed, not a legitimate
%% "not applicable" shape. Returns `error` for that case instead of
%% crashing: `candidate_sites_from_json/1` drops just the malformed element
%% (logged) rather than letting it propagate up through `entry_from_json/1`
%% and cost `parse_log/1`'s catch the *entire* ChangeLog line — including
%% that entry's unrelated `sites`/rename data — over one bad element.
%%
%% Guards against two distinct malformed shapes, not just a missing key:
%% an explicit `"span": null` (`span_from_json/1` returns `undefined`, which
%% would otherwise silently violate `candidate_site()`'s non-optional
%% `span := span()` contract) and a `span` object missing `start`/`end`
%% (`span_from_json/1` has no fallback clause for that shape and raises
%% `function_clause` — caught here rather than crashing the whole decode).
-spec candidate_site_from_json(map()) -> {ok, candidate_site()} | error.
candidate_site_from_json(#{<<"sourceFile">> := SourceFile, <<"span">> := Span} = Map) ->
    try span_from_json(Span) of
        undefined -> drop_malformed_candidate_site(Map);
        DecodedSpan -> {ok, #{source_file => SourceFile, span => DecodedSpan}}
    catch
        _:_ -> drop_malformed_candidate_site(Map)
    end;
candidate_site_from_json(Malformed) ->
    drop_malformed_candidate_site(Malformed).

-spec drop_malformed_candidate_site(term()) -> error.
drop_malformed_candidate_site(Malformed) ->
    ?LOG_WARNING("Dropping malformed candidate_site: ~p", [Malformed]),
    error.

-spec candidate_sites_to_json([candidate_site()] | undefined) -> [map()] | null.
candidate_sites_to_json(undefined) -> null;
candidate_sites_to_json(Sites) -> [candidate_site_to_json(S) || S <- Sites].

-spec candidate_sites_from_json([map()] | null) -> [candidate_site()] | undefined.
candidate_sites_from_json(null) ->
    undefined;
candidate_sites_from_json(List) when is_list(List) ->
    lists:filtermap(
        fun(S) ->
            case candidate_site_from_json(S) of
                {ok, CandidateSite} -> {true, CandidateSite};
                error -> false
            end
        end,
        List
    ).

-spec null_or(binary() | undefined) -> binary() | null.
null_or(undefined) -> null;
null_or(V) -> V.

-spec from_null(term()) -> term().
from_null(null) -> undefined;
from_null(V) -> V.

%%% ----------------------------------------------------------------------------
%%% Errors
%%% ----------------------------------------------------------------------------

-spec append_error(term()) -> #beamtalk_error{}.
append_error(Reason) ->
    #beamtalk_error{
        kind = changelog_write_error,
        class = 'ChangeLog',
        selector = 'append:',
        message = <<"Failed to persist ChangeLog entry to disk">>,
        hint = <<"Check that the workspace changes/ directory is writable">>,
        details = #{reason => Reason}
    }.
