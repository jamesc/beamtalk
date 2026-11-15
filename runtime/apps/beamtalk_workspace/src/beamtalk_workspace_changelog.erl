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
| `side`                 | `"instance"`\|`"class"`\|null                               | ADR 0112: explicit only for `"remove-method"`/`"rename-method"` (ADR 0114); legacy `"instance"`/`"class"`-kind entries derive it from `kind` (`entry_side/1`); always null for `"remove-class"`/`"rename-class"` (no method-level target) |
| `source_ref`           | string \| null                                              | null for `"remove-method"`/`"remove-class"`/`"rename-class"`/`"rename-method"` (nothing replaces the deleted text; a multi-site entry's per-site bodies live under `sites` instead) |
| `prev_source_ref`      | string \| null                                              | null for `new-class`; the removed class's full prior source for `"remove-class"`; null for `"rename-class"`/`"rename-method"` (superseded by `sites`) |
| `sourceFile`           | string \| null                                              | null for stdlib/dynamic; also null for `"rename-class"`/`"rename-method"` (ambiguous for a multi-file entry — see `sites`) |
| `span`                 | `{start,end}` \| null                                       | null for `new-class` and `"remove-class"` (no byte range within a whole-file removal); the excised span for `"remove-method"` (a future flush-excise step); null for `"rename-class"`/`"rename-method"` (see `sites`) |
| `old_class`            | string \| null                                              | ADR 0114: `"rename-class"`-only — the pre-rename class name |
| `old_selector`         | string \| null                                              | ADR 0114: `"rename-method"`-only — the pre-rename selector (`selector` holds the new one) |
| `old_path`/`new_path`  | string \| null                                              | ADR 0114: `"rename-class"`-only — the file path before/after a rename that also moves the backing file; null for a dynamic class |
| `sites`                | `[{sourceFile,span,source_ref,prev_source_ref}\|null,...] \| null` | ADR 0114: `"rename-class"`/`"rename-method"`-only — `sites[0]` is the definition/declaration site (`null` only for a dynamic class with no backing file), `sites[1..]` are every other rewritten reference |
| `candidate_sites`      | `[{sourceFile,span},...] \| null`                           | ADR 0114: `"rename-method"`-only — reported, never auto-rewritten senders; no `source_ref`/`prev_source_ref` since nothing here is ever spliced |
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
stdlib facade are later phases. In run mode (no workspace, no
`workspace_id`) the gen_server keeps state in ETS only and never touches disk —
release nodes do not start a workspace, so this code is a no-op there.
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include("beamtalk_workspace_changelog.hrl").

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
    %% ADR 0114: per-site body persistence for the shared
    %% multi-site rewrite mechanism — see this function's doc.
    store_site_body/1
]).

%% The `Workspace changeLog` / `dirtyMethods` views live in
%% `beamtalk_workspace_changelog_views` (the Beamtalk FFI surface, ADR 0082
%% Phase 1); these two helpers are exported for that module.
-export([
    is_active/1,
    survivor_seqs/1
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
    %% ADR 0114: reads a `site()`'s own ref directly, for
    %% `beamtalk_workspace_flush`'s multi-site rename-class splice — see this
    %% function's own doc for why it cannot reuse `read_source_body/1`/
    %% `read_prev_source_body/1` verbatim.
    read_site_body/1,
    %% ADR 0114.
    entry_old_class/1,
    entry_old_selector/1,
    entry_old_path/1,
    entry_new_path/1,
    entry_sites/1,
    entry_candidate_sites/1
]).

%% ADR 0114: shadow-detection and flushability helpers for the
%% multi-site `'rename-class'`/`'rename-method'` kinds.
-export([
    target_key/1,
    shadow_key/1,
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
-export([read_source_file/1]).

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
    %% ADR 0114: see the matching `#entry{}` fields' docs above.
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
recorded prior source body (ADR 0082 Phase 4; add/new-class/class-side
extensions; `remove-class` extension ADR 0113).

Used by `Workspace changes revert: aMethod` to look up the pre-patch state that
must be restored. Returns:

  - `{ok, PrevBody, Entry}` when an active entry for the target exists *and* its
    prior body can be recovered (from a recorded `prev_source_ref`, or from the
    current on-disk body) — the typical method-*modify* revert path. The entry's
    `kind` (`instance`/`class`) tells the caller which side to re-install on. This
    is also the path a `'remove-method'` entry reaches: its `prev_source_ref` is
    the removed method's pre-removal body, so re-installing it (on the entry's
    recorded `side`) is exactly what undoes the removal (ADR 0112).
  - `{remove, Entry}` when the most recent active entry is an *addition*: a
    brand-new method whose selector did not exist before the patch (no recorded
    prior body AND the selector is absent from the on-disk source / the class has
    no on-disk source), or a `new-class` entry. The pre-patch state was "this did
    not exist", so revert is a removal rather than a body re-install.
  - `{reinstall_class, PrevBody, Entry}` when the most recent active entry is a
    `'remove-class'` (ADR 0113): the pre-removal state was "this class
    existed", recorded as the whole-file `prev_source_ref` `capture_class_removal_
    snapshot/1` captures before teardown (falling back to a direct read of the
    entry's own `sourceFile` — `recover_class_prev_from_disk/1` — when the
    recorded body itself is unreadable, e.g. pruned by ChangeLog rotation).
    Revert recompiles and reinstalls the whole class from `PrevBody`, not a
    single-method patch.
  - `{revert_rename, Entry}` when the most recent active entry is a
    `'rename-class'`/`'rename-method'` (ADR 0114): a multi-site
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
with an explicit `Side` (ADR 0112) when the caller knows which side it
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
(`instance` | `class`) when it is not `undefined` (ADR 0112). Side is
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
            %% Reverting a new-class creation removes the class.
            {remove, Entry};
        [#entry{kind = 'remove-class'} = Entry | _] ->
            %% Reverting a class removal recompiles and reinstalls the whole
            %% class from its recorded prior source (ADR 0113). If
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
            %% ADR 0114: a multi-site target — the caller rewrites
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
            %% → revert = removal). recover_prev_from_disk/1 tells them
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
%%   - `{remove, Entry}'    — add: remove the just-added method.
%%   - `{error, no_prev_source}' — modify whose prior body is genuinely
%%     unrecoverable: the file can't be read, the span no longer resolves, or
%%     the selector is absent but the entry recorded a `prev_source_ref' (so it
%%     was definitively a modify, not an add); never silently delete it.
%%
%% Invariant + limit: the on-disk body returned for a modify is the true
%% pre-patch body only while the entry is unflushed AND the file has not been
%% edited externally (VSCode/git) since the patch. The normal-flow entries that
%% *do* record `prev_source' don't reach here; this is a
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
    %% entry's raw `kind` can be `'remove-method'` (ADR 0112), which
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
                            %% is "absent", so revert removes it.
                            %%
                            %% Residual ambiguity (accepted, unavoidable): an
                            %% older *modify* entry recorded before prev_source_ref
                            %% tracking also carries no prev_source_ref, so if its
                            %% method was externally removed from the file (git
                            %% restore / editor revert) AFTER the live patch, we
                            %% cannot tell it apart from an add and will treat
                            %% revert as a removal. Normal-flow entries record
                            %% prev_source_ref and never reach this branch.
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
            %% NOT be silently deleted — surface a loud error.
            {error, no_prev_source}
    end;
recover_prev_from_disk(_Entry) ->
    %% No on-disk source attribution (dynamic / source-less class), or any other
    %% shape we cannot resolve against disk. We cannot positively distinguish an
    %% *add* from a *modify* here — there is no `prev_source` and no file to probe
    %% for the selector — so refuse loudly rather than risk deleting a method that
    %% existed before the patch (never a silent delete). Positive
    %% add evidence only comes from `selector_not_found` against a readable source
    %% file (handled above), which is the new-method-on-a-project-class case the
    %% LiveView add-revert flow produces.
    {error, no_prev_source}.

-doc """
Fallback for a `'remove-class'` entry (ADR 0113) whose recorded
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

%% For each `(class, selector, side)` target, the seq of the most-recent
%% *active* entry — the "survivor" that `Workspace flush` would apply and that
%% the pending-changes view shows. Every other active entry for the same
%% target is *shadowed*: an older patch (or a patch superseded by a revert,
%% since a revert is itself a patch — ADR 0082 "Undo") that newer state
%% replaced. Flush's `shadow_duplicates/1` uses the same `shadow_key/1`
%% (BT-4859), so the displayed dirty set matches what flush actually writes. Inactive entries (prior-epoch / orphan / flushed) never
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
%% `survivor_seqs/1`, `entry_to_value/2` and flush's `shadow_duplicates/1` so all agree on what shadows what.
%%
%% Whole-class-level entries (`'new-class'`, `'class-def'`, `'remove-class'`)
%% carry no `selector`, so without a tie-breaker they would all collide on
%% the same `(class, undefined, undefined)` key. Concretely: a `'class-def'`
%% redefinition (always `flushable: false`) of a class whose
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
-spec shadow_key(entry()) -> shadow_key().
shadow_key(#entry{selector = undefined} = E) ->
    {E#entry.class, undefined, E#entry.kind};
shadow_key(E) ->
    {E#entry.class, E#entry.selector, entry_side(E)}.

%%% ----------------------------------------------------------------------------
%%% Per-site shadow-detection key (ADR 0114)
%%% ----------------------------------------------------------------------------

-doc """
Per-site shadow-detection keys for `Entry` (ADR 0114).

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

Exported for the future multi-site rewrite mechanism and for
tests. Not yet wired into `survivor_seqs/1`/the `ChangeEntry` `shadowed`
flag — `'rename-class'`/`'rename-method'` flush and dirty-view integration
is out of scope here (schema only).
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
`beamtalk_repl_loader_changelog_entries:classify_source_file/1`/`no_source_reason/1` exactly
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

%%% ----------------------------------------------------------------------------
%%% Entry accessors
%%% ----------------------------------------------------------------------------
%%% The entry() type is opaque; these accessors are the supported way to read a
%%% ChangeEntry's fields (consumed by later phases and by tests).

-spec entry_seq(entry()) -> non_neg_integer().
entry_seq(#entry{seq = V}) -> V.

-doc """
The workspace epoch `Entry` was appended in (ADR 0113).

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
flush_file_kind_wire_corpus.json`) that pins the Rust LSP consumer's
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
    beamtalk_workspace_kinds:all().

-doc """
The side (`instance` | `class`) a method-shaped entry targets, or `undefined`
for an entry with no side (`'new-class'`, `unknown`) (ADR 0112).

The supported way to read side — never pattern-match `#entry.side` or
`entry_kind/1` directly for this. A `'remove-method'` entry stores `side`
explicitly (`kind` alone can no longer double as the side discriminator once
it is spent distinguishing removal from patch); a legacy `instance`/`class`
-kind patch entry has no stored `side` — it derives one from its own `kind`,
exactly as `kind` always doubled for side before this ADR. This single
accessor is what both the shared shadow key (`shadow_key/1`, used by flush) and `revert:`'s side-resolution
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

-doc "The pre-rename class name for a `'rename-class'` entry (ADR 0114); `undefined` otherwise.".
-spec entry_old_class(entry()) -> binary() | undefined.
entry_old_class(#entry{old_class = V}) -> V.

-doc "The pre-rename selector for a `'rename-method'` entry (ADR 0114); `undefined` otherwise.".
-spec entry_old_selector(entry()) -> binary() | undefined.
entry_old_selector(#entry{old_selector = V}) -> V.

-doc "The pre-rename file path for a `'rename-class'` entry (ADR 0114); `undefined` otherwise.".
-spec entry_old_path(entry()) -> binary() | undefined.
entry_old_path(#entry{old_path = V}) -> V.

-doc "The post-rename file path for a `'rename-class'` entry (ADR 0114); `undefined` otherwise.".
-spec entry_new_path(entry()) -> binary() | undefined.
entry_new_path(#entry{new_path = V}) -> V.

-doc """
The multi-site rewrite list for a `'rename-class'`/`'rename-method'` entry
(ADR 0114); `undefined` otherwise. `sites[0]` is always the
definition/declaration site; a bare `undefined` element (rather than a
`site()` map) is the dynamic-class "no declaration site" case.
""".
-spec entry_sites(entry()) -> [site() | undefined] | undefined.
entry_sites(#entry{sites = V}) -> V.

-doc """
The reported-but-never-rewritten candidate sender list for a
`'rename-method'` entry (ADR 0114); `undefined` otherwise
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
`prev_source_ref`) from `<workspace>/changes/sources/<ref>.bt` (ADR 0114).

Generalizes `read_source_body/1`/`read_prev_source_body/1` to a `site()`'s
own per-site ref, rather than the OWNING ENTRY's top-level `source_ref`/
`prev_source_ref` — which are always `undefined` for a `'rename-class'`/
`'rename-method'` entry (the per-site bodies live under `sites` instead, see
the ChangeEntry schema doc's `source_ref`/`sites` rows). `beamtalk_workspace_
flush`'s multi-site rename-class splice needs to read a specific site's ref
directly, so this takes the raw `binary() | undefined` ref rather than an
`entry()` — reuses `read_source_file/1` verbatim (AGENTS.md's no-duplicate-
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
entry's `sites` list (ADR 0114).

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
            case beamtalk_workspace_changelog_log:write_file_atomic(Path, Body) of
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
    ChangesDir = beamtalk_workspace_changelog_log:changes_dir(WorkspaceId),
    beamtalk_workspace_changelog_log:warn_if_persistence_skipped(WorkspaceId, ChangesDir),
    ensure_ets(),
    State0 = #state{
        changes_dir = ChangesDir,
        log_path = beamtalk_workspace_changelog_log:log_path(ChangesDir),
        next_seq = 0,
        epoch = 0
    },
    beamtalk_workspace_changelog_log:sweep_stale_temps(ChangesDir),
    State1 = beamtalk_workspace_changelog_log:load_from_disk(State0),
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
    beamtalk_workspace_changelog_log:truncate_log(State),
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
do_append(RawInput, State) ->
    %% BT-4173: entries persist raw bytes (disk format stability) — unwrap
    %% tagged Strings from FFI input at the boundary.
    Input = normalize_append_input(RawInput),
    Seq = State#state.next_seq,
    Source = maps:get(source, Input, undefined),
    %% ADR 0112: `source` is optional — a `'remove-method'` entry has no new
    %% body, so it gets no `source_ref` (and therefore no on-disk source file;
    %% mirrors how `prev_source_ref` already stays `undefined` when there is
    %% no `prev_source`).
    SourceRef =
        case Source of
            undefined -> undefined;
            _ -> beamtalk_workspace_changelog_log:source_ref_filename(Seq, source)
        end,
    PrevSource = maps:get(prev_source, Input, undefined),
    PrevSourceRef =
        case PrevSource of
            undefined -> undefined;
            _ -> beamtalk_workspace_changelog_log:source_ref_filename(Seq, prev)
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
    case beamtalk_workspace_changelog_log:persist_append(Entry, Source, PrevSource, State) of
        ok ->
            ets:insert(?ETS_TABLE, {Seq, Entry}),
            State1 = State#state{next_seq = Seq + 1},
            State2 = maybe_rotate(State1),
            {ok, Seq, State2};
        {error, Reason} ->
            {error, append_error(Reason)}
    end.

%% BT-4173: unwrap tagged-String text fields from FFI `append:` input —
%% the entry record and on-disk format stay raw binaries.
-spec normalize_append_input(map()) -> map().
normalize_append_input(Input) ->
    Fields = [
        class,
        selector,
        source,
        prev_source,
        source_file,
        old_class,
        old_selector,
        old_path,
        new_path,
        author,
        not_flushable_reason
    ],
    lists:foldl(
        fun(Field, Acc) ->
            case maps:get(Field, Acc, undefined) of
                {beamtalk_string, Bin} when is_binary(Bin) -> Acc#{Field => Bin};
                _ -> Acc
            end
        end,
        Input,
        Fields
    ).

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
    case beamtalk_workspace_changelog_log:rewrite_log(Updated, State) of
        ok ->
            lists:foreach(fun(E) -> ets:insert(?ETS_TABLE, {E#entry.seq, E}) end, Updated),
            ok;
        {error, _} = Err ->
            Err
    end.

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
            beamtalk_workspace_changelog_log:rotate_transactional(ToArchive, ToKeep, State)
    end.

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
%%% Errors
%%% ----------------------------------------------------------------------------

-spec append_error(term()) -> #beamtalk_error{}.
append_error(Reason) ->
    #beamtalk_error{
        kind = changelog_write_error,
        class = 'ChangeLog',
        selector = 'append:',
        %% BT-4173: error message/hint are tagged Strings (see beamtalk.hrl).
        message = beamtalk_primitive:string_from_binary(
            <<"Failed to persist ChangeLog entry to disk">>
        ),
        hint = beamtalk_primitive:string_from_binary(
            <<"Check that the workspace changes/ directory is writable">>
        ),
        details = #{reason => Reason}
    }.
