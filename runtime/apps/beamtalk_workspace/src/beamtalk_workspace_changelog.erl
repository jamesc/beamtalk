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

| Field                  | Type                                   | Notes |
|------------------------|----------------------------------------|-------|
| `ts`                   | integer (ms since epoch)               | append time |
| `seq`                  | integer                                | monotonic per workspace |
| `epoch`                | integer                                | bumped each workspace start |
| `class`                | string                                 | e.g. `"Counter"` |
| `selector`             | string \| null                         | null for `new-class` |
| `kind`                 | `"instance"`\|`"class"`\|`"new-class"` | open enum |
| `source_ref`           | string                                 | filename in `sources/` |
| `prev_source_ref`      | string \| null                         | null for `new-class` |
| `sourceFile`           | string \| null                         | null for stdlib/dynamic |
| `span`                 | `{start,end}` \| null                  | null for `new-class` |
| `intent`               | `"durable"`\|`"ephemeral"`             | |
| `flushable`            | boolean                                | true iff in-project source |
| `not_flushable_reason` | string \| null                         | `"stdlib"`/`"dynamic"`/`"dependency:<path>"` |
| `author`               | string                                 | session/tool id |
| `author_kind`          | `"human"`\|`"agent"`                   | audit metadata |

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
install hook that *emits* entries, `Workspace flush`, and the `ChangeLog.bt`
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
    find_revert_target/2
]).

%% Beamtalk FFI surface (ADR 0082 Phase 1, BT-2284). These build the data the
%% `ChangeLog.bt` / `ChangeEntry.bt` value objects wrap: each entry becomes a
%% `$beamtalk_class`-tagged map and `dirtyMethods/0` returns the per-class set
%% of dirty selectors. The FFI dispatches on the Beamtalk selector verbatim, so
%% these entry points are named in camelCase (`changeLog`, `dirtyMethods`) to
%% match the selectors used in `ChangeLog.bt` / `WorkspaceInterface.bt`. Called
%% via `(Erlang beamtalk_workspace_changelog) ...` from the compiled stdlib.
-export([
    changeLog/0,
    dirtyMethods/0,
    change_entries/0
]).

%% Accessors on the opaque entry type (used by callers and tests).
-export([
    entry_seq/1,
    entry_class/1,
    entry_selector/1,
    entry_kind/1,
    entry_intent/1,
    entry_flushable/1,
    entry_flushed/1,
    entry_author_kind/1,
    entry_is_orphan/1,
    entry_is_prior_epoch/1,
    entry_source_file/1,
    entry_span/1,
    entry_source_ref/1,
    entry_prev_source_ref/1,
    read_source_body/1,
    read_prev_source_body/1
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
-export([changes_dir/1, entry_to_json/1, entry_from_json/1, body_delta/2]).

-define(ETS_TABLE, beamtalk_changelog_entries).
%% Bounded ring: keep at most this many entries on disk before rotating older
%% segments into archive/ (ADR 0082, "ChangeLog growth").
-define(MAX_ENTRIES, 1000).

%%% ----------------------------------------------------------------------------
%%% Types
%%% ----------------------------------------------------------------------------

%% `kind` is an open enum (ADR 0082): newer writers may add values this beam does
%% not know. Decoding maps any unrecognised value to `unknown` so history is
%% preserved across versions rather than dropped.
-type kind() :: instance | class | 'new-class' | unknown.
-type intent() :: durable | ephemeral | unknown.
-type author_kind() :: human | agent | unknown.
-type span() :: #{start := non_neg_integer(), 'end' := non_neg_integer()}.

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
    source_ref :: binary(),
    prev_source_ref :: binary() | undefined,
    source_file :: binary() | undefined,
    span :: span() | undefined,
    intent :: intent(),
    flushable :: boolean(),
    not_flushable_reason :: binary() | undefined,
    author :: binary(),
    author_kind :: author_kind(),
    %% True once a `Workspace flush` has written this entry's patch to disk
    %% (ADR 0082 Phase 2). Persisted so the entry stays excluded from the
    %% active view across workspace restarts: history is preserved in the
    %% log for audit, but the entry is no longer considered "dirty".
    flushed = false :: boolean(),
    %% Derived, in-memory only — not persisted (recomputed on restart).
    prior_epoch = false :: boolean(),
    orphan = false :: boolean()
}).

-opaque entry() :: #entry{}.

%% Input map accepted by append/1. Bodies (`source`, `prev_source`) are passed
%% in full; the gen_server writes them to sources/ and stores only the refs.
-type append_input() :: #{
    class := binary(),
    kind := kind(),
    source := binary(),
    intent := intent(),
    flushable := boolean(),
    author := binary(),
    author_kind := author_kind(),
    selector => binary() | undefined,
    prev_source => binary() | undefined,
    source_file => binary() | undefined,
    span => span() | undefined,
    not_flushable_reason => binary() | undefined
}.

-export_type([entry/0, append_input/0, kind/0, intent/0, author_kind/0, span/0]).

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
""".
-spec entries() -> [entry()].
entries() ->
    case ets:info(?ETS_TABLE, id) of
        undefined ->
            [];
        _ ->
            Es = [E || {_Seq, E} <- ets:tab2list(?ETS_TABLE)],
            lists:keysort(#entry.seq, Es)
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
recorded prior source body (ADR 0082 Phase 4, BT-2290).

Used by `Workspace changes revert: aMethod` to look up the pre-patch body that
must be re-installed. Returns:

  - `{ok, PrevBody, Entry}` when an active entry for the target exists *and* has
    a `prev_source_ref` whose body can be read from `sources/` — the typical
    method-revert path.
  - `{error, no_entry}` when no active entry targets `(Class, Selector)`
    (nothing to revert: either never patched, or already reverted/flushed).
  - `{error, no_prev_source}` when the most recent entry's `prev_source_ref`
    is missing or unreadable (e.g. an entry from before the source-body
    persistence — should not happen in normal flow but defensive).

`Class` is the unsuffixed display name as a binary; `Selector` is an atom or a
binary (an atom is converted to a binary so the comparison matches the entry's
recorded selector). A new-class entry has `selector = undefined` and therefore
never matches a regular selector lookup — the rejection of new-class reverts
lives at the FFI boundary (`changeLogRevert/1`) rather than here.
""".
-spec find_revert_target(binary(), atom() | binary()) ->
    {ok, binary(), entry()} | {error, no_entry | no_prev_source}.
find_revert_target(Class, Selector) when is_binary(Class) ->
    SelectorBin = revert_selector_binary(Selector),
    Candidates = lists:filter(
        fun(E) ->
            E#entry.class =:= Class andalso
                E#entry.selector =:= SelectorBin andalso
                (not E#entry.prior_epoch) andalso
                (not E#entry.orphan) andalso
                (not E#entry.flushed)
        end,
        entries()
    ),
    case lists:reverse(lists:keysort(#entry.seq, Candidates)) of
        [] ->
            {error, no_entry};
        [#entry{prev_source_ref = undefined} = Entry | _] ->
            %% No recorded prior body. Fall back to the method's CURRENT on-disk
            %% body when the entry knows its source file — an unflushed patch's
            %% disk body IS its pre-patch body, so revert can still reconstruct it
            %% rather than failing outright (resilience for entries recorded
            %% before source attribution was preserved).
            recover_prev_from_disk(Entry);
        [Entry | _] ->
            case read_prev_source_body(Entry) of
                {ok, Body} -> {ok, Body, Entry};
                {error, _} -> recover_prev_from_disk(Entry)
            end
    end.

%% Reconstruct a method's pre-patch body from its on-disk source file when no
%% `prev_source' was recorded. Returns `{ok, Body, Entry}' on success, else
%% `{error, no_prev_source}'. A brand-new method (absent on disk) has no prior
%% body to recover, so `resolve_method_span' reports `selector_not_found' and we
%% surface the original error.
%%
%% Invariant + limit: this returns the method's CURRENT on-disk body, which is
%% the true pre-patch body only while the entry is unflushed AND the file has
%% not been edited externally (VSCode/git) since the patch. The normal-flow
%% entries that *do* record `prev_source' (BT-2553 follow-up) don't reach here;
%% this is a best-effort fallback for entries that predate source attribution,
%% so reverting to the live disk body is the most faithful reconstruction
%% available — a later flush still runs its own byte-span/prev_source conflict
%% check before writing.
-spec recover_prev_from_disk(entry()) -> {ok, binary(), entry()} | {error, no_prev_source}.
recover_prev_from_disk(
    #entry{source_file = File, class = Class, selector = Selector, kind = Kind} = Entry
) when
    is_binary(File), is_binary(Selector)
->
    case file:read_file(File) of
        {ok, DiskSource} ->
            case beamtalk_compiler:resolve_method_span(DiskSource, Class, Selector, Kind) of
                {ok, _Span, PrevBody} -> {ok, PrevBody, Entry};
                _ -> {error, no_prev_source}
            end;
        {error, _} ->
            {error, no_prev_source}
    end;
recover_prev_from_disk(_Entry) ->
    {error, no_prev_source}.

%% Normalise the selector argument: callers may pass an atom or a binary.
-spec revert_selector_binary(atom() | binary()) -> binary().
revert_selector_binary(Sel) when is_binary(Sel) -> Sel;
revert_selector_binary(Sel) when is_atom(Sel) -> atom_to_binary(Sel, utf8).

%%% ----------------------------------------------------------------------------
%%% Beamtalk FFI surface (ADR 0082 Phase 1, BT-2284)
%%% ----------------------------------------------------------------------------
%%% These functions translate the opaque `#entry{}` records into the
%%% `$beamtalk_class`-tagged maps that the `ChangeLog.bt` / `ChangeEntry.bt`
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
of entries (as `ChangeEntry` maps) under `entries`, so the `ChangeLog.bt`
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
dispatches the instance methods defined in `ChangeEntry.bt`. The full set is
returned (including prior-epoch and orphan entries) so `ChangeLog select:` can
still reach them; the default collection views filter on the per-entry `active`
flag in Beamtalk. Internal helper for `changeLog/0`; not used by the stdlib API
(`ChangeLog.bt` only ever calls `changeLog`) — exported for tests/helpers.
""".
-spec change_entries() -> [map()].
change_entries() ->
    All = entries(),
    Survivors = survivor_seqs(All),
    [entry_to_value(E, Survivors) || E <- All].

%% For each `(class, selector)` target, the seq of the most-recent *active*
%% entry — the "survivor" that `Workspace flush` would apply and that the
%% pending-changes view shows. Every other active entry for the same target is
%% *shadowed*: an older patch (or a patch superseded by a revert, since a revert
%% is itself a patch — ADR 0082 "Undo") that newer state replaced. Mirrors
%% `beamtalk_workspace_flush:shadow_duplicates/1` so the displayed dirty set
%% matches what flush actually writes. Inactive entries (prior-epoch / orphan /
%% flushed) never survive and never shadow.
-spec survivor_seqs([#entry{}]) -> #{{binary(), binary() | undefined} => non_neg_integer()}.
survivor_seqs(Entries) ->
    lists:foldl(
        fun(E, Acc) ->
            case is_active(E) of
                false ->
                    Acc;
                true ->
                    Key = {E#entry.class, E#entry.selector},
                    case Acc of
                        #{Key := Max} when Max >= E#entry.seq -> Acc;
                        _ -> Acc#{Key => E#entry.seq}
                    end
            end
        end,
        #{},
        Entries
    ).

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
%% placeholder so the per-class entry is still visible.
-spec dirty_selector(#entry{}) -> atom().
dirty_selector(#entry{selector = undefined}) -> 'new-class';
dirty_selector(#entry{selector = Sel}) -> binary_to_atom(Sel, utf8).

%% Build a `ChangeEntry` value-object map from an `#entry{}` record. Field keys
%% match the `field:` declarations in `ChangeEntry.bt`; `self.field` reads them.
%% Atoms (selector, kind, intent, authorKind) are surfaced as Beamtalk Symbols;
%% the derived `active` flag is `true` iff the entry is current-epoch, not an
%% orphan, and not flushed (the default dirty view). The derived `shadowed` flag
%% is `true` iff the entry is active but a *newer* active entry exists for the
%% same `(class, selector)` — an older patch superseded by a later patch/revert.
%% `Survivors` maps each `(class, selector)` to its surviving (highest) seq.
-spec entry_to_value(#entry{}, map()) -> map().
entry_to_value(#entry{} = E, Survivors) ->
    Active = is_active(E),
    Shadowed =
        Active andalso
            maps:get({E#entry.class, E#entry.selector}, Survivors, E#entry.seq) =/= E#entry.seq,
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
        diff => diff_value(Diff)
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

-spec entry_class(entry()) -> binary().
entry_class(#entry{class = V}) -> V.

-spec entry_selector(entry()) -> binary() | undefined.
entry_selector(#entry{selector = V}) -> V.

-spec entry_kind(entry()) -> kind().
entry_kind(#entry{kind = V}) -> V.

-spec entry_intent(entry()) -> intent().
entry_intent(#entry{intent = V}) -> V.

-spec entry_flushable(entry()) -> boolean().
entry_flushable(#entry{flushable = V}) -> V.

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

-spec entry_source_ref(entry()) -> binary().
entry_source_ref(#entry{source_ref = V}) -> V.

-spec entry_prev_source_ref(entry()) -> binary() | undefined.
entry_prev_source_ref(#entry{prev_source_ref = V}) -> V.

-doc """
Read the patched method body (or full new-class source) recorded for `Entry`
from `<workspace>/changes/sources/<source_ref>.bt`.

Returns `{ok, Body}` or `{error, Reason}`. Used by `Workspace flush` (ADR 0082
Phase 2) to splice the patched body back into the on-disk file. In run mode (no
workspace_id, no `changes/` dir) returns `{error, no_workspace}`.
""".
-spec read_source_body(entry()) -> {ok, binary()} | {error, term()}.
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

%%% ----------------------------------------------------------------------------
%%% gen_server callbacks
%%% ----------------------------------------------------------------------------

init(Config) ->
    %% Inherit the runtime logging domain for every log call from this process.
    logger:set_process_metadata(#{domain => [beamtalk, runtime]}),
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
    SourceRef = source_ref_filename(Seq, source),
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
        source_ref = SourceRef,
        prev_source_ref = PrevSourceRef,
        source_file = maps:get(source_file, Input, undefined),
        span = maps:get(span, Input, undefined),
        intent = maps:get(intent, Input),
        flushable = maps:get(flushable, Input),
        not_flushable_reason = maps:get(not_flushable_reason, Input, undefined),
        author = maps:get(author, Input),
        author_kind = maps:get(author_kind, Input)
    },
    case persist_append(Entry, maps:get(source, Input), PrevSource, State) of
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
-spec persist_append(#entry{}, binary(), binary() | undefined, #state{}) ->
    ok | {error, term()}.
persist_append(_Entry, _Source, _PrevSource, #state{changes_dir = undefined}) ->
    ok;
persist_append(Entry, Source, PrevSource, State) ->
    SourcesDir = filename:join(State#state.changes_dir, "sources"),
    case filelib:ensure_path(SourcesDir) of
        ok ->
            SourcePath = filename:join(SourcesDir, binary_to_list(Entry#entry.source_ref)),
            case write_file_atomic(SourcePath, Source) of
                ok ->
                    case write_prev_source(SourcesDir, Entry, PrevSource) of
                        ok -> append_metadata_line(Entry, State);
                        Err -> Err
                    end;
                Err ->
                    Err
            end;
        {error, Reason} ->
            {error, {ensure_path, SourcesDir, Reason}}
    end.

-spec write_prev_source(string(), #entry{}, binary() | undefined) -> ok | {error, term()}.
write_prev_source(_SourcesDir, #entry{prev_source_ref = undefined}, _PrevSource) ->
    ok;
write_prev_source(SourcesDir, #entry{prev_source_ref = Ref}, PrevSource) ->
    Path = filename:join(SourcesDir, binary_to_list(Ref)),
    write_file_atomic(Path, PrevSource).

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

-spec refs_of(#entry{}) -> [binary()].
refs_of(#entry{source_ref = SR, prev_source_ref = undefined}) -> [SR];
refs_of(#entry{source_ref = SR, prev_source_ref = PR}) -> [SR, PR].

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
            ets:new(?ETS_TABLE, [named_table, public, set, {read_concurrency, true}]);
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
        <<"source_ref">> => E#entry.source_ref,
        <<"prev_source_ref">> => null_or(E#entry.prev_source_ref),
        <<"sourceFile">> => null_or(E#entry.source_file),
        <<"span">> => span_to_json(E#entry.span),
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
        source_ref = maps:get(<<"source_ref">>, Map),
        prev_source_ref = from_null(maps:get(<<"prev_source_ref">>, Map, null)),
        source_file = from_null(maps:get(<<"sourceFile">>, Map, null)),
        span = span_from_json(maps:get(<<"span">>, Map, null)),
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

-spec span_to_json(span() | undefined) -> map() | null.
span_to_json(undefined) -> null;
span_to_json(#{start := Start, 'end' := End}) -> #{<<"start">> => Start, <<"end">> => End}.

-spec span_from_json(map() | null) -> span() | undefined.
span_from_json(null) -> undefined;
span_from_json(#{<<"start">> := Start, <<"end">> := End}) -> #{start => Start, 'end' => End}.

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
