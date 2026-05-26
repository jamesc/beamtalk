%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_flush).

%%% **DDD Context:** Workspace Context

-moduledoc """
`Workspace flush` and `Workspace flush:` implementation (ADR 0082 Phase 2).

Writes pending ChangeLog entries to disk via trivia-preserving byte-span splice
(no AST reprint), atomically (`<file>.tmp` → atomic rename), with external-edit
conflict detection and post-write pruning of the affected entries from the
active view.

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
  files => [<<"path1">>, ...],          %% files written (in rename order)
  newClasses => M,                      %% subset of `flushed` for new-class entries
  skipped => [#{seq, reason}],          %% durable non-flushable entries seen
  conflicts => [#{file, reason, seqs}]  %% Phase A / Phase B conflicts
}
```

A success path returns `flushed > 0` (or `0` when there is nothing to flush),
zero conflicts, an empty `skipped` list, and `files` reflecting the renamed set.
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([
    flush/0,
    flush/1,
    flush_kinds/1
]).

%% Exported for tests.
-export([
    splice/3,
    group_by_file/1,
    complete_flush/4,
    filter_shadowed_by_survivor/2,
    renamed_target_keys/1
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
Flush all pending durable+flushable ChangeEntries (ADR 0082 Phase 2).

Returns the summary map described in the module docs. Never raises on a normal
conflict — conflicts are reported in the `conflicts` field so the caller can
decide what to do (re-flush after editing, discard via `Workspace changes
clear`, inspect via `Workspace diff:`). Hard runtime errors (e.g. the changelog
server is not running) come back as `{error, #beamtalk_error{}}`.
""".
-spec flush() -> {ok, map()} | {error, #beamtalk_error{}}.
flush() ->
    do_flush(any).

-doc """
Flush only the ChangeEntries that match `Filter`.

`Filter` is one of:

  - a Beamtalk class object (a `#beamtalk_object{}` whose class atom ends in
    `" class"`) — filter by the class's display name
  - a class name atom (`'Counter'`) or binary (`<<"Counter">>`) — filter by name
  - a selector atom, including `'new-class'` — filter by entry kind/selector
  - a Beamtalk Dictionary `#{ #file => "..." }` (Symbol-keyed) — filter by
    `sourceFile`

Anything else surfaces a structured error.
""".
-spec flush(term()) -> {ok, map()} | {error, #beamtalk_error{}}.
flush(Filter) ->
    case normalise_filter(Filter) of
        {ok, F} -> do_flush(F);
        {error, _} = Err -> Err
    end.

-doc """
Flush only the ChangeEntries whose kind or author_kind is in `Kinds`
(ADR 0082 Phase 4, BT-2290).

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
fails loudly rather than silently flushing the wrong set.

Returns the same `FlushResult` summary as `flush/0` / `flush/1`.
""".
-spec flush_kinds([atom()]) -> {ok, map()} | {error, #beamtalk_error{}}.
flush_kinds(Kinds) when is_list(Kinds) ->
    case classify_kinds(Kinds) of
        {ok, EntryKinds, AuthorKinds} ->
            do_flush({kinds, EntryKinds, AuthorKinds});
        {error, _} = Err ->
            Err
    end;
flush_kinds(_Other) ->
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
            "flushKinds: requires at least one kind Symbol — use Workspace flush to flush "
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
classify_kind(instance) -> entry;
classify_kind(class) -> entry;
classify_kind('new-class') -> entry;
classify_kind(human) -> author;
classify_kind(agent) -> author;
classify_kind(_) -> unknown.

-spec unknown_kind_error([atom()]) -> #beamtalk_error{}.
unknown_kind_error(Unknowns) ->
    Joined = lists:join(<<", ">>, [atom_to_binary(K, utf8) || K <- Unknowns]),
    filter_error(
        iolist_to_binary([
            <<"flushKinds: unrecognised kind symbol(s): ">>,
            Joined,
            <<
                ". Allowed: #instance, #class, #'new-class' (entry kinds); "
                "#human, #agent (author kinds)"
            >>
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
                    <<"flush: argument is an Object but not a Class — pass a class, a Symbol, or a Dictionary">>
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

-spec do_flush(filter()) -> {ok, map()} | {error, #beamtalk_error{}}.
do_flush(Filter) ->
    Pending = filter_entries(beamtalk_workspace_changelog:flushable_pending(), Filter),
    %% `flushable_pending` already excludes non-flushable entries — the
    %% `skipped` field of the response is documentation of what would be
    %% skipped if non-flushable durable entries were in scope, which only
    %% happens when the caller passes a filter that selects them explicitly.
    %% For Phase 2 the caller-visible "skipped" list is always empty (the
    %% non-flushable entries are simply not in `flushable_pending`); we keep
    %% the field for forward-compat with Phase 3's surfaces.
    case Pending of
        [] ->
            {ok, empty_summary()};
        _ ->
            run_flush(Pending)
    end.

-spec filter_entries([term()], filter()) -> [term()].
filter_entries(Entries, any) ->
    Entries;
filter_entries(Entries, {class, ClassBin}) ->
    [E || E <- Entries, beamtalk_workspace_changelog:entry_class(E) =:= ClassBin];
filter_entries(Entries, {selector, 'new-class'}) ->
    [E || E <- Entries, beamtalk_workspace_changelog:entry_kind(E) =:= 'new-class'];
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

-spec run_flush([term()]) -> {ok, map()} | {error, #beamtalk_error{}}.
run_flush(Pending) ->
    %% Shadow duplicates: for each (class, selector) keep only the highest-seq
    %% entry as the "applied" one. Shadowed entries are also marked flushed
    %% afterwards (their target was reached by a later entry).
    {Applied, Shadowed} = shadow_duplicates(Pending),
    Groups = group_by_file(Applied),
    case phase_a(Groups) of
        {ok, Prepared} ->
            phase_b(Prepared, Shadowed);
        {error, _} = Err ->
            Err;
        {conflict, Conflicts} ->
            {ok, conflict_summary(Conflicts)}
    end.

%%% ----------------------------------------------------------------------------
%%% Shadowing
%%% ----------------------------------------------------------------------------

%% Keep only the most-recent entry for each (class, selector) target. Older
%% entries with the same target are returned as `Shadowed` and will be marked
%% flushed at the end (their patch is already on disk via the newer entry).
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

-spec target_key(term()) -> {binary(), binary() | undefined}.
target_key(E) ->
    {beamtalk_workspace_changelog:entry_class(E), beamtalk_workspace_changelog:entry_selector(E)}.

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

-record(prepared, {
    %% Absolute target path on disk.
    file :: binary(),
    %% The `<file>.tmp` we wrote in Phase A.
    tmp :: string(),
    %% The entries whose patches were merged into this file's new body.
    entries :: [term()],
    %% Whether the target file existed prior to flush (informational).
    pre_existing :: boolean()
}).

-spec phase_a([{binary(), [term()]}]) ->
    {ok, [#prepared{}]} | {error, #beamtalk_error{}} | {conflict, [map()]}.
phase_a(Groups) ->
    phase_a_loop(Groups, [], []).

phase_a_loop([], Prepared, []) ->
    {ok, lists:reverse(Prepared)};
phase_a_loop([], Prepared, Conflicts) ->
    %% Any Phase A conflict aborts the whole flush — clean up tmps already
    %% written (a partial commit is *worse* than a conflict report).
    cleanup_tmps(Prepared),
    {conflict, lists:reverse(Conflicts)};
phase_a_loop([{File, Entries} | Rest], Prepared, Conflicts) ->
    case prepare_file(File, Entries) of
        {ok, Rec} ->
            phase_a_loop(Rest, [Rec | Prepared], Conflicts);
        {conflict, ConflictMap} ->
            phase_a_loop(Rest, Prepared, [ConflictMap | Conflicts]);
        {error, _} = Err ->
            cleanup_tmps(Prepared),
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
        _ -> prepare_splice(File, Entries)
    end;
prepare_file(File, Entries) ->
    case lists:any(fun is_new_class_entry/1, Entries) of
        true ->
            %% Defensive: a new-class entry mixed with siblings should not occur
            %% because subsequent patches against a not-yet-flushed new class
            %% are non-flushable (see comment on prepare_file/2). Surface as a
            %% conflict rather than silently producing an inconsistent file.
            {conflict,
                conflict_map(
                    File,
                    <<"mixed_new_class_and_splice">>,
                    Entries,
                    <<
                        "Cannot flush a new-class entry alongside other patches in the "
                        "same operation — flush the new-class entry first, then re-flush "
                        "to apply method patches against the newly created file"
                    >>
                )};
        false ->
            prepare_splice(File, Entries)
    end.

-spec is_new_class_entry(term()) -> boolean().
is_new_class_entry(E) ->
    beamtalk_workspace_changelog:entry_kind(E) =:= 'new-class'.

%%% ----------------------------------------------------------------------------
%%% New-class write
%%% ----------------------------------------------------------------------------

prepare_new_class(File, Entries, Entry) ->
    AbsPath = binary_to_list(File),
    %% Use file:read_file_info/1 (not filelib:is_regular/1) so any existing
    %% filesystem entry — directory, symlink, unreadable path — is caught
    %% up front as target_exists. Otherwise a directory at the target would
    %% slip past Phase A and fail later with an opaque rename_failed conflict.
    %% Mirrors the BT-2285 fix in beamtalk_repl_loader:validate_target_path/1.
    case file:read_file_info(AbsPath) of
        {error, enoent} ->
            case beamtalk_workspace_changelog:read_source_body(Entry) of
                {ok, Body} ->
                    case write_tmp(AbsPath, Body) of
                        {ok, Tmp} ->
                            {ok, #prepared{
                                file = File,
                                tmp = Tmp,
                                entries = Entries,
                                pre_existing = false
                            }};
                        {error, _} = Err ->
                            wrap_io_error(Err, File)
                    end;
                {error, Reason} ->
                    {error, source_body_error(File, Reason)}
            end;
        _Other ->
            %% Any existing filesystem entry (regular file, directory, symlink)
            %% blocks new-class; also treat unreadable paths (eacces, etc.) as
            %% existing rather than silently overwriting. `_Other` is either
            %% `{ok, FileInfo}` or `{error, Reason}` where Reason is something
            %% other than enoent (e.g. eacces, eloop).
            {conflict,
                conflict_map(File, <<"target_exists">>, Entries, <<
                    "newClass:at: target already exists on disk — choose a different path "
                    "or clear the pending entry"
                >>)}
    end.

%%% ----------------------------------------------------------------------------
%%% Method splice
%%% ----------------------------------------------------------------------------

prepare_splice(File, Entries) ->
    AbsPath = binary_to_list(File),
    case file:read_file(AbsPath) of
        {ok, Disk} ->
            case apply_splices(Disk, Entries) of
                {ok, NewBody} ->
                    case write_tmp(AbsPath, NewBody) of
                        {ok, Tmp} ->
                            {ok, #prepared{
                                file = File,
                                tmp = Tmp,
                                entries = Entries,
                                pre_existing = true
                            }};
                        {error, _} = Err ->
                            wrap_io_error(Err, File)
                    end;
                {conflict, ConflictMap} ->
                    {conflict, ConflictMap};
                {error, _} = Err ->
                    Err
            end;
        {error, Reason} ->
            {conflict,
                conflict_map(
                    File,
                    <<"source_file_unreadable">>,
                    Entries,
                    iolist_to_binary([
                        <<"Could not read source file: ">>, atom_to_binary(Reason, utf8)
                    ])
                )}
    end.

%% Apply each entry's splice to Body. Entries are applied in *descending* span
%% order so earlier byte offsets are not shifted by later replacements. An
%% external-edit conflict on any entry short-circuits with `{conflict, ...}`.
-spec apply_splices(binary(), [term()]) ->
    {ok, binary()} | {conflict, map()} | {error, #beamtalk_error{}}.
apply_splices(Body, Entries) ->
    %% Sort by span start descending so later splices (closer to end of file)
    %% are applied first, leaving earlier spans' byte offsets intact for the
    %% next iteration.
    Sorted = lists:sort(
        fun(A, B) ->
            span_start(A) > span_start(B)
        end,
        Entries
    ),
    apply_splices_loop(Body, Sorted).

apply_splices_loop(Body, []) ->
    {ok, Body};
apply_splices_loop(Body, [Entry | Rest]) ->
    case splice_one(Body, Entry) of
        {ok, NewBody} -> apply_splices_loop(NewBody, Rest);
        Other -> Other
    end.

-spec splice_one(binary(), term()) ->
    {ok, binary()} | {conflict, map()} | {error, #beamtalk_error{}}.
splice_one(Body, Entry) ->
    case beamtalk_workspace_changelog:entry_span(Entry) of
        undefined ->
            %% A splice entry with no span: the install hook recorded
            %% `selector_not_found` (a brand-new method added live, the file
            %% has no prior definition to overwrite). Append the body after
            %% a separating newline if the file does not already end in one.
            case beamtalk_workspace_changelog:read_source_body(Entry) of
                {ok, NewSrc} ->
                    {ok, append_method(Body, NewSrc)};
                {error, Reason} ->
                    {error,
                        source_body_error(
                            beamtalk_workspace_changelog:entry_source_file(Entry), Reason
                        )}
            end;
        #{start := Start, 'end' := End} ->
            case beamtalk_workspace_changelog:read_prev_source_body(Entry) of
                {ok, PrevBody} ->
                    splice_with_prev(Body, Entry, Start, End, PrevBody);
                {error, Reason} ->
                    {error,
                        prev_source_error(
                            beamtalk_workspace_changelog:entry_source_file(Entry), Reason
                        )}
            end
    end.

splice_with_prev(Body, Entry, Start, End, PrevBody) ->
    File = beamtalk_workspace_changelog:entry_source_file(Entry),
    case in_range(Body, Start, End) of
        false ->
            {conflict,
                conflict_map(
                    File,
                    <<"span_out_of_range">>,
                    [Entry],
                    iolist_to_binary([
                        <<
                            "Recorded byte span ",
                            (integer_to_binary(Start))/binary,
                            "..",
                            (integer_to_binary(End))/binary,
                            " is outside the current ",
                            (integer_to_binary(byte_size(Body)))/binary,
                            "-byte file — the file changed externally"
                        >>
                    ])
                )};
        true ->
            Actual = binary:part(Body, Start, End - Start),
            case Actual =:= PrevBody of
                false ->
                    {conflict,
                        conflict_map(
                            File,
                            <<"external_edit">>,
                            [Entry],
                            <<
                                "External edit detected: the bytes at the recorded "
                                "span no longer match the patch's recorded prev_source. "
                                "Re-flush after reconciling, or use `Workspace changes "
                                "clear` to discard the pending entries"
                            >>
                        )};
                true ->
                    case beamtalk_workspace_changelog:read_source_body(Entry) of
                        {ok, NewSrc} ->
                            {ok, splice(Body, {Start, End}, NewSrc)};
                        {error, Reason} ->
                            {error, source_body_error(File, Reason)}
                    end
            end
    end.

-spec in_range(binary(), non_neg_integer(), non_neg_integer()) -> boolean().
in_range(Bin, Start, End) ->
    is_integer(Start) andalso
        is_integer(End) andalso
        Start >= 0 andalso
        End >= Start andalso
        End =< byte_size(Bin).

-doc """
Splice `Replacement` into `Body` at the byte range `{Start, End}` (half-open).
Pure helper exported for tests.
""".
-spec splice(binary(), {non_neg_integer(), non_neg_integer()}, binary()) -> binary().
splice(Body, {Start, End}, Replacement) ->
    Before = binary:part(Body, 0, Start),
    After = binary:part(Body, End, byte_size(Body) - End),
    <<Before/binary, Replacement/binary, After/binary>>.

%% Append a method body (which itself may or may not end in a newline) after a
%% blank-line separator. Used for `selector_not_found` entries — a brand-new
%% method added live whose disk source does not yet have it. Ensures the disk
%% file ends in exactly one trailing newline before appending.
-spec append_method(binary(), binary()) -> binary().
append_method(Body, NewSrc) ->
    Trimmed = strip_trailing_newlines(Body),
    NewSrcWithNl = ensure_trailing_newline(NewSrc),
    <<Trimmed/binary, "\n\n", NewSrcWithNl/binary>>.

strip_trailing_newlines(<<>>) ->
    <<>>;
strip_trailing_newlines(Bin) ->
    case binary:last(Bin) of
        $\n -> strip_trailing_newlines(binary:part(Bin, 0, byte_size(Bin) - 1));
        _ -> Bin
    end.

ensure_trailing_newline(<<>>) ->
    <<"\n">>;
ensure_trailing_newline(Bin) ->
    case binary:last(Bin) of
        $\n -> Bin;
        _ -> <<Bin/binary, "\n">>
    end.

%%% ----------------------------------------------------------------------------
%%% Phase B: rename
%%% ----------------------------------------------------------------------------

%% Sequential rename. On the first failure: stop, mark every already-renamed
%% file's entries as flushed, and surface a per-file status report. Earlier
%% successes are *not* rolled back — POSIX rename is one-way and the on-disk
%% state is the new authoritative source for those files.
-spec phase_b([#prepared{}], [term()]) -> {ok, map()}.
phase_b(Prepared, Shadowed) ->
    phase_b_loop(Prepared, Shadowed, [], []).

phase_b_loop([], Shadowed, Renamed, Failed) ->
    Files = lists:reverse([P#prepared.file || P <- Renamed]),
    RenamedEntries = lists:flatten([P#prepared.entries || P <- Renamed]),
    %% Only mark shadowed entries whose survivor (same class+selector) was
    %% actually renamed in this Phase B. When Phase B aborts mid-loop, a
    %% shadowed entry whose survivor never made it to disk must stay pending
    %% — otherwise we silently lose the change from the active view while it
    %% is not on disk. See Copilot review on PR #2325.
    SurvivorKeys = renamed_target_keys(RenamedEntries),
    AppliedShadowed = filter_shadowed_by_survivor(Shadowed, SurvivorKeys),
    EntriesToMark = RenamedEntries ++ AppliedShadowed,
    Seqs = [beamtalk_workspace_changelog:entry_seq(E) || E <- EntriesToMark],
    complete_flush(Files, lists:reverse(Renamed), Failed, Seqs);
phase_b_loop([P | Rest], Shadowed, Renamed, Failed) ->
    case file:rename(P#prepared.tmp, binary_to_list(P#prepared.file)) of
        ok ->
            phase_b_loop(Rest, Shadowed, [P | Renamed], Failed);
        {error, Reason} ->
            ?LOG_ERROR(
                "Workspace flush: Phase B rename failed",
                #{
                    file => P#prepared.file,
                    reason => Reason,
                    domain => [beamtalk, runtime]
                }
            ),
            %% A Phase B failure: stop, but keep the already-renamed files.
            FailedHere = #{
                file => P#prepared.file,
                reason => <<"rename_failed">>,
                seqs => [seq(E) || E <- P#prepared.entries],
                detail => atom_to_binary(Reason, utf8)
            },
            %% Clean up any unattempted tmps so we do not leave stale files
            %% behind for the next flush to trip over.
            cleanup_tmps([P | Rest]),
            phase_b_loop([], Shadowed, Renamed, [FailedHere | Failed])
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
-spec complete_flush([binary()], [#prepared{}], [map()], [non_neg_integer()]) ->
    {ok, map()}.
complete_flush(Files, Renamed, Failed, Seqs) ->
    %% ADR 0082 Phase 3 (BT-2289): broadcast flush completion so LSP clients
    %% can emit `workspace/applyEdit` for each touched file. Fire BEFORE
    %% `mark_flushed/1` so editor refresh fires reliably in the mixed-success
    %% case where renames succeeded but the ChangeLog server is unreachable —
    %% the files are already on disk at this point and the editor needs to
    %% realign regardless of marker outcome. Fire-and-forget: the broadcaster
    %% swallows missing-server errors so flush never fails on a downstream
    %% subscriber issue.
    beamtalk_flush_events:on_files_flushed(Files),
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
            {ok, success_summary(Files, Renamed, Failed)};
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
                        "mark the entries as flushed — they still appear in "
                        "`Workspace changes` and will conflict on re-flush. "
                        "Detail: "
                    >>,
                    io_lib:format("~p", [Reason])
                ])
            },
            {ok, success_summary(Files, Renamed, [MarkerFailure | Failed])}
    end.

%%% ----------------------------------------------------------------------------
%%% Helpers
%%% ----------------------------------------------------------------------------

-spec write_tmp(string(), binary()) -> {ok, string()} | {error, term()}.
write_tmp(AbsPath, Body) ->
    Tmp = AbsPath ++ ".tmp",
    _ = filelib:ensure_dir(AbsPath),
    case file:write_file(Tmp, Body) of
        ok -> {ok, Tmp};
        {error, Reason} -> {error, {write, Reason}}
    end.

-spec cleanup_tmps([#prepared{}]) -> ok.
cleanup_tmps(Prepared) ->
    lists:foreach(
        fun(#prepared{tmp = Tmp}) ->
            _ = file:delete(Tmp)
        end,
        Prepared
    ),
    ok.

-spec span_start(term()) -> integer().
span_start(E) ->
    case beamtalk_workspace_changelog:entry_span(E) of
        #{start := S} -> S;
        undefined -> -1
    end.

-spec seq(term()) -> non_neg_integer().
seq(E) ->
    beamtalk_workspace_changelog:entry_seq(E).

%%% ----------------------------------------------------------------------------
%%% Summary
%%% ----------------------------------------------------------------------------

-spec empty_summary() -> map().
empty_summary() ->
    base_summary(0, [], 0, [], []).

-spec success_summary([binary()], [#prepared{}], [map()]) -> map().
success_summary(Files, Renamed, Failed) ->
    AllEntries = lists:flatten([P#prepared.entries || P <- Renamed]),
    Flushed = length(AllEntries),
    NewClassCount = length([E || E <- AllEntries, is_new_class_entry(E)]),
    base_summary(Flushed, Files, NewClassCount, [], Failed).

-spec conflict_summary([map()]) -> map().
conflict_summary(Conflicts) ->
    base_summary(0, [], 0, [], Conflicts).

-spec base_summary(non_neg_integer(), [binary()], non_neg_integer(), [map()], [map()]) -> map().
base_summary(Flushed, Files, NewClasses, Skipped, Conflicts) ->
    #{
        '$beamtalk_class' => 'FlushResult',
        flushed => Flushed,
        files => Files,
        newClasses => NewClasses,
        skipped => Skipped,
        conflicts => Conflicts
    }.

-spec conflict_map(binary(), binary(), [term()], binary()) -> map().
conflict_map(File, Reason, Entries, Detail) ->
    #{
        file => File,
        reason => Reason,
        seqs => [seq(E) || E <- Entries],
        detail => Detail
    }.

%%% ----------------------------------------------------------------------------
%%% Errors
%%% ----------------------------------------------------------------------------

-spec filter_error(binary()) -> #beamtalk_error{}.
filter_error(Message) ->
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'flush:'),
    beamtalk_error:with_message(Err1, Message).

-spec source_body_error(binary() | undefined, term()) -> #beamtalk_error{}.
source_body_error(File, Reason) ->
    Err0 = beamtalk_error:new(source_body_unreadable, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'flush'),
    Err2 = beamtalk_error:with_message(
        Err1,
        iolist_to_binary([
            <<"Could not read recorded patch body from ChangeLog sources/ for ">>,
            file_label(File)
        ])
    ),
    beamtalk_error:with_details(Err2, #{reason => Reason}).

-spec prev_source_error(binary() | undefined, term()) -> #beamtalk_error{}.
prev_source_error(File, Reason) ->
    Err0 = beamtalk_error:new(prev_source_unreadable, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'flush'),
    Err2 = beamtalk_error:with_message(
        Err1,
        iolist_to_binary([
            <<"Could not read recorded prior body from ChangeLog sources/ for ">>,
            file_label(File)
        ])
    ),
    beamtalk_error:with_details(Err2, #{reason => Reason}).

-spec wrap_io_error({error, term()}, binary()) -> {error, #beamtalk_error{}}.
wrap_io_error({error, Reason}, File) ->
    Err0 = beamtalk_error:new(flush_io_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'flush'),
    Err2 = beamtalk_error:with_message(
        Err1,
        iolist_to_binary([<<"I/O error writing ">>, file_label(File), <<".tmp">>])
    ),
    {error, beamtalk_error:with_details(Err2, #{reason => Reason})}.

-spec file_label(binary() | undefined) -> binary().
file_label(undefined) -> <<"<unknown>">>;
file_label(File) when is_binary(File) -> File.
