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
  - **Tier 2** — destroys a file: `'remove-class'`. Only applied when the
    caller passes `ConfirmDestructive = true` (`flush/2`, `flush_kinds/2`) or
    calls the unscoped `flush_including_destructive/0`. Never silently
    reached — no workspace setting or environment variable can imply it. A
    pending Tier 2 entry left out of the applied set is reported in the
    summary's `skipped` field with `reason => <<"destructive">>`.

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
    %% ADR 0112 (BT-3187): exercises the `(class, selector, side)` shadow-key
    %% fix directly — the shadow-key logic is exercised without going
    %% through prepare_splice/2.
    shadow_duplicates/1,
    target_key/1,
    %% ADR 0113 Phase 2: tier classification, exported for direct unit
    %% coverage independent of a full flush round-trip.
    entry_tier/1,
    %% ADR 0113 Phase 2: lets a crash-recovery test construct the exact
    %% staged path a real flush attempt would have produced, without
    %% duplicating the naming format in the test module.
    delete_staging_path/3
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
classify_kind(instance) -> entry;
classify_kind(class) -> entry;
classify_kind('new-class') -> entry;
%% ADR 0082 extension (BT-3248): redefining an *existing* class's whole
%% definition composes exactly like any other entry-kind filter.
classify_kind('class-def') -> entry;
%% ADR 0113 Phase 2 (BT-3207): `flushKinds:` accepts both removal kinds too —
%% `#'remove-method'` (Tier 1) composes exactly like any other entry-kind
%% filter; `#'remove-class'` (Tier 2) still needs `confirmDestructive: true`
%% (via `flush_kinds/2`) to actually apply, same as an unfiltered destructive
%% flush — `flushKinds:` only narrows *which* entries are in scope, tiering
%% decides whether an in-scope Tier 2 entry is applied or reported skipped.
classify_kind('remove-method') -> entry;
classify_kind('remove-class') -> entry;
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
                ". Allowed: #instance, #class, #'new-class', #'class-def', "
                "#'remove-method', #'remove-class' (entry kinds); #human, #agent "
                "(author kinds)"
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
    Sel =:= 'new-class'; Sel =:= 'remove-method'; Sel =:= 'remove-class'
->
    %% `'new-class'`/`'remove-class'` entries carry selector: null; a
    %% `'remove-method'` entry carries the real removed selector (e.g. #foo),
    %% not null. Either way, filtering on the bare marker atom is meant to
    %% mean "all removals/additions of this kind", not a literal selector
    %% match — matching on entry_selector/1 would return nothing for the
    %% null-selector kinds and would only ever match a method coincidentally
    %% named e.g. #'remove-method' for the other, so kind-matching is what
    %% this filter form is actually for. Unlike `instance`/`class` (also
    %% valid kind() values, but ambiguous with real getter selectors of the
    %% same name), none of these three collides with a plausible method
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
    %% applied post-shadow (ADR 0112, BT-3187): a `(class, selector, side)`
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
    Groups = group_by_file(ToApply),
    case phase_a(Groups) of
        {ok, Prepared} ->
            phase_b(Prepared, Shadowed, Skipped);
        {error, _} = Err ->
            Err;
        {conflict, Conflicts} ->
            {ok, conflict_summary(Conflicts, Skipped)}
    end.

%%% ----------------------------------------------------------------------------
%%% Tiering (ADR 0113 Phase 2)
%%% ----------------------------------------------------------------------------

-doc """
Classify a ChangeEntry into flush's destructive-confirmation tier.

Tier 1 — edits a still-existing file (`instance`, `class`, `'new-class'`,
`'remove-method'`) — applies under ordinary `flush/0` / `flush/1` /
`flush_kinds/1` with no gate. Tier 2 — destroys a file (`'remove-class'`) —
only applies when the caller passes `ConfirmDestructive = true`.
""".
-spec entry_tier(term()) -> tier1 | tier2.
entry_tier(E) ->
    case beamtalk_workspace_changelog:entry_kind(E) of
        'remove-class' -> tier2;
        _ -> tier1
    end.

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

%% ADR 0112 (BT-3187) required fix: keyed on `(class, selector, side)`, not
%% just `(class, selector)` — once `kind` is spent distinguishing
%% `'remove-method'` from a patch, it can no longer also carry side, so a
%% `(class, selector)`-only key could incorrectly shadow an instance-side
%% patch of `Counter >> #foo` against a class-side `Counter class
%% removeSelector: #foo` (or vice versa). `entry_side/1` derives side from
%% `kind` for legacy `instance`/`class` entries and reads it directly for
%% `'remove-method'` entries, so both shapes key consistently here.
-spec target_key(term()) -> {binary(), binary() | undefined, instance | class | undefined}.
target_key(E) ->
    {
        beamtalk_workspace_changelog:entry_class(E),
        beamtalk_workspace_changelog:entry_selector(E),
        beamtalk_workspace_changelog:entry_side(E)
    }.

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
    %% The staging path Phase A produced: `<file>.tmp` for `op = write`,
    %% `<file>.tmp-delete-<epoch>-<seq>` for `op = delete` (ADR 0113), unused
    %% for `op = noop`.
    tmp :: string(),
    %% The entries whose patches were merged into this file's new body (or,
    %% for `op = delete` / `op = noop`, the single `'remove-class'` entry).
    entries :: [term()],
    %% Whether the target file existed prior to flush (informational for
    %% `op = write`; for `op = delete` distinguishes "this attempt performed
    %% the stage-rename" (`true`) from "a prior attempt already staged it,
    %% this run only resumed" (`false`) — see `cleanup_one/1`).
    pre_existing :: boolean(),
    %% Phase B commit action (ADR 0113): `write` renames `tmp` into `file`
    %% (patches, `'new-class'`, `'remove-method'`); `delete` unlinks the
    %% staged `tmp` (class removal); `noop` performs no I/O (the target file
    %% was already gone — external-edit soft success).
    op = write :: write | delete | noop
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
        'remove-class' -> prepare_remove_class(File, Entries, Entry);
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
                        conflict_map(
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
                    prepare_splice(File, Entries)
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
                    "newClass:at: target already exists on disk; choose a different path "
                    "or clear the pending entry"
                >>)}
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
                    wrap_io_error({error, Reason}, File)
            end;
        {error, enoent} ->
            resolve_missing_remove_class_target(File, Entries, StagedPath);
        {error, Reason} ->
            wrap_io_error({error, Reason}, File)
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
                            "-byte file; the file changed externally"
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
                    replacement_for(Entry, Body, Start, End, File)
            end
    end.

%% The bytes to splice into `{Start, End}` once the external-edit check has
%% passed. A `'remove-method'` entry (ADR 0113 Phase 2: Tier 1, no gate — see
%% module doc) has no `source_ref` — nothing replaces the excised text, so the
%% replacement is empty, mechanically identical to a patch whose new body
%% happens to be `<<>>`. Every other kind reads its recorded `source_ref`.
-spec replacement_for(term(), binary(), non_neg_integer(), non_neg_integer(), binary() | undefined) ->
    {ok, binary()} | {error, #beamtalk_error{}}.
replacement_for(Entry, Body, Start, End, File) ->
    case beamtalk_workspace_changelog:entry_kind(Entry) of
        'remove-method' ->
            {ok, splice(Body, {Start, End}, <<>>)};
        _ ->
            case beamtalk_workspace_changelog:read_source_body(Entry) of
                {ok, NewSrc} ->
                    %% BT-2584: the stored `source` is already the on-disk
                    %% byte-span shape (`source_ref == disk[span]` by
                    %% construction — the install hook reshaped the
                    %% compiler's canonical body to the span's base
                    %% indentation via `reindent_method_source`). The splice
                    %% is a verbatim byte replacement; no reshaping here.
                    %% This retires the former cross-layer reconciliation
                    %% (BT-2577's `reindent/2`).
                    {ok, splice(Body, {Start, End}, NewSrc)};
                {error, Reason} ->
                    {error, source_body_error(File, Reason)}
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
    Trimmed = beamtalk_workspace_reshape:strip_trailing_newlines(Body),
    NewSrcWithNl = beamtalk_workspace_reshape:ensure_trailing_newline(NewSrc),
    <<Trimmed/binary, "\n\n", NewSrcWithNl/binary>>.

%%% ----------------------------------------------------------------------------
%%% Phase B: commit (rename into place, or unlink a staged delete)
%%% ----------------------------------------------------------------------------

%% Sequential commit. On the first failure: stop, mark every already-committed
%% file's entries as flushed, and surface a per-file status report. Earlier
%% successes are *not* rolled back — POSIX rename/unlink are one-way and the
%% on-disk state is the new authoritative source for those files. `Skipped`
%% (ADR 0113: pending Tier 2 entries left out of this flush) passes straight
%% through to the summary — Phase B does nothing with it but report it.
-spec phase_b([#prepared{}], [term()], [map()]) -> {ok, map()}.
phase_b(Prepared, Shadowed, Skipped) ->
    phase_b_loop(Prepared, Shadowed, [], [], Skipped).

phase_b_loop([], Shadowed, Committed, Failed, Skipped) ->
    Files = lists:reverse([P#prepared.file || P <- Committed]),
    CommittedEntries = lists:flatten([P#prepared.entries || P <- Committed]),
    %% Only mark shadowed entries whose survivor (same class+selector) was
    %% actually committed in this Phase B. When Phase B aborts mid-loop, a
    %% shadowed entry whose survivor never made it to disk must stay pending
    %% — otherwise we silently lose the change from the active view while it
    %% is not on disk. See Copilot review on PR #2325.
    SurvivorKeys = renamed_target_keys(CommittedEntries),
    AppliedShadowed = filter_shadowed_by_survivor(Shadowed, SurvivorKeys),
    EntriesToMark = CommittedEntries ++ AppliedShadowed,
    Seqs = [beamtalk_workspace_changelog:entry_seq(E) || E <- EntriesToMark],
    complete_flush(Files, lists:reverse(Committed), Failed, Seqs, Skipped);
phase_b_loop([P | Rest], Shadowed, Committed, Failed, Skipped) ->
    case commit(P) of
        ok ->
            phase_b_loop(Rest, Shadowed, [P | Committed], Failed, Skipped);
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
                seqs => [seq(E) || E <- P#prepared.entries],
                detail => atom_to_binary(Reason, utf8)
            },
            %% Clean up any unattempted tmps so we do not leave stale files
            %% behind for the next flush to trip over.
            cleanup_tmps([P | Rest]),
            phase_b_loop([], Shadowed, Committed, [FailedHere | Failed], Skipped)
    end.

%% Phase B's commit action, dispatched by `P#prepared.op`:
%%   - `write`  — rename `tmp` (the freshly-written `<file>.tmp`) into `file`.
%%   - `delete` — unlink the staged `<file>.tmp-delete-<epoch>-<seq>`
%%     (ADR 0113): `file` is already absent from its original location by this
%%     point (Phase A's stage-rename already happened, this attempt's or an
%%     earlier one's), so this is the operation that finally makes the
%%     removal durable.
%%   - `noop`   — the external-edit soft success (already gone); no I/O.
-spec commit(#prepared{}) -> ok | {error, term()}.
commit(#prepared{op = write, tmp = Tmp, file = File}) ->
    file:rename(Tmp, binary_to_list(File));
commit(#prepared{op = delete, tmp = Tmp}) ->
    file:delete(Tmp);
commit(#prepared{op = noop}) ->
    ok.

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
    %% ADR 0082 Phase 3 (BT-2289): broadcast flush completion so LSP clients
    %% can emit `workspace/applyEdit` for each touched file. Fire BEFORE
    %% `mark_flushed/1` so editor refresh fires reliably in the mixed-success
    %% case where renames succeeded but the ChangeLog server is unreachable —
    %% the files are already on disk at this point and the editor needs to
    %% realign regardless of marker outcome.
    %%
    %% BT-2531: the typed `FlushCompleted` announcement on the SystemAnnouncer
    %% bus is now the sole flush-completion push source (the legacy
    %% `beamtalk_flush_events` broadcast was retired). Fire-and-forget: the
    %% announcer swallows missing-bus errors so flush never fails on a downstream
    %% subscriber issue.
    %%
    %% BT-3212: alongside the flat `Files` list, also announce each file's
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
            {ok, success_summary(Files, Renamed, Failed, Skipped)};
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
            {ok, success_summary(Files, Renamed, [MarkerFailure | Failed], Skipped)}
    end.

%% Announce `FlushCompleted` on the `SystemAnnouncer` bus after a flush has
%% written files to disk (ADR 0093 §2, BT-2530). `files` carries the absolute
%% binary paths, matching the legacy `{flush_completed, Files}` broadcast.
%%
%% `FileKinds` (BT-3212, ADR 0113 LSP follow-up) is the per-file companion:
%% one `#{file => Path, kind => Kind}` map per entry in `Files`, where `Kind`
%% is `beamtalk_workspace_changelog:entry_kind/1`'s own enum value verbatim
%% (`'new-class'`, `'remove-class'`, `instance`, `class`, `'remove-method'`,
%% `unknown`) — forwarded as-is rather than collapsed into a new taxonomy, so
%% a consumer buckets it itself (the LSP: `'new-class'` -> `CreateFile`,
%% `'remove-class'` -> `DeleteFile`, anything else -> an ordinary patch).
%% `file_kind_map/1` builds each entry from the Phase B `#prepared{}` record.
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
%% `#prepared{}` record (BT-3212). A `'new-class'`/`'remove-class'` file
%% group is always single-entry (`prepare_file/2`'s mixing guards forbid
%% combining either with sibling patches); a splice group may hold several
%% entries but they are always `instance`/`class`/`'remove-method'` — never
%% mixed with `'new-class'`/`'remove-class'` — so any entry in the group
%% yields the same LSP-relevant bucket ("an ordinary patch"). Taking the
%% first entry's `entry_kind/1` is therefore always representative, not just
%% a convenient default.
-spec file_kind_map(#prepared{}) -> map().
file_kind_map(#prepared{file = File, entries = [Entry | _]}) ->
    #{file => File, kind => beamtalk_workspace_changelog:entry_kind(Entry)}.

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

%% Undo Phase A staging for every not-yet-committed `Prepared` record, so an
%% aborted flush (Phase A conflict on another file, or a Phase B failure
%% partway through) leaves no stale staging artefact behind. Dispatches by
%% `op` (ADR 0113) since `write` and `delete` staged different things:
-spec cleanup_tmps([#prepared{}]) -> ok.
cleanup_tmps(Prepared) ->
    lists:foreach(fun cleanup_one/1, Prepared),
    ok.

%% `write` (patch / new-class / remove-method): delete the freshly-written
%% `<file>.tmp` — the original `file` was never touched, so there is nothing
%% else to undo.
cleanup_one(#prepared{op = write, tmp = Tmp}) ->
    _ = file:delete(Tmp),
    ok;
%% `delete` (class removal) whose stage-rename happened *in this flush
%% attempt* (`pre_existing = true`, per `prepare_remove_class/3`): rename the
%% staged file back to its original location, undoing our own Phase A step —
%% Phase A promises "no rename happened" on a whole-flush abort.
cleanup_one(#prepared{op = delete, tmp = Tmp, file = File, pre_existing = true}) ->
    _ = file:rename(Tmp, binary_to_list(File)),
    ok;
%% `delete` whose stage-rename was already sitting on disk *before* this
%% flush attempt began (`pre_existing = false` — a resumed mid-delete-crash
%% recovery, ADR 0113): this attempt did not perform that rename, so aborting
%% must leave it exactly as found for a future flush to resume, not un-stage
%% work a different (earlier) attempt already committed to.
cleanup_one(#prepared{op = delete, pre_existing = false}) ->
    ok;
%% `noop` (already-gone soft success): no I/O happened, nothing to undo.
cleanup_one(#prepared{op = noop}) ->
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
    base_summary(0, [], 0, 0, [], []).

-spec success_summary([binary()], [#prepared{}], [map()], [map()]) -> map().
success_summary(Files, Committed, Failed, Skipped) ->
    AllEntries = lists:flatten([P#prepared.entries || P <- Committed]),
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
