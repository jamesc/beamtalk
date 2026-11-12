%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_rewrite).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Multi-site source rewrite, class move and rename revert for the Beamtalk REPL
(ADR 0114).

Owns the transactional "splice N sites, recompile, hot-reload" mechanism
(`rewrite_sites/2` and its validate-only twin `validate_sites/2`), the pure
file move `move_class/2` built on it, and the `renameTo:` /
`renameSelector:to:` revert `revert_rename_sites/1` that replays a recorded
ChangeLog entry's sites in reverse.

Extracted from `beamtalk_repl_loader` (BT-4702). It is a client of the loader's
compile/install/source-tracking primitives (`compile_reload_source/5`,
`install_reload_result/2`, `patch_module_target/1`, …), which stay in
`beamtalk_repl_loader`; nothing in the loader calls back into this module
except for the `rewrite_span()` type used by its single-site splice helpers.
""".

-export([
    %% ADR 0114: the shared multi-site rewrite mechanism —
    %% generalizes `beamtalk_repl_loader:remove_method/3` for
    %% `renameTo:`/`renameSelector:to:` to call with different site lists. See
    %% rewrite_sites/2's doc for the in-memory atomicity protocol.
    rewrite_sites/2,
    validate_sites/2,
    %% ADR 0114 Phase 2: `Workspace moveClass:to:` — a pure file
    %% move, reusing rewrite_sites/2 with a single byte-identical definition
    %% site. See move_class/2's own doc.
    move_class/2,
    %% ADR 0114 Phase 4: `Workspace changes revert:` for a pending
    %% `'rename-class'`/`'rename-method'` entry — rewrites every recorded
    %% `sites` entry back to its own `prev_source_ref`, reusing rewrite_sites/2
    %% in reverse. See revert_rename_sites/1's own doc.
    revert_rename_sites/1
]).

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([
    %% ADR 0114 Phase 4: multi-site rewrite revert helpers.
    class_names_by_source_file/0,
    current_spans_for_group/1,
    build_revert_sites/1
]).
-endif.

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% ChangeLog entry construction lives in its own module (BT-4683).
-define(ENTRIES, beamtalk_repl_loader_changelog_entries).

%%% ----------------------------------------------------------------------------
%%% Multi-site rewrite types (ADR 0114) — see rewrite_sites/2's doc.
%%% ----------------------------------------------------------------------------

-type rewrite_span() :: #{start := non_neg_integer(), 'end' := non_neg_integer()}.

%% One site a multi-site rewrite must touch: `Class`'s CURRENT
%% `beamtalk_workspace_meta:get_class_source/1` text has `NewText` spliced in
%% at `Span` (the same "current in-memory merged source" convention
%% `remove_method/3` already uses for its own single-site span — NOT
%% necessarily the on-disk byte offsets; resolving a site's span against
%% whatever source a caller's site-discovery step used is that step's own
%% responsibility, out of scope here).
%% `SourceFile` is carried through only for ChangeLog attribution
%% (`beamtalk_workspace_changelog`'s `site()` shape) — it plays no role in
%% the splice/compile/install mechanism itself.
-type rewrite_site() :: #{
    class := binary(),
    source_file := binary() | undefined,
    span := rewrite_span(),
    new_text := binary()
}.

%% A `rewrite_site()` after a successful `rewrite_sites/2` call: `prev_source`
%% is the text that occupied `span` before the rewrite (sliced from the
%% class's pre-rewrite source) and `source` is `new_text` again, surfaced
%% under the ChangeLog's own field name — together these are exactly a
%% ChangeLog `site()`'s `prev_source_ref`/`source_ref` bodies before they are
%% written to `sources/` and turned into refs (see `emit_rewrite_change_entry/2`).
-type installed_rewrite_site() :: #{
    class := binary(),
    source_file := binary() | undefined,
    span := rewrite_span(),
    prev_source := binary(),
    source := binary()
}.

%% `rewrite_sites/2`'s success result: `definition` is the installed
%% definition site (`undefined` when the caller passed no definition site —
%% mirrors the ChangeLog schema's `sites[0] = null` dynamic-class case) and
%% `sites` is every installed reference/sender site, in the order their
%% owning class-groups were installed (see `rewrite_sites/2`'s doc for why
%% that is not always the caller's exact original interleaving).
-type rewrite_result() :: #{
    definition := installed_rewrite_site() | undefined,
    sites := [installed_rewrite_site()]
}.

-export_type([
    rewrite_span/0,
    rewrite_site/0,
    installed_rewrite_site/0,
    rewrite_result/0
]).

%%% ----------------------------------------------------------------------------
%%% Shared multi-site rewrite mechanism (ADR 0114)
%%% ----------------------------------------------------------------------------

-doc """
Rewrite a definition site plus N reference/sender sites transactionally, in
memory, generalizing `remove_method/3`'s single-site splice+recompile+
hot-reload for the multi-site case ADR 0114's `renameTo:`/
`renameSelector:to:` both need (not yet built — this is the
shared mechanism both will call with different, already-computed site lists;
site *discovery* itself is out of scope here).

`DefinitionSite` is the primary target's own declaration/definition
(`undefined` only for a dynamic, source-less class being renamed — mirrors
the ChangeLog schema's `sites[0] = null` case, ADR 0114 § ChangeLog schema).
`ReferenceSites` is every other site to rewrite alongside it. Both are plain
`rewrite_site()` maps — this function has no opinion on WHY a site is being
rewritten (a class name, a selector, anything else a future primitive needs);
it only knows how to splice `new_text` into `class`'s current source at
`span`, recompile, and hot-reload.

## The in-memory atomicity protocol (ADR 0114 § Decision, final paragraph)

The ADR explicitly leaves open how to avoid leaving a rename half-applied in
memory if rewriting confirmed site 5 of 10 fails partway through — before any
flush happens, across N separate class gen_servers, with no OTP cross-process
transaction primitive to roll back a hot-reloaded module once live actors may
hold references to it (same precedent ADR 0082 already established). This
function's answer, per the ADR's own steer:

1. **Group sites by owning class.** Two sites can legitimately target the
   SAME class (e.g. a method-rename's definition and a same-class self-send
   both live in `Counter`'s own file) — these MUST be merged into one splice
   + one recompile, never two independent ones, or the second recompile
   would silently discard the first's edit (it would start again from the
   unmodified `beamtalk_workspace_meta` source). `group_sites_by_class/1`
   also rejects overlapping/out-of-bounds spans within a class up front —
   a structural bug in the caller's site list, not a compile failure, so it
   is reported before any compile is even attempted.
2. **Validate every class-group's rewritten source FIRST — a pure,
   non-mutating pass.** Each group's post-splice source is compiled via
   `compile_reload_source/4` (the same `beamtalk_repl_compiler:compile_file/4`
   + `verify_class_present/3` step `reload_compile_and_load/4` already runs
   for the single-site case) but NEVER installed at this stage — no
   `code:load_binary/3`, no `activate_module/4`, nothing observable to any
   other process. If ANY group fails to compile, `rewrite_sites/2` returns
   `{error, {validation_failed, PerClassReasons}}` immediately and NOTHING
   has been mutated: every class's `beamtalk_workspace_meta` source and
   loaded module are exactly as they were before this call. This is what
   gives the "no class left in a half-rewritten state" guarantee.
3. **Only once every group has validated does the install pass run** — for
   each group in turn, `class_source_unchanged/1` re-checks that the class's
   `beamtalk_workspace_meta` source is still byte-identical to what
   `build_class_group/2` snapshotted into it BEFORE validation ran (see
   point 4 below), then `install_reload_result/2` (load + hot-reload)
   updates `beamtalk_workspace_meta`'s tracked source to match. Since every
   group already compiled successfully, `install_reload_result/2` failing
   here is expected to be rare (see its own doc — `code:load_binary/3`
   failing on a binary that just came out of a successful compile is a
   BEAM-level anomaly, not a source problem). It is still handled
   defensively: **the pathological case where install fails after
   validation passed** leaves every group installed BEFORE the failing one
   already live (there is still no cross-gen-server rollback — the ADR's own
   accepted limit) while the failing group and everything after it in
   install order never installed. `rewrite_sites/2` reports exactly this via
   `{error, {partial_install_failure, FailedClass, Reason, InstalledClasses}}`
   so the caller (and its ChangeLog bookkeeping) can tell a documented,
   bounded partial application apart from "nothing happened" — it must NOT
   be treated as equivalent to a clean validation-phase abort.
4. **Concurrent-writer detection at install time.** Steps 1-2 only
   guarantee atomicity WITHIN this one call — nothing serializes a batch
   against another eval-worker process concurrently mutating the same class
   via `Counter compile: '...'`, a single-site patch, or another
   `rewrite_sites/2` call entirely (each REPL/MCP/LSP session gets its own
   worker off `beamtalk_repl_shell`, and this module has never had a
   cross-session lock around `beamtalk_workspace_meta`'s read-then-later-
   write sequence — same class of race the single-site `capture/4`/
   `rollback/4` "two concurrent sessions... race" comment already
   acknowledges elsewhere in this module, just widened here roughly
   proportionally to batch size, since validating every OTHER group first
   can leave an early group's snapshot stale by the time its own turn to
   install comes up). Rather than a distributed lock (explicitly out of
   scope — this is detect-and-reject, not a mutex), each group re-checks its
   own class's CURRENT tracked source against its snapshot immediately
   before `?MODULE:install_reload_result/2` starts for that group. A
   mismatch (or the class no longer having a tracked source at all) means
   some other write landed in the validate-or-earlier-install window;
   `rewrite_sites/2` reports `{error, {stale_snapshot, Class}}` for that
   group and stops the batch there — installing this group's precomputed
   `new_source` anyway would silently discard the concurrent edit (the
   lost-update bug this exists to close). Groups already installed earlier
   in this same call stay installed (same bounded-partial-application shape
   point 3 already documents); nothing after the stale group installs. The
   caller can always retry safely: re-run site discovery against current
   state and call `rewrite_sites/2` again — a rejected install never
   corrupts anything, it only refuses to overwrite what it can no longer
   prove is still the snapshot it validated against.

   This check narrows the race window but does not fully close it: a writer
   that lands strictly BETWEEN `install_reload_result/2` returning
   (`code:load_binary/3` + `activate_module/4` — real work, not
   instantaneous) and this group's own `set_class_source/2` call a few lines
   later is not detected — that write's own tracked source is silently
   overwritten by this group's `set_class_source/2`, the one gap
   `class_source_unchanged/1`'s single check-before-install placement does
   not cover. Closing that residual gap would mean re-running the check
   again immediately before `set_class_source/2`, but by then this group's
   OWN code is already loaded and live (`install_reload_result/2` already
   ran) — failing at that point cannot "undo" the load, so it would trade one
   silent-overwrite bug for a different inconsistency (code live, tracked
   source refused) with no clean recovery story of its own. Left as a known,
   narrower residual instance of the same accepted-limitation class this
   module's `capture/4`/`rollback/4` comment already documents for the
   single-site path, rather than introduced as a new one.

Known, accepted limitation this protocol does NOT close: `compile_file/4`
itself has a side effect independent of installation — it registers each
compiled class's `referenced_aliases` into `beamtalk_alias_xref`
(`compile_file_core/4`'s own doc) as part of merely COMPILING, not installing.
A class-group that validates but is never installed (because an earlier or
later group in the same batch failed validation) can therefore still leave a
stale alias-xref registration behind. This is not a regression this protocol
introduces — `reload_compile_and_load/4`'s existing single-site path already
has this exact property today (compile-time alias registration happens
before the install half runs) — so generalizing to N sites does not make it
qualitatively worse. Fixing it is `beamtalk_alias_xref` bookkeeping
robustness, not the class-reinstallation atomicity ADR 0114 asks this issue
to design.

## What this function does NOT do

No ChangeLog entry (see `emit_rewrite_change_entry/2`, a separate best-effort
call mirroring `emit_remove_change_entry/5`'s placement after `remove_method/3`)
and no `beamtalk_class_lifecycle`-style purge of a class's OLD registered
name — that only applies when a rename changes what name a class is
registered under, which is `renameTo:`'s own concern (out of scope: this
function never touches `beamtalk_class_registry`). Per-site xref reindexing
IS covered, but not via an explicit purge call: `activate_module/4`'s
`register_classes/2` → generated `register_class/0` → `beamtalk_object_class:
update_class/2` → `refresh_xref/2` already purges-then-reregisters a class's
xref rows on every ordinary recompile (ADR 0087 Phase 2) — this function's
job is simply to make sure THAT existing pipeline runs for every rewritten
site's own class, not just a single primary target, which is exactly the
"must be wired in explicitly" gap ADR 0114's Consequences section names.
Calling `beamtalk_class_lifecycle:purge_compiler_cache/1` additionally here
would be actively wrong, not merely redundant: it unconditionally drops a
class's `beamtalk_compiler_server` cache entry, which would immediately
erase the entry `activate_module/4`'s own `register_class/0` call just
freshly (re-)registered for a same-name class-group.
""".
-spec rewrite_sites(rewrite_site() | undefined, [rewrite_site()]) ->
    {ok, rewrite_result()}
    | {error,
        no_sites
        | {class_source_unavailable, binary()}
        | {invalid_or_overlapping_span, binary(), rewrite_span()}
        | {validation_failed, [{binary(), term()}]}
        | {partial_install_failure, binary(), term(), [binary()]}
        | {stale_snapshot, binary()}}.
rewrite_sites(undefined, []) ->
    {error, no_sites};
rewrite_sites(DefinitionSite, ReferenceSites) when is_list(ReferenceSites) ->
    case validate_and_group_sites(DefinitionSite, ReferenceSites) of
        {ok, Validated} -> install_rewrite_groups(Validated, DefinitionSite);
        {error, _} = Err -> Err
    end.

-doc """
Run `rewrite_sites/2`'s own validation pass (grouping + the non-mutating
compile-only check) WITHOUT installing anything — same pure prefix
`rewrite_sites/2` itself runs before its install pass, exposed standalone
for a caller that needs to know a rewrite WOULD succeed before committing
some other, unrelated mutation first (ADR 0114:
`beamtalk_class_refactor:classRenameTo/2`'s dynamic-class path moves
the class's registry identity via a completely separate call,
`beamtalk_object_class:rename/2` — outside this module's own atomicity
protocol entirely — so it validates the reference-site rewrite here FIRST,
moves the identity second, and only then calls `rewrite_sites/2` itself to
actually install; that ordering is what keeps a rejected rewrite from ever
being preceded by a committed identity move).

Returns `ok` on success (the caller then repeats the same arguments to
`rewrite_sites/2` to actually install) or the same `{error, Reason}` shapes
`rewrite_sites/2` itself returns for its own validation-phase failures —
`no_sites`, `{class_source_unavailable, _}`, `{invalid_or_overlapping_span,
_, _}`, or `{validation_failed, _}`. Never returns
`{partial_install_failure, ...}` — that shape is specific to the install
pass this function never runs.
""".
-spec validate_sites(rewrite_site() | undefined, [rewrite_site()]) ->
    ok
    | {error,
        no_sites
        | {class_source_unavailable, binary()}
        | {invalid_or_overlapping_span, binary(), rewrite_span()}
        | {validation_failed, [{binary(), term()}]}}.
validate_sites(undefined, []) ->
    {error, no_sites};
validate_sites(DefinitionSite, ReferenceSites) when is_list(ReferenceSites) ->
    case validate_and_group_sites(DefinitionSite, ReferenceSites) of
        {ok, _Validated} -> ok;
        {error, _} = Err -> Err
    end.

%% One class's worth of a multi-site rewrite: every site targeting `class`,
%% merged into a single pre-computed post-splice source ready to compile.
-record(rewrite_class_group, {
    class :: binary(),
    module_name_override :: binary() | undefined,
    source_path :: string() | undefined,
    original_source :: binary(),
    new_source :: binary(),
    sites :: [rewrite_site()]
}).

%% Partition `Sites` by their `class` field, preserving first-seen class
%% order (so install order is deterministic) and each class's own sites in
%% their original relative order, then build a `#rewrite_class_group{}` per
%% class: fetch that class's CURRENT tracked source once, validate its sites'
%% spans don't overlap or run past the end of that source, and pre-compute
%% the merged post-splice source (`apply_site_splices/2`).
-spec group_sites_by_class([rewrite_site()]) ->
    {ok, [#rewrite_class_group{}]}
    | {error,
        {class_source_unavailable, binary()}
        | {invalid_or_overlapping_span, binary(), rewrite_span()}}.
group_sites_by_class(Sites) ->
    {ClassOrderRev, Grouped} = lists:foldl(
        fun(#{class := Class} = Site, {OrderAcc, MapAcc}) ->
            case maps:is_key(Class, MapAcc) of
                true -> {OrderAcc, MapAcc#{Class => [Site | maps:get(Class, MapAcc)]}};
                false -> {[Class | OrderAcc], MapAcc#{Class => [Site]}}
            end
        end,
        {[], #{}},
        Sites
    ),
    build_class_groups(lists:reverse(ClassOrderRev), Grouped, []).

-spec build_class_groups([binary()], #{binary() => [rewrite_site()]}, [#rewrite_class_group{}]) ->
    {ok, [#rewrite_class_group{}]}
    | {error,
        {class_source_unavailable, binary()}
        | {invalid_or_overlapping_span, binary(), rewrite_span()}}.
build_class_groups([], _Grouped, Acc) ->
    {ok, lists:reverse(Acc)};
build_class_groups([Class | Rest], Grouped, Acc) ->
    ClassSites = lists:reverse(maps:get(Class, Grouped)),
    case build_class_group(Class, ClassSites) of
        {ok, Group} -> build_class_groups(Rest, Grouped, [Group | Acc]);
        {error, _} = Err -> Err
    end.

-spec build_class_group(binary(), [rewrite_site()]) ->
    {ok, #rewrite_class_group{}}
    | {error,
        {class_source_unavailable, binary()}
        | {invalid_or_overlapping_span, binary(), rewrite_span()}}.
build_class_group(Class, Sites) ->
    case beamtalk_workspace_meta:get_class_source(Class) of
        undefined ->
            {error, {class_source_unavailable, Class}};
        Source ->
            SourceBin = unicode:characters_to_binary(Source),
            case validate_no_overlaps(Class, Sites, byte_size(SourceBin)) of
                ok ->
                    {ModuleNameOverride, SourcePath} = beamtalk_repl_loader:patch_module_target(
                        Class
                    ),
                    {ok, #rewrite_class_group{
                        class = Class,
                        module_name_override = ModuleNameOverride,
                        source_path = SourcePath,
                        original_source = SourceBin,
                        new_source = apply_site_splices(SourceBin, Sites),
                        sites = Sites
                    }};
                {error, _} = Err ->
                    Err
            end
    end.

-doc """
The shared `{start, 'end'}` sort key for a `rewrite_span()` (ADR 0114)
— the ONE place every function that must order same-class
sites consistently derives its comparator from: `validate_no_overlaps/3`
(ascending, overlap detection), `apply_site_splices/2` (descending, so it
applies rightmost-first), and revert's own `current_spans_for_group/1`
(ascending — the inverse of `apply_site_splices/2`'s own ordering, needed to
replay the same cumulative-offset math left-to-right). A same-start tie (a
zero-length insertion sharing a start with a same-position replacement) MUST
sort identically across all three, or a mismatch can silently corrupt a
spliced source (PR #3522's finding) or misplace a reverted one — each call
site still picks its own ascending/descending direction (a genuine semantic
difference), but none may re-derive the KEY itself, which is what actually
needs to stay identical.
""".
-spec span_start_end(rewrite_span()) -> {non_neg_integer(), non_neg_integer()}.
span_start_end(#{start := Start, 'end' := End}) -> {Start, End}.

%% Every span for one class must be well-formed (`start =< end =< SourceSize`)
%% and disjoint from every other span for that same class — two sites
%% touching overlapping text is a structural bug in the caller's site list
%% (site discovery, out of scope here), not something a splice can resolve.
%% Sorting by start first turns pairwise overlap-checking into a single
%% linear "does the next span start at or after where the last one ended"
%% fold.
-spec validate_no_overlaps(binary(), [rewrite_site()], non_neg_integer()) ->
    ok | {error, {invalid_or_overlapping_span, binary(), rewrite_span()}}.
validate_no_overlaps(Class, Sites, SourceSize) ->
    %% Secondary sort key on `'end'` (not just `start`), via the shared
    %% `span_start_end/1` key — see that function's own doc for why two sites
    %% sharing a start (e.g. a zero-length insertion alongside a same-position
    %% replacement) must sort the same way here as in `apply_site_splices/2`
    %% (reversed, since it applies rightmost-first).
    Spans = lists:sort(
        fun(A, B) -> span_start_end(A) =< span_start_end(B) end,
        [maps:get(span, S) || S <- Sites]
    ),
    validate_spans(Class, Spans, 0, SourceSize).

-spec validate_spans(binary(), [rewrite_span()], non_neg_integer(), non_neg_integer()) ->
    ok | {error, {invalid_or_overlapping_span, binary(), rewrite_span()}}.
validate_spans(_Class, [], _Min, _Size) ->
    ok;
validate_spans(Class, [#{start := Start, 'end' := End} = Span | Rest], Min, Size) ->
    case Start =< End andalso End =< Size andalso Start >= Min of
        true -> validate_spans(Class, Rest, End, Size);
        false -> {error, {invalid_or_overlapping_span, Class, Span}}
    end.

%% Apply every site's splice to `Source` in one pass. Sites are applied
%% rightmost-span-first so that an earlier (lower-offset) site's byte offsets
%% are never invalidated by a splice whose replacement text has a different
%% length than the span it replaces — this is exactly what lets two sites in
%% the same class (e.g. a method-rename's definition plus a same-class
%% self-send) merge into one recompile instead of two independent ones that
%% would silently clobber each other.
%%
%% Ties on `start` (e.g. a zero-length insertion sharing a start with a
%% same-position replacement — `validate_no_overlaps/3` deterministically
%% accepts exactly this shape) break on `'end'`, descending: the larger-`end`
%% span (the "real" replacement) is applied first, so the zero-length
%% insertion is spliced against an accumulator where that replacement's new
%% text already begins at `start` — landing cleanly at that boundary rather
%% than being applied against the ORIGINAL span first and then having a
%% same-start sibling slice into already-shifted, no-longer-original-relative
%% bytes. Per review feedback on PR #3522: before this comparator existed,
%% `apply_site_splices/2`'s ties fell back to `lists:sort/2`'s stability
%% (the caller's original list order), which could corrupt the merged source
%% for the caller-order half of that tie that put the zero-length site first.
%% This uses the SAME `span_start_end/1` key `validate_no_overlaps/3` does
%% (only the direction differs) — see that function's own doc for why the key
%% itself must never be re-derived independently.
-spec apply_site_splices(binary(), [rewrite_site()]) -> binary().
apply_site_splices(Source, Sites) ->
    RightmostFirst = lists:sort(
        fun(#{span := SpanA}, #{span := SpanB}) ->
            span_start_end(SpanA) >= span_start_end(SpanB)
        end,
        Sites
    ),
    lists:foldl(
        fun(#{span := Span, new_text := NewText}, Acc) ->
            beamtalk_repl_loader:splice_replace(Acc, Span, NewText)
        end,
        Source,
        RightmostFirst
    ).

%% Phase 1 of the atomicity protocol: compile (never install) every group's
%% pre-computed post-splice source. All-or-nothing — a single failure aborts
%% before any group's `{Group, Compiled}` pair is even assembled, let alone
%% installed.
-spec validate_rewrite_groups([#rewrite_class_group{}]) ->
    {ok, [{#rewrite_class_group{}, term()}]} | {error, {validation_failed, [{binary(), term()}]}}.
validate_rewrite_groups(Groups) ->
    Results = [{Group, compile_rewrite_group(Group)} || Group <- Groups],
    %% Every element of Results is either `{G, {error, Reason}}` or
    %% `{G, {ok, ...}}` (compile_rewrite_group/1's only two return shapes) —
    %% when no failures exist, Results IS already the all-`{ok, ...}` list
    %% install_rewrite_groups/2 expects, in original group order.
    case [{G#rewrite_class_group.class, Reason} || {G, {error, Reason}} <- Results] of
        [] -> {ok, Results};
        Failures -> {error, {validation_failed, Failures}}
    end.

%% Shared prefix of `rewrite_sites/2` and `validate_sites/2`: group + validate
%% only, never install. `rewrite_sites/2` feeds this call's `{ok, Validated}`
%% straight into `install_rewrite_groups/2`; `validate_sites/2` just discards
%% `Validated` and reports `ok`.
-spec validate_and_group_sites(rewrite_site() | undefined, [rewrite_site()]) ->
    {ok, [{#rewrite_class_group{}, term()}]}
    | {error,
        no_sites
        | {class_source_unavailable, binary()}
        | {invalid_or_overlapping_span, binary(), rewrite_span()}
        | {validation_failed, [{binary(), term()}]}}.
validate_and_group_sites(undefined, []) ->
    {error, no_sites};
validate_and_group_sites(DefinitionSite, ReferenceSites) ->
    AllSites =
        case DefinitionSite of
            undefined -> ReferenceSites;
            _ -> [DefinitionSite | ReferenceSites]
        end,
    case group_sites_by_class(AllSites) of
        {ok, Groups} -> validate_rewrite_groups(Groups);
        {error, _} = Err -> Err
    end.

-spec compile_rewrite_group(#rewrite_class_group{}) ->
    {ok, protocol_definition, map()} | {ok, compiled, binary(), [map()], atom()} | {error, term()}.
compile_rewrite_group(#rewrite_class_group{
    module_name_override = ModuleNameOverride,
    source_path = SourcePath,
    new_source = NewSourceBin
}) ->
    LoadPath = beamtalk_repl_loader:source_path_or_empty(SourcePath),
    NewSourceStr = unicode:characters_to_list(NewSourceBin),
    %% ADR 0127 §10a (BT-3650): same ambient `protocol_sources` merge
    %% `reload_compile_and_load/4` and `reload_protocol_fanout/3` do, so a
    %% rewritten class whose `uses:` names a protocol from a different,
    %% already-loaded file doesn't report "unknown protocol".
    PrebuiltIndexes = beamtalk_repl_loader:with_ambient_protocol_sources(
        beamtalk_repl_compiler:build_class_indexes()
    ),
    beamtalk_repl_loader:compile_reload_source(
        NewSourceStr, LoadPath, ModuleNameOverride, undefined, PrebuiltIndexes
    ).

%% Is `Group`'s class's CURRENT `beamtalk_workspace_meta` source
%% still byte-identical to what `build_class_group/2` snapshotted into
%% `original_source` before this batch's validation pass ran? `false` for a
%% class whose source is no longer trackable at all (e.g. removed by a
%% concurrent `removeFromSystem` in the same window) — that is exactly as
%% unsafe to install over as a changed source, not a separate case. See
%% `rewrite_sites/2`'s doc, point 4, for why this check exists and why it is
%% re-run per group immediately before that group's own install rather than
%% once up front.
-spec class_source_unchanged(#rewrite_class_group{}) -> boolean().
class_source_unchanged(#rewrite_class_group{class = Class, original_source = OriginalSource}) ->
    case beamtalk_workspace_meta:get_class_source(Class) of
        undefined -> false;
        CurrentSource -> unicode:characters_to_binary(CurrentSource) =:= OriginalSource
    end.

%% Phase 2 of the atomicity protocol: install every already-validated group,
%% in class-group order, updating `beamtalk_workspace_meta`'s tracked source
%% to match each newly-installed class. See `rewrite_sites/2`'s doc for the
%% pathological partial-install-failure case and the stale-snapshot
%% case this handles defensively.
-spec install_rewrite_groups([{#rewrite_class_group{}, term()}], rewrite_site() | undefined) ->
    {ok, rewrite_result()}
    | {error, {partial_install_failure, binary(), term(), [binary()]} | {stale_snapshot, binary()}}.
install_rewrite_groups(ValidatedGroups, DefinitionSite) ->
    install_rewrite_groups(ValidatedGroups, DefinitionSite, []).

-spec install_rewrite_groups(
    [{#rewrite_class_group{}, term()}], rewrite_site() | undefined, [#rewrite_class_group{}]
) ->
    {ok, rewrite_result()}
    | {error, {partial_install_failure, binary(), term(), [binary()]} | {stale_snapshot, binary()}}.
install_rewrite_groups([], DefinitionSite, InstalledRev) ->
    {ok, build_rewrite_result(DefinitionSite, lists:reverse(InstalledRev))};
install_rewrite_groups([{Group, Compiled} | Rest], DefinitionSite, InstalledRev) ->
    #rewrite_class_group{class = Class} = Group,
    case class_source_unchanged(Group) of
        false ->
            %% A concurrent writer landed on this class's tracked
            %% source between this batch's own validation pass and this
            %% group's own install turn. Fail cleanly and stop the batch here
            %% — never install `NewSource` over it, which would silently
            %% discard the other writer's edit (the lost-update bug this
            %% check exists to close). Everything strictly before this group
            %% in `InstalledRev` already installed and stays installed —
            %% same bounded-partial-application shape `partial_install_failure`
            %% below already has, just for a different, cleanly-detected cause.
            {error, {stale_snapshot, Class}};
        true ->
            install_rewrite_group(Group, Compiled, Rest, DefinitionSite, InstalledRev)
    end.

%% The actual load+hot-reload+tracked-source-update for one already-validated,
%% not-stale group — split out of `install_rewrite_groups/3` purely so that
%% function's own `case class_source_unchanged(Group) of ... end` reads as a
%% single guard clause rather than nesting this whole body a level deeper.
%% `SourcePath`/`NewSource` are read straight off `Group` rather than taking
%% them as separate params — they are already `Group`'s own fields, so
%% passing them alongside `Group` would just be re-stating what `Group`
%% already carries.
-spec install_rewrite_group(
    #rewrite_class_group{},
    term(),
    [{#rewrite_class_group{}, term()}],
    rewrite_site() | undefined,
    [#rewrite_class_group{}]
) ->
    {ok, rewrite_result()}
    | {error, {partial_install_failure, binary(), term(), [binary()]} | {stale_snapshot, binary()}}.
install_rewrite_group(Group, Compiled, Rest, DefinitionSite, InstalledRev) ->
    #rewrite_class_group{class = Class, source_path = SourcePath, new_source = NewSource} = Group,
    LoadPath = beamtalk_repl_loader:source_path_or_empty(SourcePath),
    %% A remote call into `beamtalk_repl_loader` (where `install_reload_result/2`
    %% lives) so `beamtalk_repl_loader_rewrite_sites_tests.erl`'s
    %% `partial_install_failure` coverage can intercept it via
    %% `meck:new(beamtalk_repl_loader, [passthrough])` — see that test module's
    %% own moduledoc for why.
    case beamtalk_repl_loader:install_reload_result(Compiled, LoadPath) of
        {ok, ClassNames} ->
            NewSourceStr = unicode:characters_to_list(NewSource),
            lists:foreach(
                fun(#{name := Name}) ->
                    beamtalk_workspace_meta:set_class_source(
                        beamtalk_repl_loader:normalize_class_source_key(Name), NewSourceStr
                    )
                end,
                ClassNames
            ),
            install_rewrite_groups(Rest, DefinitionSite, [Group | InstalledRev]);
        {error, Reason} ->
            InstalledClasses = [G#rewrite_class_group.class || G <- lists:reverse(InstalledRev)],
            {error, {partial_install_failure, Class, Reason, InstalledClasses}}
    end.

%% Build `rewrite_sites/2`'s success result from the installed groups: split
%% each group's sites back into "the definition site" (matched by
%% `{class, span}` against the caller's original `DefinitionSite`) and
%% "everything else", recording each site's pre-rewrite text (sliced from
%% its group's `original_source`, before any splice) alongside its `new_text`
%% under the ChangeLog's own `prev_source`/`source` field names.
-spec build_rewrite_result(rewrite_site() | undefined, [#rewrite_class_group{}]) ->
    rewrite_result().
build_rewrite_result(DefinitionSite, InstalledGroups) ->
    IndexedSites = lists:flatmap(
        fun(#rewrite_class_group{original_source = OrigSource, sites = Sites}) ->
            [installed_site(OrigSource, Site) || Site <- Sites]
        end,
        InstalledGroups
    ),
    case DefinitionSite of
        undefined ->
            #{definition => undefined, sites => IndexedSites};
        #{class := DefClass, span := DefSpan} ->
            {DefList, RefList} = lists:partition(
                fun(#{class := C, span := S}) -> C =:= DefClass andalso S =:= DefSpan end,
                IndexedSites
            ),
            case DefList of
                [DefInstalled | _] -> #{definition => DefInstalled, sites => RefList};
                [] -> #{definition => undefined, sites => IndexedSites}
            end
    end.

-spec installed_site(binary(), rewrite_site()) -> installed_rewrite_site().
installed_site(OriginalSource, #{
    class := Class, source_file := SourceFile, span := Span, new_text := NewText
}) ->
    #{
        class => Class,
        source_file => SourceFile,
        span => Span,
        prev_source => slice(OriginalSource, Span),
        source => NewText
    }.

-spec slice(binary(), rewrite_span()) -> binary().
slice(Source, #{start := Start, 'end' := End}) ->
    Len = End - Start,
    <<_Before:Start/binary, Text:Len/binary, _After/binary>> = Source,
    Text.

%%% ----------------------------------------------------------------------------
%%% Class move (ADR 0114 Phase 2)
%%% ----------------------------------------------------------------------------

-doc """
Move `ClassName`'s `.bt` file to `NewPathBin`, leaving its name and every
reference to it untouched (ADR 0114 Phase 2; backs `Workspace
moveClass:to:`).

Unlike `beamtalk_class_refactor:classRenameTo/2`, nothing
about the class's identity changes here — no collision check, no cross-file
reference discovery, no registry re-registration — so `moveClass:to:` has no
sites beyond the single file being moved (ADR 0114 § "`Workspace
moveClass:to:`"). The mechanism is still the shared one: resolve
the class's own declaration span (`beamtalk_compiler:resolve_class_span/2`,
the same "header + state, never a method" resolver `add_class_def_flushability/2`
above already uses) and rewrite it to ITSELF — a byte-identical splice — via
`rewrite_sites/2`, purely so a `'rename-class'` ChangeLog entry with
`old_class == class`, `old_path` = the class's current `source_file`, and
`new_path = NewPathBin` gets recorded. `Workspace flush` already
knows how to replay that entry as a pure file move (the declaration site,
plus any same-file self-reference, folded into one splice against `old_path`,
written to `new_path`, then `old_path` unlinked).

Reusing `rewrite_sites/2` rather than a bespoke "just log it" shortcut keeps
this on the one tested atomicity mechanism every other rename-shaped entry
already goes through — in particular, it still forces a real compile of the
class's current tracked source before anything is logged, catching a source
that has drifted into an unparseable state rather than silently recording a
move for a class that cannot actually be reinstalled later. Because the
registered name never changes, `rewrite_sites/2`'s own hot-reload reuses the
SAME pid (`beamtalk_class_refactor:install_class_rename/3`'s doc: "a
NEW pid — hot-reload only reuses the SAME pid when the registered name is
unchanged, which a rename by definition is not" — a move by definition IS
unchanged) — the caller's own class-object reference stays valid, so this
returns `ok` rather than a reinstalled object; the FFI boundary
(`beamtalk_workspace_interface_primitives:moveClass/2`) returns the caller's
own `aClass` argument unchanged.

## Refusal (ADR 0114 § "`Workspace moveClass:to:`")

Classification reuses `capture_class_removal_snapshot/1` — the exact
stdlib/dependency/dynamic split `classRenameTo/2` already applies — with one
deliberate divergence: a dynamic (`ClassBuilder`) class has no backing file
to move at all, so it raises `no_source_file` here rather than
`classRenameTo/2`'s permissive `flushable: false` ("dynamic") treatment of
the identical classification — moving *nothing* is not a legitimate
in-memory action the way patching a dynamic class's body is. Stdlib and
dependency classes are refused for the same reason `classRenameTo/2` refuses
them: the classification this primitive shares with it only ever covers
in-project source, so neither primitive can vouch for a class living outside
it.

`NewPathBin` must resolve inside the active project tree
(`classify_source_file/1` — the same check `newClass:at:`'s
`validate_target_path/1` already applies): `Workspace flush` only ever
writes into the project tree, so a target outside it could never actually be
reached.

Returns `ok` on success or `{error, #beamtalk_error{}}` on any refusal,
resolution, or rewrite failure — the FFI boundary raises it.
""".
-spec move_class(atom(), binary()) -> ok | {error, #beamtalk_error{}}.
move_class(ClassName, NewPathBin) when is_atom(ClassName), is_binary(NewPathBin) ->
    ClassNameBin = atom_to_binary(ClassName, utf8),
    Classification = beamtalk_repl_loader:capture_class_removal_snapshot(ClassNameBin),
    case move_class_ensure_movable(ClassName, Classification) of
        {error, _} = Err ->
            Err;
        {ok, OldPathBin} ->
            case move_class_validate_target(ClassName, OldPathBin, NewPathBin) of
                {error, _} = Err ->
                    Err;
                ok ->
                    Source = maps:get(prev_source, Classification, undefined),
                    move_class_rewrite(ClassName, ClassNameBin, OldPathBin, NewPathBin, Source)
            end
    end.

%% Dynamic (no backing file at all) -> `no_source_file`, stricter than
%% `classRenameTo/2`'s permissive treatment of the same classification (see
%% `move_class/2`'s own doc for why). Stdlib / dependency -> refuse,
%% mirroring `classRenameTo/2`'s own refusal table exactly. An ordinary
%% flushable project class -> proceed, carrying its current `source_file`
%% forward as `old_path`.
-spec move_class_ensure_movable(atom(), map()) -> {ok, binary()} | {error, #beamtalk_error{}}.
move_class_ensure_movable(_ClassName, #{flushable := true, source_file := SourceFile}) ->
    {ok, SourceFile};
move_class_ensure_movable(ClassName, #{not_flushable_reason := <<"dynamic">>}) ->
    {error, move_class_no_source_file_error(ClassName)};
move_class_ensure_movable(ClassName, #{not_flushable_reason := <<"stdlib">>}) ->
    {error, move_class_stdlib_refusal_error(ClassName)};
move_class_ensure_movable(ClassName, #{not_flushable_reason := <<"dependency:", _/binary>> = Reason}) ->
    {error, move_class_dependency_refusal_error(ClassName, Reason)};
move_class_ensure_movable(ClassName, _Classification) ->
    %% Defensive: `capture_class_removal_snapshot/1` never produces any other
    %% shape (its only non-flushable reasons are "dynamic", "stdlib", or a
    %% "dependency:" prefix — see `no_source_reason/1`/`classify_source_file/1`)
    %% but fails the same way as the dynamic case rather than crashing on an
    %% unmatched map, per "never panic on user input".
    {error, move_class_no_source_file_error(ClassName)}.

-spec move_class_validate_target(atom(), binary(), binary()) -> ok | {error, #beamtalk_error{}}.
move_class_validate_target(ClassName, OldPathBin, NewPathBin) ->
    %% A target equal to the class's current path is not a legitimate move: the
    %% flush commit path for `op = move` (`beamtalk_workspace_flush:commit/1`)
    %% renames the staged .tmp into NewPath and then deletes OldPath — when the
    %% two are the same file, that delete removes the file it just wrote,
    %% losing the class's source entirely. Refuse eagerly, mirroring
    %% `newClass:at:`'s own eager `validate_target_path/1` check.
    case
        filename:absname(binary_to_list(NewPathBin)) =:=
            filename:absname(binary_to_list(OldPathBin))
    of
        true ->
            {error,
                move_class_error(
                    same_path,
                    ClassName,
                    <<"moveClass:to: target is the class's current path; nothing to move">>
                )};
        false ->
            case beamtalk_repl_loader:classify_source_file(NewPathBin) of
                {flushable, _AbsPath} ->
                    ok;
                {not_flushable, _Reason} ->
                    {error,
                        move_class_error(
                            target_outside_project,
                            ClassName,
                            iolist_to_binary([
                                <<"moveClass:to: target is outside the project source tree: ">>,
                                NewPathBin,
                                <<"; a class can only be moved to a path inside the current project">>
                            ])
                        )}
            end
    end.

-spec move_class_rewrite(atom(), binary(), binary(), binary(), binary() | undefined) ->
    ok | {error, #beamtalk_error{}}.
move_class_rewrite(ClassName, _ClassNameBin, _OldPathBin, _NewPathBin, undefined) ->
    {error,
        move_class_error(
            class_source_unavailable,
            ClassName,
            <<"moveClass:to: could not read this class's current tracked source">>
        )};
move_class_rewrite(ClassName, ClassNameBin, OldPathBin, NewPathBin, Source) ->
    case beamtalk_compiler:resolve_class_span(Source, ClassNameBin) of
        {ok, #{start := Start, 'end' := End} = Span, _PrevSource} ->
            Text = binary:part(Source, Start, End - Start),
            DefinitionSite = #{
                class => ClassNameBin,
                source_file => OldPathBin,
                span => Span,
                new_text => Text
            },
            case rewrite_sites(DefinitionSite, []) of
                {ok, RewriteResult} ->
                    emit_move_class_change_entry(
                        ClassNameBin, OldPathBin, NewPathBin, RewriteResult
                    ),
                    ok;
                {error, Reason} ->
                    {error, move_class_rewrite_failed_error(ClassName, Reason)}
            end;
        {error, Reason, Message} ->
            {error,
                move_class_error(
                    class_span_unresolved,
                    ClassName,
                    iolist_to_binary([
                        <<"moveClass:to: could not locate this class's own declaration: ">>,
                        io_lib:format("~p ~s", [Reason, Message])
                    ])
                )}
    end.

-spec emit_move_class_change_entry(binary(), binary(), binary(), rewrite_result()) -> ok.
emit_move_class_change_entry(ClassNameBin, OldPathBin, NewPathBin, RewriteResult) ->
    Spec = #{
        kind => 'rename-class',
        class => ClassNameBin,
        old_class => ClassNameBin,
        old_path => OldPathBin,
        new_path => NewPathBin,
        intent => durable,
        author => ?ENTRIES:new_class_author(),
        author_kind => ?ENTRIES:new_class_author_kind()
    },
    ?ENTRIES:emit_rewrite_change_entry(Spec, RewriteResult).

-spec move_class_error(atom(), atom(), binary()) -> #beamtalk_error{}.
move_class_error(Kind, ClassName, Message) ->
    Err0 = beamtalk_error:new(Kind, ClassName),
    Err1 = beamtalk_error:with_selector(Err0, 'moveClass:to:'),
    beamtalk_error:with_message(Err1, Message).

-spec move_class_no_source_file_error(atom()) -> #beamtalk_error{}.
move_class_no_source_file_error(ClassName) ->
    ClassNameBin = atom_to_binary(ClassName, utf8),
    move_class_error(
        no_source_file,
        ClassName,
        iolist_to_binary([
            ClassNameBin,
            <<" has no backing .bt file to move (it was created dynamically via ClassBuilder)">>
        ])
    ).

-spec move_class_stdlib_refusal_error(atom()) -> #beamtalk_error{}.
move_class_stdlib_refusal_error(ClassName) ->
    ClassNameBin = atom_to_binary(ClassName, utf8),
    move_class_error(
        runtime_error,
        ClassName,
        iolist_to_binary([<<"Cannot move stdlib class '">>, ClassNameBin, <<"'">>])
    ).

-spec move_class_dependency_refusal_error(atom(), binary()) -> #beamtalk_error{}.
move_class_dependency_refusal_error(ClassName, Reason) ->
    ClassNameBin = atom_to_binary(ClassName, utf8),
    move_class_error(
        runtime_error,
        ClassName,
        iolist_to_binary([
            <<"Cannot move dependency class '">>, ClassNameBin, <<"' (">>, Reason, <<")">>
        ])
    ).

-spec move_class_rewrite_failed_error(atom(), term()) -> #beamtalk_error{}.
move_class_rewrite_failed_error(ClassName, Reason) ->
    ClassNameBin = atom_to_binary(ClassName, utf8),
    move_class_error(
        runtime_error,
        ClassName,
        beamtalk_error:chars_to_binary(
            io_lib:format("Could not move class '~ts': ~tp", [ClassNameBin, Reason])
        )
    ).

%%% ----------------------------------------------------------------------------
%%% Multi-site rewrite revert (ADR 0114)
%%% ----------------------------------------------------------------------------

-doc """
Revert a pending `'rename-class'`/`'rename-method'` ChangeEntry (ADR 0114 §
Undo): rewrite every recorded `sites` entry back to its own `prev_source_ref`,
against that site's own recorded location — not by re-running `renameTo:`/
`renameSelector:to:`, which would re-execute xref discovery against
POST-rename state and could compute a different site list than the original
rename touched (a referencing file could have been independently edited in
between, per the ADR's own reasoning). `candidate_sites` are never touched —
they were never rewritten in the first place (ADR 0114: reported, never
auto-rewritten senders).

## Locating each site's CURRENT position

A `site()`'s own recorded `span` is where it sat in its owning class's source
BEFORE the original rewrite (`rewrite_sites/2`'s `installed_site/2` records
the caller's PRE-splice span verbatim — see that function's own doc). By the
time revert runs, that class's tracked source
(`beamtalk_workspace_meta:get_class_source/1`) has the NEW text (`source_ref`)
sitting there instead, at a position shifted by however much EARLIER
(lower-offset) sites in the SAME class-group changed length. `current_spans_
for_group/1` recomputes each site's live position by replaying that same
per-class cumulative-offset math forward from every site's own recorded
`span`/`source_ref` length — the same rightmost-first non-interference
property `apply_site_splices/2` relies on for splicing, run left-to-right
here to LOCATE positions instead.

## Resolving each site's owning class

A persisted `site()` carries `sourceFile`, not the class name that owns it
(dropped when `installed_site/2`'s own `class` field is flattened into the
ChangeLog's `site()` shape — see `site_append_input/1`) — and a file's
basename is not a safe class-name guess (`newClass:at:` accepts a
`to_snake_case/1` basename too, per `beamtalk_workspace_flush:derive_new_
path/3`'s doc). `class_names_by_source_file/0` asks the registry the same way
`class_source_file/1` resolves the FORWARD direction, for every currently
live class, and inverts it.

## Class-identity restoration (`'rename-class'` only)

Reference-site reversal alone is enough for `'rename-method'` (no identity
ever changed there). For `'rename-class'`, the definition site's own
reverse-splice (`sites[0]`'s `prev_source_ref`) already re-declares
`old_class` in the class's own source — `rewrite_sites/2`'s own install
pipeline therefore registers a fresh pid under `old_class` as an ordinary
side effect of that recompile, exactly mirroring how the FORWARD rename's own
definition-site splice is what originally registered the NEW name
(`beamtalk_class_refactor:install_class_rename/3`'s doc). `finish_
rename_class_revert/1` only needs to retire the now-stale registration under
the CURRENT (post-rename) name — reusing `install_class_rename/3` itself,
called with the two names swapped, rather than a second copy of its
whereis/stop/purge sequence.

A dynamic class renamed with zero reference sites (`sites = [undefined]`,
ADR 0114's `sites[0] = null` case) has nothing to splice either way —
`do_revert_rewrite/2` mirrors `rewrite_class_sites/4`'s own trivial-success
shortcut for that shape, so only the identity move runs.

A pure `Workspace moveClass:to:` entry never changes a
class's name at all — its `'rename-class'` entry records `old_class ==
class` by construction — so there is no "CURRENT (post-rename) name" that
differs from `old_class` for `install_class_rename/3` to retire. Calling it
anyway (as an earlier version of this function did) resolves BOTH names to
the SAME live pid — the one `do_revert_rewrite/2` just reinstalled under
this unchanged name — and its ordinary-class clause would stop and purge
that pid as if it were a stale leftover registration, destroying the class
`revert_rename_sites/1` was asked to restore. `finish_rename_class_revert/1`
special-cases `old_class =:= class` and returns `{ok, OldNameBin}` directly,
skipping `install_class_rename/3` entirely for that shape.

Returns `{ok, RevertedClassNameBin}` (the class's name AFTER revert —
`old_class` for `'rename-class'`, the entry's own stable `class` for
`'rename-method'`) on success, or `{error, Reason}` on any resolution/read/
rewrite failure — never a partial revert: a `rewrite_sites/2` validation
failure (the shared mechanism's own all-or-nothing guarantee) or a single
site whose owning class can no longer be resolved, or whose recorded body can
no longer be read, aborts before anything is spliced. The caller (`beamtalk_
workspace_interface_primitives:revert_rename_entry/2`) wraps any `{error, _}`
into a structured `#beamtalk_error{}` and retires the original entry
(`mark_flushed/1`, mirroring `'remove-class'` revert's own "undo emits no
fresh entry" convention) on success.

Before splicing, every touched class's CURRENT tracked source is verified to
still hold exactly the bytes the original rewrite left there
(`current_spans_for_group/1` → `verify_current_spans/1` — see that function's
own doc): an intervening, UNRELATED edit to one of those same classes between
the rename and this revert is refused loudly here rather than spliced over.
This is revert's own analogue of `'remove-class'` revert's explicit drift
check (`check_no_external_drift/3`), applied to a tracked-source
SPAN rather than a whole disk file, since a rename revert can touch several
classes' sources rather than one file. Ordinary method-patch revert
(`install_revert_patch/4`) still has no equivalent check and re-installs
`PrevBody` unconditionally — this function does not mirror that.
""".
-spec revert_rename_sites(beamtalk_workspace_changelog:entry()) ->
    {ok, binary()} | {error, term()}.
revert_rename_sites(Entry) ->
    case build_revert_sites(Entry) of
        {ok, {DefinitionSite, ReferenceSites}} ->
            revert_rename_sites(Entry, DefinitionSite, ReferenceSites);
        {error, _} = Err ->
            Err
    end.

%% `DefinitionSite =:= undefined` with a NON-empty `ReferenceSites` is the
%% dynamic-class `'rename-class'` signature (ADR 0114's `sites[0] = null`
%% shape, filtered out by `resolve_revert_sites/2` before this point — see
%% this section's own doc). That is the one shape where identity restore
%% (`finish_rename_revert/1`) and the reference-site splice are separate
%% mutations with no shared rollback, mirroring the forward path's identical
%% split (`beamtalk_behaviour_intrinsics:do_rename_and_rewrite/7`'s
%% dynamic-class clause). Splicing first and moving identity second — safe
%% for an ordinary class, whose definition-site splice performs the identity
%% move as a side effect of the SAME `rewrite_sites/2` transaction — would
%% leave reference sites pointing at a name nothing answers to if the
%% identity move then failed, with no way to retry (the sites are already
%% spliced, so `verify_current_spans/1`'s drift check refuses a second
%% attempt on the next call). Fixed the same way `do_rename_and_rewrite/7`
%% was: validate the splice FIRST (non-mutating), move identity second,
%% splice the reference sites (now expected to succeed) last. Every other
%% shape — an ordinary-class `'rename-class'` (`DefinitionSite` defined) or
%% any `'rename-method'` (identity never changes) — keeps the original
%% splice-then-finish order.
-spec revert_rename_sites(
    beamtalk_workspace_changelog:entry(), rewrite_site() | undefined, [rewrite_site()]
) -> {ok, binary()} | {error, term()}.
revert_rename_sites(Entry, undefined, [_ | _] = ReferenceSites) ->
    case beamtalk_workspace_changelog:entry_kind(Entry) of
        'rename-class' -> revert_dynamic_class_rename_sites(Entry, ReferenceSites);
        'rename-method' -> revert_rename_sites_splice_first(Entry, undefined, ReferenceSites)
    end;
revert_rename_sites(Entry, DefinitionSite, ReferenceSites) ->
    revert_rename_sites_splice_first(Entry, DefinitionSite, ReferenceSites).

-spec revert_dynamic_class_rename_sites(beamtalk_workspace_changelog:entry(), [rewrite_site()]) ->
    {ok, binary()} | {error, term()}.
revert_dynamic_class_rename_sites(Entry, ReferenceSites) ->
    case validate_sites(undefined, ReferenceSites) of
        ok ->
            case finish_rename_revert(Entry) of
                {ok, _RevertedClassNameBin} = Ok ->
                    case do_revert_rewrite(undefined, ReferenceSites) of
                        {ok, _RewriteResult} -> Ok;
                        {error, _} = Err -> Err
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

-spec revert_rename_sites_splice_first(
    beamtalk_workspace_changelog:entry(), rewrite_site() | undefined, [rewrite_site()]
) -> {ok, binary()} | {error, term()}.
revert_rename_sites_splice_first(Entry, DefinitionSite, ReferenceSites) ->
    case do_revert_rewrite(DefinitionSite, ReferenceSites) of
        {ok, _RewriteResult} -> finish_rename_revert(Entry);
        {error, _} = Err -> Err
    end.

%% Trivial-success shortcut mirroring `rewrite_class_sites/4`'s own
%% dynamic-class-with-nothing-to-rewrite case (see this section's own doc).
-spec do_revert_rewrite(rewrite_site() | undefined, [rewrite_site()]) ->
    {ok, rewrite_result()} | {error, term()}.
do_revert_rewrite(undefined, []) ->
    {ok, #{definition => undefined, sites => []}};
do_revert_rewrite(DefinitionSite, ReferenceSites) ->
    rewrite_sites(DefinitionSite, ReferenceSites).

-spec finish_rename_revert(beamtalk_workspace_changelog:entry()) ->
    {ok, binary()} | {error, term()}.
finish_rename_revert(Entry) ->
    case beamtalk_workspace_changelog:entry_kind(Entry) of
        'rename-class' -> finish_rename_class_revert(Entry);
        'rename-method' -> {ok, beamtalk_workspace_changelog:entry_class(Entry)}
    end.

%% Retire the stale post-rename registration and confirm `old_class` is now
%% live, reusing `install_class_rename/3` with the two names swapped (see
%% this section's own doc for why that is sound, not a coincidental fit).
-spec finish_rename_class_revert(beamtalk_workspace_changelog:entry()) ->
    {ok, binary()} | {error, term()}.
finish_rename_class_revert(Entry) ->
    CurrentNameBin = beamtalk_workspace_changelog:entry_class(Entry),
    OldNameBin = beamtalk_workspace_changelog:entry_old_class(Entry),
    case CurrentNameBin =:= OldNameBin of
        true ->
            %% A pure `Workspace moveClass:to:` entry — a
            %% file-location-only move never changes a class's name, so its
            %% `'rename-class'` entry records `old_class == class` by
            %% construction (`move_class/2`'s own doc). `do_revert_rewrite/2`
            %% — earlier in this same revert call chain
            %% (`revert_rename_sites_splice_first/3` calls it before
            %% `finish_rename_revert/1` reaches this function) — already
            %% spliced the definition site back via `rewrite_sites/2`, which
            %% reinstalls THIS SAME name and reuses the SAME pid (its own
            %% "hot-reload only assigns a NEW pid when the registered name
            %% changed" rule — a move-revert's name never does). There is no
            %% stale post-rename registration left to retire, so skip
            %% `install_class_rename/3` entirely: its ordinary-class clause
            %% resolves `OldName`/`NewName` to the SAME live pid and would
            %% otherwise mistake the class's own just-reinstalled process for
            %% a leftover old-name registration, stopping and purging it out
            %% from under the caller.
            {ok, OldNameBin};
        false ->
            case
                {
                    beamtalk_repl_server:safe_to_existing_atom(CurrentNameBin),
                    beamtalk_repl_server:safe_to_existing_atom(OldNameBin)
                }
            of
                {{ok, CurrentName}, {ok, OldName}} ->
                    Classification = beamtalk_repl_loader:capture_class_removal_snapshot(
                        CurrentNameBin
                    ),
                    try
                        _ = beamtalk_class_refactor:install_class_rename(
                            CurrentName, OldName, Classification
                        ),
                        {ok, OldNameBin}
                    catch
                        error:#{error := #beamtalk_error{} = Err} -> {error, Err};
                        Class:Reason -> {error, {rename_identity_restore_failed, Class, Reason}}
                    end;
                _ ->
                    {error,
                        {rename_identity_restore_failed, unresolvable_class_atom,
                            {CurrentNameBin, OldNameBin}}}
            end
    end.

%% Build the reversed `rewrite_site()` list from `Entry`'s recorded `sites`:
%% resolve each site's owning class + read both its recorded bodies (aborting
%% on the first unresolvable/unreadable one), recompute current positions per
%% class-group and verify each one still holds exactly the text the original
%% rewrite left there (`assign_current_spans/1` — an intervening, unrelated
%% edit to a touched class between the rename and this revert is refused
%% loudly here rather than spliced over), then split the result back into
%% `{DefinitionSite, ReferenceSites}` for `rewrite_sites/2`.
-spec build_revert_sites(beamtalk_workspace_changelog:entry()) ->
    {ok, {rewrite_site() | undefined, [rewrite_site()]}} | {error, term()}.
build_revert_sites(Entry) ->
    Sites = beamtalk_workspace_changelog:entry_sites(Entry),
    ClassMap = class_names_by_source_file(),
    case resolve_revert_sites(Sites, ClassMap) of
        {ok, Resolved} ->
            case assign_current_spans(Resolved) of
                {ok, WithSpans} -> {ok, split_definition(WithSpans)};
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end.

%% A `SourceFile -> ClassNameBin` reverse map built from every currently live
%% class (see this section's own doc for why a basename guess is not safe
%% here instead).
-spec class_names_by_source_file() -> #{binary() => binary()}.
class_names_by_source_file() ->
    lists:foldl(
        fun({Name, ModuleName, _Pid}, Acc) ->
            case beamtalk_reflection:source_file_from_module(ModuleName) of
                nil ->
                    Acc;
                SourceFile when is_binary(SourceFile) ->
                    Acc#{SourceFile => atom_to_binary(Name, utf8)}
            end
        end,
        #{},
        beamtalk_class_registry:live_class_entries()
    ).

%% One resolved (but not yet position-adjusted) revert site, as a plain map:
%% `is_definition` (was this `Entry`'s `sites[0]`?), `class` (this site's own
%% resolved owning class), `source_file`, `orig_span` (the recorded pre-rewrite
%% span), `cur_len` (the byte length of what's occupying that span NOW —
%% `source_ref`'s own body), `new_text` (what to splice back in —
%% `prev_source_ref`'s own body).
-type revert_site() :: #{
    is_definition := boolean(),
    class := binary(),
    source_file := binary(),
    orig_span := rewrite_span(),
    cur_len := non_neg_integer(),
    expected_current := binary(),
    new_text := binary()
}.

-spec resolve_revert_sites(
    [beamtalk_workspace_changelog:site() | undefined] | undefined, #{binary() => binary()}
) -> {ok, [revert_site()]} | {error, term()}.
resolve_revert_sites(Sites, ClassMap) ->
    %% `undefined` (never actually produced today — `entry_sites/1` is always
    %% a list, possibly `[undefined]` for a sourceless dynamic-class
    %% definition) degrades to "no sites",
    %% defensively, rather than crashing on a future producer that omits it.
    IndexedSites = lists:zip(
        lists:seq(0, length(sites_or_undefined(Sites)) - 1), sites_or_undefined(Sites)
    ),
    resolve_revert_sites(IndexedSites, ClassMap, []).

-spec sites_or_undefined([beamtalk_workspace_changelog:site() | undefined] | undefined) ->
    [beamtalk_workspace_changelog:site() | undefined].
sites_or_undefined(undefined) -> [];
sites_or_undefined(Sites) when is_list(Sites) -> Sites.

-spec resolve_revert_sites(
    [{non_neg_integer(), beamtalk_workspace_changelog:site() | undefined}],
    #{binary() => binary()},
    [revert_site()]
) -> {ok, [revert_site()]} | {error, term()}.
resolve_revert_sites([], _ClassMap, Acc) ->
    {ok, lists:reverse(Acc)};
resolve_revert_sites([{0, undefined} | Rest], ClassMap, Acc) ->
    %% `sites[0] = null`: the dynamic-class "no declaration site" case — never
    %% legitimately anywhere else in the list (ADR 0114 § ChangeLog schema).
    resolve_revert_sites(Rest, ClassMap, Acc);
resolve_revert_sites([{_Index, undefined} | _Rest], _ClassMap, _Acc) ->
    %% An `undefined` site past index 0 would violate the invariant the
    %% clause above relies on — refuse loudly (this module's own convention
    %% elsewhere: `verify_current_spans/1`, `check_no_external_drift/3`, ...)
    %% rather than silently under-reverting one site. Unreachable under every
    %% current ChangeLog producer; a defensive backstop, not a live path.
    {error, revert_site_unexpectedly_undefined};
resolve_revert_sites([{Index, Site} | Rest], ClassMap, Acc) ->
    case resolve_revert_site(Site, ClassMap, Index =:= 0) of
        {ok, Resolved} -> resolve_revert_sites(Rest, ClassMap, [Resolved | Acc]);
        {error, _} = Err -> Err
    end.

-spec resolve_revert_site(
    beamtalk_workspace_changelog:site(), #{binary() => binary()}, boolean()
) -> {ok, revert_site()} | {error, term()}.
resolve_revert_site(#{source_file := undefined}, _ClassMap, _IsDefinition) ->
    %% A NON-null site (this clause never reaches the dynamic-class
    %% `sites[0] = null` case — that is filtered out by index before this
    %% function is ever called) can still legitimately carry `source_file =
    %% undefined`: `rewrite_site()`'s own doc says the field is
    %% ChangeLog-attribution-only, and `class_source_file_for/1` returns
    %% `undefined` for a class with no backing file — reachable if a
    %% `'rename-method'` confirmed sender site lives in a sourceless dynamic
    %% subclass. `class_names_by_source_file/0`'s reverse lookup has no file
    %% to key by in that case, so this is reported distinctly from a
    %% resolvable-but-unrecognised file rather than folded into the generic
    %% `revert_site_malformed` catch-all below.
    {error, revert_site_no_source_file};
resolve_revert_site(#{source_file := SourceFile, span := Span} = Site, ClassMap, IsDefinition) when
    is_binary(SourceFile), is_map(Span)
->
    case maps:get(SourceFile, ClassMap, undefined) of
        undefined ->
            {error, {revert_site_class_unresolved, SourceFile}};
        ClassBin ->
            case
                {
                    beamtalk_workspace_changelog:read_site_body(
                        maps:get(source_ref, Site, undefined)
                    ),
                    beamtalk_workspace_changelog:read_site_body(
                        maps:get(prev_source_ref, Site, undefined)
                    )
                }
            of
                {{ok, CurrentBody}, {ok, PrevBody}} ->
                    {ok, #{
                        is_definition => IsDefinition,
                        class => ClassBin,
                        source_file => SourceFile,
                        orig_span => Span,
                        cur_len => byte_size(CurrentBody),
                        expected_current => CurrentBody,
                        new_text => PrevBody
                    }};
                _ ->
                    {error, {revert_site_body_unreadable, SourceFile, Span}}
            end
    end;
resolve_revert_site(_Site, _ClassMap, _IsDefinition) ->
    {error, revert_site_malformed}.

%% Group `RevertSites` by owning class and recompute + verify each group's
%% CURRENT spans (`current_spans_for_group/1`) — `rewrite_sites/2`'s own
%% grouping re-derives class membership from each `rewrite_site()`'s own
%% `class` field regardless of input order, so the flattened output order
%% here does not matter to correctness. Aborts on the first group whose
%% drift check fails, before any group is spliced.
-spec assign_current_spans([revert_site()]) ->
    {ok, [{revert_site(), rewrite_span()}]} | {error, term()}.
assign_current_spans(RevertSites) ->
    Grouped = lists:foldr(
        fun(#{class := Class} = S, Acc) ->
            maps:update_with(Class, fun(L) -> [S | L] end, [S], Acc)
        end,
        #{},
        RevertSites
    ),
    assign_current_spans_by_group(maps:values(Grouped), []).

-spec assign_current_spans_by_group([[revert_site()]], [{revert_site(), rewrite_span()}]) ->
    {ok, [{revert_site(), rewrite_span()}]} | {error, term()}.
assign_current_spans_by_group([], Acc) ->
    {ok, Acc};
assign_current_spans_by_group([Group | Rest], Acc) ->
    case current_spans_for_group(Group) of
        {ok, WithSpans} -> assign_current_spans_by_group(Rest, WithSpans ++ Acc);
        {error, _} = Err -> Err
    end.

-doc """
Within one class-group, sorted by each site's ORIGINAL span via the SAME
shared `span_start_end/1` key `validate_no_overlaps/3`/`apply_site_splices/2`
use (see that function's own doc for why the key itself must never be
re-derived independently): a site's CURRENT start offset is its own original
start plus the sum of every EARLIER (lower-original-offset) site's own length
delta (`cur_len - original span length`) — the same non-interference property
`apply_site_splices/2`'s rightmost-first application relies on for splicing,
replayed left-to-right here to LOCATE positions instead.

Once every site's current span is computed, `verify_current_spans/1` confirms
each one's owning class's CURRENT tracked source actually holds the exact
bytes the original rewrite left there (`expected_current`, the site's own
recorded `source_ref` body) before this group is trusted for splicing — an
intervening, unrelated edit to a touched class between the original rename
and this revert (e.g. an ordinary `compile:source:` patch landing on the
same file while the rename sat pending) would otherwise silently shift what
this cumulative-offset math computes as "current", corrupting the class on
splice rather than merely producing a stale result. This is revert's own
analogue of `check_no_external_drift/3`'s `'remove-class'`-specific disk
comparison — same "never guess, refuse loudly" posture, applied to
a tracked-source SPAN rather than a whole disk file, since a rename revert
can touch several classes' sources rather than one file.
""".
-spec current_spans_for_group([revert_site()]) ->
    {ok, [{revert_site(), rewrite_span()}]} | {error, term()}.
current_spans_for_group(Group) ->
    Sorted = lists:sort(
        fun(#{orig_span := SpanA}, #{orig_span := SpanB}) ->
            span_start_end(SpanA) =< span_start_end(SpanB)
        end,
        Group
    ),
    %% `lists:mapfoldl/3` returns `{MappedList, FinalAcc}` — the mapped list
    %% FIRST, the accumulator SECOND (easy to get backwards; got it backwards
    %% once already during development, per this comment's own existence).
    {WithSpans, _Delta} = lists:mapfoldl(
        fun(#{orig_span := #{start := S, 'end' := E}, cur_len := CurLen} = Site, Delta) ->
            CurStart = S + Delta,
            CurEnd = CurStart + CurLen,
            NewDelta = Delta + (CurLen - (E - S)),
            {{Site, #{start => CurStart, 'end' => CurEnd}}, NewDelta}
        end,
        0,
        Sorted
    ),
    verify_current_spans(WithSpans).

%% Read the group's shared owning class's CURRENT tracked source ONCE and
%% confirm every site's computed span holds exactly its recorded
%% `expected_current` bytes there.
-spec verify_current_spans([{revert_site(), rewrite_span()}]) ->
    {ok, [{revert_site(), rewrite_span()}]} | {error, term()}.
verify_current_spans([]) ->
    {ok, []};
verify_current_spans([{#{class := Class}, _} | _] = WithSpans) ->
    case beamtalk_workspace_meta:get_class_source(Class) of
        undefined ->
            {error, {revert_class_source_unavailable, Class}};
        Source ->
            check_spans_match(WithSpans, unicode:characters_to_binary(Source))
    end.

-spec check_spans_match([{revert_site(), rewrite_span()}], binary()) ->
    {ok, [{revert_site(), rewrite_span()}]} | {error, term()}.
check_spans_match([], _SourceBin) ->
    {ok, []};
check_spans_match(
    [
        {#{class := Class, expected_current := Expected} = Site, #{start := S, 'end' := E} = Span}
        | Rest
    ],
    SourceBin
) ->
    Len = E - S,
    case SourceBin of
        <<_:S/binary, Expected:Len/binary, _/binary>> ->
            case check_spans_match(Rest, SourceBin) of
                {ok, More} -> {ok, [{Site, Span} | More]};
                {error, _} = Err -> Err
            end;
        _ ->
            {error, {revert_site_drifted, Class, Span}}
    end.

-spec split_definition([{revert_site(), rewrite_span()}]) ->
    {rewrite_site() | undefined, [rewrite_site()]}.
split_definition(SitesWithSpans) ->
    lists:foldl(
        fun({#{is_definition := IsDef} = Site, CurSpan}, {DefAcc, RefAcc}) ->
            RewriteSite = to_rewrite_site(Site, CurSpan),
            case IsDef of
                true -> {RewriteSite, RefAcc};
                false -> {DefAcc, [RewriteSite | RefAcc]}
            end
        end,
        {undefined, []},
        SitesWithSpans
    ).

-spec to_rewrite_site(revert_site(), rewrite_span()) -> rewrite_site().
to_rewrite_site(#{class := Class, source_file := SourceFile, new_text := NewText}, CurSpan) ->
    #{class => Class, source_file => SourceFile, span => CurSpan, new_text => NewText}.
