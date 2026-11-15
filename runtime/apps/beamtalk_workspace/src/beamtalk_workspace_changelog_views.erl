%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_changelog_views).

%%% **DDD Context:** Workspace Context

-moduledoc """
Read-side views over the ChangeLog: the `Workspace diff`/`changeLog` value maps (per-entry `method_delta` against the on-disk body, whitespace-normalised body comparison) and the `dirtyMethods` selector list. Moved verbatim out of `beamtalk_workspace_changelog` (BT-4907); the changelog's entries come from its public API, but `entry_to_value/2` and `method_delta/1` read the `#entry{}` fields directly via the shared header (hence the `no_opaque` exemption below).
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include("beamtalk_workspace_changelog.hrl").

-export([changeLog/0, dirtyMethods/0, change_entries/0]).

%% `beamtalk_workspace_changelog:entry()` is opaque, but these two views read
%% the `#entry{}` record fields directly (the record lives in the shared
%% `beamtalk_workspace_changelog.hrl`, as it does for the codec and log
%% modules). They were inside the defining module before the split, where
%% opacity does not apply; keep that by exempting exactly these functions.
-dialyzer({no_opaque, [entry_to_value/2, method_delta/1]}).

%% Exported for tests only.
-export([body_delta/2]).

%%% ----------------------------------------------------------------------------
%%% Beamtalk FFI surface (ADR 0082 Phase 1)
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
`(Erlang beamtalk_workspace_changelog_views) changeLog`.
""".
%% The `'$beamtalk_class' := 'ChangeLog'` tag lets the type checker (via
%% beamtalk_spec_reader) infer this FFI result as the `ChangeLog` Beamtalk
%% class rather than a bare `Dictionary`, matching `Workspace>>changes`
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
    All = beamtalk_workspace_changelog:entries(),
    Survivors = beamtalk_workspace_changelog:survivor_seqs(All),
    [entry_to_value(E, Survivors) || E <- All].

-doc """
Return the dirty methods derived from the *active* entries as a Beamtalk
Dictionary `#{ClassSymbol => Set(selectorSymbol)}`.

Only active entries (current epoch, not orphaned) contribute, matching the ADR's
"dirty state" semantics. New-class entries (no selector) are recorded under the
class with the placeholder selector `#'new-class'`, mirroring the ADR REPL example
`DoubleCounterTest -> #'new-class'`. Called via
`(Erlang beamtalk_workspace_changelog_views) dirtyMethods`.
""".
-spec dirtyMethods() -> #{atom() => map()}.
dirtyMethods() ->
    Active = beamtalk_workspace_changelog:active_entries(),
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
%% (redefinition of an *existing* class's whole definition) also
%% carries no selector — it gets its own `#'class-def'` placeholder rather
%% than reusing `#new-class`, so the dirty view does not misreport a
%% redefinition as a brand-new class. `'rename-class'` (ADR 0114)
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
    Active = beamtalk_workspace_changelog:is_active(E),
    Shadowed =
        Active andalso
            maps:get(beamtalk_workspace_changelog:shadow_key(E), Survivors, E#entry.seq) =/=
                E#entry.seq,
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
        side => side_symbol(beamtalk_workspace_changelog:entry_side(E)),
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
        %% ADR 0114: `undefined` for every kind except
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
%% (ADR 0082 Phase 5+): `{Clean, Diff}` where `Clean` is true iff the
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
    case beamtalk_workspace_changelog:read_source_body(E) of
        {ok, MemBody} -> {false, beamtalk_workspace_diff:unified(<<>>, MemBody)};
        _ -> {false, undefined}
    end;
method_delta(#entry{kind = Kind, selector = Selector, source_file = File} = E) when
    (Kind =:= instance orelse Kind =:= class), is_binary(Selector), is_binary(File)
->
    try
        case {beamtalk_workspace_changelog:read_source_body(E), file:read_file(File)} of
            {{ok, MemBody}, {ok, DiskSource}} ->
                body_delta(disk_method_body(DiskSource, E#entry.class, Selector, Kind), MemBody);
            _ ->
                {false, undefined}
        end
    catch
        _:_ -> {false, undefined}
    end;
method_delta(#entry{kind = 'class-def', source_file = File} = E) when is_binary(File) ->
    %% Same disk-vs-memory delta as an instance/class-kind method
    %% patch above, just resolved at whole-class granularity
    %% (`resolve_class_span/2` instead of `resolve_method_span/4`) — a
    %% redefinition of an *existing* class always has a prior on-disk body to
    %% diff against (unlike `'new-class'`, which never does).
    try
        case {beamtalk_workspace_changelog:read_source_body(E), file:read_file(File)} of
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

%% The class's current whole-definition body on disk. Unlike a
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
`{Clean, Diff}` (ADR 0082). Both sides are normalised first —
trailing whitespace trimmed and the common leading indentation stripped — so the
comparison and diff are on *content*, not layout. This is deliberate (per the
"whitespace-only reformat vs real change" criterion): the on-disk span is
file-indented and doc-inclusive while the stored body is the compiler's
canonical column-0 form, so without the dedent every doc-commented method would
read as dirty with an indentation-noise diff. `Clean = true` (no diff) when the
normalised bodies are equal — the revert-to-disk case that drops out of the
pending view. Exported for tests. (A future change will make a single
representation flow end-to-end and retire this normalisation.)
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
