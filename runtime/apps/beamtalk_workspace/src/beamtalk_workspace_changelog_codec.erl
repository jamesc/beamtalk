%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_changelog_codec).

%%% **DDD Context:** Workspace Context

-moduledoc """
JSON codec for ChangeEntry persistence (the `changes.jsonl` line schema, ADR 0082): encode/decode of entries, span and site maps, with open-enum decoding that maps unrecognised kind/intent/side values to `unknown` (logged) so history survives across versions. Moved verbatim out of `beamtalk_workspace_changelog` (BT-4907).
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include("beamtalk_workspace_changelog.hrl").

-export([
    entry_from_json/1,
    entry_to_json/1
]).

%%% ----------------------------------------------------------------------------
%%% JSON (de)serialisation
%%% ----------------------------------------------------------------------------

-doc """
Encode a ChangeEntry to a compact JSON binary (one `changes.jsonl` line, no
trailing newline). Exported for tests. The derived in-memory flags
(`prior_epoch`, `orphan`) are not persisted — they are recomputed on restart.
""".
-spec entry_to_json(#entry{}) -> binary().
entry_to_json(#entry{} = E) ->
    Map = #{
        <<"ts">> => E#entry.ts,
        <<"seq">> => E#entry.seq,
        <<"epoch">> => E#entry.epoch,
        <<"class">> => E#entry.class,
        <<"selector">> => null_or(E#entry.selector),
        <<"kind">> => beamtalk_workspace_kinds:wire_name(E#entry.kind),
        %% ADR 0112: `undefined` for every legacy `instance`/`class`-kind entry
        %% (side is derived from `kind` for those, never persisted — additive
        %% field, ADR 0112 § ChangeLog interaction) — only a `'remove-method'`
        %% entry stores this.
        <<"side">> => side_json(E#entry.side),
        <<"source_ref">> => null_or(E#entry.source_ref),
        <<"prev_source_ref">> => null_or(E#entry.prev_source_ref),
        <<"sourceFile">> => null_or(E#entry.source_file),
        <<"span">> => span_to_json(E#entry.span),
        %% ADR 0114.
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
        <<"flushed">> => E#entry.flushed,
        <<"cancelled">> => E#entry.cancelled
    },
    iolist_to_binary(json:encode(Map)).

-doc "Decode a `changes.jsonl` line into a ChangeEntry record. Exported for tests.".
-spec entry_from_json(binary()) -> #entry{}.
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
        %% ADR 0114: absent in every metadata line written before
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
        flushed = maps:get(<<"flushed">>, Map, false),
        cancelled = maps:get(<<"cancelled">>, Map, false)
    }.

%% Enum decoders use an explicit allowlist with a safe `unknown` fallback rather
%% than binary_to_existing_atom/2. A value written by a newer build (kind is an
%% open enum) — or a corrupt closed-enum field — would otherwise throw and cause
%% parse_log/1 to silently drop the whole line, losing history. Mapping to
%% `unknown` keeps the entry; it is excluded from the active view via prior_epoch
%% on restart regardless.
-spec decode_kind(binary()) -> kind().
decode_kind(Wire) ->
    case beamtalk_workspace_kinds:decode(Wire) of
        {ok, Kind} ->
            Kind;
        error ->
            log_unknown_enum(kind, Wire),
            unknown
    end.

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

%% ADR 0114: `sites`/`candidate_sites` (de)serialisation. A whole
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
