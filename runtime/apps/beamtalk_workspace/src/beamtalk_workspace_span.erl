%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_span).

%%% **DDD Context:** Workspace Context

-moduledoc """
Byte-span primitives shared by flush, the ChangeLog, the loader and the
rewrite machinery (BT-4894).

A span is a half-open byte range `[start, end)` into a source binary, carried
either as a `#{start := S, 'end' := E}` map (the ChangeLog / rewrite shape) or a
`{S, E}` tuple. This is the single leaf module below all four callers
(`beamtalk_workspace_flush`, `beamtalk_workspace_changelog`,
`beamtalk_repl_loader`, `beamtalk_repl_loader_rewrite`) — range checking,
slicing, splicing and the "does the recorded span still hold the recorded
text" check live here and nowhere else.
""".

-export([
    valid_span/3,
    in_range/3,
    slice/2,
    splice/3,
    matches_at/3,
    matches_span/3,
    check_span/3
]).

-export_type([span/0, conflict_reason/0]).

-type span() :: #{start := non_neg_integer(), 'end' := non_neg_integer()} | {integer(), integer()}.
-type conflict_reason() ::
    {span_out_of_range, Start :: term(), End :: term(), Size :: non_neg_integer()}
    | external_edit.

-doc """
`true` iff `[Start, End)` is a well-formed span inside a source of `Size`
bytes: integers, `0 =< Start =< End =< Size`.
""".
-spec valid_span(term(), term(), non_neg_integer()) -> boolean().
valid_span(Start, End, Size) ->
    is_integer(Start) andalso
        is_integer(End) andalso
        Start >= 0 andalso
        End >= Start andalso
        End =< Size.

-doc "`true` iff `[Start, End)` is a well-formed span inside `Bin`.".
-spec in_range(binary(), term(), term()) -> boolean().
in_range(Bin, Start, End) ->
    valid_span(Start, End, byte_size(Bin)).

-doc "The bytes of `Source` covered by `Span`. Crashes (`badarg`) on an out-of-range span.".
-spec slice(binary(), span()) -> binary().
slice(Source, Span) ->
    {Start, End} = bounds(Span),
    binary:part(Source, Start, End - Start).

-doc "Replace the bytes covered by `Span` in `Body` with `Replacement`.".
-spec splice(binary(), span(), binary()) -> binary().
splice(Body, Span, Replacement) ->
    {Start, End} = bounds(Span),
    Before = binary:part(Body, 0, Start),
    After = binary:part(Body, End, byte_size(Body) - End),
    <<Before/binary, Replacement/binary, After/binary>>.

-doc """
`true` iff `Body` holds `Text` starting at byte offset `Start`. `Start` is
`integer()` (not `non_neg_integer()`) because callers pass a recorded offset
plus an accumulated shift.
""".
-spec matches_at(binary(), integer(), binary()) -> boolean().
matches_at(Body, Start, Text) ->
    Len = byte_size(Text),
    Start >= 0 andalso
        Start + Len =< byte_size(Body) andalso
        binary:part(Body, Start, Len) =:= Text.

-doc "`true` iff `Span` is in range of `Body` and the bytes it covers equal `Expected`.".
-spec matches_span(binary(), span() | undefined, binary()) -> boolean().
matches_span(_Body, undefined, _Expected) ->
    false;
matches_span(Body, Span, Expected) ->
    {Start, End} = bounds(Span),
    in_range(Body, Start, End) andalso binary:part(Body, Start, End - Start) =:= Expected.

-doc """
Check that `Span` still holds `Expected` in `Body`: `ok`, or
`{conflict, {span_out_of_range, Start, End, Size}}` when the span is outside
the file, or `{conflict, external_edit}` when it is in range but the bytes
differ.
""".
-spec check_span(binary(), span(), binary()) -> ok | {conflict, conflict_reason()}.
check_span(Body, Span, Expected) ->
    {Start, End} = bounds(Span),
    case in_range(Body, Start, End) of
        false ->
            {conflict, {span_out_of_range, Start, End, byte_size(Body)}};
        true ->
            case binary:part(Body, Start, End - Start) =:= Expected of
                true -> ok;
                false -> {conflict, external_edit}
            end
    end.

-spec bounds(span()) -> {term(), term()}.
bounds(#{start := Start, 'end' := End}) -> {Start, End};
bounds({Start, End}) -> {Start, End}.
