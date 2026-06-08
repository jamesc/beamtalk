%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_object_printer).

%%% **DDD Context:** Object System Context

-moduledoc """
Canonical structural renderer for Beamtalk objects.

This module is the **single source of truth** for the structural
`ClassName(field: value, ...)` representation (ADR 0094, Phase 1).
Both the compiled stdlib (`Value.bt`) and the runtime fallback
(`beamtalk_object_ops`, `beamtalk_primitive`, `beamtalk_reflection`)
must call through this module to guarantee byte-identical output.

## Format

    ClassName(field1: value1, field2: value2)

- Fields are rendered in **sorted** order (deterministic output).
- Each field value is rendered via its own `printString` (Debug form).
- A class with no fields renders as `ClassName()`.

## Bounded recursion

Nested values expand recursively, bounded by:
- **depth cap** (default `?DEFAULT_DEPTH_CAP`): guards pathological nesting
- **width cap** (default `?DEFAULT_WIDTH_CAP`): max fields per level
- **total-length cap** (default `?DEFAULT_LENGTH_CAP`): guards wide-and-shallow
- **cycle guard**: emits `...` instead of looping

When any bound is hit, the truncated position renders as `...`.

## Usage

    beamtalk_object_printer:structural(ClassName, Fields)
    beamtalk_object_printer:structural(ClassName, Fields, Opts)

Where `Fields` is a list of `{atom(), term()}` pairs (already extracted
from the object's state map by the caller).

See also: ADR 0094 sections 3, 4, 6; Critical Risks #1, #4
""".

-export([
    structural/2,
    structural/3
]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% Tunable constants
%%% ============================================================================

%% Maximum nesting depth before elision.
-define(DEFAULT_DEPTH_CAP, 5).

%% Maximum number of fields/elements rendered per collection level.
-define(DEFAULT_WIDTH_CAP, 50).

%% Maximum total byte length of the rendered output.
-define(DEFAULT_LENGTH_CAP, 10000).

%% Elision marker emitted when a bound is exceeded.
-define(ELISION, <<"...">>).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Render `ClassName(field: value, ...)` with default bounds.

`ClassName` is an atom. `Fields` is a list of `{Name :: atom(), Value :: term()}`
pairs. Fields are sorted by name for deterministic output.
""".
-spec structural(atom(), [{atom(), term()}]) -> binary().
structural(ClassName, Fields) ->
    structural(ClassName, Fields, #{}).

-doc """
Render `ClassName(field: value, ...)` with caller-supplied options.

Supported options:
- `depth` — maximum nesting depth (default 5)
- `width` — maximum fields per level (default 50)
- `length` — maximum total output bytes (default 10000)

All options are optional; missing keys use defaults.
""".
-spec structural(atom(), [{atom(), term()}], map()) -> binary().
structural(ClassName, Fields, Opts) ->
    Depth = maps:get(depth, Opts, ?DEFAULT_DEPTH_CAP),
    Width = maps:get(width, Opts, ?DEFAULT_WIDTH_CAP),
    Length = maps:get(length, Opts, ?DEFAULT_LENGTH_CAP),
    Seen = #{},
    {Result, _SeenOut} = render_structural(ClassName, Fields, Depth, Width, Length, Seen),
    Result.

%%% ============================================================================
%%% Internal rendering
%%% ============================================================================

%% All internal render functions return `{binary(), Seen}` so the cycle-guard
%% set propagates across sibling fields (not just down the recursion).

-spec render_structural(
    atom(),
    [{atom(), term()}],
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    map()
) ->
    {binary(), map()}.
render_structural(ClassName, _Fields, 0, _Width, _Length, Seen) ->
    %% Depth exhausted: emit elided form.
    Bin = iolist_to_binary([
        atom_to_binary(ClassName, utf8),
        <<"(", ?ELISION/binary, ")">>
    ]),
    {Bin, Seen};
render_structural(ClassName, Fields, Depth, Width, Length, Seen) ->
    ClassBin = atom_to_binary(ClassName, utf8),
    SortedFields = lists:sort(Fields),
    %% Each nested level gets a fresh width budget (Width is the per-level cap).
    {FieldIoList, Seen2} = render_fields(SortedFields, Depth - 1, Width, Width, Length, Seen, []),
    Result = iolist_to_binary([ClassBin, <<"(">>, FieldIoList, <<")">>]),
    {maybe_truncate(Result, Length), Seen2}.

-spec render_fields(
    [{atom(), term()}],
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    map(),
    list()
) ->
    {iolist(), map()}.
render_fields([], _Depth, _MaxWidth, _Remaining, _Length, Seen, Acc) ->
    {lists:join(<<", ">>, lists:reverse(Acc)), Seen};
render_fields(_Fields, _Depth, _MaxWidth, 0, _Length, Seen, Acc) ->
    %% Width exhausted: append elision marker.
    {lists:join(<<", ">>, lists:reverse([?ELISION | Acc])), Seen};
render_fields([{Name, Value} | Rest], Depth, MaxWidth, Remaining, Length, Seen, Acc) ->
    NameBin = atom_to_binary(Name, utf8),
    %% Nested values get the full MaxWidth budget, not the parent's remaining count.
    {ValueBin, Seen2} = render_value(Value, Depth, MaxWidth, Length, Seen),
    FieldBin = iolist_to_binary([NameBin, <<": ">>, ValueBin]),
    render_fields(Rest, Depth, MaxWidth, Remaining - 1, Length, Seen2, [FieldBin | Acc]).

-spec render_value(
    term(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    map()
) -> {binary(), map()}.
render_value(Value, Depth, Width, Length, Seen) when is_map(Value) ->
    %% Tagged map (Value instance): recurse structurally if it has user fields.
    case beamtalk_tagged_map:class_of(Value) of
        undefined ->
            %% Plain map — use printString.
            {beamtalk_primitive:print_string(Value), Seen};
        ClassName ->
            %% Cycle guard: phash2 hashes by *value* (structural equality).
            %% For immutable Values, structurally equal maps produce identical
            %% output, so eliding a duplicate is correct. Hash collisions between
            %% genuinely different maps are possible (~1 in 2^27) but benign —
            %% the worst case is eliding a field that would have rendered differently.
            %% True cycles cannot occur with immutable Values (ADR 0042).
            Id = erlang:phash2(Value),
            case maps:is_key(Id, Seen) of
                true ->
                    {?ELISION, Seen};
                false ->
                    NewSeen = Seen#{Id => true},
                    UserKeys = beamtalk_tagged_map:user_field_keys(Value),
                    Fields = [{K, maps:get(K, Value)} || K <- UserKeys],
                    render_structural(ClassName, Fields, Depth, Width, Length, NewSeen)
            end
    end;
render_value(#beamtalk_object{} = Value, _Depth, _Width, _Length, Seen) ->
    %% Actor/object references: use printString (opaque — no field access).
    {beamtalk_primitive:print_string(Value), Seen};
render_value(Value, _Depth, _Width, _Length, Seen) ->
    %% Primitives (integers, strings, booleans, symbols, etc.): use printString.
    {beamtalk_primitive:print_string(Value), Seen}.

-spec maybe_truncate(binary(), non_neg_integer()) -> binary().
maybe_truncate(Bin, MaxLength) when byte_size(Bin) > MaxLength ->
    Prefix = binary:part(Bin, 0, MaxLength),
    <<Prefix/binary, ?ELISION/binary>>;
maybe_truncate(Bin, _MaxLength) ->
    Bin.
