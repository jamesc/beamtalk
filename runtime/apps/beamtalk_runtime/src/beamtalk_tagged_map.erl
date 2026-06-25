%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_tagged_map).

%%% **DDD Context:** Object System Context

-moduledoc """
Tagged map value object — centralizes tagged-map operations.

A "tagged map" is an Erlang map with a `'$beamtalk_class'` key that
identifies its type. Tagged maps represent:

- **Actor state** (`Counter`, `LoggingCounter`, etc.)
- **Value type instances** (`Point`, etc.)
- **CompiledMethod** objects
- **Dynamic object** instances

This module is the single source of truth for:
- The tag key atom (eliminates 40+ bare literal duplications)
- Classification: is a map tagged? What class is it?
- Internal field filtering: which keys are system vs user?
- Display formatting for transcript/REPL output

See also: beamtalk_primitive For dispatch based on tagged map classification.
See also: beamtalk_object For Object base class reflection using tagged maps.
""".

%% Tag key constant
-export([class_key/0]).

%% Classification
-export([class_of/1, class_of/2, is_tagged/1]).

%% Internal field management
-export([internal_fields/0, user_field_keys/1]).

%% Canonical Array representation readers (ADR 0090)
-export([array_data/1, array_to_list/1, array_size/1]).

%% Display
-export([format_for_display/1]).

%%% ============================================================================
%%% Tag Key Constant
%%% ============================================================================

-doc """
Returns the atom used as the tag key in tagged maps.

This is the single source of truth for the key. All modules that create
or inspect tagged maps should use this function instead of a bare literal.
""".
-spec class_key() -> atom().
class_key() -> '$beamtalk_class'.

%%% ============================================================================
%%% Classification
%%% ============================================================================

-doc """
Returns the class name for a tagged map, or `undefined` for non-tagged maps.

Prefers `$beamtalk_class` when present (dynamic actors, runtime-created maps,
and compiled actors with pre-migration state all carry it). Falls back to
calling `Mod:class_name()` via `__class_mod__` for compiled actors that have
been migrated (no longer carry `$beamtalk_class` in instance state) — the
module function is always correct after hot-reload.
Non-map values always return `undefined`.
""".
-spec class_of(term()) -> atom() | undefined.
class_of(Map) when is_map(Map) ->
    % elp:fixme W0032 maps:find with complex branch logic
    case maps:find('$beamtalk_class', Map) of
        {ok, Class} when is_atom(Class) ->
            Class;
        _ ->
            class_name_from_mod(Map, undefined)
    end;
class_of(_) ->
    undefined.

-doc "Returns the class name for a tagged map, or `Default` for non-tagged maps.".
-spec class_of(term(), atom()) -> atom().
class_of(Map, Default) when is_map(Map) ->
    % elp:fixme W0032 maps:find with complex branch logic
    case maps:find('$beamtalk_class', Map) of
        {ok, Class} when is_atom(Class) ->
            Class;
        _ ->
            class_name_from_mod(Map, Default)
    end;
class_of(_, Default) ->
    Default.

-doc "Returns `true` if the value is a tagged map (has a valid class key).".
-spec is_tagged(term()) -> boolean().
is_tagged(Map) when is_map(Map) ->
    % elp:fixme W0032 maps:find with complex branch logic
    case maps:find('$beamtalk_class', Map) of
        {ok, Class} when is_atom(Class) -> true;
        _ -> maps:is_key('__class_mod__', Map)
    end;
is_tagged(_) ->
    false.

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

-doc "Try `Mod:class_name()` via `__class_mod__`, with guard for stale BEAM files.".
-spec class_name_from_mod(map(), atom()) -> atom().
class_name_from_mod(Map, Default) ->
    % elp:fixme W0032 maps:find with complex branch logic
    case maps:find('__class_mod__', Map) of
        {ok, Mod} when is_atom(Mod) ->
            case erlang:function_exported(Mod, class_name, 0) of
                true -> Mod:class_name();
                false -> Default
            end;
        _ ->
            Default
    end.

%%% ============================================================================
%%% Internal Field Management
%%% ============================================================================

-doc """
Returns the canonical list of internal field names in tagged maps.

These fields are system metadata, not user-visible instance variables.
Used by reflection methods like `fieldNames` to filter out internals.
""".
-spec internal_fields() -> [atom()].
internal_fields() ->
    ['$beamtalk_class', '__class_mod__', '__methods__', '__registry_pid__'].

-doc """
Returns only user-visible field keys from a tagged map state.

Filters out all internal fields (class, class_mod, methods, registry_pid).
""".
-spec user_field_keys(map()) -> [atom()].
user_field_keys(State) when is_map(State) ->
    Internals = internal_fields(),
    [K || K <- maps:keys(State), not lists:member(K, Internals)].

%%% ============================================================================
%%% Array Representation Readers (ADR 0090)
%%%
%%% A Beamtalk `Array` is the tagged map
%%% `#{'$beamtalk_class' => 'Array', 'data' => #{0 => V0, 1 => V1, ...}}`,
%%% where `'data'` is a canonical index→value map (keys `0..N-1`). The canonical
%%% map representation is owned and constructed by `beamtalk_array` (stdlib);
%%% these readers let lower-layer runtime modules (which cannot depend on stdlib)
%%% interpret the `'data'` payload from a single source of truth. They are
%%% forge-tolerant — a malformed `'data'` (missing or not a map, BT-2509) degrades
%%% to an empty array rather than crashing.
%%% ============================================================================

-doc """
Return the canonical index→value `'data'` map of an Array tagged map.

Degrades to `#{}` when `'data'` is absent or not a map (forged/malformed input).
""".
-spec array_data(map()) -> #{non_neg_integer() => term()}.
array_data(Subject) when is_map(Subject) ->
    case maps:get(data, Subject, #{}) of
        Data when is_map(Data) -> Data;
        _ -> #{}
    end.

-doc "Return the number of elements in an Array tagged map.".
-spec array_size(map()) -> non_neg_integer().
array_size(Subject) ->
    maps:size(array_data(Subject)).

-doc """
Return the elements of an Array tagged map as an ordered list (index `0..N-1`).

Sorts by the integer index key, so it is correct for canonical arrays and total
for forged ones (no crash on a non-contiguous index set).
""".
-spec array_to_list(map()) -> [term()].
array_to_list(Subject) ->
    Data = array_data(Subject),
    [Value || {_Index, Value} <- lists:sort(maps:to_list(Data))].

%%% ============================================================================
%%% Display
%%% ============================================================================

-doc """
Formats a tagged map for transcript/REPL display.

Dispatches `displayString` to the value class so user-defined
display methods are honoured. Falls back to the bare class name if
dispatch fails.
""".
-spec format_for_display(map()) -> binary().
format_for_display(Map) when is_map(Map) ->
    case class_of(Map) of
        undefined ->
            list_to_binary(io_lib:format("~p", [Map]));
        _Class ->
            beamtalk_primitive:send(Map, 'displayString', [])
    end.
