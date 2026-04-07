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
%%% Display
%%% ============================================================================

-doc """
Formats a tagged map for transcript/REPL display.

Dispatches `displayString` to the value class so user-defined
display methods are honoured. Falls back to `"a ClassName"` if
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
