%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Reflection services for instance variable access.
%%%
%%% **DDD Context:** Runtime Context
%%%
%%% This domain service provides uniform access to instance variables
%%% (fields) on Beamtalk objects. It extracts the reflection logic that
%%% was previously inline in `beamtalk_object_ops:dispatch/4`, giving a
%%% single source of truth for field read/write semantics.
%%%
%%% Follows Smalltalk-80 conventions:
%%% - `read_field/2` returns `nil` for non-existent fields
%%% - `write_field/3` creates new fields if they don't exist
%%% - `write_field/3` returns the value (not the object)
%%%
%%% @see beamtalk_object
%%% @see beamtalk_tagged_map
-module(beamtalk_reflection).

-export([field_names/1, read_field/2, write_field/3, inspect_string/1]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Returns the list of user-visible instance variable names.
%%
%% Filters out internal metadata fields (`$beamtalk_class`, `__methods__`, etc.)
%% leaving only the fields defined by the class or added at runtime.
-spec field_names(map()) -> [atom()].
field_names(State) when is_map(State) ->
    beamtalk_tagged_map:user_field_keys(State).

%% @doc Reads an instance variable by name.
%%
%% Returns `nil` for non-existent fields (Smalltalk-80 semantics).
-spec read_field(atom(), map()) -> term().
read_field(Name, State) when is_atom(Name), is_map(State) ->
    maps:get(Name, State, nil).

%% @doc Writes an instance variable, returning `{Value, NewState}`.
%%
%% Creates the field if it doesn't exist (Smalltalk-80 semantics).
%% Returns the written value (not the object), matching `instVarAt:put:`
%% convention.
-spec write_field(atom(), term(), map()) -> {term(), map()}.
write_field(Name, Value, State) when is_atom(Name), is_map(State) ->
    NewState = maps:put(Name, Value, State),
    {Value, NewState}.

%% @doc Generates an inspect string for an object: "a ClassName (field1: val1, field2: val2)".
%%
%% BT-446: Extracted from beamtalk_object_ops:dispatch/4 so that compiled
%% Object modules can call it from generated dispatch/4 code.
-spec inspect_string(map()) -> binary().
inspect_string(State) when is_map(State) ->
    ClassName = beamtalk_tagged_map:class_of(State, 'Object'),
    UserFields = field_names(State),
    FieldStrs = [iolist_to_binary([format_key(K), <<": ">>,
                                   beamtalk_primitive:print_string(maps:get(K, State))])
                 || K <- UserFields],
    FieldsPart = case FieldStrs of
        [] -> <<"">>;
        _ -> iolist_to_binary([<<" (">>, lists:join(<<", ">>, FieldStrs), <<")">>])
    end,
    iolist_to_binary([<<"a ">>, atom_to_binary(ClassName, utf8), FieldsPart]).

%% @private
%% @doc Format a field key as binary.
-spec format_key(atom()) -> binary().
format_key(K) when is_atom(K) -> atom_to_binary(K, utf8).
