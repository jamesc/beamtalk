%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Runtime helper operations for Dictionary (Erlang maps).
%%
%% BT-418: Complex Dictionary operations that cannot be inlined as
%% direct BIF calls in generated Core Erlang. Called from compiled
%% stdlib module bt@stdlib@dictionary.
%%
%% Replaces the former beamtalk_map.erl hand-written dispatch module.
-module(beamtalk_map_ops).

-export([at_if_absent/3, keys_and_values_do/2, do/2, includes/2]).

%% @doc Get value at key, or evaluate block if absent.
-spec at_if_absent(map(), term(), fun(() -> term())) -> term().
at_if_absent(Map, Key, Block) when is_function(Block, 0) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Block()
    end.

%% @doc Iterate over all key-value pairs.
-spec keys_and_values_do(map(), fun((term(), term()) -> term())) -> nil.
keys_and_values_do(Map, Block) when is_function(Block, 2) ->
    maps:foreach(Block, Map),
    nil.

%% @doc Iterate over all values in the dictionary.
-spec do(map(), fun((term()) -> term())) -> nil.
do(Map, Block) when is_function(Block, 1) ->
    maps:foreach(fun(_K, V) -> Block(V) end, Map),
    nil.

%% @doc Test if the dictionary contains the given value.
-spec includes(map(), term()) -> boolean().
includes(Map, Value) ->
    lists:member(Value, maps:values(Map)).
