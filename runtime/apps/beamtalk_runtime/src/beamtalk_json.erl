%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc JSON class implementation — JSON encoding/decoding via jsx.
%%%
%%% **DDD Context:** Runtime Context
%%%
%%% JSON provides class-side methods for parsing and generating JSON strings.
%%% Wraps the `jsx` library with proper type mapping and structured error handling.
%%%
%%% ## Type Mapping
%%%
%%% | JSON          | Beamtalk     |
%%% |---------------|--------------|
%%% | object        | Dictionary   |
%%% | array         | List         |
%%% | string        | String       |
%%% | number (int)  | Integer      |
%%% | number (float)| Float        |
%%% | true/false    | true/false   |
%%% | null          | nil          |
%%%
%%% ## Methods
%%%
%%% | Selector        | Description                              |
%%% |-----------------|------------------------------------------|
%%% | `parse:`        | JSON string → Beamtalk value             |
%%% | `generate:`     | Beamtalk value → JSON string             |
%%% | `prettyPrint:`  | Beamtalk value → formatted JSON string   |

-module(beamtalk_json).

-export(['parse:'/1, 'generate:'/1, 'prettyPrint:'/1]).
-export([has_method/1]).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Parse a JSON string into a Beamtalk value.
%%
%% JSON objects become Dictionaries (maps with binary keys), arrays become
%% Lists, strings become Strings, numbers become Integer or Float,
%% true/false stay as atoms, null becomes nil.
-spec 'parse:'(binary()) -> term().
'parse:'(JsonStr) when is_binary(JsonStr) ->
    try
        Result = jsx:decode(JsonStr, [return_maps]),
        normalize_decoded(Result)
    catch
        error:badarg ->
            Error0 = beamtalk_error:new(parse_error, 'JSON'),
            Error1 = beamtalk_error:with_selector(Error0, 'parse:'),
            Error2 = beamtalk_error:with_hint(Error1, <<"Check that the string is valid JSON">>),
            beamtalk_error:raise(Error2);
        _:Reason ->
            Error0 = beamtalk_error:new(parse_error, 'JSON'),
            Error1 = beamtalk_error:with_selector(Error0, 'parse:'),
            Error2 = beamtalk_error:with_details(Error1, #{reason => Reason}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check that the string is valid JSON">>),
            beamtalk_error:raise(Error3)
    end;
'parse:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'JSON'),
    Error1 = beamtalk_error:with_selector(Error0, 'parse:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Generate a JSON string from a Beamtalk value.
%%
%% Dictionaries become JSON objects, Lists become arrays, Strings become
%% JSON strings, Integer/Float become numbers, true/false become JSON
%% booleans, nil becomes null.
-spec 'generate:'(term()) -> binary().
'generate:'(Value) ->
    try
        Prepared = prepare_for_encode(Value),
        jsx:encode(Prepared)
    catch
        error:badarg ->
            Error0 = beamtalk_error:new(type_error, 'JSON'),
            Error1 = beamtalk_error:with_selector(Error0, 'generate:'),
            Error2 = beamtalk_error:with_hint(Error1, <<"Value cannot be converted to JSON">>),
            beamtalk_error:raise(Error2);
        _:Reason ->
            Error0 = beamtalk_error:new(type_error, 'JSON'),
            Error1 = beamtalk_error:with_selector(Error0, 'generate:'),
            Error2 = beamtalk_error:with_details(Error1, #{reason => Reason}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Value cannot be converted to JSON">>),
            beamtalk_error:raise(Error3)
    end.

%% @doc Generate a pretty-printed JSON string with indentation.
-spec 'prettyPrint:'(term()) -> binary().
'prettyPrint:'(Value) ->
    try
        Prepared = prepare_for_encode(Value),
        Compact = jsx:encode(Prepared),
        jsx:prettify(Compact)
    catch
        error:badarg ->
            Error0 = beamtalk_error:new(type_error, 'JSON'),
            Error1 = beamtalk_error:with_selector(Error0, 'prettyPrint:'),
            Error2 = beamtalk_error:with_hint(Error1, <<"Value cannot be converted to JSON">>),
            beamtalk_error:raise(Error2);
        _:Reason ->
            Error0 = beamtalk_error:new(type_error, 'JSON'),
            Error1 = beamtalk_error:with_selector(Error0, 'prettyPrint:'),
            Error2 = beamtalk_error:with_details(Error1, #{reason => Reason}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Value cannot be converted to JSON">>),
            beamtalk_error:raise(Error3)
    end.

%% @doc Check if JSON responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method('parse:') -> true;
has_method('generate:') -> true;
has_method('prettyPrint:') -> true;
has_method(_) -> false.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private
%% @doc Normalize jsx decoded values to Beamtalk conventions.
%%
%% jsx returns `null` as the atom `null`; Beamtalk uses `nil`.
-spec normalize_decoded(term()) -> term().
normalize_decoded(null) ->
    nil;
normalize_decoded(Map) when is_map(Map) ->
    maps:map(fun(_K, V) -> normalize_decoded(V) end, Map);
normalize_decoded(List) when is_list(List) ->
    lists:map(fun normalize_decoded/1, List);
normalize_decoded(Other) ->
    Other.

%% @private
%% @doc Prepare a Beamtalk value for jsx encoding.
%%
%% Beamtalk uses `nil` for null; jsx expects `null`.
%% Maps with `$beamtalk_class` tags are stripped of metadata.
-spec prepare_for_encode(term()) -> term().
prepare_for_encode(nil) ->
    null;
prepare_for_encode(Map) when is_map(Map) ->
    %% Strip $beamtalk_class tag if present (e.g., from Dictionary)
    Cleaned = maps:remove('$beamtalk_class', Map),
    maps:map(fun(_K, V) -> prepare_for_encode(V) end, Cleaned);
prepare_for_encode(List) when is_list(List) ->
    lists:map(fun prepare_for_encode/1, List);
prepare_for_encode(true) -> true;
prepare_for_encode(false) -> false;
prepare_for_encode(V) when is_integer(V) -> V;
prepare_for_encode(V) when is_float(V) -> V;
prepare_for_encode(V) when is_binary(V) -> V;
prepare_for_encode(V) when is_atom(V) ->
    %% Convert atoms (symbols) to strings for JSON compatibility
    atom_to_binary(V, utf8);
prepare_for_encode(Other) ->
    %% Last resort — try to convert to string
    Error0 = beamtalk_error:new(type_error, 'JSON'),
    Error1 = beamtalk_error:with_selector(Error0, 'generate:'),
    Error2 = beamtalk_error:with_details(Error1, #{value => Other}),
    Error3 = beamtalk_error:with_hint(Error2, <<"Only Dictionary, List, String, Integer, Float, Boolean, and nil can be converted to JSON">>),
    beamtalk_error:raise(Error3).
