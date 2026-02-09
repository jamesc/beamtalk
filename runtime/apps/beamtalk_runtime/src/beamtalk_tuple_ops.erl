%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Tuple runtime helper functions.
%%%
%%% This module provides runtime support for Tuple methods that require
%%% complex pattern matching or error handling logic. The Tuple class
%%% is now compiled from lib/Tuple.bt (BT-417), but delegates complex
%%% methods to this runtime operations module.
%%%
%%% ## Supported Functions
%%%
%%% | Function | Args | Description |
%%% |----------|------|-------------|
%%% | `unwrap/1` | Tuple | Extract value or raise error |
%%% | `unwrap_or/2` | Tuple, Default | Extract value or return default |
%%% | `unwrap_or_else/2` | Tuple, Block | Extract value or evaluate block |
%%% | `as_string/1` | Tuple | Convert to string |
%%%
%%% ## Usage Examples
%%%
%%% ```erlang
%%% %% Unwrapping
%%% beamtalk_tuple_ops:unwrap({ok, 42}). % => 42
%%% beamtalk_tuple_ops:unwrap_or({error, reason}, default). % => default
%%% ```
%%%
%%% See: BT-417, ADR 0007

-module(beamtalk_tuple_ops).
-export([unwrap/1, unwrap_or/2, unwrap_or_else/2, as_string/1]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Extract value from {ok, Value} or raise error from {error, Reason}.
-spec unwrap(tuple()) -> term().
unwrap({ok, Value}) -> Value;
unwrap({error, Reason}) -> 
    Error0 = beamtalk_error:new(type_error, 'Tuple'),
    Error1 = beamtalk_error:with_selector(Error0, 'unwrap'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Called unwrap on an error tuple">>),
    Error3 = beamtalk_error:with_details(Error2, #{reason => Reason}),
    error(Error3);
unwrap(_Tuple) ->
    %% Invalid pattern (not {ok, _} or {error, _})
    Error0 = beamtalk_error:new(does_not_understand, 'Tuple'),
    Error1 = beamtalk_error:with_selector(Error0, 'unwrap'),
    Error = beamtalk_error:with_hint(Error1, <<"unwrap requires {ok, Value} or {error, Reason} tuple">>),
    error(Error).

%% @doc Return Value from {ok, Value} or Default otherwise.
-spec unwrap_or(tuple(), term()) -> term().
unwrap_or({ok, Value}, _Default) -> Value;
unwrap_or(_Tuple, Default) -> Default.

%% @doc Return Value from {ok, Value} or evaluate block.
-spec unwrap_or_else(tuple(), function()) -> term().
unwrap_or_else({ok, Value}, _Block) when is_function(_Block, 0) ->
    Value;
unwrap_or_else(_Tuple, Block) when is_function(Block, 0) ->
    Block();
unwrap_or_else(_Tuple, _NotABlock) ->
    %% Type error: Block must be a function
    Error0 = beamtalk_error:new(type_error, 'Tuple'),
    Error1 = beamtalk_error:with_selector(Error0, 'unwrapOrElse:'),
    Error = beamtalk_error:with_hint(Error1, <<"Argument must be a block (0-arity function)">>),
    error(Error).

%% @doc Convert tuple to string representation.
-spec as_string(tuple()) -> binary().
as_string(X) ->
    %% Convert tuple to string representation
    Elements = tuple_to_list(X),
    ElementStrs = [format_element(E) || E <- Elements],
    Joined = lists:join(<<", ">>, ElementStrs),
    iolist_to_binary([<<"{">>, Joined, <<"}">>]).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @doc Format a single tuple element for string representation.
-spec format_element(term()) -> binary().
format_element(X) when is_binary(X) -> X;
format_element(X) when is_atom(X) -> atom_to_binary(X, utf8);
format_element(X) when is_integer(X) -> integer_to_binary(X);
format_element(X) when is_float(X) -> float_to_binary(X);
format_element(_X) -> <<"<term>">>.
