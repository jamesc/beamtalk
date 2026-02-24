%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Tuple runtime helper functions.
%%%
%%% **DDD Context:** Runtime Context — Domain Service
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
%%% | `at/2` | Tuple, Index | Element at index (1-based) with bounds checking |
%%% | `as_string/1` | Tuple | Convert to string |
%%%
%%% See: BT-417, ADR 0007

-module(beamtalk_tuple_ops).
-export([at/2, as_string/1, do/2]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Element at index (1-based) with bounds and type checking.
-spec at(tuple(), integer()) -> term().
at(Tuple, Idx) when is_integer(Idx), Idx >= 1, Idx =< tuple_size(Tuple) ->
    element(Idx, Tuple);
at(_Tuple, Idx) when not is_integer(Idx) ->
    Error0 = beamtalk_error:new(type_error, 'Tuple'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:'),
    Error = beamtalk_error:with_hint(Error1, <<"Index must be an integer">>),
    beamtalk_error:raise(Error);
at(Tuple, Idx) ->
    Error0 = beamtalk_error:new(does_not_understand, 'Tuple'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:'),
    Error2 = beamtalk_error:with_hint(
        Error1,
        iolist_to_binary(
            io_lib:format(
                "Index ~p is out of bounds for tuple of size ~p (1-based indexing)",
                [Idx, tuple_size(Tuple)]
            )
        )
    ),
    beamtalk_error:raise(Error2).

%% @doc Convert tuple to string representation.
-spec as_string(tuple()) -> binary().
as_string(X) ->
    Elements = tuple_to_list(X),
    ElementStrs = [format_element(E) || E <- Elements],
    Joined = lists:join(<<", ">>, ElementStrs),
    iolist_to_binary([<<"{">>, Joined, <<"}">>]).

%% @doc Iterate over each element of the tuple.
-spec do(tuple(), fun((term()) -> term())) -> 'nil'.
do(Tuple, Block) when is_function(Block, 1) ->
    lists:foreach(Block, tuple_to_list(Tuple)),
    nil.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @doc Format a single tuple element for string representation.
%% BT-536: Delegates to beamtalk_primitive:print_string/1 for consistent
%% formatting (atoms as #symbol, nested tuples as {el1, el2}, etc.)
-spec format_element(term()) -> binary().
format_element(X) -> beamtalk_primitive:print_string(X).
