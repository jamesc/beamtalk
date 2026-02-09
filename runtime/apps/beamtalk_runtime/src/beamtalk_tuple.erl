%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Tuple runtime helper functions.
%%%
%%% This module provides runtime support for Tuple methods that require
%%% complex pattern matching or error handling logic. The Tuple class
%%% is now compiled from lib/Tuple.bt (BT-417), but delegates complex
%%% methods to this runtime module.
%%%
%%% ## Supported Methods (via delegation from compiled code)
%%%
%%% | Selector | Args | Description |
%%% |----------|------|-------------|
%%% | `unwrap` | []   | Extract value or raise error |
%%% | `unwrapOr:` | [Default] | Extract value or return default |
%%% | `unwrapOrElse:` | [Block] | Extract value or evaluate block |
%%% | `asString` | [] | Convert to string |
%%%
%%% ## Usage Examples
%%%
%%% ```erlang
%%% %% Unwrapping
%%% beamtalk_tuple:dispatch('unwrap', [], {ok, 42}). % => 42
%%% beamtalk_tuple:dispatch('unwrapOr:', [default], {error, reason}). % => default
%%% ```
%%%
%%% See: BT-417, ADR 0007

-module(beamtalk_tuple).
-export([dispatch/3]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Dispatch helper methods for tuple operations.
%%
%% This function is called from compiled Tuple.bt code via @primitive
%% delegation for methods that require complex runtime logic.
-spec dispatch(atom(), list(), tuple()) -> term().

%% Unwrapping - extracts value from {ok, Value} or raises error from {error, Reason}
dispatch('unwrap', [], {ok, Value}) -> Value;
dispatch('unwrap', [], {error, Reason}) -> 
    Error0 = beamtalk_error:new(type_error, 'Tuple'),
    Error1 = beamtalk_error:with_selector(Error0, 'unwrap'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Called unwrap on an error tuple">>),
    Error3 = beamtalk_error:with_details(Error2, #{reason => Reason}),
    error(Error3);
dispatch('unwrap', [], _Tuple) ->
    %% Invalid pattern (not {ok, _} or {error, _})
    Error0 = beamtalk_error:new(does_not_understand, 'Tuple'),
    Error1 = beamtalk_error:with_selector(Error0, 'unwrap'),
    Error = beamtalk_error:with_hint(Error1, <<"unwrap requires {ok, Value} or {error, Reason} tuple">>),
    error(Error);

%% Unwrap with default - returns Value from {ok, Value} or Default otherwise
dispatch('unwrapOr:', [Default], {ok, Value}) -> Value;
dispatch('unwrapOr:', [Default], _) -> Default;

%% Unwrap with block - returns Value from {ok, Value} or evaluates block
dispatch('unwrapOrElse:', [Block], {ok, Value}) when is_function(Block, 0) ->
    Value;
dispatch('unwrapOrElse:', [Block], _Tuple) when is_function(Block, 0) ->
    Block();
dispatch('unwrapOrElse:', [_NotABlock], _Tuple) ->
    %% Type error: Block must be a function
    Error0 = beamtalk_error:new(type_error, 'Tuple'),
    Error1 = beamtalk_error:with_selector(Error0, 'unwrapOrElse:'),
    Error = beamtalk_error:with_hint(Error1, <<"Argument must be a block (0-arity function)">>),
    error(Error);

%% Conversion
dispatch('asString', [], X) ->
    %% Convert tuple to string representation
    Elements = tuple_to_list(X),
    ElementStrs = [format_element(E) || E <- Elements],
    Joined = lists:join(<<", ">>, ElementStrs),
    iolist_to_binary([<<"{">>, Joined, <<"}">>]);

%% Unknown selector
dispatch(Selector, _Args, _Tuple) ->
    Error0 = beamtalk_error:new(does_not_understand, 'Tuple'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error = beamtalk_error:with_hint(Error1, <<"Method should be handled by compiled Tuple class">>),
    error(Error).

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
