%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Transcript class implementation - standard I/O output object.
%%%
%%% **DDD Context:** Runtime Context
%%%
%%% Transcript is the standard Smalltalk I/O object. In Beamtalk it writes
%%% to stdout. It is a global singleton accessed by class name.
%%%
%%% ## Methods
%%%
%%% | Selector  | Args    | Description                        |
%%% |-----------|---------|-------------------------------------|
%%% | `show:`   | [Value] | Print value to stdout               |
%%% | `cr`      | []      | Print a newline to stdout           |
%%%
%%% ## Usage
%%%
%%% ```beamtalk
%%% Transcript show: 'Hello'
%%% Transcript cr
%%% Transcript show: 42
%%% ```
%%%
%%% ## Design
%%%
%%% - Uses `io:put_chars/1` for clean output (not `io:format`)
%%% - Converts primitive values directly (integers, atoms, etc.)
%%% - Falls back to `io_lib:format("~p", ...)` for complex types
%%% - Returns `nil` from all methods (side-effect only)
%%% - Unknown selectors are not handled here and will result in `undef`

-module(transcript).

-export(['show:'/1, cr/0]).
-export([has_method/1]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% Public API (called directly by codegen)
%%% ============================================================================

%% @doc Print a value to stdout.
%%
%% Converts the argument to a string representation and outputs it.
%% Returns nil (side-effect only).
-spec 'show:'(term()) -> 'nil'.
'show:'(Value) ->
    Str = to_string(Value),
    io:put_chars(Str),
    nil.

%% @doc Print a newline to stdout.
%%
%% Returns nil (side-effect only).
-spec cr() -> 'nil'.
cr() ->
    io:put_chars("\n"),
    nil.

%% @doc Check if Transcript responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method('show:') -> true;
has_method(cr) -> true;
has_method(_) -> false.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private
%% @doc Convert a value to its string representation for display.
-spec to_string(term()) -> iodata().
to_string(Value) when is_binary(Value) ->
    Value;
to_string(Value) when is_integer(Value) ->
    integer_to_binary(Value);
to_string(Value) when is_float(Value) ->
    float_to_binary(Value, [{decimals, 10}, compact]);
to_string(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_string(Value) when is_list(Value) ->
    try
        unicode:characters_to_binary(Value)
    catch
        _:_ -> list_to_binary(io_lib:format("~p", [Value]))
    end;
to_string(#beamtalk_object{class = Class}) ->
    <<"a ", (atom_to_binary(Class, utf8))/binary>>;
to_string(Value) when is_map(Value) ->
    case maps:find('__class__', Value) of
        {ok, Class} when is_atom(Class) ->
            <<"a ", (atom_to_binary(Class, utf8))/binary>>;
        _ -> list_to_binary(io_lib:format("~p", [Value]))
    end;
to_string(Value) ->
    %% For other complex types (tuples, etc.), use io_lib:format
    list_to_binary(io_lib:format("~p", [Value])).
