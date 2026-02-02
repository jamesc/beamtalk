%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Integer primitive class implementation.
%%%
%%% This module provides method dispatch for Erlang integers, mapping them
%%% to the Beamtalk `Integer` class. Supports arithmetic, comparison, reflection,
%%% and extension methods via the extension registry.
%%%
%%% ## Builtin Methods
%%%
%%% | Selector | Args | Description |
%%% |----------|------|-------------|
%%% | `+`      | [Y]  | Addition    |
%%% | `-`      | [Y]  | Subtraction |
%%% | `*`      | [Y]  | Multiplication |
%%% | `/`      | [Y]  | Division    |
%%% | `=`      | [Y]  | Equality    |
%%% | `<`      | [Y]  | Less than   |
%%% | `>`      | [Y]  | Greater than |
%%% | `<=`     | [Y]  | Less than or equal |
%%% | `>=`     | [Y]  | Greater than or equal |
%%% | `class`  | []   | Returns `'Integer'` |
%%% | `asString` | [] | Binary representation |
%%% | `abs`    | []   | Absolute value |
%%% | `negated` | []  | Negation (-X) |
%%%
%%% ## Usage Examples
%%%
%%% ```erlang
%%% %% Arithmetic
%%% beamtalk_integer:dispatch('+', [8], 42).  % => 50
%%% beamtalk_integer:dispatch('*', [3], 7).   % => 21
%%%
%%% %% Comparison
%%% beamtalk_integer:dispatch('<', [10], 5).  % => true
%%%
%%% %% Reflection
%%% beamtalk_integer:dispatch('class', [], 42). % => 'Integer'
%%% beamtalk_integer:dispatch('asString', [], 42). % => <<"42">>
%%%
%%% %% Numeric operations
%%% beamtalk_integer:dispatch('abs', [], -42).    % => 42
%%% beamtalk_integer:dispatch('negated', [], 42). % => -42
%%%
%%% %% Check if method exists
%%% beamtalk_integer:has_method('+').  % => true
%%% beamtalk_integer:has_method('foo'). % => false (checks extensions too)
%%% ```
%%%
%%% See: docs/internal/design-self-as-object.md Section 3.3

-module(beamtalk_integer).
-export([dispatch/3, has_method/1]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Dispatch a message to an integer value.
%%
%% Tries builtin methods first, then falls back to the extension registry
%% for user-defined methods. Raises does_not_understand error if method not found.
%%
%% Examples:
%% ```
%% dispatch('+', [8], 42)        % => 50
%% dispatch('class', [], 42)     % => 'Integer'
%% dispatch('unknown', [], 42)   % => error({does_not_understand, ...})
%% ```
-spec dispatch(atom(), list(), integer()) -> term().
dispatch(Selector, Args, Value) ->
    case builtin_dispatch(Selector, Args, Value) of
        {ok, Result} -> Result;
        not_found -> does_not_understand(Selector, Args, Value)
    end.

%% @doc Check if an integer responds to the given selector.
%%
%% Checks both builtin methods and the extension registry.
%%
%% Examples:
%% ```
%% has_method('+')   % => true
%% has_method('foo') % => false (unless registered as extension)
%% ```
-spec has_method(atom()) -> boolean().
has_method(Selector) ->
    %% Check if builtin exists
    is_builtin(Selector) orelse beamtalk_extensions:has('Integer', Selector).

%% @doc Check if a selector is a builtin method.
-spec is_builtin(atom()) -> boolean().
is_builtin('+') -> true;
is_builtin('-') -> true;
is_builtin('*') -> true;
is_builtin('/') -> true;
is_builtin('=') -> true;
is_builtin('<') -> true;
is_builtin('>') -> true;
is_builtin('<=') -> true;
is_builtin('>=') -> true;
is_builtin('class') -> true;
is_builtin('asString') -> true;
is_builtin('abs') -> true;
is_builtin('negated') -> true;
is_builtin(_) -> false.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @doc Dispatch to builtin integer methods.
%%
%% Returns {ok, Result} if method exists, not_found otherwise.
-spec builtin_dispatch(atom(), list(), integer()) -> {ok, term()} | not_found.
%% Arithmetic operations
builtin_dispatch('+', [Y], X) when is_integer(Y) -> {ok, X + Y};
builtin_dispatch('-', [Y], X) when is_integer(Y) -> {ok, X - Y};
builtin_dispatch('*', [Y], X) when is_integer(Y) -> {ok, X * Y};
builtin_dispatch('/', [Y], X) when is_integer(Y), Y =/= 0 -> {ok, X / Y};

%% Comparison operations
builtin_dispatch('=', [Y], X) -> {ok, X =:= Y};
builtin_dispatch('<', [Y], X) -> {ok, X < Y};
builtin_dispatch('>', [Y], X) -> {ok, X > Y};
builtin_dispatch('<=', [Y], X) -> {ok, X =< Y};
builtin_dispatch('>=', [Y], X) -> {ok, X >= Y};

%% Reflection
builtin_dispatch('class', [], _X) -> {ok, 'Integer'};

%% Conversion
builtin_dispatch('asString', [], X) -> {ok, integer_to_binary(X)};

%% Numeric operations
builtin_dispatch('abs', [], X) -> {ok, abs(X)};
builtin_dispatch('negated', [], X) -> {ok, -X};

%% Not a builtin method
builtin_dispatch(_, _, _) -> not_found.

%% @doc Handle doesNotUnderstand by checking extension registry.
-spec does_not_understand(atom(), list(), integer()) -> term().
does_not_understand(Selector, Args, Value) ->
    case beamtalk_extensions:lookup('Integer', Selector) of
        {ok, Fun, _Owner} -> 
            Fun(Args, Value);
        not_found -> 
            error({does_not_understand, 'Integer', Selector, length(Args)})
    end.
