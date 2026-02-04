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
%%% | `+`      | [Y]  | Addition (accepts numbers) |
%%% | `-`      | [Y]  | Subtraction (accepts numbers) |
%%% | `*`      | [Y]  | Multiplication (accepts numbers) |
%%% | `/`      | [Y]  | Division (accepts numbers, Y ≠ 0) |
%%% | `=`      | [Y]  | Equality (strict =:=) |
%%% | `<`      | [Y]  | Less than (accepts numbers) |
%%% | `>`      | [Y]  | Greater than (accepts numbers) |
%%% | `<=`     | [Y]  | Less than or equal (accepts numbers) |
%%% | `>=`     | [Y]  | Greater than or equal (accepts numbers) |
%%% | `class`  | []   | Returns `'Integer'` |
%%% | `respondsTo` | [Sel] | Returns true if responds to selector |
%%% | `asString` | [] | Binary representation |
%%% | `abs`    | []   | Absolute value |
%%% | `negated` | []  | Negation (-X) |
%%%
%%% **Note:** Arithmetic and comparison operations accept both integers and floats.
%%% Mixed integer/float operations follow Erlang's numeric tower (result type depends
%%% on operation). Equality uses strict equality (=:=), so `42 = 42.0` returns false.
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

-include("beamtalk.hrl").

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
is_builtin('respondsTo') -> true;
is_builtin('perform') -> true;
is_builtin('perform:withArgs:') -> true;
is_builtin('asString') -> true;
is_builtin('abs') -> true;
is_builtin('negated') -> true;
is_builtin('instVarNames') -> true;
is_builtin('instVarAt') -> true;
is_builtin('instVarAt:put:') -> true;
is_builtin(_) -> false.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @doc Dispatch to builtin integer methods.
%%
%% Returns {ok, Result} if method exists, not_found otherwise.
-spec builtin_dispatch(atom(), list(), integer()) -> {ok, term()} | not_found.
%% Arithmetic operations
builtin_dispatch('+', [Y], X) when is_number(Y) -> {ok, X + Y};
builtin_dispatch('-', [Y], X) when is_number(Y) -> {ok, X - Y};
builtin_dispatch('*', [Y], X) when is_number(Y) -> {ok, X * Y};
builtin_dispatch('/', [Y], X) when is_number(Y), Y =/= 0 -> {ok, X / Y};

%% Comparison operations
builtin_dispatch('=', [Y], X) when is_number(Y) -> {ok, X =:= Y};
builtin_dispatch('=', [Y], X) -> {ok, false};  % Non-numeric comparison always false
builtin_dispatch('<', [Y], X) when is_number(Y) -> {ok, X < Y};
builtin_dispatch('>', [Y], X) when is_number(Y) -> {ok, X > Y};
builtin_dispatch('<=', [Y], X) when is_number(Y) -> {ok, X =< Y};
builtin_dispatch('>=', [Y], X) when is_number(Y) -> {ok, X >= Y};

%% Reflection
builtin_dispatch('class', [], _X) -> {ok, 'Integer'};
builtin_dispatch('respondsTo', [Selector], _X) when is_atom(Selector) -> 
    {ok, has_method(Selector)};

%% Dynamic message send
builtin_dispatch('perform', [TargetSelector], X) when is_atom(TargetSelector) ->
    %% Recursive dispatch returns {ok, Result} or not_found
    builtin_dispatch(TargetSelector, [], X);
builtin_dispatch('perform:withArgs:', [TargetSelector, ArgList], X) 
  when is_atom(TargetSelector), is_list(ArgList) ->
    %% Recursive dispatch returns {ok, Result} or not_found
    builtin_dispatch(TargetSelector, ArgList, X);
builtin_dispatch('perform:withArgs:', [_TargetSelector, ArgList], _X) 
  when not is_list(ArgList) ->
    %% Type error: ArgList must be a list (consistent with actor behavior)
    error({type_error, list, ArgList});

%% Conversion
builtin_dispatch('asString', [], X) -> {ok, integer_to_binary(X)};

%% Numeric operations
builtin_dispatch('abs', [], X) -> {ok, abs(X)};
builtin_dispatch('negated', [], X) -> {ok, -X};

%% Instance variable reflection (BT-164)
%% Primitives are immutable and have no instance variables
builtin_dispatch('instVarNames', [], _X) -> 
    {ok, []};
builtin_dispatch('instVarAt', [_Name], _X) -> 
    {ok, nil};
builtin_dispatch('instVarAt:put:', [Name, _Value], _X) -> 
    %% Primitives cannot be mutated
    Error0 = beamtalk_error:new(immutable_primitive, 'Integer'),
    Error1 = beamtalk_error:with_selector(Error0, 'instVarAt:put:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Integers are immutable. Use assignment (x := newValue) instead.">>),
    Error = beamtalk_error:with_details(Error2, #{field => Name}),
    error(Error);

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
