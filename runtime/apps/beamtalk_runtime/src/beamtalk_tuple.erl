%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Tuple primitive class implementation.
%%%
%%% This module provides method dispatch for Erlang tuples, mapping them
%%% to the Beamtalk `Tuple` class. Particularly useful for Erlang interop
%%% with {ok, Value} and {error, Reason} patterns.
%%%
%%% ## Builtin Methods
%%%
%%% | Selector | Args | Description |
%%% |----------|------|-------------|
%%% | `class`  | []   | Returns `'Tuple'` |
%%% | `respondsTo` | [Sel] | Returns true if responds to selector |
%%% | `size`   | []   | Tuple size |
%%% | `at:`    | [Idx] | Element at index (1-based) |
%%% | `isOk`   | []   | Check if `{ok, _}` pattern |
%%% | `isError` | []  | Check if `{error, _}` pattern |
%%% | `unwrap` | []   | Extract value or raise error |
%%% | `unwrapOr:` | [Default] | Extract value or return default |
%%% | `unwrapOrElse:` | [Block] | Extract value or evaluate block |
%%% | `asString` | [] | Convert to string |
%%% | `instVarNames` | [] | Returns `[]` (no instance variables) |
%%% | `instVarAt` | [Name] | Returns `nil` (no fields) |
%%% | `instVarAt:put:` | [Name, Value] | Error: immutable primitive |
%%%
%%% ## Usage Examples
%%%
%%% ```erlang
%%% %% Reflection
%%% beamtalk_tuple:dispatch('class', [], {a, b}). % => 'Tuple'
%%%
%%% %% Size and access
%%% beamtalk_tuple:dispatch('size', [], {a, b, c}). % => 3
%%% beamtalk_tuple:dispatch('at:', [2], {a, b, c}). % => b
%%%
%%% %% Result pattern matching
%%% beamtalk_tuple:dispatch('isOk', [], {ok, 42}). % => true
%%% beamtalk_tuple:dispatch('isError', [], {error, not_found}). % => true
%%%
%%% %% Unwrapping
%%% beamtalk_tuple:dispatch('unwrap', [], {ok, 42}). % => 42
%%% beamtalk_tuple:dispatch('unwrapOr:', [default], {error, reason}). % => default
%%% ```
%%%
%%% See: docs/internal/design-self-as-object.md Section 3.3, 3.10

-module(beamtalk_tuple).
-export([dispatch/3, has_method/1]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Dispatch a message to a tuple value.
-spec dispatch(atom(), list(), tuple()) -> term().
dispatch(Selector, Args, Value) when is_tuple(Value) ->
    case builtin_dispatch(Selector, Args, Value) of
        {ok, Result} -> Result;
        not_found -> does_not_understand(Selector, Args, Value)
    end.

%% @doc Check if a tuple responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method(Selector) ->
    is_builtin(Selector) orelse beamtalk_extensions:has('Tuple', Selector).

%% @doc Check if a selector is a builtin method.
-spec is_builtin(atom()) -> boolean().
is_builtin('class') -> true;
is_builtin('respondsTo') -> true;
is_builtin('perform') -> true;
is_builtin('perform:withArgs:') -> true;
is_builtin('size') -> true;
is_builtin('at:') -> true;
is_builtin('isOk') -> true;
is_builtin('isError') -> true;
is_builtin('unwrap') -> true;
is_builtin('unwrapOr:') -> true;
is_builtin('unwrapOrElse:') -> true;
is_builtin('asString') -> true;
is_builtin('instVarNames') -> true;
is_builtin('instVarAt') -> true;
is_builtin('instVarAt:put:') -> true;
is_builtin(_) -> false.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @doc Dispatch to builtin tuple methods.
-spec builtin_dispatch(atom(), list(), tuple()) -> {ok, term()} | not_found.

%% Reflection
builtin_dispatch('class', [], _X) -> {ok, 'Tuple'};
builtin_dispatch('respondsTo', [Selector], _X) when is_atom(Selector) -> 
    {ok, has_method(Selector)};

%% Dynamic message send
builtin_dispatch('perform', [TargetSelector], X) when is_atom(TargetSelector) ->
    builtin_dispatch(TargetSelector, [], X);
builtin_dispatch('perform:withArgs:', [TargetSelector, ArgList], X) 
  when is_atom(TargetSelector), is_list(ArgList) ->
    builtin_dispatch(TargetSelector, ArgList, X);
builtin_dispatch('perform:withArgs:', [_TargetSelector, ArgList], _X)
  when not is_list(ArgList) ->
    %% Type error: ArgList must be a list (consistent with actor behavior)
    Error0 = beamtalk_error:new(type_error, 'Tuple'),
    Error = beamtalk_error:with_selector(Error0, 'perform:withArgs:'),
    error(Error);


%% Size
builtin_dispatch('size', [], X) -> {ok, tuple_size(X)};

%% Access (1-based indexing)
builtin_dispatch('at:', [Idx], X) when is_integer(Idx), Idx >= 1, Idx =< tuple_size(X) ->
    {ok, element(Idx, X)};

%% Pattern matching for Erlang result types
builtin_dispatch('isOk', [], {ok, _}) -> {ok, true};
builtin_dispatch('isOk', [], _) -> {ok, false};

builtin_dispatch('isError', [], {error, _}) -> {ok, true};
builtin_dispatch('isError', [], _) -> {ok, false};

%% Unwrapping - extracts value from {ok, Value} or raises error from {error, Reason}
builtin_dispatch('unwrap', [], {ok, Value}) -> {ok, Value};
builtin_dispatch('unwrap', [], {error, Reason}) -> 
    error({unwrap_error, Reason});

%% Unwrap with default - returns Value from {ok, Value} or Default otherwise
builtin_dispatch('unwrapOr:', [Default], {ok, Value}) -> {ok, Value};
builtin_dispatch('unwrapOr:', [Default], _) -> {ok, Default};

%% Unwrap with block - returns Value from {ok, Value} or evaluates block
builtin_dispatch('unwrapOrElse:', [Block], {ok, Value}) when is_function(Block, 0) ->
    {ok, Value};
builtin_dispatch('unwrapOrElse:', [Block], _Tuple) when is_function(Block, 0) ->
    {ok, Block()};

%% Conversion
builtin_dispatch('asString', [], X) ->
    %% Convert tuple to string representation
    Elements = tuple_to_list(X),
    ElementStrs = [format_element(E) || E <- Elements],
    Joined = lists:join(<<", ">>, ElementStrs),
    Result = iolist_to_binary([<<"{">>, Joined, <<"}">>]),
    {ok, Result};

%% Instance variable reflection (BT-164)
%% Primitives are immutable and have no instance variables
builtin_dispatch('instVarNames', [], _Tuple) -> 
    {ok, []};
builtin_dispatch('instVarAt', [_Name], _Tuple) -> 
    {ok, nil};
builtin_dispatch('instVarAt:put:', [Name, _Value], _Tuple) -> 
    error(immutable_primitive_error('Tuple', Name));

%% Not a builtin method
builtin_dispatch(_, _, _) -> not_found.

%% @private
%% @doc Construct immutable_primitive error for Tuple.
-spec immutable_primitive_error(atom(), term()) -> beamtalk_error:error().
immutable_primitive_error(Class, FieldName) ->
    Error0 = beamtalk_error:new(immutable_primitive, Class),
    Error1 = beamtalk_error:with_selector(Error0, 'instVarAt:put:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Tuples are immutable. Use assignment (x := newValue) instead.">>),
    beamtalk_error:with_details(Error2, #{field => FieldName}).

%% @doc Format a single tuple element for string representation.
-spec format_element(term()) -> binary().
format_element(X) when is_binary(X) -> X;
format_element(X) when is_atom(X) -> atom_to_binary(X, utf8);
format_element(X) when is_integer(X) -> integer_to_binary(X);
format_element(X) when is_float(X) -> float_to_binary(X);
format_element(_X) -> <<"<term>">>.

%% @doc Handle doesNotUnderstand by checking extension registry.
-spec does_not_understand(atom(), list(), tuple()) -> term().
does_not_understand(Selector, Args, Value) ->
    case beamtalk_extensions:lookup('Tuple', Selector) of
        {ok, Fun, _Owner} -> 
            Fun(Args, Value);
        not_found -> 
            Error0 = beamtalk_error:new(does_not_understand, 'Tuple'),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            Error = beamtalk_error:with_hint(Error1, <<"Check spelling or use 'respondsTo:' to verify method exists">>),
            error(Error)
    end.
