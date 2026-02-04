%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Boolean primitive class implementation.
%%%
%%% This module provides method dispatch for Erlang booleans (true/false atoms),
%%% mapping them to the Beamtalk `Boolean` class. Supports control flow, logical
%%% operations, and extension methods via the extension registry.
%%%
%%% ## Builtin Methods
%%%
%%% | Selector | Args | Description |
%%% |----------|------|-------------|
%%% | `class`  | []   | Returns `'Boolean'` |
%%% | `respondsTo` | [Sel] | Returns true if responds to selector |
%%% | `ifTrue:` | [Block] | Evaluate block if true |
%%% | `ifFalse:` | [Block] | Evaluate block if false |
%%% | `ifTrue:ifFalse:` | [TrueBlock, FalseBlock] | Conditional evaluation |
%%% | `not`    | []   | Logical negation |
%%% | `and:`   | [Block] | Lazy AND (short-circuit) |
%%% | `or:`    | [Block] | Lazy OR (short-circuit) |
%%% | `asString` | [] | Convert to string |
%%% | `instVarNames` | [] | Returns `[]` (no instance variables) |
%%% | `instVarAt` | [Name] | Returns `nil` (no fields) |
%%% | `instVarAt:put:` | [Name, Value] | Error: immutable primitive |
%%%
%%% ## Usage Examples
%%%
%%% ```erlang
%%% %% Reflection
%%% beamtalk_boolean:dispatch('class', [], true). % => 'Boolean'
%%%
%%% %% Control flow
%%% beamtalk_boolean:dispatch('ifTrue:', [fun() -> ok end], true). % => ok
%%% beamtalk_boolean:dispatch('ifFalse:', [fun() -> ok end], false). % => ok
%%%
%%% %% Logical operations
%%% beamtalk_boolean:dispatch('not', [], true). % => false
%%% beamtalk_boolean:dispatch('and:', [fun() -> false end], true). % => false
%%%
%%% %% Conversion
%%% beamtalk_boolean:dispatch('asString', [], true). % => <<"true">>
%%% ```
%%%
%%% See: docs/internal/design-self-as-object.md Section 3.3

-module(beamtalk_boolean).
-export([dispatch/3, has_method/1]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Dispatch a message to a boolean value.
%%
%% Tries builtin methods first, then falls back to the extension registry
%% for user-defined methods. Raises does_not_understand error if method not found.
-spec dispatch(atom(), list(), boolean()) -> term().
dispatch(Selector, Args, Value) when Value =:= true; Value =:= false ->
    case builtin_dispatch(Selector, Args, Value) of
        {ok, Result} -> Result;
        not_found -> does_not_understand(Selector, Args, Value)
    end.

%% @doc Check if a boolean responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method(Selector) ->
    is_builtin(Selector) orelse beamtalk_extensions:has('Boolean', Selector).

%% @doc Check if a selector is a builtin method.
-spec is_builtin(atom()) -> boolean().
is_builtin('class') -> true;
is_builtin('respondsTo') -> true;
is_builtin('perform') -> true;
is_builtin('perform:withArgs:') -> true;
is_builtin('ifTrue:') -> true;
is_builtin('ifFalse:') -> true;
is_builtin('ifTrue:ifFalse:') -> true;
is_builtin('not') -> true;
is_builtin('and:') -> true;
is_builtin('or:') -> true;
is_builtin('asString') -> true;
is_builtin('instVarNames') -> true;
is_builtin('instVarAt') -> true;
is_builtin('instVarAt:put:') -> true;
is_builtin(_) -> false.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @doc Dispatch to builtin boolean methods.
-spec builtin_dispatch(atom(), list(), boolean()) -> {ok, term()} | not_found.

%% Reflection
builtin_dispatch('class', [], _X) -> {ok, 'Boolean'};
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
    Error0 = beamtalk_error:new(type_error, 'Boolean'),
    Error = beamtalk_error:with_selector(Error0, 'perform:withArgs:'),
    error(Error);


%% Control flow
builtin_dispatch('ifTrue:', [Block], true) when is_function(Block, 0) ->
    {ok, Block()};
builtin_dispatch('ifTrue:', [_Block], false) ->
    {ok, nil};

builtin_dispatch('ifFalse:', [Block], false) when is_function(Block, 0) ->
    {ok, Block()};
builtin_dispatch('ifFalse:', [_Block], true) ->
    {ok, nil};

builtin_dispatch('ifTrue:ifFalse:', [TrueBlock, _FalseBlock], true) when is_function(TrueBlock, 0) ->
    {ok, TrueBlock()};
builtin_dispatch('ifTrue:ifFalse:', [_TrueBlock, FalseBlock], false) when is_function(FalseBlock, 0) ->
    {ok, FalseBlock()};

%% Logical operations
builtin_dispatch('not', [], true) -> {ok, false};
builtin_dispatch('not', [], false) -> {ok, true};

builtin_dispatch('and:', [Block], true) when is_function(Block, 0) ->
    %% Lazy evaluation - only call block if receiver is true
    Result = Block(),
    case Result of
        true -> {ok, true};
        false -> {ok, false};
        _ -> {ok, false}  % Non-boolean result is treated as false
    end;
builtin_dispatch('and:', [_Block], false) ->
    %% Short-circuit - don't evaluate block
    {ok, false};

builtin_dispatch('or:', [Block], false) when is_function(Block, 0) ->
    %% Lazy evaluation - only call block if receiver is false
    Result = Block(),
    case Result of
        true -> {ok, true};
        false -> {ok, false};
        _ -> {ok, true}  % Non-boolean result is treated as true
    end;
builtin_dispatch('or:', [_Block], true) ->
    %% Short-circuit - don't evaluate block
    {ok, true};

%% Conversion
builtin_dispatch('asString', [], true) -> {ok, <<"true">>};
builtin_dispatch('asString', [], false) -> {ok, <<"false">>};

%% Instance variable reflection (BT-164)
%% Primitives are immutable and have no instance variables
builtin_dispatch('instVarNames', [], _Value) -> 
    {ok, []};
builtin_dispatch('instVarAt', [_Name], _Value) -> 
    {ok, nil};
builtin_dispatch('instVarAt:put:', [Name, _NewValue], _Value) -> 
    error(immutable_primitive_error('Boolean', Name));

%% Not a builtin method
builtin_dispatch(_, _, _) -> not_found.

%% @private
%% @doc Construct immutable_primitive error for Boolean.
-spec immutable_primitive_error(atom(), term()) -> term().
immutable_primitive_error(Class, FieldName) ->
    Error0 = beamtalk_error:new(immutable_primitive, Class),
    Error1 = beamtalk_error:with_selector(Error0, 'instVarAt:put:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Booleans are immutable. Use assignment (x := newValue) instead.">>),
    beamtalk_error:with_details(Error2, #{field => FieldName}).

%% @doc Handle doesNotUnderstand by checking extension registry.
-spec does_not_understand(atom(), list(), boolean()) -> term().
does_not_understand(Selector, Args, Value) ->
    case beamtalk_extensions:lookup('Boolean', Selector) of
        {ok, Fun, _Owner} -> 
            Fun(Args, Value);
        not_found -> 
            Error0 = beamtalk_error:new(does_not_understand, 'Boolean'),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            Error = beamtalk_error:with_hint(Error1, <<"Check spelling or use 'respondsTo:' to verify method exists">>),
            error(Error)
    end.
