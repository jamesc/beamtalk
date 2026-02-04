%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Block primitive class implementation.
%%%
%%% This module provides method dispatch for Erlang functions (closures),
%%% mapping them to the Beamtalk `Block` class. Blocks are first-class values
%%% that can be evaluated with arguments.
%%%
%%% ## Builtin Methods
%%%
%%% | Selector | Args | Description |
%%% |----------|------|-------------|
%%% | `class`  | []   | Returns `'Block'` |
%%% | `respondsTo` | [Sel] | Returns true if responds to selector |
%%% | `value`  | []   | Evaluate block with no args |
%%% | `value:` | [Arg] | Evaluate block with 1 arg |
%%% | `value:value:` | [Arg1, Arg2] | Evaluate block with 2 args |
%%% | `arity`  | []   | Number of parameters |
%%% | `asString` | [] | Returns `<<"<Block>">>` |
%%% | `instVarNames` | [] | Returns `[]` (no instance variables) |
%%% | `instVarAt:` | [Name] | Returns `nil` (no fields) |
%%% | `instVarAt:put:` | [Name, Value] | Error: immutable primitive |
%%%
%%% ## Usage Examples
%%%
%%% ```erlang
%%% %% Reflection
%%% beamtalk_block:dispatch('class', [], fun() -> ok end). % => 'Block'
%%%
%%% %% Evaluation
%%% beamtalk_block:dispatch('value', [], fun() -> 42 end). % => 42
%%% beamtalk_block:dispatch('value:', [10], fun(X) -> X + 1 end). % => 11
%%%
%%% %% Arity
%%% beamtalk_block:dispatch('arity', [], fun() -> ok end). % => 0
%%% beamtalk_block:dispatch('arity', [], fun(X) -> X end). % => 1
%%%
%%% %% Conversion
%%% beamtalk_block:dispatch('asString', [], fun() -> ok end). % => <<"<Block>">>
%%% ```
%%%
%%% See: docs/internal/design-self-as-object.md Section 3.3

-module(beamtalk_block).
-export([dispatch/3, has_method/1]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Dispatch a message to a block (function) value.
-spec dispatch(atom(), list(), function()) -> term().
dispatch(Selector, Args, Value) when is_function(Value) ->
    case builtin_dispatch(Selector, Args, Value) of
        {ok, Result} -> Result;
        not_found -> does_not_understand(Selector, Args, Value)
    end.

%% @doc Check if a block responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method(Selector) ->
    is_builtin(Selector) orelse beamtalk_extensions:has('Block', Selector).

%% @doc Check if a selector is a builtin method.
-spec is_builtin(atom()) -> boolean().
is_builtin('class') -> true;
is_builtin('respondsTo') -> true;
is_builtin('perform') -> true;
is_builtin('perform:withArgs:') -> true;
is_builtin('value') -> true;
is_builtin('value:') -> true;
is_builtin('value:value:') -> true;
is_builtin('arity') -> true;
is_builtin('asString') -> true;
is_builtin('instVarNames') -> true;
is_builtin('instVarAt') -> true;
is_builtin('instVarAt:put:') -> true;
is_builtin(_) -> false.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @doc Dispatch to builtin block methods.
-spec builtin_dispatch(atom(), list(), function()) -> {ok, term()} | not_found.

%% Reflection
builtin_dispatch('class', [], _X) -> {ok, 'Block'};
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
    error({type_error, list, ArgList});


%% Evaluation
builtin_dispatch('value', [], Block) when is_function(Block, 0) ->
    {ok, Block()};

builtin_dispatch('value:', [Arg], Block) when is_function(Block, 1) ->
    {ok, Block(Arg)};

builtin_dispatch('value:value:', [Arg1, Arg2], Block) when is_function(Block, 2) ->
    {ok, Block(Arg1, Arg2)};

%% Arity
builtin_dispatch('arity', [], Block) when is_function(Block) ->
    {arity, Arity} = erlang:fun_info(Block, arity),
    {ok, Arity};

%% Conversion
builtin_dispatch('asString', [], _Block) -> {ok, <<"<Block>">>};

%% Instance variable reflection (BT-164)
%% Primitives are immutable and have no instance variables
builtin_dispatch('instVarNames', [], _Block) -> 
    {ok, []};
builtin_dispatch('instVarAt', [_Name], _Block) -> 
    {ok, nil};
builtin_dispatch('instVarAt:put:', [Name, _Value], _Block) -> 
    %% Primitives cannot be mutated
    Error0 = beamtalk_error:new(immutable_primitive, 'Block'),
    Error1 = beamtalk_error:with_selector(Error0, 'instVarAt:put:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Blocks are immutable. Use assignment (x := newValue) instead.">>),
    Error = beamtalk_error:with_details(Error2, #{field => Name}),
    error(Error);

%% Not a builtin method
builtin_dispatch(_, _, _) -> not_found.

%% @doc Handle doesNotUnderstand by checking extension registry.
-spec does_not_understand(atom(), list(), function()) -> term().
does_not_understand(Selector, Args, Value) ->
    case beamtalk_extensions:lookup('Block', Selector) of
        {ok, Fun, _Owner} -> 
            Fun(Args, Value);
        not_found -> 
            error({does_not_understand, 'Block', Selector, length(Args)})
    end.
