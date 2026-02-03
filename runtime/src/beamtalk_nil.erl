%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Nil primitive class implementation.
%%%
%%% This module provides method dispatch for Erlang nil atom,
%%% mapping it to the Beamtalk `UndefinedObject` class. Supports nil-aware
%%% control flow and extension methods via the extension registry.
%%%
%%% ## Builtin Methods
%%%
%%% | Selector | Args | Description |
%%% |----------|------|-------------|
%%% | `class`  | []   | Returns `'UndefinedObject'` |
%%% | `respondsTo` | [Sel] | Returns true if responds to selector |
%%% | `isNil`  | []   | Returns `true` |
%%% | `ifNil:` | [Block] | Evaluate block |
%%% | `ifNotNil:` | [Block] | Return nil (no evaluation) |
%%% | `ifNil:ifNotNil:` | [NilBlock, NotNilBlock] | Evaluate NilBlock |
%%% | `asString` | [] | Returns `<<"nil">>` |
%%%
%%% ## Usage Examples
%%%
%%% ```erlang
%%% %% Reflection
%%% beamtalk_nil:dispatch('class', [], nil). % => 'UndefinedObject'
%%%
%%% %% Nil checking
%%% beamtalk_nil:dispatch('isNil', [], nil). % => true
%%%
%%% %% Control flow
%%% beamtalk_nil:dispatch('ifNil:', [fun() -> default end], nil). % => default
%%% beamtalk_nil:dispatch('ifNotNil:', [fun(X) -> X + 1 end], nil). % => nil
%%%
%%% %% Conversion
%%% beamtalk_nil:dispatch('asString', [], nil). % => <<"nil">>
%%% ```
%%%
%%% See: docs/internal/design-self-as-object.md Section 3.3

-module(beamtalk_nil).
-export([dispatch/3, has_method/1]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Dispatch a message to nil value.
-spec dispatch(atom(), list(), nil) -> term().
dispatch(Selector, Args, nil) ->
    case builtin_dispatch(Selector, Args, nil) of
        {ok, Result} -> Result;
        not_found -> does_not_understand(Selector, Args, nil)
    end.

%% @doc Check if nil responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method(Selector) ->
    is_builtin(Selector) orelse beamtalk_extensions:has('UndefinedObject', Selector).

%% @doc Check if a selector is a builtin method.
-spec is_builtin(atom()) -> boolean().
is_builtin('class') -> true;
is_builtin('respondsTo') -> true;
is_builtin('perform') -> true;
is_builtin('perform:withArgs:') -> true;
is_builtin('isNil') -> true;
is_builtin('ifNil:') -> true;
is_builtin('ifNotNil:') -> true;
is_builtin('ifNil:ifNotNil:') -> true;
is_builtin('asString') -> true;
is_builtin(_) -> false.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @doc Dispatch to builtin nil methods.
-spec builtin_dispatch(atom(), list(), nil) -> {ok, term()} | not_found.

%% Reflection
builtin_dispatch('class', [], nil) -> {ok, 'UndefinedObject'};
builtin_dispatch('respondsTo', [Selector], nil) when is_atom(Selector) -> 
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


%% Nil checking
builtin_dispatch('isNil', [], nil) -> {ok, true};

%% Control flow
builtin_dispatch('ifNil:', [Block], nil) when is_function(Block, 0) ->
    {ok, Block()};

builtin_dispatch('ifNotNil:', [_Block], nil) ->
    %% nil does not evaluate the notNil block
    {ok, nil};

builtin_dispatch('ifNil:ifNotNil:', [NilBlock, _NotNilBlock], nil) when is_function(NilBlock, 0) ->
    %% nil evaluates the nil block
    {ok, NilBlock()};

%% Conversion
builtin_dispatch('asString', [], nil) -> {ok, <<"nil">>};

%% Not a builtin method
builtin_dispatch(_, _, _) -> not_found.

%% @doc Handle doesNotUnderstand by checking extension registry.
-spec does_not_understand(atom(), list(), nil) -> term().
does_not_understand(Selector, Args, nil) ->
    case beamtalk_extensions:lookup('UndefinedObject', Selector) of
        {ok, Fun, _Owner} -> 
            Fun(Args, nil);
        not_found -> 
            error({does_not_understand, 'UndefinedObject', Selector, length(Args)})
    end.
