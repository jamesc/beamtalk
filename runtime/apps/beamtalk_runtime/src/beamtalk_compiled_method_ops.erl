%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc CompiledMethod primitive class implementation (BT-101).
%%%
%%% This module provides method dispatch for CompiledMethod objects, enabling
%%% method introspection in Beamtalk. CompiledMethod objects are maps with
%%% `'$beamtalk_class' => 'CompiledMethod'` returned by the `>>` operator:
%%%
%%% ```beamtalk
%%% method := Counter >> #increment
%%% method selector         // => #increment
%%% method source           // => "increment => self.value := self.value + 1"
%%% method argumentCount    // => 0
%%% ```
%%%
%%% ## Builtin Methods
%%%
%%% | Selector         | Args | Description                        |
%%% |------------------|------|------------------------------------|
%%% | `selector`       | []   | Returns the method's selector atom |
%%% | `source`         | []   | Returns the method's source code   |
%%% | `argumentCount`  | []   | Returns the method's arity         |
%%% | `class`          | []   | Returns `'CompiledMethod'`         |
%%% | `printString`    | []   | Human-readable representation      |
%%% | `asString`       | []   | Same as printString                |
%%% | `respondsTo:`    | [S]  | Check if responds to selector S    |
%%%
%%% **DDD Context:** Runtime Context
-module(beamtalk_compiled_method_ops).
-export([dispatch/3, has_method/1]).

-include("beamtalk.hrl").

%% @doc Dispatch a message to a CompiledMethod map.
-spec dispatch(atom(), list(), map()) -> term().
dispatch(Selector, Args, Value) ->
    case builtin_dispatch(Selector, Args, Value) of
        {ok, Result} -> Result;
        not_found -> does_not_understand(Selector, Args, Value)
    end.

%% @doc Check if CompiledMethod responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method(Selector) ->
    is_builtin(Selector).

-spec is_builtin(atom()) -> boolean().
is_builtin('selector') -> true;
is_builtin('source') -> true;
is_builtin('argumentCount') -> true;
is_builtin('class') -> true;
is_builtin('printString') -> true;
is_builtin('asString') -> true;
is_builtin('respondsTo:') -> true;
is_builtin(_) -> false.

%%% ============================================================================
%%% Builtin Dispatch
%%% ============================================================================

-spec builtin_dispatch(atom(), list(), map()) -> {ok, term()} | not_found.

%% selector => returns the method's selector atom
builtin_dispatch('selector', [], #{'__selector__' := Sel}) ->
    {ok, Sel};

%% source => returns the method's source code binary
builtin_dispatch('source', [], #{'__source__' := Src}) ->
    {ok, Src};

%% argumentCount => returns the method's arity
builtin_dispatch('argumentCount', [], #{'__method_info__' := Info}) ->
    {ok, maps:get(arity, Info, 0)};

%% class => returns 'CompiledMethod'
builtin_dispatch('class', [], _Value) ->
    {ok, 'CompiledMethod'};

%% printString => human-readable representation
builtin_dispatch('printString', [], #{
    '__selector__' := Sel
}) ->
    {ok, iolist_to_binary(io_lib:format("a CompiledMethod(~s)", [Sel]))};

%% asString => same as printString
builtin_dispatch('asString', [], Value) ->
    builtin_dispatch('printString', [], Value);

%% respondsTo: => check if responds to a selector
builtin_dispatch('respondsTo:', [Sel], _Value) ->
    {ok, is_builtin(Sel)};

builtin_dispatch(_Selector, _Args, _Value) ->
    not_found.

%%% ============================================================================
%%% Error Handling
%%% ============================================================================

-spec does_not_understand(atom(), list(), map()) -> no_return().
does_not_understand(Selector, _Args, _Value) ->
    Error0 = beamtalk_error:new(does_not_understand, 'CompiledMethod'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1,
        <<"CompiledMethod supports: selector, source, argumentCount, class, printString, asString, respondsTo:">>),
    beamtalk_error:raise(Error2).
