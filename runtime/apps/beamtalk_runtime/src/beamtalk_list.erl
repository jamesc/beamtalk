%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Runtime dispatch for list (Array) values.
%%
%% Handles method dispatch for Erlang lists, which are exposed as
%% Array in Beamtalk. Provides basic list operations.
%%
%% BT-296: Created as part of removing hardcoded dispatch tables from codegen.
-module(beamtalk_list).

-export([dispatch/3, has_method/1]).

%% @doc Dispatch a message to a list value.
-spec dispatch(atom(), list(), list()) -> term().
dispatch(Selector, Args, Value) ->
    case builtin_dispatch(Selector, Args, Value) of
        {ok, Result} -> Result;
        not_found -> does_not_understand(Selector, Args, Value)
    end.

%% @doc Check if a list responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method(Selector) ->
    is_builtin(Selector) orelse beamtalk_extensions:has('Array', Selector).

%% Builtin methods
is_builtin('class') -> true;
is_builtin('size') -> true;
is_builtin('isEmpty') -> true;
is_builtin('first') -> true;
is_builtin('rest') -> true;
is_builtin('do:') -> true;
is_builtin('collect:') -> true;
is_builtin('select:') -> true;
is_builtin('reject:') -> true;
is_builtin('inject:into:') -> true;
is_builtin('instVarNames') -> true;
is_builtin('instVarAt') -> true;
is_builtin('instVarAt:put:') -> true;
is_builtin('respondsTo') -> true;
is_builtin(_) -> false.

builtin_dispatch('class', [], _X) -> {ok, 'Array'};
builtin_dispatch('size', [], X) -> {ok, length(X)};
builtin_dispatch('isEmpty', [], X) -> {ok, X =:= []};
builtin_dispatch('first', [], [H | _]) -> {ok, H};
builtin_dispatch('first', [], []) ->
    Error0 = beamtalk_error:new(does_not_understand, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'first'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Cannot get first element of empty list">>),
    error(Error2);
builtin_dispatch('rest', [], [_ | T]) -> {ok, T};
builtin_dispatch('rest', [], []) -> {ok, []};
builtin_dispatch('do:', [Block], X) when is_function(Block, 1) ->
    lists:foreach(Block, X),
    {ok, nil};
builtin_dispatch('collect:', [Block], X) when is_function(Block, 1) ->
    {ok, lists:map(Block, X)};
builtin_dispatch('select:', [Block], X) when is_function(Block, 1) ->
    {ok, lists:filter(Block, X)};
builtin_dispatch('reject:', [Block], X) when is_function(Block, 1) ->
    {ok, lists:filter(fun(Item) -> not Block(Item) end, X)};
builtin_dispatch('inject:into:', [Initial, Block], X) when is_function(Block, 2) ->
    {ok, lists:foldl(Block, Initial, X)};
builtin_dispatch('respondsTo', [Selector], _X) -> {ok, has_method(Selector)};
builtin_dispatch('instVarNames', [], _X) -> {ok, []};
builtin_dispatch('instVarAt', [_Name], _X) -> {ok, nil};
builtin_dispatch('instVarAt:put:', [_Name, _Value], _X) ->
    Error0 = beamtalk_error:new(immutable_primitive, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'instVarAt:put:'),
    error(Error1);
builtin_dispatch(_, _, _) -> not_found.

%% @private
does_not_understand(Selector, Args, Value) ->
    case beamtalk_extensions:lookup('Array', Selector) of
        {ok, Fun, _Owner} ->
            Fun(Args, Value);
        not_found ->
            Error0 = beamtalk_error:new(does_not_understand, 'Array'),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            Error2 = beamtalk_error:with_hint(Error1,
                <<"Check spelling or use 'respondsTo:' to verify method exists">>),
            error(Error2)
    end.
