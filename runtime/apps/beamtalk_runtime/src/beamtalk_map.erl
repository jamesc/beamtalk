%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Runtime dispatch for map (Dictionary) values.
%%
%% Handles method dispatch for Erlang maps, which are exposed as
%% Dictionary in Beamtalk. Provides basic map operations.
%%
%% BT-296: Created as part of removing hardcoded dispatch tables from codegen.
-module(beamtalk_map).

-export([dispatch/3, has_method/1]).

%% @doc Dispatch a message to a map value.
-spec dispatch(atom(), list(), map()) -> term().
dispatch(Selector, Args, Value) ->
    case builtin_dispatch(Selector, Args, Value) of
        {ok, Result} -> Result;
        not_found -> does_not_understand(Selector, Args, Value)
    end.

%% @doc Check if a map responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method(Selector) ->
    is_builtin(Selector) orelse beamtalk_extensions:has('Dictionary', Selector).

%% Builtin methods
is_builtin('class') -> true;
is_builtin('keys') -> true;
is_builtin('values') -> true;
is_builtin('size') -> true;
is_builtin('at:') -> true;
is_builtin('at:ifAbsent:') -> true;
is_builtin('at:put:') -> true;
is_builtin('includesKey:') -> true;
is_builtin('removeKey:') -> true;
is_builtin('merge:') -> true;
is_builtin('keysAndValuesDo:') -> true;
is_builtin('instVarNames') -> true;
is_builtin('instVarAt') -> true;
is_builtin('instVarAt:put:') -> true;
is_builtin('respondsTo') -> true;
is_builtin(_) -> false.

builtin_dispatch('class', [], _X) -> {ok, 'Dictionary'};
builtin_dispatch('keys', [], X) -> {ok, maps:keys(X)};
builtin_dispatch('values', [], X) -> {ok, maps:values(X)};
builtin_dispatch('size', [], X) -> {ok, maps:size(X)};
builtin_dispatch('at:', [Key], X) -> {ok, maps:get(Key, X)};
builtin_dispatch('at:ifAbsent:', [Key, Block], X) when is_function(Block, 0) ->
    case maps:find(Key, X) of
        {ok, Value} -> {ok, Value};
        error -> {ok, Block()}
    end;
builtin_dispatch('at:put:', [Key, Value], X) -> {ok, maps:put(Key, Value, X)};
builtin_dispatch('includesKey:', [Key], X) -> {ok, maps:is_key(Key, X)};
builtin_dispatch('removeKey:', [Key], X) -> {ok, maps:remove(Key, X)};
builtin_dispatch('merge:', [Other], X) -> {ok, maps:merge(X, Other)};
builtin_dispatch('keysAndValuesDo:', [Fun], X) when is_function(Fun, 2) ->
    maps:foreach(Fun, X),
    {ok, nil};
builtin_dispatch('respondsTo', [Selector], _X) -> {ok, has_method(Selector)};
builtin_dispatch('instVarNames', [], _X) -> {ok, []};
builtin_dispatch('instVarAt', [_Name], _X) -> {ok, nil};
builtin_dispatch('instVarAt:put:', [_Name, _Value], _X) ->
    Error0 = beamtalk_error:new(immutable_primitive, 'Dictionary'),
    Error1 = beamtalk_error:with_selector(Error0, 'instVarAt:put:'),
    error(Error1);
builtin_dispatch(_, _, _) -> not_found.

%% @private
does_not_understand(Selector, Args, Value) ->
    case beamtalk_extensions:lookup('Dictionary', Selector) of
        {ok, Fun, _Owner} ->
            Fun(Args, Value);
        not_found ->
            Error0 = beamtalk_error:new(does_not_understand, 'Dictionary'),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            Error2 = beamtalk_error:with_hint(Error1,
                <<"Check spelling or use 'respondsTo:' to verify method exists">>),
            error(Error2)
    end.
