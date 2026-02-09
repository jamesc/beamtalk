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
is_builtin('last') -> true;
is_builtin('at:') -> true;
is_builtin('includes:') -> true;
is_builtin('sort') -> true;
is_builtin('sort:') -> true;
is_builtin('reversed') -> true;
is_builtin('detect:') -> true;
is_builtin('detect:ifNone:') -> true;
is_builtin('do:') -> true;
is_builtin('collect:') -> true;
is_builtin('select:') -> true;
is_builtin('reject:') -> true;
is_builtin('inject:into:') -> true;
is_builtin('take:') -> true;
is_builtin('drop:') -> true;
is_builtin('flatten') -> true;
is_builtin('flatMap:') -> true;
is_builtin('count:') -> true;
is_builtin('anySatisfy:') -> true;
is_builtin('allSatisfy:') -> true;
is_builtin('zip:') -> true;
is_builtin('groupBy:') -> true;
is_builtin('partition:') -> true;
is_builtin('unique') -> true;
is_builtin('takeWhile:') -> true;
is_builtin('dropWhile:') -> true;
is_builtin('intersperse:') -> true;
is_builtin('add:') -> true;
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
builtin_dispatch('last', [], []) ->
    Error0 = beamtalk_error:new(does_not_understand, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'last'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Cannot get last element of empty list">>),
    error(Error2);
builtin_dispatch('last', [], X) -> {ok, lists:last(X)};
builtin_dispatch('at:', [N], X) when is_integer(N), N >= 1, N =< length(X) ->
    {ok, lists:nth(N, X)};
builtin_dispatch('at:', [N], _X) ->
    Error0 = beamtalk_error:new(does_not_understand, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:'),
    Hint = iolist_to_binary(io_lib:format("Index ~p is out of bounds", [N])),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    error(Error2);
builtin_dispatch('includes:', [Item], X) -> {ok, lists:member(Item, X)};
builtin_dispatch('sort', [], X) -> {ok, lists:sort(X)};
builtin_dispatch('sort:', [Block], X) when is_function(Block, 2) ->
    {ok, lists:sort(Block, X)};
builtin_dispatch('reversed', [], X) -> {ok, lists:reverse(X)};
builtin_dispatch('detect:', [Block], X) when is_function(Block, 1) ->
    case detect_helper(Block, X) of
        {ok, Found} -> {ok, Found};
        not_found ->
            Error0 = beamtalk_error:new(does_not_understand, 'Array'),
            Error1 = beamtalk_error:with_selector(Error0, 'detect:'),
            Error2 = beamtalk_error:with_hint(Error1, <<"No element matched the block">>),
            error(Error2)
    end;
builtin_dispatch('detect:ifNone:', [Block, Default], X) when is_function(Block, 1) ->
    case detect_helper(Block, X) of
        {ok, Found} -> {ok, Found};
        not_found when is_function(Default, 0) -> {ok, Default()};
        not_found -> {ok, Default}
    end;
%% Tier 2: Functional
builtin_dispatch('take:', [N], X) when is_integer(N), N >= 0 ->
    {ok, lists:sublist(X, N)};
builtin_dispatch('drop:', [N], X) when is_integer(N), N >= 0 ->
    {ok, safe_nthtail(N, X)};
builtin_dispatch('flatten', [], X) -> {ok, lists:flatten(X)};
builtin_dispatch('flatMap:', [Block], X) when is_function(Block, 1) ->
    {ok, lists:flatmap(Block, X)};
builtin_dispatch('count:', [Block], X) when is_function(Block, 1) ->
    {ok, length(lists:filter(Block, X))};
builtin_dispatch('anySatisfy:', [Block], X) when is_function(Block, 1) ->
    {ok, lists:any(Block, X)};
builtin_dispatch('allSatisfy:', [Block], X) when is_function(Block, 1) ->
    {ok, lists:all(Block, X)};
%% Tier 3: Advanced
builtin_dispatch('zip:', [Other], X) when is_list(Other) ->
    {ok, zip_to_maps(X, Other)};
builtin_dispatch('groupBy:', [Block], X) when is_function(Block, 1) ->
    {ok, group_by(Block, X)};
builtin_dispatch('partition:', [Block], X) when is_function(Block, 1) ->
    {Matching, NonMatching} = lists:partition(Block, X),
    {ok, #{<<"matching">> => Matching, <<"nonMatching">> => NonMatching}};
builtin_dispatch('unique', [], X) -> {ok, lists:usort(X)};
builtin_dispatch('takeWhile:', [Block], X) when is_function(Block, 1) ->
    {ok, lists:takewhile(Block, X)};
builtin_dispatch('dropWhile:', [Block], X) when is_function(Block, 1) ->
    {ok, lists:dropwhile(Block, X)};
builtin_dispatch('intersperse:', [Sep], X) -> {ok, intersperse(Sep, X)};
builtin_dispatch('add:', [Item], X) -> {ok, X ++ [Item]};
%% Iteration
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
detect_helper(_Block, []) -> not_found;
detect_helper(Block, [H | T]) ->
    case Block(H) of
        true -> {ok, H};
        _ -> detect_helper(Block, T)
    end.

%% @private Safe nthtail that returns [] when N >= length
safe_nthtail(0, List) -> List;
safe_nthtail(_, []) -> [];
safe_nthtail(N, [_ | T]) -> safe_nthtail(N - 1, T).

%% @private Zip two lists into a list of maps #{key => K, value => V}
zip_to_maps([], _) -> [];
zip_to_maps(_, []) -> [];
zip_to_maps([H1 | T1], [H2 | T2]) ->
    [#{<<"key">> => H1, <<"value">> => H2} | zip_to_maps(T1, T2)].

%% @private Group elements by block result into a map
group_by(Block, List) ->
    lists:foldl(fun(Item, Acc) ->
        Key = Block(Item),
        Existing = maps:get(Key, Acc, []),
        maps:put(Key, Existing ++ [Item], Acc)
    end, #{}, List).

%% @private Intersperse separator between elements
intersperse(_Sep, []) -> [];
intersperse(_Sep, [X]) -> [X];
intersperse(Sep, [H | T]) -> [H, Sep | intersperse(Sep, T)].

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
