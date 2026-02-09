%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Array/List helper operations for compiled stdlib.
%%%
%%% This module provides implementations for Array primitive methods
%%% that require more than simple Erlang BIF calls. Called by compiled
%%% stdlib Array module via `@primitive` codegen.
%%%
%%% BT-419: Created during Array migration from Option B to Option A.
%%% Analogous to `beamtalk_string_ops.erl` for String methods.

-module(beamtalk_list_ops).
-export([
    at/2,
    detect/2,
    detect_if_none/3,
    do/2,
    reject/2,
    drop/2,
    zip/2,
    group_by/2,
    partition/2,
    intersperse/2
]).

%% @doc 1-based list element access with error handling.
-spec at(list(), integer()) -> term().
at(List, N) when is_list(List), is_integer(N), N >= 1 ->
    try
        lists:nth(N, List)
    catch
        error:badarg ->
            Error0 = beamtalk_error:new(does_not_understand, 'Array'),
            Error1 = beamtalk_error:with_selector(Error0, 'at:'),
            Hint = iolist_to_binary(io_lib:format("Index ~p is out of bounds", [N])),
            Error2 = beamtalk_error:with_hint(Error1, Hint),
            error(Error2);
        error:function_clause ->
            Error0 = beamtalk_error:new(does_not_understand, 'Array'),
            Error1 = beamtalk_error:with_selector(Error0, 'at:'),
            Hint = iolist_to_binary(io_lib:format("Index ~p is out of bounds", [N])),
            Error2 = beamtalk_error:with_hint(Error1, Hint),
            error(Error2)
    end.

%% @doc Find first element matching block predicate.
-spec detect(list(), function()) -> term().
detect(List, Block) when is_list(List), is_function(Block, 1) ->
    case detect_helper(Block, List) of
        {ok, Found} -> Found;
        not_found ->
            Error0 = beamtalk_error:new(does_not_understand, 'Array'),
            Error1 = beamtalk_error:with_selector(Error0, 'detect:'),
            Error2 = beamtalk_error:with_hint(Error1, <<"No element matched the block">>),
            error(Error2)
    end.

%% @doc Find first element matching block predicate, with default.
-spec detect_if_none(list(), function(), term()) -> term().
detect_if_none(List, Block, Default) when is_list(List), is_function(Block, 1) ->
    case detect_helper(Block, List) of
        {ok, Found} -> Found;
        not_found when is_function(Default, 0) -> Default();
        not_found -> Default
    end.

%% @private
detect_helper(_Block, []) -> not_found;
detect_helper(Block, [H | T]) ->
    case Block(H) of
        true -> {ok, H};
        _ -> detect_helper(Block, T)
    end.

%% @doc Evaluate block for each element (side effects only, returns nil).
-spec do(list(), function()) -> nil.
do(List, Block) when is_list(List), is_function(Block, 1) ->
    lists:foreach(Block, List),
    nil.

%% @doc Filter elements that do NOT match block predicate.
-spec reject(list(), function()) -> list().
reject(List, Block) when is_list(List), is_function(Block, 1) ->
    lists:filter(fun(Item) -> not Block(Item) end, List).

%% @doc Drop first N elements, safe for N >= length.
-spec drop(list(), integer()) -> list().
drop(List, N) when is_list(List), is_integer(N), N >= 0 ->
    safe_nthtail(N, List).

%% @private Safe nthtail that returns [] when N >= length
safe_nthtail(0, List) -> List;
safe_nthtail(_, []) -> [];
safe_nthtail(N, [_ | T]) -> safe_nthtail(N - 1, T).

%% @doc Zip two lists into a list of maps #{key => K, value => V}.
-spec zip(list(), list()) -> list().
zip(List1, List2) when is_list(List1), is_list(List2) ->
    zip_to_maps(List1, List2).

%% @private
zip_to_maps([], _) -> [];
zip_to_maps(_, []) -> [];
zip_to_maps([H1 | T1], [H2 | T2]) ->
    [#{<<"key">> => H1, <<"value">> => H2} | zip_to_maps(T1, T2)].

%% @doc Group elements by block result into a map.
-spec group_by(list(), function()) -> map().
group_by(List, Block) when is_list(List), is_function(Block, 1) ->
    Map0 = lists:foldl(fun(Item, Acc) ->
        Key = Block(Item),
        Existing = maps:get(Key, Acc, []),
        maps:put(Key, [Item | Existing], Acc)
    end, #{}, List),
    maps:map(fun(_Key, Values) -> lists:reverse(Values) end, Map0).

%% @doc Partition elements into matching and non-matching groups.
-spec partition(list(), function()) -> map().
partition(List, Block) when is_list(List), is_function(Block, 1) ->
    {Matching, NonMatching} = lists:partition(Block, List),
    #{<<"matching">> => Matching, <<"nonMatching">> => NonMatching}.

%% @doc Intersperse separator between elements.
-spec intersperse(list(), term()) -> list().
intersperse([], _Sep) -> [];
intersperse([X], _Sep) -> [X];
intersperse([H | T], Sep) -> [H, Sep | intersperse(T, Sep)].
