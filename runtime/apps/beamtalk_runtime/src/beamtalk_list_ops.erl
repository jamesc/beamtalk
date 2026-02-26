%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Runtime helper for complex List operations.
%%
%%% **DDD Context:** Runtime Context — Domain Service
%%
%% Provides implementations for List methods that require custom logic
%% beyond simple BIF calls (bounds checking, error formatting, iteration).
%%
%% BT-419: Created as part of Array→List rename and compiled stdlib migration.
-module(beamtalk_list_ops).

-export([
    at/2,
    detect/2,
    detect_if_none/3,
    do/2,
    reject/2,
    zip/2,
    group_by/2,
    partition/2,
    intersperse/2,
    take/2,
    drop/2,
    sort_with/2,
    from_to/3
]).

-include("beamtalk.hrl").

%% @doc Access element at 1-based index with bounds checking.
-spec at(list(), term()) -> term().
at(List, N) when is_list(List), not is_integer(N) ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:'),
    Hint = iolist_to_binary(
        io_lib:format("Index must be a positive integer, got ~s", [describe_value(N)])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2);
at(List, N) when is_list(List), is_integer(N), N =< 0 ->
    Error0 = beamtalk_error:new(does_not_understand, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:'),
    Hint = iolist_to_binary(
        io_lib:format("Index ~p is out of bounds (must be >= 1)", [N])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2);
at(List, N) when is_list(List), is_integer(N), N >= 1 ->
    try
        lists:nth(N, List)
    catch
        error:badarg ->
            Error0 = beamtalk_error:new(does_not_understand, 'List'),
            Error1 = beamtalk_error:with_selector(Error0, 'at:'),
            Hint = iolist_to_binary(
                io_lib:format("Index ~p is out of bounds", [N])
            ),
            Error2 = beamtalk_error:with_hint(Error1, Hint),
            beamtalk_error:raise(Error2);
        error:function_clause ->
            Error0 = beamtalk_error:new(does_not_understand, 'List'),
            Error1 = beamtalk_error:with_selector(Error0, 'at:'),
            Hint = iolist_to_binary(
                io_lib:format("Index ~p is out of bounds", [N])
            ),
            Error2 = beamtalk_error:with_hint(Error1, Hint),
            beamtalk_error:raise(Error2)
    end.

%% @doc Find first element matching block, error if not found.
-spec detect(list(), function()) -> term().
detect(List, Block) when is_list(List), is_function(Block, 1) ->
    case detect_helper(Block, List) of
        {ok, Found} ->
            Found;
        not_found ->
            Error0 = beamtalk_error:new(does_not_understand, 'List'),
            Error1 = beamtalk_error:with_selector(Error0, 'detect:'),
            Error2 = beamtalk_error:with_hint(Error1, <<"No element matched the block">>),
            beamtalk_error:raise(Error2)
    end;
detect(List, Block) when is_list(List) ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'detect:'),
    Hint = iolist_to_binary(
        io_lib:format("Block must be a unary function (arity 1), got ~s", [describe_value(Block)])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).

%% @doc Find first element matching block, return default if not found.
-spec detect_if_none(list(), function(), term()) -> term().
detect_if_none(List, Block, Default) when is_list(List), is_function(Block, 1) ->
    case detect_helper(Block, List) of
        {ok, Found} -> Found;
        not_found when is_function(Default, 0) -> Default();
        not_found -> Default
    end;
detect_if_none(List, Block, _Default) when is_list(List) ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'detect:ifNone:'),
    Hint = iolist_to_binary(
        io_lib:format("Block must be a unary function (arity 1), got ~s", [describe_value(Block)])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).

%% @doc Iterate over elements with side effects.
-spec do(list(), function()) -> 'nil'.
do(List, Block) when is_list(List), is_function(Block, 1) ->
    lists:foreach(Block, List),
    nil;
do(List, Block) when is_list(List) ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'do:'),
    Hint = iolist_to_binary(
        io_lib:format("Block must be a unary function (arity 1), got ~s", [describe_value(Block)])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).

%% @doc Filter out elements matching block.
-spec reject(list(), function()) -> list().
reject(List, Block) when is_list(List), is_function(Block, 1) ->
    lists:filter(fun(Item) -> not Block(Item) end, List);
reject(List, Block) when is_list(List) ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'reject:'),
    Hint = iolist_to_binary(
        io_lib:format("Block must be a unary function (arity 1), got ~s", [describe_value(Block)])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).

%% @doc Take N elements with validation.
-spec take(list(), term()) -> list().
take(List, N) when is_list(List), not is_integer(N) ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'take:'),
    Hint = iolist_to_binary(
        io_lib:format("Argument must be a non-negative integer, got ~s", [describe_value(N)])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2);
take(List, N) when is_list(List), is_integer(N), N < 0 ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'take:'),
    Hint = iolist_to_binary(
        io_lib:format("Argument must be a non-negative integer, got: ~p", [N])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2);
take(List, N) when is_list(List), is_integer(N), N >= 0 ->
    lists:sublist(List, N).

%% @doc Drop N elements with validation.
-spec drop(list(), term()) -> list().
drop(List, N) when is_list(List), not is_integer(N) ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'drop:'),
    Hint = iolist_to_binary(
        io_lib:format("Argument must be a non-negative integer, got ~s", [describe_value(N)])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2);
drop(List, N) when is_list(List), is_integer(N), N < 0 ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'drop:'),
    Hint = iolist_to_binary(
        io_lib:format("Argument must be a non-negative integer, got: ~p", [N])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2);
drop(List, N) when is_list(List), is_integer(N), N >= 0 ->
    safe_nthtail(N, List).

%% @doc Sort with comparator block, with validation.
-spec sort_with(list(), term()) -> list().
sort_with(List, Block) when is_list(List), is_function(Block, 2) ->
    lists:sort(Block, List);
sort_with(_List, Block) ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'sort:'),
    Hint = iolist_to_binary(
        io_lib:format("sort: expects a 2-argument block, got ~s", [describe_value(Block)])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).

%% @doc Zip two lists into a list of maps #{key => K, value => V}.
-spec zip(list(), list()) -> list().
zip(List, Other) when is_list(List), is_list(Other) ->
    zip_to_maps(List, Other);
zip(List, Other) when is_list(List) ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'zip:'),
    Hint = iolist_to_binary(
        io_lib:format("zip: expects a List as argument, got ~s", [describe_value(Other)])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).

%% @doc Group elements by block result into a map.
-spec group_by(list(), function()) -> map().
group_by(List, Block) when is_list(List), is_function(Block, 1) ->
    Map0 = lists:foldl(
        fun(Item, Acc) ->
            Key = Block(Item),
            Existing = maps:get(Key, Acc, []),
            maps:put(Key, [Item | Existing], Acc)
        end,
        #{},
        List
    ),
    maps:map(fun(_Key, Values) -> lists:reverse(Values) end, Map0);
group_by(_List, Block) ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'groupBy:'),
    Hint = iolist_to_binary(
        io_lib:format("groupBy: expects a 1-argument block, got ~s", [describe_value(Block)])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).

%% @doc Partition list into matching and non-matching.
-spec partition(list(), function()) -> map().
partition(List, Block) when is_list(List), is_function(Block, 1) ->
    {Matching, NonMatching} = lists:partition(Block, List),
    #{<<"matching">> => Matching, <<"nonMatching">> => NonMatching};
partition(_List, Block) ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'partition:'),
    Hint = iolist_to_binary(
        io_lib:format("partition: expects a 1-argument block, got ~s", [describe_value(Block)])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).

%% @doc Intersperse separator between elements.
-spec intersperse(list(), term()) -> list().
intersperse([], _Sep) -> [];
intersperse([X], _Sep) -> [X];
intersperse([H | T], Sep) -> [H, Sep | intersperse(T, Sep)].

%% @doc Extract subsequence from Start to End (1-based, inclusive).
-spec from_to(list(), term(), term()) -> list().
from_to(List, Start, End) when
    is_list(List),
    is_integer(Start),
    is_integer(End),
    Start >= 1,
    End >= Start
->
    Len = End - Start + 1,
    lists:sublist(safe_nthtail(Start - 1, List), Len);
from_to(List, Start, End) when
    is_list(List),
    is_integer(Start),
    is_integer(End),
    Start >= 1,
    End < Start
->
    [];
from_to(List, Start, _End) when is_list(List), not is_integer(Start) ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'from:to:'),
    Hint = iolist_to_binary(
        io_lib:format("Start index must be a positive integer, got ~s", [describe_value(Start)])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2);
from_to(List, _Start, End) when is_list(List), not is_integer(End) ->
    Error0 = beamtalk_error:new(type_error, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'from:to:'),
    Hint = iolist_to_binary(
        io_lib:format("End index must be a positive integer, got ~s", [describe_value(End)])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2);
from_to(List, Start, _End) when is_list(List), is_integer(Start), Start < 1 ->
    Error0 = beamtalk_error:new(does_not_understand, 'List'),
    Error1 = beamtalk_error:with_selector(Error0, 'from:to:'),
    Hint = iolist_to_binary(
        io_lib:format("Start index ~p is out of bounds (must be >= 1)", [Start])
    ),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).

%% Internal helpers

detect_helper(_Block, []) ->
    not_found;
detect_helper(Block, [H | T]) ->
    case Block(H) of
        true -> {ok, H};
        _ -> detect_helper(Block, T)
    end.

safe_nthtail(0, List) -> List;
safe_nthtail(_, []) -> [];
safe_nthtail(N, [_ | T]) -> safe_nthtail(N - 1, T).

zip_to_maps([], _) -> [];
zip_to_maps(_, []) -> [];
zip_to_maps([H1 | T1], [H2 | T2]) -> [#{<<"key">> => H1, <<"value">> => H2} | zip_to_maps(T1, T2)].

%% @doc Return a human-readable description of a value for error messages.
%%
%% For blocks, includes the arity so wrong-arity errors are clear.
%% For other values, shows the Beamtalk class name.
-spec describe_value(term()) -> binary().
describe_value(V) when is_function(V) ->
    {arity, A} = erlang:fun_info(V, arity),
    iolist_to_binary(io_lib:format("a ~p-argument block", [A]));
describe_value(V) ->
    ClassName = beamtalk_primitive:class_of(V),
    <<"a ", (atom_to_binary(ClassName))/binary>>.
