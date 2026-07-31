%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_list).

%%% **DDD Context:** Object System Context

-moduledoc """
Runtime helper for complex List operations.

Provides implementations for List methods that require custom logic
beyond simple BIF calls (bounds checking, error formatting, iteration).

BT-419: Created as part of Array→List rename and compiled stdlib migration.
""".

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
    from_to/3,
    reverse_group_values/1,
    unique/1,
    sorted_strict_unique/1,
    strict_member_sorted/2
]).

-doc """
Access element at 1-based index with bounds checking.

BT-3021: three distinct failures, three distinct kinds — a non-integer index is
a `type_error`, indexing an empty List is `empty_collection`, and any other
out-of-range index is `index_out_of_bounds` (matching `Array`/`String` `at:`).
None of them is `does_not_understand`: the List understands `at:` perfectly
well, so reporting a dispatch failure would send the reader hunting for a typo.
""".
-spec at(list(), term()) -> term().
at(List, N) when is_list(List), not is_integer(N) ->
    Hint = iolist_to_binary(
        io_lib:format("Index must be a positive integer, got ~s", [describe_value(N)])
    ),
    beamtalk_error:raise_type_error('List', 'at:', Hint);
at(List, N) when is_list(List), is_integer(N), N =< 0 ->
    %% An index below 1 is malformed whether or not the List is empty, so this
    %% is checked before the emptiness clause.
    raise_out_of_bounds(N, <<" (must be >= 1)">>);
at([], _N) ->
    raise_empty('at:');
at(List, N) when is_list(List), is_integer(N), N >= 1 ->
    try
        lists:nth(N, List)
    catch
        error:badarg -> raise_out_of_bounds(N, <<>>);
        error:function_clause -> raise_out_of_bounds(N, <<>>)
    end.

-doc "Raise `empty_collection` for an accessor called on an empty List.".
-spec raise_empty(atom()) -> no_return().
raise_empty(Selector) ->
    Hint = <<"List is empty; guard with `isEmpty` before indexing">>,
    beamtalk_error:raise(beamtalk_error:new(empty_collection, 'List', Selector, Hint)).

-doc "Raise `index_out_of_bounds` for an in-range-shaped but out-of-range index.".
-spec raise_out_of_bounds(integer(), binary()) -> no_return().
raise_out_of_bounds(N, Suffix) ->
    Hint = iolist_to_binary(
        io_lib:format("Index ~p is out of bounds~s", [N, Suffix])
    ),
    beamtalk_error:raise(beamtalk_error:new(index_out_of_bounds, 'List', 'at:', Hint)).

-doc """
Find first element matching block, error if not found.

BT-3025: raises `not_found` when no element matches. It used to raise
`does_not_understand`, which claimed the List had no `detect:` at all and sent
readers hunting for a typo. Use `detect:ifNone:` for the non-raising form.
""".
-spec detect(list(), function()) -> term().
detect(List, Block) when is_list(List), is_function(Block, 1) ->
    case detect_helper(Block, List) of
        {ok, Found} ->
            Found;
        not_found ->
            Hint = <<"No element matched the block; use `detect:ifNone:` to supply a default">>,
            beamtalk_error:raise(
                beamtalk_error:new(not_found, 'List', 'detect:', Hint)
            )
    end;
detect(List, Block) when is_list(List) ->
    Hint = iolist_to_binary(
        io_lib:format("Block must be a unary function (arity 1), got ~s", [describe_value(Block)])
    ),
    beamtalk_error:raise_type_error('List', 'detect:', Hint).

-doc "Find first element matching block, return default if not found.".
-spec detect_if_none(list(), function(), term()) -> term().
detect_if_none(List, Block, Default) when is_list(List), is_function(Block, 1) ->
    case detect_helper(Block, List) of
        {ok, Found} -> Found;
        not_found when is_function(Default, 0) -> Default();
        not_found -> Default
    end;
detect_if_none(List, Block, _Default) when is_list(List) ->
    Hint = iolist_to_binary(
        io_lib:format("Block must be a unary function (arity 1), got ~s", [describe_value(Block)])
    ),
    beamtalk_error:raise_type_error('List', 'detect:ifNone:', Hint).

-doc "Iterate over elements with side effects.".
-spec do(list(), function()) -> 'nil'.
do(List, Block) when is_list(List), is_function(Block, 1) ->
    lists:foreach(Block, List),
    nil;
do(List, Block) when is_list(List) ->
    Hint = iolist_to_binary(
        io_lib:format("Block must be a unary function (arity 1), got ~s", [describe_value(Block)])
    ),
    beamtalk_error:raise_type_error('List', 'do:', Hint).

-doc "Filter out elements matching block.".
-spec reject(list(), function()) -> list().
reject(List, Block) when is_list(List), is_function(Block, 1) ->
    lists:filter(fun(Item) -> not Block(Item) end, List);
reject(List, Block) when is_list(List) ->
    Hint = iolist_to_binary(
        io_lib:format("Block must be a unary function (arity 1), got ~s", [describe_value(Block)])
    ),
    beamtalk_error:raise_type_error('List', 'reject:', Hint).

-doc "Take N elements with validation.".
-spec take(list(), term()) -> list().
take(List, N) when is_list(List), not is_integer(N) ->
    Hint = iolist_to_binary(
        io_lib:format("Argument must be a non-negative integer, got ~s", [describe_value(N)])
    ),
    beamtalk_error:raise_type_error('List', 'take:', Hint);
take(List, N) when is_list(List), is_integer(N), N < 0 ->
    Hint = iolist_to_binary(
        io_lib:format("Argument must be a non-negative integer, got: ~p", [N])
    ),
    beamtalk_error:raise_type_error('List', 'take:', Hint);
take(List, N) when is_list(List), is_integer(N), N >= 0 ->
    lists:sublist(List, N).

-doc "Drop N elements with validation.".
-spec drop(list(), term()) -> list().
drop(List, N) when is_list(List), not is_integer(N) ->
    Hint = iolist_to_binary(
        io_lib:format("Argument must be a non-negative integer, got ~s", [describe_value(N)])
    ),
    beamtalk_error:raise_type_error('List', 'drop:', Hint);
drop(List, N) when is_list(List), is_integer(N), N < 0 ->
    Hint = iolist_to_binary(
        io_lib:format("Argument must be a non-negative integer, got: ~p", [N])
    ),
    beamtalk_error:raise_type_error('List', 'drop:', Hint);
drop(List, N) when is_list(List), is_integer(N), N >= 0 ->
    safe_nthtail(N, List).

-doc "Sort with comparator block, with validation.".
-spec sort_with(list(), term()) -> list().
sort_with(List, Block) when is_list(List), is_function(Block, 2) ->
    lists:sort(Block, List);
sort_with(_List, Block) ->
    Hint = iolist_to_binary(
        io_lib:format("sort: expects a 2-argument block, got ~s", [describe_value(Block)])
    ),
    beamtalk_error:raise_type_error('List', 'sort:', Hint).

-doc "Zip two lists into a list of 2-element List pairs [Elem1, Elem2].".
-spec zip(list(), list()) -> list().
zip(List, Other) when is_list(List), is_list(Other) ->
    zip_to_pairs(List, Other);
zip(List, Other) when is_list(List) ->
    Hint = iolist_to_binary(
        io_lib:format("zip: expects a List as argument, got ~s", [describe_value(Other)])
    ),
    beamtalk_error:raise_type_error('List', 'zip:', Hint).

-doc "Group elements by block result into a map.".
-spec group_by(list(), function()) -> map().
group_by(List, Block) when is_list(List), is_function(Block, 1) ->
    Map0 = lists:foldl(
        fun(Item, Acc) ->
            Key = Block(Item),
            Existing = maps:get(Key, Acc, []),
            Acc#{Key => [Item | Existing]}
        end,
        #{},
        List
    ),
    maps:map(fun(_Key, Values) -> lists:reverse(Values) end, Map0);
group_by(_List, Block) ->
    Hint = iolist_to_binary(
        io_lib:format("groupBy: expects a 1-argument block, got ~s", [describe_value(Block)])
    ),
    beamtalk_error:raise_type_error('List', 'groupBy:', Hint).

-doc "Partition list into matching and non-matching.".
-spec partition(list(), function()) -> map().
partition(List, Block) when is_list(List), is_function(Block, 1) ->
    {Matching, NonMatching} = lists:partition(Block, List),
    #{<<"matching">> => Matching, <<"nonMatching">> => NonMatching};
partition(_List, Block) ->
    Hint = iolist_to_binary(
        io_lib:format("partition: expects a 1-argument block, got ~s", [describe_value(Block)])
    ),
    beamtalk_error:raise_type_error('List', 'partition:', Hint).

-doc "Intersperse separator between elements.".
-spec intersperse(list(), term()) -> list().
intersperse([], _Sep) -> [];
intersperse([X], _Sep) -> [X];
intersperse([H | T], Sep) -> [H, Sep | intersperse(T, Sep)].

-doc """
Extract subsequence from Start to End (1-based, inclusive).

BT-3025: a start index below 1 raises `index_out_of_bounds`, matching `at/2`.
It used to raise `does_not_understand`, which reported a malformed index as a
dispatch failure. An `End` below `Start` is an empty range, not an error.
""".
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
    Hint = iolist_to_binary(
        io_lib:format("Start index must be a positive integer, got ~s", [describe_value(Start)])
    ),
    beamtalk_error:raise_type_error('List', 'from:to:', Hint);
from_to(List, _Start, End) when is_list(List), not is_integer(End) ->
    Hint = iolist_to_binary(
        io_lib:format("End index must be a positive integer, got ~s", [describe_value(End)])
    ),
    beamtalk_error:raise_type_error('List', 'from:to:', Hint);
from_to(List, Start, _End) when is_list(List), is_integer(Start), Start < 1 ->
    Hint = iolist_to_binary(
        io_lib:format("Start index ~p is out of bounds (must be >= 1)", [Start])
    ),
    beamtalk_error:raise(beamtalk_error:new(index_out_of_bounds, 'List', 'from:to:', Hint)).

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

zip_to_pairs([], _) -> [];
zip_to_pairs(_, []) -> [];
zip_to_pairs([H1 | T1], [H2 | T2]) -> [[H1, H2] | zip_to_pairs(T1, T2)].

-doc """
BT-1487: Reverse the value lists in a groupBy result map.

During foldl-based groupBy with state threading, elements are prepended
to each group (building reversed lists). This reverses them to restore
the original order.
""".
-spec reverse_group_values(map()) -> map().
reverse_group_values(Map) when is_map(Map) ->
    maps:map(fun(_Key, Values) -> lists:reverse(Values) end, Map).

-doc """
Return a human-readable description of a value for error messages.

For blocks, includes the arity so wrong-arity errors are clear.
For other values, shows the Beamtalk class name.
""".
-spec describe_value(term()) -> binary().
describe_value(V) when is_function(V) ->
    {arity, A} = erlang:fun_info(V, arity),
    iolist_to_binary(io_lib:format("a ~p-argument block", [A]));
describe_value(V) ->
    ClassName = beamtalk_primitive:class_of(V),
    atom_to_binary(ClassName).

%%% ============================================================================
%%% Strict (`=:=`) element identity — BT-2997
%%% ============================================================================
%%%
%%% `ordsets` and `lists:usort/1` decide element identity with Erlang term
%%% *order*, which is `==` semantics: it treats the integer `1` and the float
%%% `1.0` as the same element. Beamtalk's element identity is `=:=` — matching
%%% `Dictionary` keys (Erlang maps), `List>>includes:`, and ADR 0002's
%%% strict-by-default equality, which BT-1562 established for `5 =:= 5.0`.
%%%
%%% These helpers keep the term-order-sorted list representation (which
%%% `beamtalk_set`, `beamtalk_inspector`, `beamtalk_primitive` and
%%% `beamtalk_stream` all rely on) but decide identity with `=:=`.
%%%
%%% `==` and `=:=` disagree only for numbers — an integer and a float of equal
%%% value — so after sorting, mutually-`==` elements form a short contiguous
%%% run. Every operation below scans that run strictly.

-doc """
`List>>unique` — remove duplicate elements.

Sorts, as `lists:usort/1` did, but deduplicates with `=:=`, so `1` and `1.0`
are kept as distinct elements.
""".
-spec unique(list()) -> list().
unique(List) when is_list(List) ->
    sorted_strict_unique(lists:sort(List)).

-doc """
Strictly deduplicate an already term-order-sorted list.

Keeps elements that merely compare `==` (`1` and `1.0`); removes only `=:=`
duplicates. All `=:=`-identical terms sort contiguously, so a run scan is
sufficient.
""".
-spec sorted_strict_unique(list()) -> list().
sorted_strict_unique(Sorted) ->
    %% Tail-recursive: a large list would otherwise recurse once per distinct
    %% element, which `lists:usort/1` never did.
    sorted_strict_unique(Sorted, []).

-spec sorted_strict_unique(list(), list()) -> list().
sorted_strict_unique([], Acc) ->
    lists:reverse(Acc);
sorted_strict_unique([H | T], Acc) ->
    {Run, Rest} = lists:splitwith(fun(X) -> X == H end, T),
    %% Dedupe the run against a *run-local* accumulator, not the output one:
    %% elements outside the run compare `/=` to everything in it, so they can
    %% never be `=:=` equal, and scanning them would make this quadratic.
    %% The result is reversed and at most two elements, so `++` is constant.
    Distinct = strict_uniq_run([H | Run], []),
    sorted_strict_unique(Rest, Distinct ++ Acc).

-doc """
True if `Elem` is `=:=` some member of `Sorted`.

**`Sorted` must be sorted by Erlang term order.** The scan stops as soon as the
list passes `Elem`, so an unsorted list yields a silently wrong answer rather
than an error — hence the name. `beamtalk_set` keeps its `elements` field in
this form, as does `unique/1`'s output.
""".
-spec strict_member_sorted(term(), list()) -> boolean().
strict_member_sorted(_Elem, []) ->
    false;
strict_member_sorted(Elem, [H | T]) when H == Elem ->
    %% Inside a run of mutually-`==` terms: only `=:=` counts as a match.
    H =:= Elem orelse strict_member_sorted(Elem, T);
strict_member_sorted(Elem, [H | T]) when H < Elem ->
    strict_member_sorted(Elem, T);
strict_member_sorted(_Elem, _PastIt) ->
    %% Reached an element greater than Elem; sorted ascending, so it is absent.
    false.

%% Strictly deduplicate one `==`-equal run onto a reversed accumulator.
%%
%% A run may be long (`[1, 1, 1, 1.0]`), but the number of *distinct* terms it
%% can contribute is at most two — an integer and a float of equal value are
%% the only way Erlang terms compare `==` without comparing `=:=`. So the
%% `lists:any/2` membership check runs against an at-most-two-element list and
%% is constant, not quadratic.
-spec strict_uniq_run(list(), list()) -> list().
strict_uniq_run([], Acc) ->
    Acc;
strict_uniq_run([H | T], Acc) ->
    case lists:any(fun(Y) -> Y =:= H end, Acc) of
        true -> strict_uniq_run(T, Acc);
        false -> strict_uniq_run(T, [H | Acc])
    end.
