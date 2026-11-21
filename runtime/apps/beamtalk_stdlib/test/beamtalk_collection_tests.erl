%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_collection_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_collection module.

Tests inject_into/3 and to_list/1 for list inputs.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Test Fixtures
%%% ============================================================================

%% Load stdlib so non-list Collection classes (e.g. Interval) can dispatch do:.
%%
%% Boot through the shared beamtalk_test_boot helper rather than an
%% ad hoc pg/bootstrap/init sequence — see beamtalk_test_runner_tests's
%% live_setup/0 for the ordering this must preserve.
stdlib_setup() ->
    beamtalk_test_boot:boot_real_stdlib('Interval'),
    ok.

stdlib_teardown(_) -> ok.

%%% ============================================================================
%%% inject_into/3
%%% ============================================================================

inject_into_sum_test() ->
    ?assertEqual(
        10, beamtalk_collection:inject_into([1, 2, 3, 4], 0, fun(Acc, E) -> Acc + E end)
    ).

inject_into_empty_list_test() ->
    ?assertEqual(99, beamtalk_collection:inject_into([], 99, fun(Acc, _E) -> Acc end)).

inject_into_build_list_test() ->
    Result = beamtalk_collection:inject_into([1, 2, 3], [], fun(Acc, E) -> Acc ++ [E] end),
    ?assertEqual([1, 2, 3], Result).

inject_into_accumulator_order_test() ->
    %% Block receives (Acc, Elem) — accumulator first, element second
    Result = beamtalk_collection:inject_into([a, b, c], [], fun(Acc, E) -> [E | Acc] end),
    %% foldl processes left to right, so reversed result
    ?assertEqual([c, b, a], Result).

inject_into_single_element_test() ->
    ?assertEqual(42, beamtalk_collection:inject_into([42], 0, fun(_Acc, E) -> E end)).

%%% ============================================================================
%%% to_list/1 — list input (trivial path)
%%% ============================================================================

to_list_already_list_test() ->
    List = [1, 2, 3],
    ?assertEqual(List, beamtalk_collection:to_list(List)).

to_list_empty_list_test() ->
    ?assertEqual([], beamtalk_collection:to_list([])).

to_list_single_element_test() ->
    ?assertEqual([x], beamtalk_collection:to_list([x])).

to_list_nested_list_test() ->
    ?assertEqual([[1, 2], [3, 4]], beamtalk_collection:to_list([[1, 2], [3, 4]])).

to_list_mixed_types_test() ->
    ?assertEqual([1, a, <<"str">>], beamtalk_collection:to_list([1, a, <<"str">>])).

%%% ============================================================================
%%% inject_into/3 — additional coverage
%%% ============================================================================

inject_into_string_accumulator_test() ->
    %% Build a string by accumulating elements
    Result = beamtalk_collection:inject_into(
        [<<"a">>, <<"b">>, <<"c">>],
        <<>>,
        fun(Acc, E) -> <<Acc/binary, E/binary>> end
    ),
    ?assertEqual(<<"abc">>, Result).

inject_into_map_accumulator_test() ->
    %% Build a map by accumulating key-value pairs
    Result = beamtalk_collection:inject_into(
        [{a, 1}, {b, 2}],
        #{},
        fun(Acc, {K, V}) -> maps:put(K, V, Acc) end
    ),
    ?assertEqual(#{a => 1, b => 2}, Result).

inject_into_count_test() ->
    %% Count elements
    Result = beamtalk_collection:inject_into([x, y, z], 0, fun(Acc, _E) -> Acc + 1 end),
    ?assertEqual(3, Result).

%%% ============================================================================
%%% to_list/1 — non-list collection path
%%% ============================================================================
%%%
%%% These tests exercise the non-list branch of to_list/1, which dispatches
%%% `do:` on the receiver via beamtalk_primitive:send/3. Requires stdlib to
%%% be loaded so that Interval (a Collection subclass) can respond to do:.

to_list_non_list_test_() ->
    {setup, fun stdlib_setup/0, fun stdlib_teardown/1, [
        {"to_list on Interval yields integers in range", fun to_list_interval_test/0},
        {"to_list on empty Interval yields empty list", fun to_list_empty_interval_test/0},
        {"inject_into on Interval sums the range", fun inject_into_interval_test/0},
        {"to_list leaves unrelated mailbox messages untouched",
            fun to_list_preserves_mailbox_test/0},
        {"to_list leaves no accumulator messages behind", fun to_list_drains_mailbox_test/0}
    ]}.

%% Build an Interval via direct map construction (matches stdlib/src/interval.bt).
make_interval(From, To) ->
    #{'$beamtalk_class' => 'Interval', from => From, to => To, step => 1}.

to_list_interval_test() ->
    Interval = make_interval(1, 5),
    Result = beamtalk_collection:to_list(Interval),
    ?assertEqual([1, 2, 3, 4, 5], Result).

to_list_empty_interval_test() ->
    %% from > to with step 1 → empty
    Interval = make_interval(5, 1),
    Result = beamtalk_collection:to_list(Interval),
    ?assertEqual([], Result).

inject_into_interval_test() ->
    Interval = make_interval(1, 10),
    %% Sum 1..10 = 55
    Sum = beamtalk_collection:inject_into(Interval, 0, fun(Acc, E) -> Acc + E end),
    ?assertEqual(55, Sum).

%% to_list/1 accumulates through the caller's mailbox so the iteration
%% block works across a process boundary. That makes mailbox hygiene part of its
%% contract: unrelated messages must be left in place and in order, because
%% to_list may run inside an actor's callback.
to_list_preserves_mailbox_test() ->
    self() ! {decoy, 1},
    self() ! {decoy, 2},
    ?assertEqual([1, 2, 3], beamtalk_collection:to_list(make_interval(1, 3))),
    ?assertEqual(
        {decoy, 1},
        receive
            M1 -> M1
        after 0 -> timeout
        end
    ),
    ?assertEqual(
        {decoy, 2},
        receive
            M2 -> M2
        after 0 -> timeout
        end
    ),
    ?assertEqual(
        empty,
        receive
            M3 -> M3
        after 0 -> empty
        end
    ).

%% The accumulator messages must all be consumed — a stray `'$bt_to_list'` tuple
%% left behind would reach the enclosing process's handle_info/2.
to_list_drains_mailbox_test() ->
    ?assertEqual([1, 2, 3, 4, 5], beamtalk_collection:to_list(make_interval(1, 5))),
    ?assertEqual(
        empty,
        receive
            Any -> Any
        after 0 -> empty
        end
    ).

%%% ============================================================================
%%% from_list_like/2 — result reconstruction
%%% ============================================================================
%%%
%%% Reconstructs a collect:/select:/reject: fold result so its type matches the
%%% original receiver, mirroring the pure (non-mutating) list-op path.

from_list_like_binary_receiver_test() ->
    %% String (binary) receiver → binary result via iolist_to_binary.
    ?assertEqual(<<"abc">>, beamtalk_collection:from_list_like(<<"xyz">>, [$a, $b, $c])).

from_list_like_array_receiver_test() ->
    %% Array receiver → Array result (matches the pure collect: path).
    Array = beamtalk_array:from_list([1, 2, 3]),
    Result = beamtalk_collection:from_list_like(Array, [2, 4, 6]),
    ?assertEqual(beamtalk_array:from_list([2, 4, 6]), Result).

from_list_like_list_receiver_test() ->
    %% Raw Erlang list receiver → list result, unchanged.
    ?assertEqual([2, 4, 6], beamtalk_collection:from_list_like([1, 2, 3], [2, 4, 6])).

from_list_like_set_receiver_test() ->
    %% BT-3615: Set receiver → Set result (matches the pure collect:/select:
    %% path's species answer), deduplicating.
    Set = beamtalk_set:from_list([1, 2, 3]),
    ?assertEqual(
        beamtalk_set:from_list([2, 4]), beamtalk_collection:from_list_like(Set, [2, 4, 2])
    ).

from_list_like_other_receiver_test() ->
    %% Non-Array, non-Set, non-binary, non-list receiver (e.g. a Bag map) → list unchanged.
    Bag = #{'$beamtalk_class' => 'Bag', counts => #{}},
    ?assertEqual([1, 2], beamtalk_collection:from_list_like(Bag, [1, 2])).

%%% ============================================================================
%%% to_list_for_threading/2 — the receiver guard (BT-3916)
%%% ============================================================================

to_list_for_threading_test_() ->
    {setup, fun stdlib_setup/0, fun stdlib_teardown/1, [
        {"a list passes through for any selector", fun to_list_for_threading_list_test/0},
        {"do: is never guarded", fun to_list_for_threading_do_test/0},
        {"a stdlib selector folds over do:", fun to_list_for_threading_stdlib_test/0},
        {"a selector the receiver lacks is does_not_understand",
            fun to_list_for_threading_dnu_test/0},
        {"a runtime extension on a collection message refuses (BT-3951)",
            fun to_list_for_threading_extension_test/0}
    ]}.

to_list_for_threading_list_test() ->
    ?assertEqual([1, 2], beamtalk_collection:to_list_for_threading([1, 2], 'anySatisfy:')).

to_list_for_threading_do_test() ->
    ?assertEqual(
        [1, 2, 3],
        beamtalk_collection:to_list_for_threading(make_interval(1, 3), 'do:')
    ).

to_list_for_threading_stdlib_test() ->
    lists:foreach(
        fun(Selector) ->
            ?assertEqual(
                [1, 2, 3],
                beamtalk_collection:to_list_for_threading(make_interval(1, 3), Selector)
            )
        end,
        ['inject:into:', 'collect:', 'select:', 'anySatisfy:', 'detect:ifNone:']
    ).

to_list_for_threading_dnu_test() ->
    ?assertBtError(
        does_not_understand,
        _,
        'noSuchSelector:',
        beamtalk_collection:to_list_for_threading(make_interval(1, 3), 'noSuchSelector:')
    ).

to_list_for_threading_extension_test() ->
    Fun = fun(_Args, _Self, State) -> {reply, nil, State} end,
    %% An extension that is not a collection message changes nothing.
    ok = beamtalk_extensions:register('Interval', 'bt3951Harmless', Fun, bt3951_test),
    try
        ?assertEqual(
            [1, 2, 3],
            beamtalk_collection:to_list_for_threading(make_interval(1, 3), 'collect:')
        )
    after
        beamtalk_extensions:unregister('Interval', 'bt3951Harmless')
    end,
    %% A class-side `withAll:` (reached through `species`) poisons it too.
    Tag = beamtalk_class_registry:class_object_tag('Interval'),
    ok = beamtalk_extensions:register(Tag, 'withAll:', fun(_Args, _Self) -> nil end, bt3951_test),
    try
        ?assertBtError(
            stateful_block_dispatch,
            beamtalk_collection:to_list_for_threading(make_interval(1, 3), 'collect:')
        )
    after
        beamtalk_extensions:unregister('Interval', 'withAll:', true)
    end,
    %% ... but only the selectors that end in `species withAll:`.
    ok = beamtalk_extensions:register(Tag, 'withAll:', fun(_Args, _Self) -> nil end, bt3951_test),
    try
        ?assertEqual(
            [1, 2, 3],
            beamtalk_collection:to_list_for_threading(make_interval(1, 3), 'inject:into:')
        )
    after
        beamtalk_extensions:unregister('Interval', 'withAll:', true)
    end,
    %% One on a different overridable selector poisons the fold.
    ok = beamtalk_extensions:register('Interval', 'inject:into:', Fun, bt3951_test),
    try
        ?assertBtError(
            stateful_block_dispatch,
            beamtalk_collection:to_list_for_threading(make_interval(1, 3), 'collect:')
        )
    after
        beamtalk_extensions:unregister('Interval', 'inject:into:')
    end.

%%% ============================================================================
%%% keyed_foldl/5 and from_list_like/2 over a Dictionary (BT-5097)
%%% ============================================================================

%% The fold lambdas below have the shape codegen compiles: `{AccList, Count}`, the
%% result list at element 1, one element added per call at most. The reference is
%% the same lambda over `lists:foldl` on the values, which is what a list receiver
%% gets; the keyed fold must agree on the count, the order of the calls and the
%% survivors.

keyed_foldl_test_() ->
    Dict = #{a => 1, b => 2, c => 3, d => 4},
    Select = fun(Pred) ->
        fun(V, {Acc, N}) ->
            case Pred(N, V) of
                true -> {[V | Acc], N + 1};
                false -> {Acc, N + 1}
            end
        end
    end,
    Collect = fun(V, {Acc, N}) -> {[V * 10 | Acc], N + 1} end,
    Wrapped = fun(Lambda) -> fun(V, {Inner, Self}) -> {Lambda(V, Inner), Self + 1} end end,
    Rebuild = fun(Dict0, Out) -> beamtalk_collection:from_list_like(Dict0, lists:reverse(Out)) end,
    [
        {"select keeps the keys of the entries the lambda kept", fun() ->
            Lambda = Select(fun(_N, V) -> V > 2 end),
            {Out, 4} = beamtalk_collection:keyed_foldl(Lambda, {[], 0}, Dict, 'select:', [1]),
            ?assertEqual(#{c => 3, d => 4}, Rebuild(Dict, Out))
        end},
        {"the predicate sees duplicate values one at a time, so the right keys survive", fun() ->
            Dup = #{a => 1, b => 1, c => 1},
            Lambda = Select(fun(N, _V) -> N =:= 1 end),
            {Out, 3} = beamtalk_collection:keyed_foldl(Lambda, {[], 0}, Dup, 'select:', [1]),
            Result = Rebuild(Dup, Out),
            ?assertEqual(1, map_size(Result)),
            %% maps:to_list order is the call order: the second entry survives.
            [{_, _} = Second | _] = tl(maps:to_list(Dup)),
            ?assertEqual(maps:from_list([Second]), Result)
        end},
        {"collect answers every key with the lambda's answer", fun() ->
            {Out, 4} = beamtalk_collection:keyed_foldl(Collect, {[], 0}, Dict, 'collect:', [1]),
            ?assertEqual(#{a => 10, b => 20, c => 30, d => 40}, Rebuild(Dict, Out))
        end},
        {"collect keeps answers that are empty lists or lists", fun() ->
            L = fun(V, {Acc, N}) -> {[lists:duplicate(V - 1, V) | Acc], N + 1} end,
            {Out, 2} = beamtalk_collection:keyed_foldl(L, {[], 0}, #{x => 1, y => 2}, 'collect:', [
                1
            ]),
            ?assertEqual(#{x => [], y => [2]}, Rebuild(#{x => 1, y => 2}, Out))
        end},
        {"an empty dictionary runs the lambda never and answers an empty one", fun() ->
            {Out, 0} = beamtalk_collection:keyed_foldl(Collect, {[], 0}, #{}, 'collect:', [1]),
            ?assertEqual(#{}, Rebuild(#{}, Out))
        end},
        {"a wrapped accumulator is walked through element 1", fun() ->
            {{Out, 4}, 4} = beamtalk_collection:keyed_foldl(
                Wrapped(Collect), {{[], 0}, 0}, Dict, 'collect:', [1, 1]
            ),
            ?assertEqual(#{a => 10, b => 20, c => 30, d => 40}, Rebuild(Dict, Out))
        end},
        {"agrees with the list fold over the values on every call and survivor", fun() ->
            Lambda = Select(fun(N, V) -> (N + V) rem 2 =:= 0 end),
            {ListOut, 4} = lists:foldl(Lambda, {[], 0}, maps:values(Dict)),
            {DictOut, 4} = beamtalk_collection:keyed_foldl(Lambda, {[], 0}, Dict, 'select:', [1]),
            ?assertEqual(lists:reverse(ListOut), [V || {_K, V} <- lists:reverse(DictOut)])
        end}
    ].

from_list_like_dictionary_receiver_test() ->
    ?assertEqual(#{a => 1}, beamtalk_collection:from_list_like(#{b => 2}, [{a, 1}])),
    ?assertEqual(#{}, beamtalk_collection:from_list_like(#{b => 2}, [])).
