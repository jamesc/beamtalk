%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime benchmark — block state-threading overhead
%%%
%%% @doc Erlang simulations of Beamtalk's block state-threading patterns.
%%%
%%% Beamtalk blocks that mutate local variables use a "StateAcc" calling
%%% convention: local vars are packed/unpacked from a map on each iteration.
%%% This module provides both the naive Erlang equivalent and the StateAcc
%%% simulation so the overhead can be measured directly.
%%%
%%% All benchmarks reflect post-BT-1321 codegen: binary op codegen no longer
%%% emits maybe_await (ADR-0043: all actor sends are synchronous).

-module(bench_block_threading).

-export([
    %% timesRepeat: equivalents
    sum_native/1,
    sum_stateacc/1,
    sum_direct_params/1,
    %% scale loop: multiply-by-literal, 1 variable + 1 literal per op
    scale_native/1,
    scale_direct_params_literal_opt/1,
    %% collect: / lists:map equivalents
    double_native/1,
    double_stateacc_block/1,
    %% inject:into: equivalents
    fold_native/1,
    fold_stateacc_block/1,
    fold_inject_into_wrapper/1,
    fold_inline_swap/1,
    %% BT-1329: nested list op inside counted loop — StateAcc fallback vs tuple target
    nested_stateacc_list_op/2,
    nested_tuple_list_op/2,
    %% BT-1276: list-op with LOCAL VARIABLE mutation — tuple-acc vs StateAcc map
    do_native_mutation/1,
    do_stateacc_mutation/1,
    do_tuple_acc_mutation/1,
    collect_stateacc_mutation/1,
    collect_tuple_acc_mutation/1,
    fold_stateacc_mutation/1,
    fold_tuple_acc_mutation/1,
    %% BT-1342: counted loop with both local + field mutations
    mixed_stateacc/1,
    mixed_full_extract/1,
    mixed_native/1,
    %% Crossover: stateacc vs full extract on small map with 1..4 mutated fields
    crossover_stateacc_1field/1,
    crossover_stateacc_2fields/1,
    crossover_stateacc_3fields/1,
    crossover_stateacc_4fields/1,
    crossover_full_extract_1field/1,
    crossover_full_extract_2fields/1,
    crossover_full_extract_3fields/1,
    crossover_full_extract_4fields/1,
    %% Large actor state map (>32 keys) — past BEAM small-map JIT threshold
    mixed_bigmap_stateacc/1,
    mixed_bigmap_full_extract/1,
    %% Arity scaling: full extraction with increasing param counts
    mixed_bigmap_full_extract_8params/1,
    mixed_bigmap_full_extract_12params/1,
    mixed_bigmap_full_extract_16params/1
]).

%%====================================================================
%% timesRepeat: pattern
%%====================================================================

%% @doc Idiomatic Erlang: tail-recursive accumulator.
%% Equivalent to:
%%   sum := 0. N timesRepeat: [sum := sum + 1]. sum
%% but without any overhead.
-spec sum_native(non_neg_integer()) -> integer().
sum_native(N) ->
    sum_native_loop(N, 0).

sum_native_loop(0, Acc) -> Acc;
sum_native_loop(N, Acc) -> sum_native_loop(N - 1, Acc + 1).

%% @doc Simulates what Beamtalk's timesRepeat: codegen produces.
%%
%% Generated Core Erlang for:
%%   sum := 0.
%%   1000 timesRepeat: [:i | sum := sum + 1].
%%
%%   letrec 'repeat'/2 = fun (I, StateAcc) ->
%%       case I > 0 of
%%           true ->
%%               let Sum = maps:get('__local__sum', StateAcc) in
%%               let NewSum = Sum + 1 in
%%               let StateAcc1 = maps:put('__local__sum', NewSum, StateAcc) in
%%               apply 'repeat'/2 (I - 1, StateAcc1)
%%           false -> {nil, StateAcc}
%%       end
%%   in
%%   let {_, FinalState} = apply 'repeat'/2 (N, #{'__local__sum' => 0}) in
%%   maps:get('__local__sum', FinalState)
-spec sum_stateacc(non_neg_integer()) -> integer().
sum_stateacc(N) ->
    InitState = #{'__local__sum' => 0},
    {_, FinalState} = sum_stateacc_loop(N, InitState),
    maps:get('__local__sum', FinalState).

sum_stateacc_loop(0, StateAcc) ->
    {nil, StateAcc};
sum_stateacc_loop(I, StateAcc) ->
    Sum = maps:get('__local__sum', StateAcc),
    NewSum = Sum + 1,
    StateAcc1 = maps:put('__local__sum', NewSum, StateAcc),
    sum_stateacc_loop(I - 1, StateAcc1).

%% @doc Simulates what Beamtalk's timesRepeat: codegen produces after BT-1275.
%%
%% Direct-params pattern (no per-iteration maps:get/put):
%%
%%   letrec 'repeat'/2 = fun (I, Sum) ->
%%       case I > 0 of
%%           true ->
%%               let Sum1 = Sum + 1 in
%%               apply 'repeat'/2 (I - 1, Sum1)
%%           false ->
%%               let ExitSA = maps:put('__local__sum', Sum, State) in
%%               {nil, ExitSA}
%%       end
%%   in
%%   let {_, FinalState} = apply 'repeat'/2 (N, 0) in
%%   maps:get('__local__sum', FinalState)
%%
%% One maps:put at loop exit; zero per iteration.
-spec sum_direct_params(non_neg_integer()) -> integer().
sum_direct_params(N) ->
    {_, FinalState} = sum_direct_params_loop(N, 0),
    maps:get('__local__sum', FinalState).

sum_direct_params_loop(0, Sum) ->
    ExitSA = maps:put('__local__sum', Sum, maps:new()),
    {nil, ExitSA};
sum_direct_params_loop(I, Sum) ->
    Sum1 = Sum + 1,
    sum_direct_params_loop(I - 1, Sum1).

%%====================================================================
%% Scale loop: multiply-by-literal
%%
%% Best-case benchmark for the BT-1286 literal-skipping optimisation:
%% every binary op has exactly one variable operand and one literal operand.
%%
%%   N timesRepeat: [:i | result := result * 2]
%%
%% Two variants:
%%   scale_native              — idiomatic Erlang, zero overhead
%%   scale_direct_params_literal_opt  — BT-1275 + BT-1286: literals not wrapped
%%                                      (post-BT-1286 + post-BT-1321 codegen)
%%====================================================================

%% @doc Idiomatic Erlang baseline.
%%
%% Assigns result := I * 2 each iteration so values stay fixnum (max = 2*N).
-spec scale_native(non_neg_integer()) -> integer().
scale_native(N) ->
    scale_native_loop(N, 0).

scale_native_loop(0, Result) -> Result;
scale_native_loop(I, _Result) ->
    scale_native_loop(I - 1, I * 2).

%% @doc Post-BT-1286 + post-BT-1321: direct params, no maybe_await on any operand.
%%
%% Simulates (result := I * 2 each iteration):
%%   let Result1 = call 'erlang':'*'(I, 2) in
%%   apply 'repeat'/2 (call 'erlang':'-'(I, 1), Result1)
%%
%% Zero maybe_await calls per iteration.
-spec scale_direct_params_literal_opt(non_neg_integer()) -> integer().
scale_direct_params_literal_opt(N) ->
    {_, FinalState} = scale_literal_opt_loop(N, 0),
    maps:get('__local__result', FinalState).

scale_literal_opt_loop(0, Result) ->
    {nil, maps:put('__local__result', Result, maps:new())};
scale_literal_opt_loop(I, _Result) ->
    Result1 = I * 2,
    scale_literal_opt_loop(I - 1, Result1).

%%====================================================================
%% collect: / lists:map pattern
%%====================================================================

%% @doc Idiomatic Erlang: lists:map with inline fun.
%% Equivalent to: #(1 2 3 ... N) collect: [:x | x * 2]
-spec double_native(list()) -> list().
double_native(List) ->
    lists:map(fun(X) -> X * 2 end, List).

%% @doc Simulates Beamtalk's stateful block for collect:.
%%
%% When the block captures mutated variables, it gets the StateAcc convention:
%%   fun(X, StateAcc) -> {X * 2, StateAcc}
%% Even when no variables are mutated, the block is called with an extra StateAcc
%% argument and must return {Result, NewStateAcc}.
-spec double_stateacc_block(list()) -> list().
double_stateacc_block(List) ->
    InitState = #{},
    BlockFun = fun(X, StateAcc) ->
        {X * 2, StateAcc}
    end,
    {Results, _} = lists:mapfoldl(BlockFun, InitState, List),
    Results.

%%====================================================================
%% inject:into: / lists:foldl pattern
%%====================================================================

%% @doc Idiomatic Erlang: lists:foldl with inline fun.
%% Equivalent to: list inject: 0 into: [:acc :x | acc + x]
-spec fold_native(list()) -> integer().
fold_native(List) ->
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, List).

%% @doc Simulates Beamtalk's stateful block for inject:into:.
%%
%% Beamtalk's inject:into: block takes (acc, x) in user code. The generated
%% lists:foldl fun wraps this as fun(X, {AccIn, StateAcc}) -> {NewAcc, NewStateAcc}.
%% The user-visible block is (AccIn, X) — accumulator first, element second.
-spec fold_stateacc_block(list()) -> integer().
fold_stateacc_block(List) ->
    InitState = #{},
    %% User block: [:acc :x | acc + x] — accumulator (AccIn) first, element (X) second.
    %% Wrapped for lists:foldl which passes element first, accumulator second.
    BlockFun = fun(X, {AccIn, StateAcc}) ->
        NewAcc = AccIn + X,
        {NewAcc, StateAcc}
    end,
    {Result, _} = lists:foldl(BlockFun, {0, InitState}, List),
    Result.

%% @doc Simulates current codegen for pure inject:into: — goes through
%% beamtalk_collection:inject_into which wraps the block for arg swap
%% and calls to_list.
%%
%% BT-1327: This benchmarks the ACTUAL overhead of the pure inject:into:
%% path (no mutations) as the compiler currently generates it.
-spec fold_inject_into_wrapper(list()) -> integer().
fold_inject_into_wrapper(List) ->
    Block = fun(Acc, X) -> Acc + X end,
    beamtalk_collection:inject_into(List, 0, Block).

%% @doc Simulates BT-1327 optimized inject:into: — inline lists:foldl with
%% compile-time arg swap and is_list guard.
%%
%% For a literal block [:acc :x | acc + x], the compiler can emit the block
%% body directly with the foldl parameter order (Elem, Acc) instead of the
%% Beamtalk convention (Acc, Elem), eliminating the wrapper function entirely:
%%   fun (X, Acc) -> Acc + X end
-spec fold_inline_swap(list()) -> integer().
fold_inline_swap(List) ->
    SafeList = case erlang:is_list(List) of
        true -> List;
        false -> beamtalk_collection:to_list(List)
    end,
    lists:foldl(fun(X, Acc) -> Acc + X end, 0, SafeList).

%%====================================================================
%% BT-1329: nested list op inside counted loop
%%
%% Simulates the pattern:
%%   count := 0. total := 0.
%%   1 to: N do: [:i |
%%       total := items inject: 0 into: [:acc :x | count := count + 1. acc + x]].
%%   count
%%
%% The inner inject:into: block mutates `count` from the outer scope on every
%% element. This makes it a Tier-2 call returning {Result, NewStateAcc}, which
%% forces the outer loop off direct-params (BT-1275) and onto full StateAcc map
%% threading even though `total` and `count` could both be direct params.
%%
%% BT-1329 target: inner block emits {Result, Count1} as an expanded tuple
%% (no StateAcc map). Outer loop unpacks Count1 directly as a fun parameter.
%%====================================================================

%% @doc Current StateAcc fallback for nested list op with outer variable mutation.
%%
%% Outer loop: maps:get('__local__count') + maps:put per outer iteration.
%% Inner foldl block: maps:get('__local__count') + maps:put per element (Tier-2).
%%
%% Total maps:get/put: N_outer * (1 outer read + N_items inner reads/writes + 1 outer write).
-spec nested_stateacc_list_op(non_neg_integer(), list()) -> integer().
nested_stateacc_list_op(Outer, Items) ->
    InitState = maps:put('__local__total', 0, maps:put('__local__count', 0, #{})),
    {_, FinalState} = nested_stateacc_loop(Outer, InitState, Items),
    maps:get('__local__count', FinalState).

nested_stateacc_loop(0, StateAcc, _Items) ->
    {nil, StateAcc};
nested_stateacc_loop(I, StateAcc, Items) ->
    %% Inner foldl block mutates 'count' from outer scope on every element.
    %% Returns {Result, NewStateAcc} (Tier-2): outer loop must use NewStateAcc.
    InnerFun = fun(X, {AccIn, SA}) ->
        C = maps:get('__local__count', SA),
        NewSA = maps:put('__local__count', C + 1, SA),
        {AccIn + X, NewSA}
    end,
    {NewTotal, NewStateAcc} = lists:foldl(InnerFun, {0, StateAcc}, Items),
    StateAcc1 = maps:put('__local__total', NewTotal, NewStateAcc),
    nested_stateacc_loop(I - 1, StateAcc1, Items).

%% @doc BT-1329 target: expanded tuple eliminates StateAcc at both levels.
%%
%% Inner foldl block returns {Result, Count1} — the mutated outer var as a
%% tuple position rather than packed in a map. Outer loop threads {Total, Count}
%% as direct fun parameters.
%%
%% Total maps operations: zero. Per element: Count + 1 + tuple construction only.
-spec nested_tuple_list_op(non_neg_integer(), list()) -> integer().
nested_tuple_list_op(Outer, Items) ->
    {_, {_Total, Count}} = nested_tuple_loop(Outer, {0, 0}, Items),
    Count.

nested_tuple_loop(0, {Total, Count}, _Items) ->
    {nil, {Total, Count}};
nested_tuple_loop(I, {_Total, Count}, Items) ->
    %% Inner foldl: accumulator is {AccIn, Count} — expanded tuple, no StateAcc.
    InnerFun = fun(X, {AccIn, C}) -> {AccIn + X, C + 1} end,
    {NewTotal, NewCount} = lists:foldl(InnerFun, {0, Count}, Items),
    nested_tuple_loop(I - 1, {NewTotal, NewCount}, Items).

%%====================================================================
%% BT-1276: list-op with LOCAL VARIABLE mutation
%% These simulate the codegen output for blocks that capture and mutate
%% outer-scope locals (e.g. `total := 0; list do: [:x | total := total + x]`).
%%
%% StateAcc approach: maps:get / maps:put per iteration (pre-BT-1276).
%% Tuple-acc approach: element(N, T) / {V1, ..., VN} per iteration (BT-1276).
%%====================================================================

%% @doc Idiomatic Erlang baseline for `do:` with a local mutation.
%% Equivalent to: total := 0. list do: [:item | total := total + item]. total
-spec do_native_mutation(list()) -> integer().
do_native_mutation(List) ->
    lists:foldl(fun(Item, Total) -> Total + Item end, 0, List).

%% @doc Simulates `do:` with a local mutation using old StateAcc map approach.
%%
%% Beamtalk source equivalent:
%%   total := 0. list do: [:item | total := total + item]. total
-spec do_stateacc_mutation(list()) -> integer().
do_stateacc_mutation(List) ->
    Total0 = 0,
    InitState = maps:put('__local__total', Total0, #{}),
    BlockFun = fun(Item, StateAcc) ->
        Total = maps:get('__local__total', StateAcc),
        NewTotal = Total + Item,
        maps:put('__local__total', NewTotal, StateAcc)
    end,
    FinalState = lists:foldl(BlockFun, InitState, List),
    maps:get('__local__total', FinalState).

%% @doc Simulates `do:` with a local mutation using BT-1276 tuple-acc approach.
%%
%% Generated pattern (accumulator is a flat tuple {Total}):
%%   fun(Item, StateAcc) ->
%%       Total = element(1, StateAcc),
%%       NewTotal = Total + Item,
%%       {NewTotal}
%% After foldl: element(1, FoldResult) extracts the final value.
%% (The one-time maps:put repack for the outer method body is omitted here
%% as it is not part of the per-element hot path being benchmarked.)
-spec do_tuple_acc_mutation(list()) -> integer().
do_tuple_acc_mutation(List) ->
    Total0 = 0,
    BlockFun = fun(Item, StateAcc) ->
        Total = erlang:element(1, StateAcc),
        NewTotal = Total + Item,
        {NewTotal}
    end,
    FoldResult = lists:foldl(BlockFun, {Total0}, List),
    erlang:element(1, FoldResult).

%% @doc Simulates `collect:` with a local mutation (count) using StateAcc map approach.
%%
%% Beamtalk source equivalent:
%%   count := 0.
%%   list collect: [:x | count := count + 1. x * 2]
-spec collect_stateacc_mutation(list()) -> list().
collect_stateacc_mutation(List) ->
    Count0 = 0,
    InitState = maps:put('__local__count', Count0, #{}),
    BlockFun = fun(X, {AccList, StateAcc}) ->
        Count = maps:get('__local__count', StateAcc),
        NewCount = Count + 1,
        NewStateAcc = maps:put('__local__count', NewCount, StateAcc),
        Result = X * 2,
        {[Result | AccList], NewStateAcc}
    end,
    {RevResults, _} = lists:foldl(BlockFun, {[], InitState}, List),
    lists:reverse(RevResults).

%% @doc Simulates `collect:` with a local mutation using BT-1276 tuple-acc approach.
%%
%% Accumulator is {AccList, Count} — no StateAcc map allocation per iteration.
-spec collect_tuple_acc_mutation(list()) -> list().
collect_tuple_acc_mutation(List) ->
    Count0 = 0,
    BlockFun = fun(X, {AccList, Count}) ->
        NewCount = Count + 1,
        Result = X * 2,
        {[Result | AccList], NewCount}
    end,
    {RevResults, _} = lists:foldl(BlockFun, {[], Count0}, List),
    lists:reverse(RevResults).

%% @doc Simulates `inject:into:` with a local mutation (count) using StateAcc map approach.
%%
%% Beamtalk source equivalent:
%%   count := 0.
%%   list inject: 0 into: [:acc :x | count := count + 1. acc + x]
%%
%% Per-iteration cost: maps:get + maps:put (allocates new map) + arithmetic.
-spec fold_stateacc_mutation(list()) -> integer().
fold_stateacc_mutation(List) ->
    Count0 = 0,
    InitState = maps:put('__local__count', Count0, #{}),
    BlockFun = fun(X, {AccIn, StateAcc}) ->
        Count = maps:get('__local__count', StateAcc),
        NewCount = Count + 1,
        NewStateAcc = maps:put('__local__count', NewCount, StateAcc),
        NewAcc = AccIn + X,
        {NewAcc, NewStateAcc}
    end,
    {Result, _} = lists:foldl(BlockFun, {0, InitState}, List),
    Result.

%% @doc Simulates `inject:into:` with a local mutation using BT-1276 tuple-acc approach.
%%
%% Accumulator is {Acc, Count} — no StateAcc map allocation per iteration.
%% Per-iteration cost: arithmetic + tuple construction only.
-spec fold_tuple_acc_mutation(list()) -> integer().
fold_tuple_acc_mutation(List) ->
    Count0 = 0,
    BlockFun = fun(X, {AccIn, Count}) ->
        NewCount = Count + 1,
        NewAcc = AccIn + X,
        {NewAcc, NewCount}
    end,
    {Result, _} = lists:foldl(BlockFun, {0, Count0}, List),
    Result.

%%====================================================================
%% BT-1326: Counted loop with BOTH local variable + actor field mutations
%%
%% Simulates:
%%   sum := 0
%%   1000 timesRepeat: [sum := sum + 1. self.n := self.n + 1]
%%   -- then extract sum --
%%
%% Three variants:
%%   mixed_stateacc  — old StateAcc path: all state packed into one map
%%   mixed_hybrid    — BT-1326 hybrid: locals as direct params, State threaded separately
%%   mixed_native    — idiomatic Erlang baseline (no overhead)
%%====================================================================

%% @doc Simulates old StateAcc path for a loop with both local + field mutations.
%%
%% Generated Core Erlang (pre-BT-1326):
%%   letrec 'loop'/2 = fun(I, StateAcc) ->
%%     case I =< N of
%%       true ->
%%         Sum = maps:get('__local__sum', StateAcc),
%%         Sum1 = Sum + 1,
%%         N0 = maps:get('n', StateAcc),
%%         StateAcc1 = maps:put('n', N0 + 1, StateAcc),
%%         StateAcc2 = maps:put('__local__sum', Sum1, StateAcc1),
%%         apply 'loop'/2 (I + 1, StateAcc2)
%%       false -> {nil, StateAcc}
%%     end
%%   in apply 'loop'/2 (1, maps:put('__local__sum', 0, State))
-spec mixed_stateacc(non_neg_integer()) -> integer().
mixed_stateacc(N) ->
    %% Realistic actor State has metadata keys ($beamtalk_class, __class_mod__,
    %% __methods__) plus field keys.  A 1-key map is unrealistically small and
    %% hides map-operation overhead behind BEAM's small-map JIT optimisation.
    BaseState = #{'$beamtalk_class' => 'MyActor', '__class_mod__' => test,
                  '__methods__' => #{}, 'n' => 0},
    InitState = maps:put('__local__sum', 0, BaseState),
    {_, FinalState} = mixed_stateacc_loop(1, N, InitState),
    maps:get('__local__sum', FinalState).

mixed_stateacc_loop(I, N, StateAcc) when I > N ->
    {nil, StateAcc};
mixed_stateacc_loop(I, N, StateAcc) ->
    Sum = maps:get('__local__sum', StateAcc),
    Sum1 = Sum + 1,
    N0 = maps:get('n', StateAcc),
    StateAcc1 = maps:put('n', N0 + 1, StateAcc),
    StateAcc2 = maps:put('__local__sum', Sum1, StateAcc1),
    mixed_stateacc_loop(I + 1, N, StateAcc2).

%% @doc Idiomatic Erlang baseline for the mixed local + field mutation pattern.
-spec mixed_native(non_neg_integer()) -> integer().
mixed_native(N) ->
    mixed_native_loop(1, N, 0, 0).

mixed_native_loop(I, N, Sum, _FieldN) when I > N -> Sum;
mixed_native_loop(I, N, Sum, FieldN) ->
    mixed_native_loop(I + 1, N, Sum + 1, FieldN + 1).

%% @doc BT-1342 full-extract: both locals AND mutated fields as direct params.
%% Same 4-key map as mixed_stateacc, but locals AND fields are lifted
%% to direct params. Loop body is pure arithmetic — zero map ops.
-spec mixed_full_extract(non_neg_integer()) -> integer().
mixed_full_extract(N) ->
    InitState = #{'$beamtalk_class' => 'MyActor', '__class_mod__' => test,
                  '__methods__' => #{}, 'n' => 0},
    FieldN0 = maps:get('n', InitState),
    {_, FinalState} = mixed_full_extract_loop(1, N, 0, FieldN0, InitState),
    maps:get('__local__sum', FinalState).

mixed_full_extract_loop(I, N, Sum, FieldN, State) when I > N ->
    State1 = maps:put('n', FieldN, State),
    ExitSA = maps:put('__local__sum', Sum, State1),
    {nil, ExitSA};
mixed_full_extract_loop(I, N, Sum, FieldN, State) ->
    mixed_full_extract_loop(I + 1, N, Sum + 1, FieldN + 1, State).

%%====================================================================
%% Crossover benchmarks: stateacc vs full extract on small map.
%%
%% Vary the number of mutated fields (1..4) to find the threshold where
%% full extraction starts beating BEAM's small-map JIT optimisation.
%% All use the same small base state (metadata + fields = ~6-8 keys).
%%
%% Each loop iteration: increments Sum (local) + increments N mutated fields.
%% StateAcc does maps:get + maps:put for each; full extract uses params.
%%====================================================================

crossover_base_state(NumFields) ->
    Base = #{'$beamtalk_class' => 'MyActor', '__class_mod__' => test,
             '__methods__' => #{}},
    lists:foldl(
        fun(I, Acc) ->
            Key = list_to_atom("f" ++ integer_to_list(I)),
            maps:put(Key, 0, Acc)
        end, Base, lists:seq(1, NumFields)).

%% --- 1 mutated field ---

-spec crossover_stateacc_1field(non_neg_integer()) -> integer().
crossover_stateacc_1field(N) ->
    InitState = maps:put('__local__sum', 0, crossover_base_state(1)),
    {_, FS} = crossover_sa1_loop(1, N, InitState),
    maps:get('__local__sum', FS).

crossover_sa1_loop(I, N, SA) when I > N -> {nil, SA};
crossover_sa1_loop(I, N, SA) ->
    Sum = maps:get('__local__sum', SA) + 1,
    V1 = maps:get('f1', SA) + 1,
    SA1 = maps:put('f1', V1, maps:put('__local__sum', Sum, SA)),
    crossover_sa1_loop(I + 1, N, SA1).

-spec crossover_full_extract_1field(non_neg_integer()) -> integer().
crossover_full_extract_1field(N) ->
    State = crossover_base_state(1),
    F1 = maps:get('f1', State),
    {_, FS} = crossover_fe1_loop(1, N, 0, F1, State),
    maps:get('__local__sum', FS).

crossover_fe1_loop(I, N, Sum, F1, State) when I > N ->
    S1 = maps:put('f1', F1, State),
    {nil, maps:put('__local__sum', Sum, S1)};
crossover_fe1_loop(I, N, Sum, F1, State) ->
    crossover_fe1_loop(I + 1, N, Sum + 1, F1 + 1, State).

%% --- 2 mutated fields ---

-spec crossover_stateacc_2fields(non_neg_integer()) -> integer().
crossover_stateacc_2fields(N) ->
    InitState = maps:put('__local__sum', 0, crossover_base_state(2)),
    {_, FS} = crossover_sa2_loop(1, N, InitState),
    maps:get('__local__sum', FS).

crossover_sa2_loop(I, N, SA) when I > N -> {nil, SA};
crossover_sa2_loop(I, N, SA) ->
    Sum = maps:get('__local__sum', SA) + 1,
    V1 = maps:get('f1', SA) + 1,
    V2 = maps:get('f2', SA) + 1,
    SA1 = maps:put('f2', V2, maps:put('f1', V1, maps:put('__local__sum', Sum, SA))),
    crossover_sa2_loop(I + 1, N, SA1).

-spec crossover_full_extract_2fields(non_neg_integer()) -> integer().
crossover_full_extract_2fields(N) ->
    State = crossover_base_state(2),
    F1 = maps:get('f1', State),
    F2 = maps:get('f2', State),
    {_, FS} = crossover_fe2_loop(1, N, 0, F1, F2, State),
    maps:get('__local__sum', FS).

crossover_fe2_loop(I, N, Sum, F1, F2, State) when I > N ->
    S1 = maps:put('f1', F1, maps:put('f2', F2, State)),
    {nil, maps:put('__local__sum', Sum, S1)};
crossover_fe2_loop(I, N, Sum, F1, F2, State) ->
    crossover_fe2_loop(I + 1, N, Sum + 1, F1 + 1, F2 + 1, State).

%% --- 3 mutated fields ---

-spec crossover_stateacc_3fields(non_neg_integer()) -> integer().
crossover_stateacc_3fields(N) ->
    InitState = maps:put('__local__sum', 0, crossover_base_state(3)),
    {_, FS} = crossover_sa3_loop(1, N, InitState),
    maps:get('__local__sum', FS).

crossover_sa3_loop(I, N, SA) when I > N -> {nil, SA};
crossover_sa3_loop(I, N, SA) ->
    Sum = maps:get('__local__sum', SA) + 1,
    V1 = maps:get('f1', SA) + 1,
    V2 = maps:get('f2', SA) + 1,
    V3 = maps:get('f3', SA) + 1,
    SA1 = maps:put('f3', V3, maps:put('f2', V2, maps:put('f1', V1, maps:put('__local__sum', Sum, SA)))),
    crossover_sa3_loop(I + 1, N, SA1).

-spec crossover_full_extract_3fields(non_neg_integer()) -> integer().
crossover_full_extract_3fields(N) ->
    State = crossover_base_state(3),
    F1 = maps:get('f1', State),
    F2 = maps:get('f2', State),
    F3 = maps:get('f3', State),
    {_, FS} = crossover_fe3_loop(1, N, 0, F1, F2, F3, State),
    maps:get('__local__sum', FS).

crossover_fe3_loop(I, N, Sum, F1, F2, F3, State) when I > N ->
    S1 = maps:put('f1', F1, maps:put('f2', F2, maps:put('f3', F3, State))),
    {nil, maps:put('__local__sum', Sum, S1)};
crossover_fe3_loop(I, N, Sum, F1, F2, F3, State) ->
    crossover_fe3_loop(I + 1, N, Sum + 1, F1 + 1, F2 + 1, F3 + 1, State).

%% --- 4 mutated fields ---

-spec crossover_stateacc_4fields(non_neg_integer()) -> integer().
crossover_stateacc_4fields(N) ->
    InitState = maps:put('__local__sum', 0, crossover_base_state(4)),
    {_, FS} = crossover_sa4_loop(1, N, InitState),
    maps:get('__local__sum', FS).

crossover_sa4_loop(I, N, SA) when I > N -> {nil, SA};
crossover_sa4_loop(I, N, SA) ->
    Sum = maps:get('__local__sum', SA) + 1,
    V1 = maps:get('f1', SA) + 1,
    V2 = maps:get('f2', SA) + 1,
    V3 = maps:get('f3', SA) + 1,
    V4 = maps:get('f4', SA) + 1,
    SA1 = maps:put('f4', V4, maps:put('f3', V3, maps:put('f2', V2, maps:put('f1', V1, maps:put('__local__sum', Sum, SA))))),
    crossover_sa4_loop(I + 1, N, SA1).

-spec crossover_full_extract_4fields(non_neg_integer()) -> integer().
crossover_full_extract_4fields(N) ->
    State = crossover_base_state(4),
    F1 = maps:get('f1', State),
    F2 = maps:get('f2', State),
    F3 = maps:get('f3', State),
    F4 = maps:get('f4', State),
    {_, FS} = crossover_fe4_loop(1, N, 0, F1, F2, F3, F4, State),
    maps:get('__local__sum', FS).

crossover_fe4_loop(I, N, Sum, F1, F2, F3, F4, State) when I > N ->
    S1 = maps:put('f1', F1, maps:put('f2', F2, maps:put('f3', F3, maps:put('f4', F4, State)))),
    {nil, maps:put('__local__sum', Sum, S1)};
crossover_fe4_loop(I, N, Sum, F1, F2, F3, F4, State) ->
    crossover_fe4_loop(I + 1, N, Sum + 1, F1 + 1, F2 + 1, F3 + 1, F4 + 1, State).

%%====================================================================
%% BT-1326 large-map variants: same as mixed_* but with >32-key actor state.
%%
%% BEAM's JIT uses a flat tuple representation for small maps (up to ~32
%% keys). Above that threshold maps switch to a HAMT representation where
%% maps:get/put are O(log32 N) instead of O(1). The small-map variants
%% above give StateAcc an unfair advantage because *all* map ops hit the
%% fast flat path. These bigmap variants use a 34-key state map to show
%% the real-world scaling where hybrid eliminates map ops for locals.
%%====================================================================

%% @doc Helper to build a large base state map (34 keys — above small-map threshold).
bigmap_base_state() ->
    lists:foldl(
        fun(I, Acc) ->
            Key = list_to_atom("field_" ++ integer_to_list(I)),
            maps:put(Key, 0, Acc)
        end,
        #{'$beamtalk_class' => 'BigActor', '__class_mod__' => test,
          '__methods__' => #{}, 'n' => 0},
        lists:seq(1, 30)).

%% @doc StateAcc path with large map — locals packed into same big map.
-spec mixed_bigmap_stateacc(non_neg_integer()) -> integer().
mixed_bigmap_stateacc(N) ->
    BaseState = bigmap_base_state(),
    InitState = maps:put('__local__sum', 0, BaseState),
    {_, FinalState} = mixed_bigmap_stateacc_loop(1, N, InitState),
    maps:get('__local__sum', FinalState).

mixed_bigmap_stateacc_loop(I, N, StateAcc) when I > N ->
    {nil, StateAcc};
mixed_bigmap_stateacc_loop(I, N, StateAcc) ->
    Sum = maps:get('__local__sum', StateAcc),
    Sum1 = Sum + 1,
    N0 = maps:get('n', StateAcc),
    StateAcc1 = maps:put('n', N0 + 1, StateAcc),
    StateAcc2 = maps:put('__local__sum', Sum1, StateAcc1),
    mixed_bigmap_stateacc_loop(I + 1, N, StateAcc2).

%% @doc Full extraction: lift both locals AND mutated fields to direct params.
%% Extract at loop entry, pass as params, repack on exit. Zero map ops in the
%% loop body — represents the theoretical ceiling for codegen optimisation.
%%
%% Hypothetical codegen:
%%   letrec 'loop'/4 = fun(I, Sum, FieldN, State) ->
%%     case I =< N of
%%       true -> apply 'loop'/4 (I+1, Sum+1, FieldN+1, State)
%%       false ->
%%         State1 = maps:put('n', FieldN, State),
%%         ExitSA = maps:put('__local__sum', Sum, State1),
%%         {nil, ExitSA}
%%     end
%%   in
%%     FieldN0 = maps:get('n', State),
%%     apply 'loop'/4 (1, 0, FieldN0, State)
-spec mixed_bigmap_full_extract(non_neg_integer()) -> integer().
mixed_bigmap_full_extract(N) ->
    InitState = bigmap_base_state(),
    FieldN0 = maps:get('n', InitState),
    {_, FinalState} = mixed_bigmap_full_extract_loop(1, N, 0, FieldN0, InitState),
    maps:get('__local__sum', FinalState).

mixed_bigmap_full_extract_loop(I, N, Sum, FieldN, State) when I > N ->
    State1 = maps:put('n', FieldN, State),
    ExitSA = maps:put('__local__sum', Sum, State1),
    {nil, ExitSA};
mixed_bigmap_full_extract_loop(I, N, Sum, FieldN, State) ->
    mixed_bigmap_full_extract_loop(I + 1, N, Sum + 1, FieldN + 1, State).

%%====================================================================
%% Arity scaling: how does BEAM handle increasing param counts?
%%
%% BEAM has dedicated call opcodes for small arities. These benchmarks
%% test whether full extraction degrades as we lift more fields out of
%% the map (simulating actors with many mutated fields + locals).
%%
%% 5 params = base full_extract (I, N, Sum, FieldN, State)
%% 8 params = + 3 extra mutated fields
%% 12 params = + 7 extra mutated fields
%% 16 params = + 11 extra mutated fields
%%====================================================================

%% @doc 8 params: I, N, Sum, FieldN, F1, F2, F3, State
-spec mixed_bigmap_full_extract_8params(non_neg_integer()) -> integer().
mixed_bigmap_full_extract_8params(N) ->
    InitState = bigmap_base_state(),
    FieldN0 = maps:get('n', InitState),
    F1 = maps:get('field_1', InitState),
    F2 = maps:get('field_2', InitState),
    F3 = maps:get('field_3', InitState),
    {_, FinalState} = full_extract_8p_loop(1, N, 0, FieldN0, F1, F2, F3, InitState),
    maps:get('__local__sum', FinalState).

full_extract_8p_loop(I, N, Sum, FieldN, F1, F2, F3, State) when I > N ->
    S1 = maps:put('n', FieldN, State),
    S2 = maps:put('field_1', F1, S1),
    S3 = maps:put('field_2', F2, S2),
    S4 = maps:put('field_3', F3, S3),
    ExitSA = maps:put('__local__sum', Sum, S4),
    {nil, ExitSA};
full_extract_8p_loop(I, N, Sum, FieldN, F1, F2, F3, State) ->
    full_extract_8p_loop(I + 1, N, Sum + 1, FieldN + 1, F1 + 1, F2 + 1, F3 + 1, State).

%% @doc 12 params: I, N, Sum, FieldN, F1..F7, State
-spec mixed_bigmap_full_extract_12params(non_neg_integer()) -> integer().
mixed_bigmap_full_extract_12params(N) ->
    InitState = bigmap_base_state(),
    FieldN0 = maps:get('n', InitState),
    F1 = maps:get('field_1', InitState),
    F2 = maps:get('field_2', InitState),
    F3 = maps:get('field_3', InitState),
    F4 = maps:get('field_4', InitState),
    F5 = maps:get('field_5', InitState),
    F6 = maps:get('field_6', InitState),
    F7 = maps:get('field_7', InitState),
    {_, FinalState} = full_extract_12p_loop(1, N, 0, FieldN0, F1, F2, F3, F4, F5, F6, F7, InitState),
    maps:get('__local__sum', FinalState).

full_extract_12p_loop(I, N, Sum, FieldN, F1, F2, F3, F4, F5, F6, F7, State) when I > N ->
    S1 = maps:put('n', FieldN, State),
    S2 = maps:put('field_1', F1, S1),
    S3 = maps:put('field_2', F2, S2),
    S4 = maps:put('field_3', F3, S3),
    S5 = maps:put('field_4', F4, S4),
    S6 = maps:put('field_5', F5, S5),
    S7 = maps:put('field_6', F6, S6),
    S8 = maps:put('field_7', F7, S7),
    ExitSA = maps:put('__local__sum', Sum, S8),
    {nil, ExitSA};
full_extract_12p_loop(I, N, Sum, FieldN, F1, F2, F3, F4, F5, F6, F7, State) ->
    full_extract_12p_loop(I + 1, N, Sum + 1, FieldN + 1, F1 + 1, F2 + 1, F3 + 1, F4 + 1, F5 + 1, F6 + 1, F7 + 1, State).

%% @doc 16 params: I, N, Sum, FieldN, F1..F11, State
-spec mixed_bigmap_full_extract_16params(non_neg_integer()) -> integer().
mixed_bigmap_full_extract_16params(N) ->
    InitState = bigmap_base_state(),
    FieldN0 = maps:get('n', InitState),
    F1 = maps:get('field_1', InitState),
    F2 = maps:get('field_2', InitState),
    F3 = maps:get('field_3', InitState),
    F4 = maps:get('field_4', InitState),
    F5 = maps:get('field_5', InitState),
    F6 = maps:get('field_6', InitState),
    F7 = maps:get('field_7', InitState),
    F8 = maps:get('field_8', InitState),
    F9 = maps:get('field_9', InitState),
    F10 = maps:get('field_10', InitState),
    F11 = maps:get('field_11', InitState),
    {_, FinalState} = full_extract_16p_loop(1, N, 0, FieldN0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, InitState),
    maps:get('__local__sum', FinalState).

full_extract_16p_loop(I, N, Sum, FieldN, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, State) when I > N ->
    S1 = maps:put('n', FieldN, State),
    S2 = maps:put('field_1', F1, S1),
    S3 = maps:put('field_2', F2, S2),
    S4 = maps:put('field_3', F3, S3),
    S5 = maps:put('field_4', F4, S4),
    S6 = maps:put('field_5', F5, S5),
    S7 = maps:put('field_6', F6, S6),
    S8 = maps:put('field_7', F7, S7),
    S9 = maps:put('field_8', F8, S8),
    S10 = maps:put('field_9', F9, S9),
    S11 = maps:put('field_10', F10, S10),
    S12 = maps:put('field_11', F11, S11),
    ExitSA = maps:put('__local__sum', Sum, S12),
    {nil, ExitSA};
full_extract_16p_loop(I, N, Sum, FieldN, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, State) ->
    full_extract_16p_loop(I + 1, N, Sum + 1, FieldN + 1, F1 + 1, F2 + 1, F3 + 1, F4 + 1, F5 + 1, F6 + 1, F7 + 1, F8 + 1, F9 + 1, F10 + 1, F11 + 1, State).
