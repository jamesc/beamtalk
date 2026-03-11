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
    %% BT-1326: counted loop with BOTH local + field mutations — hybrid vs StateAcc
    mixed_stateacc/1,
    mixed_hybrid/1,
    mixed_native/1
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

%% @doc Simulates BT-1326 hybrid params path for a loop with both local + field mutations.
%%
%% Generated Core Erlang (post-BT-1326):
%%   letrec 'loop'/3 = fun(I, Sum, State) ->
%%     case I =< N of
%%       true ->
%%         Sum1 = Sum + 1,
%%         N0 = maps:get('n', State),
%%         State1 = maps:put('n', N0 + 1, State),
%%         apply 'loop'/3 (I + 1, Sum1, State1)
%%       false ->
%%         ExitSA = maps:put('__local__sum', Sum, State),
%%         {nil, ExitSA}
%%     end
%%   in apply 'loop'/3 (1, 0, State)
-spec mixed_hybrid(non_neg_integer()) -> integer().
mixed_hybrid(N) ->
    InitState = #{'$beamtalk_class' => 'MyActor', '__class_mod__' => test,
                  '__methods__' => #{}, 'n' => 0},
    {_, FinalState} = mixed_hybrid_loop(1, N, 0, InitState),
    maps:get('__local__sum', FinalState).

mixed_hybrid_loop(I, N, Sum, State) when I > N ->
    ExitSA = maps:put('__local__sum', Sum, State),
    {nil, ExitSA};
mixed_hybrid_loop(I, N, Sum, State) ->
    Sum1 = Sum + 1,
    N0 = maps:get('n', State),
    State1 = maps:put('n', N0 + 1, State),
    mixed_hybrid_loop(I + 1, N, Sum1, State1).

%% @doc Idiomatic Erlang baseline for the mixed local + field mutation pattern.
-spec mixed_native(non_neg_integer()) -> integer().
mixed_native(N) ->
    mixed_native_loop(1, N, 0, 0).

mixed_native_loop(I, N, Sum, _FieldN) when I > N -> Sum;
mixed_native_loop(I, N, Sum, FieldN) ->
    mixed_native_loop(I + 1, N, Sum + 1, FieldN + 1).
