%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime benchmark — block state-threading and maybe_await overhead
%%%
%%% @doc Erlang simulations of Beamtalk's block state-threading patterns.
%%%
%%% Beamtalk blocks that mutate local variables use a "StateAcc" calling
%%% convention: local vars are packed/unpacked from a map on each iteration.
%%% This module provides both the naive Erlang equivalent and the StateAcc
%%% simulation so the overhead can be measured directly.
%%%
%%% Also measures `beamtalk_future:maybe_await/1` overhead: the codegen wraps
%%% every arithmetic operand in a maybe_await call (in case the value is a
%%% future), adding 2 extra calls per binary operation.

-module(bench_block_threading).

-export([
    %% timesRepeat: equivalents
    sum_native/1,
    sum_stateacc/1,
    sum_stateacc_maybe_await/1,
    sum_procdict/1,
    sum_direct_params/1,
    sum_direct_params_maybe_await/1,
    %% scale loop: multiply-by-literal, 1 variable + 1 literal per op
    scale_native/1,
    scale_direct_params_all_wrapped/1,
    scale_direct_params_literal_opt/1,
    %% collect: / lists:map equivalents
    double_native/1,
    double_stateacc_block/1,
    %% inject:into: equivalents
    fold_native/1,
    fold_stateacc_block/1
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

%% @doc Like sum_stateacc but also wraps each arithmetic operand in maybe_await,
%% simulating the full generated output including the future-await guard on
%% every operand.
%%
%% Generated Core Erlang actually emits:
%%   let NewSum = call 'erlang':'+'(
%%       call 'beamtalk_future':'maybe_await'(Sum),
%%       call 'beamtalk_future':'maybe_await'(1)) in ...
-spec sum_stateacc_maybe_await(non_neg_integer()) -> integer().
sum_stateacc_maybe_await(N) ->
    InitState = #{'__local__sum' => 0},
    {_, FinalState} = sum_stateacc_maybe_await_loop(N, InitState),
    maps:get('__local__sum', FinalState).

sum_stateacc_maybe_await_loop(0, StateAcc) ->
    {nil, StateAcc};
sum_stateacc_maybe_await_loop(I, StateAcc) ->
    Sum = maps:get('__local__sum', StateAcc),
    NewSum = beamtalk_future:maybe_await(Sum) + beamtalk_future:maybe_await(1),
    StateAcc1 = maps:put('__local__sum', NewSum, StateAcc),
    sum_stateacc_maybe_await_loop(beamtalk_future:maybe_await(I) - 1, StateAcc1).

%% @doc Uses process dictionary instead of StateAcc map.
%% put/get are O(1) hash ops on the process heap with no allocation on read.
%% No new map is created per iteration — mutations are destructive updates.
-spec sum_procdict(non_neg_integer()) -> integer().
sum_procdict(N) ->
    OldSum = get('__local__sum'),
    try
        put('__local__sum', 0),
        sum_procdict_loop(N),
        get('__local__sum')
    after
        case OldSum of
            undefined -> erase('__local__sum');
            _         -> put('__local__sum', OldSum)
        end
    end.

sum_procdict_loop(0) -> ok;
sum_procdict_loop(I) ->
    put('__local__sum', get('__local__sum') + 1),
    sum_procdict_loop(I - 1).

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

%% @doc Like sum_direct_params but also wraps arithmetic in maybe_await,
%% simulating the full generated output (BT-1275 + maybe_await guards).
-spec sum_direct_params_maybe_await(non_neg_integer()) -> integer().
sum_direct_params_maybe_await(N) ->
    {_, FinalState} = sum_direct_params_await_loop(N, 0),
    maps:get('__local__sum', FinalState).

sum_direct_params_await_loop(0, Sum) ->
    ExitSA = maps:put('__local__sum', Sum, maps:new()),
    {nil, ExitSA};
sum_direct_params_await_loop(I, Sum) ->
    Sum1 = beamtalk_future:maybe_await(Sum) + beamtalk_future:maybe_await(1),
    sum_direct_params_await_loop(beamtalk_future:maybe_await(I) - 1, Sum1).

%%====================================================================
%% Scale loop: multiply-by-literal
%%
%% This is the best-case benchmark for the BT-1286 literal-skipping
%% optimisation: every binary op has exactly one variable operand and
%% one literal operand, so BT-1286 eliminates 50% of the maybe_await
%% calls (1 saved out of 2 per op).
%%
%%   N timesRepeat: [:i | result := result * 2]
%%
%% Three variants:
%%   scale_native              — idiomatic Erlang, zero overhead
%%   scale_direct_params_all_wrapped  — BT-1275 shape, ALL operands wrapped
%%                                      (pre-BT-1286 codegen)
%%   scale_direct_params_literal_opt  — BT-1275 + BT-1286: literal 2 not wrapped
%%                                      (post-BT-1286 codegen)
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

%% @doc Pre-BT-1286: all operands wrapped in maybe_await.
%%
%% Simulates (result := I * 2 each iteration):
%%   let Result1 = call 'erlang':'*'(
%%       call 'beamtalk_future':'maybe_await'(I),
%%       call 'beamtalk_future':'maybe_await'(2)) in
%%   apply 'repeat'/2 (call 'erlang':'-'(
%%       call 'beamtalk_future':'maybe_await'(I),
%%       call 'beamtalk_future':'maybe_await'(1)), Result1)
%%
%% 4 maybe_await calls per iteration.
-spec scale_direct_params_all_wrapped(non_neg_integer()) -> integer().
scale_direct_params_all_wrapped(N) ->
    {_, FinalState} = scale_all_wrapped_loop(N, 0),
    maps:get('__local__result', FinalState).

scale_all_wrapped_loop(0, Result) ->
    {nil, maps:put('__local__result', Result, maps:new())};
scale_all_wrapped_loop(I, _Result) ->
    Result1 = beamtalk_future:maybe_await(I) * beamtalk_future:maybe_await(2),
    scale_all_wrapped_loop(beamtalk_future:maybe_await(I) - beamtalk_future:maybe_await(1), Result1).

%% @doc Post-BT-1286: literal operands (2, 1) not wrapped.
%%
%% Simulates (result := I * 2 each iteration, literals skip maybe_await):
%%   let Result1 = call 'erlang':'*'(
%%       call 'beamtalk_future':'maybe_await'(I),
%%       2) in                                         %% literal — no wrap
%%   apply 'repeat'/2 (call 'erlang':'-'(
%%       call 'beamtalk_future':'maybe_await'(I),
%%       1), Result1)                                  %% literal — no wrap
%%
%% 2 maybe_await calls per iteration (50% reduction vs all_wrapped).
-spec scale_direct_params_literal_opt(non_neg_integer()) -> integer().
scale_direct_params_literal_opt(N) ->
    {_, FinalState} = scale_literal_opt_loop(N, 0),
    maps:get('__local__result', FinalState).

scale_literal_opt_loop(0, Result) ->
    {nil, maps:put('__local__result', Result, maps:new())};
scale_literal_opt_loop(I, _Result) ->
    Result1 = beamtalk_future:maybe_await(I) * 2,
    scale_literal_opt_loop(beamtalk_future:maybe_await(I) - 1, Result1).

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
        {beamtalk_future:maybe_await(X) * beamtalk_future:maybe_await(2), StateAcc}
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
        NewAcc = beamtalk_future:maybe_await(AccIn) + beamtalk_future:maybe_await(X),
        {NewAcc, StateAcc}
    end,
    {Result, _} = lists:foldl(BlockFun, {0, InitState}, List),
    Result.
