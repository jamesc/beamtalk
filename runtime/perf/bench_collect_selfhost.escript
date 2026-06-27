#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% Benchmark: self-hosting cost of pure-BT enumeration vs native primitives
%% (BT-2692 / BT-2708 de-primitivization spike).
%%
%% Measures, on a List receiver:
%%   - collect:/select: — native List @primitive vs the pure-BT Collection
%%     versions (do:/inject:-based, dispatched as bt@stdlib@collection).
%%   - sum — native beamtalk_collection:sum/1 (lists:foldl) vs the pure-BT
%%     inject:into: + `+` block that a self-hosted sum compiles to.
%%
%% Both variants are asserted to produce identical output. Run from runtime/
%% after `just build`:
%%
%%   escript perf/bench_collect_selfhost.escript
-mode(compile).

main(_) ->
    code:add_pathsa(filelib:wildcard("_build/default/lib/*/ebin")),
    code:add_pathsa(filelib:wildcard("apps/*/ebin")),
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    {ok, _} = application:ensure_all_started(beamtalk_compiler),
    timer:sleep(500),
    lists:foreach(fun(B) -> code:ensure_loaded(list_to_atom(filename:basename(B, ".beam"))) end,
                  filelib:wildcard("apps/beamtalk_stdlib/ebin/bt@stdlib@*.beam")),
    timer:sleep(1500),
    Blk = fun(X) -> X * 2 end,
    Pred = fun(X) -> X rem 2 =:= 0 end,
    lists:foreach(fun(N) -> run_size(N, Blk, Pred) end, [1000, 100000]),
    bench_reduce(),
    bench_guard(),
    ok.

run_size(N, Blk, Pred) ->
    L = lists:seq(1, N),
    Iters = case N >= 100000 of true -> 50; false -> 2000 end,
    %% sanity: both produce identical output
    Same = ('bt@stdlib@list':'collect:'(L, Blk) =:= 'bt@stdlib@collection':'collect:'(L, Blk)),
    %% warm
    'bt@stdlib@list':'collect:'(L, Blk), 'bt@stdlib@collection':'collect:'(L, Blk),
    {NatC, _} = timer:tc(fun() -> rep(Iters, fun() -> 'bt@stdlib@list':'collect:'(L, Blk) end) end),
    {PbtC, _} = timer:tc(fun() -> rep(Iters, fun() -> 'bt@stdlib@collection':'collect:'(L, Blk) end) end),
    {NatS, _} = timer:tc(fun() -> rep(Iters, fun() -> 'bt@stdlib@list':'select:'(L, Pred) end) end),
    {PbtS, _} = timer:tc(fun() -> rep(Iters, fun() -> 'bt@stdlib@collection':'select:'(L, Pred) end) end),
    io:format("~n=== N=~p, ~p iters (collect: outputs identical: ~p) ===~n", [N, Iters, Same]),
    io:format("collect:  native ~8.1f us/op | pureBT ~8.1f us/op | ratio ~.2fx~n",
              [NatC/Iters, PbtC/Iters, PbtC/NatC]),
    io:format("select:   native ~8.1f us/op | pureBT ~8.1f us/op | ratio ~.2fx~n",
              [NatS/Iters, PbtS/Iters, PbtS/NatS]).

rep(0, _) -> ok;
rep(K, F) -> F(), rep(K-1, F).

%% --- appended: reducing-op comparison (sum) ---
%% native sum = beamtalk_collection:sum/1 (lists:foldl).
%% pure-BT sum = inject:into: with a `+` block (what self-hosted sum compiles to).
bench_reduce() ->
    lists:foreach(fun(N) ->
        L = lists:seq(1, N),
        Iters = case N >= 100000 of true -> 50; false -> 2000 end,
        Add = fun(Acc, E) -> Acc + E end,
        Same = (beamtalk_collection:sum(L) =:= 'bt@stdlib@list':'inject:into:'(L, 0, Add)),
        beamtalk_collection:sum(L), 'bt@stdlib@list':'inject:into:'(L, 0, Add),
        {Nat, _} = timer:tc(fun() -> rep(Iters, fun() -> beamtalk_collection:sum(L) end) end),
        {Pbt, _} = timer:tc(fun() -> rep(Iters, fun() -> 'bt@stdlib@list':'inject:into:'(L, 0, Add) end) end),
        io:format("sum  N=~6w  native ~8.2f us/op | pureBT(inject) ~8.2f us/op | ratio ~.2fx  (same: ~p)~n",
                  [N, Nat/Iters, Pbt/Iters, Pbt/Nat, Same])
    end, [1000, 100000]).

%% --- BT-2709: arithmetic-operator guard vs bare `erlang:'+'` ---
%% `+ - * /` are now dispatchable messages. For a statically-numeric receiver
%% (literal, `self` in Integer/Float, `:: Number` param, `self.field`) codegen
%% keeps the bare BIF; otherwise it emits a runtime `is_number` guard that picks
%% the BIF for numbers and `beamtalk_message_dispatch:send/3` for objects. This
%% measures the per-add cost of that guard against the bare BIF in a tight loop —
%% the honest upper bound, since real code with unknown receivers also evaluates
%% the operands and the surrounding expression.
bench_guard() ->
    N = 5000000,
    Reps = 25,
    Bare = fun BareLoop(0, A) -> A; BareLoop(K, A) -> BareLoop(K - 1, A + K) end,
    %% Mirrors the generated guard: is_number(Lhs) ? BIF : dispatch.
    Guard = fun GuardLoop(0, A) -> A;
                GuardLoop(K, A) ->
                    A2 = case is_number(A) of
                             true -> A + K;
                             false -> guard_fallback(A, K)
                         end,
                    GuardLoop(K - 1, A2)
            end,
    Bare(N, 0), Guard(N, 0),   %% warm
    BareUs = min_us(Reps, fun() -> Bare(N, 0) end),
    GuardUs = min_us(Reps, fun() -> Guard(N, 0) end),
    io:format("~n=== arithmetic guard vs bare (N=~p adds/loop, best of ~p) ===~n", [N, Reps]),
    io:format("bare  erlang:'+'  : ~8.1f us/loop~n", [float(BareUs)]),
    io:format("guarded is_number : ~8.1f us/loop~n", [float(GuardUs)]),
    io:format("overhead          : ~.3f ns/add | ratio ~.2fx~n",
              [(GuardUs - BareUs) * 1000 / N, GuardUs / BareUs]).

%% Never reached for numeric input; present so the guard's false arm is live.
guard_fallback(A, B) -> A + B.

min_us(Reps, F) ->
    lists:min([element(1, timer:tc(F)) || _ <- lists:seq(1, Reps)]).
