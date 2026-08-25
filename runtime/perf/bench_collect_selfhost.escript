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
    bench_guard_fold(),
    bench_number_coercion(),
    bench_number_coercion_dispatch(),
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

%% --- BT-2709: guard cost at the realistic fold level (sum / inject:into:) ---
%% The tight loop above is the worst case (the add is the whole body). This
%% measures the guard where it actually lands in stdlib collection code: a
%% `lists:foldl` accumulator step — the shape `sum`/`inject:into:` compile to —
%% so the per-element list traversal is the "surrounding work" that dilutes the
%% guard's relative cost. A/B over the same list: bare `Acc + X` vs the guarded
%% form codegen now emits for the (statically non-numeric) fold accumulator.
bench_guard_fold() ->
    N = 1000000,
    Reps = 25,
    List = lists:seq(1, N),
    Bare = fun() -> lists:foldl(fun(X, Acc) -> Acc + X end, 0, List) end,
    Guard = fun() ->
        lists:foldl(
            fun(X, Acc) ->
                case is_number(Acc) of
                    true -> Acc + X;
                    false -> guard_fallback(Acc, X)
                end
            end,
            0,
            List
        )
    end,
    %% Warm + correctness gate: assert the two accumulators agree *before*
    %% timing, so a logic divergence fails loudly instead of whispering in a
    %% footer after the numbers have already printed.
    BareResult = Bare(),
    GuardResult = Guard(),
    BareResult =:= GuardResult orelse
        error({guard_fold_mismatch, BareResult, GuardResult}),
    BareUs = min_us(Reps, Bare),
    GuardUs = min_us(Reps, Guard),
    io:format("~n=== fold sum: guarded vs bare accumulator (N=~p elems, best of ~p) ===~n", [
        N, Reps
    ]),
    io:format("bare  foldl Acc+X   : ~8.1f us~n", [float(BareUs)]),
    io:format("guarded foldl       : ~8.1f us~n", [float(GuardUs)]),
    io:format("overhead            : ~.3f ns/elem | ratio ~.2fx~n", [
        (GuardUs - BareUs) * 1000 / N, GuardUs / BareUs
    ]).

%% Never reached for numeric input; present so the guard's false arm is live.
guard_fallback(A, B) -> A + B.

%% --- BT-3265 (ADR 0116): number-on-the-left coercion, against the real
%% codegen shape BT-3263 wired up (generate_binary_op in
%% crates/beamtalk-core/src/codegen/core_erlang/operators.rs), not the ADR's
%% own hand-written spike module. Two call-site shapes:
%%
%%   - `total + delta` (right operand statically numeric, e.g. `:: Number`-
%%     typed or a literal): the compile-time skip means codegen emits the
%%     exact bare BIF with no try at all — byte-for-byte the same code as
%%     before this ADR (verified by
%%     test_number_coercion_bare_bif_unaffected_for_total_plus_delta in
%%     tests/expressions.rs). This is `Bare` below, identical in shape to
%%     bench_guard/0's own `Bare` loop.
%%   - `5 + aVector` (right operand's type is genuinely unknown): codegen
%%     wraps the BIF in try/catch/is_number/send_number_coercion, mirroring
%%     test_number_coercion_try_for_literal_left_unknown_right. `CoerceTry`
%%     below hand-mirrors that exact shape (as bench_guard/0 already does
%%     for its own is_number guard) on the never-failing path, since real
%%     code at an already-dynamically-dispatched call site pays this cost on
%%     every add regardless of whether the fallback ever fires.
%%
%% The dispatch-triggering (catch actually fires) case is measured
%% separately in bench_number_coercion_dispatch/0 below, using the real
%% beamtalk_message_dispatch:send_number_coercion/4 (not a stub) — a
%% reference data point for the strictly rarer path, not a zero-cost claim.
bench_number_coercion() ->
    N = 5000000,
    Reps = 25,
    Bare = fun BareLoop(0, A) -> A; BareLoop(K, A) -> BareLoop(K - 1, A + K) end,
    %% Mirrors the generated try/catch: try the bare BIF; on badarith,
    %% re-check is_number(Right) — true means an unrelated numeric failure
    %% (re-raise unchanged), false means a genuine coercion miss (dispatch to
    %% plusFromNumber:). Every iteration here adds two real integers, so the
    %% try always succeeds and the catch handler never runs.
    CoerceTry =
        fun CoerceLoop(0, A) ->
               A;
           CoerceLoop(K, A) ->
               A2 =
                   try
                       A + K
                   of
                       Result -> Result
                   catch
                       Type:Error:Stack ->
                           case {Type, Error} of
                               {error, badarith} when true ->
                                   case is_number(K) of
                                       true -> erlang:raise(Type, Error, Stack);
                                       false ->
                                           beamtalk_message_dispatch:send_number_coercion(
                                               K, 'plusFromNumber:', [A], '+'
                                           )
                                   end;
                               _ ->
                                   erlang:raise(Type, Error, Stack)
                           end
                   end,
               CoerceLoop(K - 1, A2)
        end,
    %% sanity: both produce identical output
    BareResult = Bare(N, 0),
    CoerceResult = CoerceTry(N, 0),
    BareResult =:= CoerceResult orelse
        error({number_coercion_mismatch, BareResult, CoerceResult}),
    BareUs = min_us(Reps, fun() -> Bare(N, 0) end),
    CoerceUs = min_us(Reps, fun() -> CoerceTry(N, 0) end),
    io:format("~n=== number coercion: try/catch vs bare (N=~p adds/loop, best of ~p) ===~n", [N, Reps]),
    io:format("bare  erlang:'+'  (total + delta)     : ~8.1f us/loop~n", [float(BareUs)]),
    io:format("coercion try/catch (5 + aVector, happy): ~8.1f us/loop~n", [float(CoerceUs)]),
    io:format("overhead                               : ~.3f ns/add | ratio ~.2fx~n",
              [(CoerceUs - BareUs) * 1000 / N, CoerceUs / BareUs]).

%% --- BT-3265 (ADR 0116): reference data point for the dispatch-triggering
%% (catch actually fires) case — the ADR's own REPL example, `5 + "not a
%% vector"` (§ REPL example): String implements no `plusFromNumber:`, so
%% every call raises does_not_understand with the added hint. Exercises the
%% real send_number_coercion/4 (RightClass lookup, DNU class/selector match,
%% hint formatting, re-raise), not a stub — orders of magnitude slower than
%% the happy path above (exception raise/unwind dominates), which is exactly
%% the point: this cost is paid only on the already-failing path, never on
%% the try/catch's own happy path measured by bench_number_coercion/0.
bench_number_coercion_dispatch() ->
    N = 20000,
    Reps = 10,
    Miss = fun() -> lists:foreach(fun coercion_miss/1, lists:seq(1, N)) end,
    Miss(),   %% warm
    Us = min_us(Reps, Miss),
    io:format("~n=== number coercion: DNU+hint dispatch reference (N=~p, best of ~p) ===~n", [N, Reps]),
    io:format("send_number_coercion/4 (5 + \"not a vector\") : ~8.2f us/op~n", [Us / N]).

coercion_miss(K) ->
    try
        beamtalk_message_dispatch:send_number_coercion(<<"not a vector">>, 'plusFromNumber:', [K], '+')
    catch
        error:_ -> ok
    end.

min_us(Reps, F) ->
    lists:min([element(1, timer:tc(F)) || _ <- lists:seq(1, Reps)]).
