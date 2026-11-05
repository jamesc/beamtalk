#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% BT-4416: cost of an `equals:` send to a primitive receiver.
%%
%% `a equals: b` compiles to `beamtalk_message_dispatch:send/3`. Before
%% BT-4416 that walked the receiver's whole dispatch chain (for a Binary:
%% bt@stdlib@binary -> collection -> value -> object, an extension-registry
%% probe at each level) to reach `Object>>equals:`, which is `=:=`. The fast
%% path answers a raw hit with `true` and, when nothing on the chain overrides
%% `equals:`, a raw miss with `false`.
%%
%% "before" calls the stdlib module's `dispatch/3` directly, which is the
%% chain the old `send/3` ran after `classify_receiver/1` and
%% `module_for_value/2`; "after" is the real `send/3`. "raw" is `=:=` alone.
%%
%% Run from `runtime/` after `just build`:
%%   escript perf/bench_equals_fast_path.escript
-mode(compile).

main(_) ->
    code:add_pathsa(filelib:wildcard("_build/default/lib/*/ebin")),
    code:add_pathsa(filelib:wildcard("apps/*/ebin")),
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    timer:sleep(300),
    lists:foreach(
        fun(B) -> code:ensure_loaded(list_to_atom(filename:basename(B, ".beam"))) end,
        filelib:wildcard("apps/beamtalk_stdlib/ebin/bt@stdlib@*.beam")
    ),
    timer:sleep(800),
    A = <<"content-type">>,
    Miss = <<"content-length">>,
    Hit = binary:copy(A),
    bench("Binary, miss", 'bt@stdlib@binary', A, Miss),
    bench("Binary, hit ", 'bt@stdlib@binary', A, Hit),
    bench("Integer, miss", 'bt@stdlib@integer', 42, 43),
    bench("Symbol, miss ", 'bt@stdlib@symbol', foo, bar),
    ok.

bench(Label, Mod, X, Y) ->
    K = 200000,
    Before = fun() -> Mod:dispatch('equals:', [Y], X) end,
    After = fun() -> beamtalk_message_dispatch:send(X, 'equals:', [Y]) end,
    Raw = fun() -> X =:= Y end,
    Expected = X =:= Y,
    Expected = Before(),
    Expected = After(),
    io:format("~n=== equals: ~s (~p calls) ===~n", [Label, K]),
    run("before (full dispatch chain)", Before, K),
    run("after  (send/3 fast path)   ", After, K),
    run("raw =:=                     ", Raw, K).

run(Label, F, K) ->
    F(),
    {Us, _} = timer:tc(fun() -> loop(F, K) end),
    io:format("  ~s ~10.4f us/op~n", [Label, Us / K]).

loop(_F, 0) -> ok;
loop(F, N) ->
    _ = F(),
    loop(F, N - 1).
