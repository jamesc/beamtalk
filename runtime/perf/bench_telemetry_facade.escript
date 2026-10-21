#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% BT-4025: per-call cost of the stdlib `Telemetry` facade's emit path against
%% a raw `telemetry:execute/3`, and against the inline-FFI spelling the
%% BT-4019 spike used (`(Erlang telemetry) execute:with:with:`, which compiles
%% to `beamtalk_erlang_proxy:direct_call/3`).
%%
%% Rows (each one emit of `[bench, bt4025, event]` with a 1-key measurements
%% map and a 1-key metadata map):
%%   raw        telemetry:execute/3
%%   runtime    beamtalk_telemetry:execute/3 (what the @primitive lowers to)
%%   facade     bt@stdlib@telemetry:'class_execute:measurements:metadata:'/4,
%%              the function a compiled static `Telemetry execute: ...` send
%%              direct-calls (sealed, stateless, `class sealed`; ADR 0129)
%%   ffi        beamtalk_erlang_proxy:direct_call(telemetry, execute, [...])
%% each with 0 handlers and with 1 no-op handler attached.
%%
%% Run after `just build`:
%%   ERL_FLAGS="+S 1" escript runtime/perf/bench_telemetry_facade.escript 31 200000
-mode(compile).
-export([noop/4]).

main([BatchesS, RepsS]) ->
    Batches = list_to_integer(BatchesS),
    Reps = list_to_integer(RepsS),
    Root = filename:dirname(filename:dirname(filename:dirname(filename:absname(escript:script_name())))),
    [
        code:add_patha(D)
     || D <- filelib:wildcard(filename:join(Root, "runtime/_build/default/lib/*/ebin")) ++
            [filename:join(Root, "runtime/apps/beamtalk_stdlib/ebin")]
    ],
    logger:set_primary_config(level, error),
    {ok, _} = application:ensure_all_started(beamtalk_stdlib),
    Facade = 'bt@stdlib@telemetry',
    {module, Facade} = code:ensure_loaded(Facade),
    Name = [bench, bt4025, event],
    M = #{count => 1},
    Md = #{key => value},
    Rows = [
        {raw, fun() -> telemetry:execute(Name, M, Md) end},
        {runtime, fun() -> beamtalk_telemetry:execute(Name, M, Md) end},
        {facade, fun() -> Facade:'class_execute:measurements:metadata:'(undefined, Name, M, Md) end},
        {ffi, fun() -> beamtalk_erlang_proxy:direct_call(telemetry, execute, [Name, M, Md]) end}
    ],
    report("0 handlers", Rows, Batches, Reps),
    ok = telemetry:attach(bench_bt4025, Name, fun ?MODULE:noop/4, #{}),
    report("1 no-op handler", Rows, Batches, Reps),
    telemetry:detach(bench_bt4025).

noop(_, _, _, _) -> ok.

report(Label, Rows, Batches, Reps) ->
    io:format("~n~s (median of ~p batches x ~p calls, ns/call)~n", [Label, Batches, Reps]),
    %% Interleave the rows batch by batch so drift hits all of them equally.
    Samples = lists:foldl(
        fun(_, Acc) ->
            maps:from_list([{K, [time_batch(F, Reps) | maps:get(K, Acc, [])]} || {K, F} <- Rows])
        end,
        #{},
        lists:seq(1, Batches)
    ),
    Raw = median(maps:get(raw, Samples)),
    [
        begin
            Med = median(maps:get(K, Samples)),
            io:format("  ~-8s ~8.1f  (~8.1f vs raw)~n", [K, Med, Med - Raw])
        end
     || {K, _} <- Rows
    ].

time_batch(F, Reps) ->
    T0 = erlang:monotonic_time(nanosecond),
    loop(F, Reps),
    (erlang:monotonic_time(nanosecond) - T0) / Reps.

loop(_, 0) -> ok;
loop(F, N) ->
    F(),
    loop(F, N - 1).

median(L) -> lists:nth((length(L) + 1) div 2, lists:sort(L)).
