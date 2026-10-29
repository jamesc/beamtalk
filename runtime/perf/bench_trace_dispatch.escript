#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% BT-4103: per-dispatch cost of the trace store's always-on aggregate path.
%%
%% Rows (median ns/call):
%%   hit_dis    record_dispatch/6 on an existing key, tracing disabled
%%              (the steady-state always-on path).
%%   churn_dis  record_dispatch/6 with a never-before-seen pid per call,
%%              tracing disabled (a wave of new short-lived actors).
%%   hit_en     record_dispatch/6 on an existing key, tracing enabled.
%%   churn_en   record_dispatch/6 with a never-before-seen pid per call,
%%              tracing enabled.
%%   handler    record_dispatch_stop/2 end to end (the events-on recording
%%              path, BT-4113; it was the handle_dispatch_stop/4 telemetry
%%              handler: map access, time conversion, record + enabled
%%              check), hit.
%%
%% Churn pids are pre-made dead pids (spawn + immediate exit in setup, outside
%% the timed region); the write path never checks liveness. Each churn batch
%% uses fresh pids so every call takes the key-miss path.
%%
%% Run after `just build`:
%%   ERL_FLAGS="+S 1" escript runtime/perf/bench_trace_dispatch.escript
-mode(compile).

main(_) ->
    Root = filename:dirname(
        filename:dirname(filename:dirname(filename:absname(escript:script_name())))
    ),
    [
        code:add_patha(D)
     || D <- filelib:wildcard(filename:join(Root, "runtime/_build/default/lib/*/ebin"))
    ],
    logger:set_primary_config(level, error),
    {ok, _} = application:ensure_all_started(telemetry),
    {ok, _} = beamtalk_trace_store:start_link(),
    beamtalk_trace_store:disable(),
    beamtalk_trace_store:clear(),

    HitPid = spawn(fun() -> receive stop -> ok end end),
    beamtalk_trace_store:record_dispatch(HitPid, increment, 1000, ok, sync, 'Counter'),
    Meta = #{pid => HitPid, selector => increment, mode => sync, outcome => ok, class => 'Counter'},

    io:format("~nrecord_dispatch steady state (median of 31 batches x 200000 calls, ns/call)~n"),
    report(
        [
            {hit_dis, fun() ->
                beamtalk_trace_store:record_dispatch(HitPid, increment, 1000, ok, sync, 'Counter')
            end},
            {handler, fun() ->
                beamtalk_trace_store:record_dispatch_stop(Meta, 1000)
            end}
        ],
        31,
        200000
    ),

    io:format("~nchurn: never-before-seen pid per call (median of 9 batches x 20000 calls, ns/call)~n"),
    report_churn(disabled, 9, 20000),

    beamtalk_trace_store:enable(),
    io:format("~ntracing enabled (median of 31 batches x 200000 calls, ns/call)~n"),
    report(
        [
            {hit_en, fun() ->
                beamtalk_trace_store:record_dispatch(HitPid, increment, 1000, ok, sync, 'Counter')
            end}
        ],
        31,
        200000
    ),
    io:format("~nchurn, tracing enabled (median of 9 batches x 20000 calls, ns/call)~n"),
    report_churn(enabled, 9, 20000),

    HitPid ! stop,
    init:stop().

report(Rows, Batches, Reps) ->
    Samples = lists:foldl(
        fun(_, Acc) ->
            maps:from_list([{K, [time_batch(F, Reps) | maps:get(K, Acc, [])]} || {K, F} <- Rows])
        end,
        #{},
        lists:seq(1, Batches)
    ),
    [
        io:format("  ~-10s ~8.1f~n", [K, median(maps:get(K, Samples))])
     || {K, _} <- Rows
    ].

report_churn(Mode, Batches, Reps) ->
    Label =
        case Mode of
            disabled -> churn_dis;
            enabled -> churn_en
        end,
    Samples = [
        begin
            Pids = make_dead_pids(Reps),
            T0 = erlang:monotonic_time(nanosecond),
            lists:foreach(
                fun(P) ->
                    beamtalk_trace_store:record_dispatch(P, increment, 1000, ok, sync, 'Counter')
                end,
                Pids
            ),
            (erlang:monotonic_time(nanosecond) - T0) / Reps
        end
     || _ <- lists:seq(1, Batches)
    ],
    io:format("  ~-10s ~8.1f~n", [Label, median(Samples)]).

make_dead_pids(N) ->
    Parent = self(),
    Ref = make_ref(),
    [spawn(fun() -> Parent ! {Ref, self()} end) || _ <- lists:seq(1, N)],
    [receive {Ref, P} -> P end || _ <- lists:seq(1, N)].

time_batch(F, Reps) ->
    T0 = erlang:monotonic_time(nanosecond),
    loop(F, Reps),
    (erlang:monotonic_time(nanosecond) - T0) / Reps.

loop(_, 0) -> ok;
loop(F, N) ->
    F(),
    loop(F, N - 1).

median(L) -> lists:nth((length(L) + 1) div 2, lists:sort(L)).
