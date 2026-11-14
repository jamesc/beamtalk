#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% BT-4840: cost of `beamtalk_class_metadata:insert/5` (a class load) under ONE
%% hot superclass as that superclass's subclass count N grows, plus the cost of
%% `match_subclasses/1` (subclass query) at that N (must not regress).
%%
%%   insert_us   mean microseconds per `insert/5` of a fresh subclass of Hot
%%   query_us    mean microseconds per `match_subclasses(Hot)` (returns N names)
%% Linear growth of insert_us with N means the subclass index insert scans the
%% superclass's existing edges.
%%
%% Run after `rebar3 compile` (from runtime/):
%%   escript perf/bench_subclass_index.escript
-mode(compile).

main(_) ->
    Root = filename:dirname(filename:dirname(filename:absname(escript:script_name()))),
    [code:add_pathz(P) || P <- filelib:wildcard(filename:join([Root, "_build", "default", "lib", "*", "ebin"]))],
    ok = beamtalk_class_metadata:new(),
    io:format("~8s ~12s ~12s~n", ["N", "insert_us", "query_us"]),
    lists:foldl(fun(N, Have) -> populate(Have, N), report(N), N end, 0, [100, 1000, 5000, 20000]).

populate(From, To) ->
    [beamtalk_class_metadata:insert(cls(I), mod, [], 'Hot', false) || I <- lists:seq(From + 1, To)],
    ok.

report(N) ->
    Reps = 200,
    {T, _} = timer:tc(fun() ->
        [beamtalk_class_metadata:insert(cls(1000000 + I), mod, [], 'Hot', false)
         || I <- lists:seq(1, Reps)]
    end),
    %% Drop the probe classes so the next N is exact.
    [beamtalk_class_metadata:delete(cls(1000000 + I)) || I <- lists:seq(1, Reps)],
    QReps = 20,
    {QT, _} = timer:tc(fun() ->
        [beamtalk_class_metadata:match_subclasses('Hot') || _ <- lists:seq(1, QReps)]
    end),
    io:format("~8w ~12.2f ~12.1f~n", [N, T / Reps, QT / QReps]).

cls(I) -> list_to_atom("BenchCls" ++ integer_to_list(I)).
