#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% BT-4831: cost of `beamtalk_class_metadata:insert/5` (a class load) as the
%% class-method funs table grows, plus the per-lookup cost of the dispatch-side
%% `lookup_class_method_fun/2` (must not regress).
%%
%% For each table size N (classes already holding funs) it reports:
%%   insert_us   mean microseconds per `insert/5` of a fresh class
%%   lookup_ns   mean nanoseconds per `lookup_class_method_fun/2` hit
%% Linear growth of insert_us with N means the load scans the whole funs table.
%%
%% Run after `rebar3 compile` (from runtime/):
%%   escript perf/bench_class_load_funs.escript
-mode(compile).

main(_) ->
    Root = filename:dirname(filename:dirname(filename:absname(escript:script_name()))),
    [code:add_pathz(P) || P <- filelib:wildcard(filename:join([Root, "_build", "default", "lib", "*", "ebin"]))],
    ok = beamtalk_class_metadata:new(),
    io:format("~8s ~12s ~12s~n", ["N", "insert_us", "lookup_ns"]),
    lists:foldl(fun(N, Have) -> populate(Have, N), report(N), N end, 0, [100, 1000, 5000, 20000]).

populate(From, To) ->
    [begin
         Name = cls(I),
         beamtalk_class_metadata:insert(Name, mod, [], sup(I), false),
         [beamtalk_class_metadata:put_class_method_fun(Name, sel(S), #{block => fun() -> ok end, arity => 0})
          || S <- lists:seq(1, 3)],
         beamtalk_class_metadata:set_runtime_class_methods(Name, true)
     end || I <- lists:seq(From + 1, To)],
    ok.

report(N) ->
    Reps = 200,
    {T, _} = timer:tc(fun() ->
        [beamtalk_class_metadata:insert(cls(1000000 + I), mod, [], sup(1000000 + I), false)
         || I <- lists:seq(1, Reps)]
    end),
    LReps = 200000,
    {LT, _} = timer:tc(fun() ->
        [beamtalk_class_metadata:lookup_class_method_fun(cls(1 + (I rem N)), sel(1))
         || I <- lists:seq(1, LReps)]
    end),
    io:format("~8w ~12.2f ~12.1f~n", [N, T / Reps, LT * 1000 / LReps]).

cls(I) -> list_to_atom("BenchCls" ++ integer_to_list(I)).
sel(I) -> list_to_atom("sel" ++ integer_to_list(I)).
sup(I) -> list_to_atom("BenchSup" ++ integer_to_list(I)).
