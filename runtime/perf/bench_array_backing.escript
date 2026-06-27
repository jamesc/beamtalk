#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% Array backing perf check (BT-2696): the current canonical index->map backing
%% (ADR 0090, chosen for =:=/phash2 correctness) vs the alternatives a Vector /
%% bit-trie would resemble. Measures single random at:/at:put: cost (persistent:
%% each op runs on the original structure, result discarded).
-mode(compile).

main(_) ->
    code:add_pathsa(filelib:wildcard("_build/default/lib/*/ebin")),
    code:add_pathsa(filelib:wildcard("apps/*/ebin")),
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    timer:sleep(300),
    lists:foreach(fun(B) -> code:ensure_loaded(list_to_atom(filename:basename(B, ".beam"))) end,
                  filelib:wildcard("apps/beamtalk_stdlib/ebin/bt@stdlib@*.beam")),
    timer:sleep(800),
    lists:foreach(fun bench/1, [1000, 100000]),
    ok.

bench(N) ->
    K = 200000,
    List = lists:seq(1, N),
    BtArr = beamtalk_array:from_list(List),          %% current: tagged index->map
    RawMap = maps:from_list([{I-1, I} || I <- List]), %% raw 0-based map
    Arr = array:from_list(List),                      %% erlang :array module
    Tup = list_to_tuple(List),                        %% raw tuple
    Idx = [rand:uniform(N) || _ <- lists:seq(1, K)],  %% 1-based random indices
    io:format("~n=== N=~p, ~p random ops ===~n", [N, K]),
    %% READ (at:)
    rd("at: beamtalk_array ", fun(I) -> beamtalk_array:at(BtArr, I) end, Idx, K),
    rd("at: raw maps:get   ", fun(I) -> maps:get(I-1, RawMap) end, Idx, K),
    rd("at: :array module  ", fun(I) -> array:get(I-1, Arr) end, Idx, K),
    rd("at: tuple element  ", fun(I) -> element(I, Tup) end, Idx, K),
    rd("at: list lists:nth ", fun(I) -> lists:nth(I, List) end, Idx, K),
    %% WRITE (at:put:) — persistent single update
    wr("at:put: beamtalk_array", fun(I) -> beamtalk_array:at_put(BtArr, I, 0) end, Idx, K),
    wr("at:put: raw maps:put  ", fun(I) -> maps:put(I-1, 0, RawMap) end, Idx, K),
    wr("at:put: :array set    ", fun(I) -> array:set(I-1, 0, Arr) end, Idx, K),
    wr("at:put: tuple setelem ", fun(I) -> setelement(I, Tup, 0) end, Idx, K).

rd(Label, F, Idx, K) ->
    F(1),
    {Us, _} = timer:tc(fun() -> lists:foreach(F, Idx) end),
    io:format("  ~s ~7.3f ns/op~n", [Label, (Us * 1000) / K]).
wr(Label, F, Idx, K) -> rd(Label, F, Idx, K).
