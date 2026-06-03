#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% Benchmark: SystemNavigation sendersOf: — legacy source-scan vs xref index
%% (ADR 0087 Phase 3, BT-2299).
%%
%% Measures the wall-clock cost of resolving the senders of a widely-sent
%% selector across the full loaded stdlib workspace (81 classes, >1000 methods)
%% two ways:
%%
%%   - "before" — the legacy path: walk every loaded class, fetch each method's
%%     source, and ask the compiler (`find_senders_in_source/2`) to find matching
%%     sends. This is exactly what `sendersOf:` did pre-migration.
%%   - "after"  — the migrated path: a single `beamtalk_xref:senders_of_bt/1`
%%     ETS read.
%%
%% Run from the `runtime/` directory after `just build`:
%%
%%   escript perf/bench_senders_xref.escript
%%
%% Requires the compiler port binary (built into target/debug) for the "before"
%% measurement; the "after" measurement needs only the runtime app.

-mode(compile).

main(_) ->
    code:add_pathsa(filelib:wildcard("_build/default/lib/*/ebin")),
    code:add_pathsa(filelib:wildcard("apps/*/ebin")),
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    {ok, _} = application:ensure_all_started(beamtalk_compiler),
    timer:sleep(500),
    %% Force-load the compiled stdlib modules so their on_load hooks register
    %% the full 81-class workspace (the app start alone only brings up the
    %% bootstrap stubs). Mirrors what `just build` + a REPL session would load.
    lists:foreach(
        fun(Beam) ->
            Mod = list_to_atom(filename:basename(Beam, ".beam")),
            code:ensure_loaded(Mod)
        end,
        filelib:wildcard("apps/beamtalk_stdlib/ebin/bt@stdlib@*.beam")
    ),
    %% Let stdlib classes finish loading + indexing.
    timer:sleep(1500),

    Selector = 'asString',
    Classes = beamtalk_class_registry:live_class_entries(),
    io:format("Workspace: ~p loaded classes~n", [length(Classes)]),

    %% Warm both paths once (compiler cache, ETS read concurrency).
    _ = legacy_senders(Selector, Classes),
    _ = beamtalk_xref:senders_of_bt(Selector),

    BeforeIters = 5,
    AfterIters = 1000,

    {BeforeUs, BeforeCount} = time_avg(
        fun() -> length(legacy_senders(Selector, Classes)) end, BeforeIters
    ),
    {AfterUs, AfterCount} = time_avg(
        fun() ->
            #{indexed := Rows} = beamtalk_xref:senders_of_bt(Selector),
            length(Rows)
        end,
        AfterIters
    ),

    io:format("~n=== sendersOf: #~p ===~n", [Selector]),
    io:format("before (source-scan, ~p iters): ~.3f ms/op  (~p hits)~n", [
        BeforeIters, BeforeUs / 1000, BeforeCount
    ]),
    io:format("after  (xref ETS,    ~p iters): ~.3f ms/op  (~p hits)~n", [
        AfterIters, AfterUs / 1000, AfterCount
    ]),
    case AfterUs > 0 of
        true -> io:format("speedup: ~.1fx~n", [BeforeUs / AfterUs]);
        false -> io:format("speedup: (after too fast to measure)~n")
    end,

    %% BT-2384: isolate the loaded-class-set computation that the miss-policy
    %% partition depends on. The old path (`live_class_entries/0`) issues one
    %% gen_server:call per loaded class; the new path (`loaded_class_entries/0`)
    %% is a single ETS scan plus local is_process_alive/1 filtering.
    {RegWalkUs, _} = time_avg(
        fun() -> length(beamtalk_class_registry:live_class_entries()) end, 200
    ),
    {EtsReadUs, _} = time_avg(
        fun() -> length(beamtalk_class_registry:loaded_class_entries()) end, 1000
    ),
    io:format("~n=== loaded-class set (miss-partition input) ===~n", []),
    io:format("registry walk  (live_class_entries/0,   200 iters): ~.3f ms/op~n", [
        RegWalkUs / 1000
    ]),
    io:format("ETS read       (loaded_class_entries/0, 1000 iters): ~.3f ms/op~n", [
        EtsReadUs / 1000
    ]),
    ok.

%% Average microseconds per iteration, plus the last result's count.
time_avg(Fun, Iters) ->
    {TotalUs, LastCount} = lists:foldl(
        fun(_, {AccUs, _}) ->
            {Us, Count} = timer:tc(Fun),
            {AccUs + Us, Count}
        end,
        {0, 0},
        lists:seq(1, Iters)
    ),
    {TotalUs / Iters, LastCount}.

%% Legacy pre-migration path: walk every class (instance-side methods only),
%% parse each method source via the compiler, count matching send lines.
legacy_senders(Selector, Classes) ->
    SelBin = atom_to_binary(Selector, utf8),
    lists:foldl(
        fun({_Name, _Mod, Pid}, Acc) ->
            InstSels = safe_call(fun() -> beamtalk_object_class:methods(Pid) end, []),
            scan_methods(Pid, InstSels, SelBin) ++ Acc
        end,
        [],
        Classes
    ).

scan_methods(Pid, Selectors, SelBin) ->
    lists:foldl(
        fun(Sel, Acc) ->
            case method_source(Pid, Sel) of
                nil -> Acc;
                <<>> -> Acc;
                Source ->
                    Lines = safe_call(
                        fun() ->
                            case beamtalk_compiler:find_senders_in_source(Source, SelBin) of
                                {ok, Ls} -> Ls;
                                Ls when is_list(Ls) -> Ls;
                                _ -> []
                            end
                        end,
                        []
                    ),
                    [{Sel, L} || L <- Lines] ++ Acc
            end
        end,
        [],
        Selectors
    ).

method_source(Pid, Sel) ->
    safe_call(
        fun() ->
            case beamtalk_object_class:method(Pid, Sel) of
                Info when is_map(Info) -> maps:get('__source__', Info, nil);
                _ -> nil
            end
        end,
        nil
    ).

safe_call(Fun, Default) ->
    try
        Fun()
    catch
        _:_ -> Default
    end.
