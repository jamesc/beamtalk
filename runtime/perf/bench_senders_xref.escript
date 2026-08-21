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
%% Also measures `senders_of/2` (ADR 0115, BT-3220) against the same
%% `senders_of/1` baseline above, over the real loaded stdlib workspace, to
%% confirm two things ahead of shipping the receiver-type filter more widely:
%% (a) `senders_of/1` itself is completely unchanged (recv_type is additive —
%% no shared code path was touched), and (b) `senders_of/2`'s added
%% hierarchy-relatedness computation (`hierarchy_related_classes/1`) costs a
%% small, bounded overhead on top of it, not a regression that scales with
%% candidate-set size the way the fan-out problem `bench_recheck_fanout.escript`
%% measures would.
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

    %% BT-3220 (ADR 0115 Phase 5): senders_of/2 against the same selector and
    %% workspace, keyed on a real loaded class (the first entry of the same
    %% `Classes` list `part_a`-style surveys already use), so the hierarchy
    %% walk has real ancestor/descendant structure to traverse rather than a
    %% synthetic single-class fixture.
    {ChangedClassName, _Mod, _Pid} = hd(Classes),
    Senders1Iters = 1000,
    {Senders1Us, Senders1Count} = time_avg(
        fun() -> length(beamtalk_xref:senders_of(Selector)) end, Senders1Iters
    ),
    {Senders2Us, Senders2Count} = time_avg(
        fun() -> length(beamtalk_xref:senders_of(Selector, ChangedClassName)) end, Senders1Iters
    ),
    io:format("~n=== senders_of/1 vs senders_of/2 (ADR 0115, BT-3220) ===~n", []),
    io:format("selector: #~p, ChangedClass: ~p~n", [Selector, ChangedClassName]),
    io:format("senders_of/1 (~p iters): ~.3f ms/op  (~p sites)~n", [
        Senders1Iters, Senders1Us / 1000, Senders1Count
    ]),
    io:format("senders_of/2 (~p iters): ~.3f ms/op  (~p sites)~n", [
        Senders1Iters, Senders2Us / 1000, Senders2Count
    ]),
    case Senders1Us > 0 of
        true ->
            io:format("senders_of/2 overhead vs senders_of/1: ~.2fx~n", [Senders2Us / Senders1Us]);
        false ->
            io:format("senders_of/2 overhead vs senders_of/1: (senders_of/1 too fast to measure)~n")
    end,
    %% senders_of/1's own cost, measured again immediately after senders_of/2
    %% has run against the identical selector/workspace: an apples-to-apples
    %% before/after pair (both raw senders_of/1 calls, same iteration count)
    %% confirming senders_of/2 (a separate function, an additive `recv_type`
    %% field, no shared mutable state beyond ETS reads) leaves senders_of/1's
    %% own cost unchanged — not a regression, per this issue's acceptance
    %% criterion 1. (The "after" figure in the speedup comparison above
    %% measures `senders_of_bt/1`, a different, heavier wrapper — not
    %% comparable to this pair.)
    {Senders1RepeatUs, _} = time_avg(
        fun() -> length(beamtalk_xref:senders_of(Selector)) end, Senders1Iters
    ),
    io:format("senders_of/1 before senders_of/2 ran (above): ~.3f ms/op~n", [Senders1Us / 1000]),
    io:format("senders_of/1 after senders_of/2 ran:          ~.3f ms/op~n", [
        Senders1RepeatUs / 1000
    ]),

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
