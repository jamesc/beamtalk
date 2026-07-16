#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% Benchmark: leaf-change re-check fan-out (ADR 0107 Phase A, BT-2856 follow-up,
%% BT-2873).
%%
%% Measures whether `beamtalk_recheck:trigger_leaf_change/1`'s "recompile
%% every live class" sweep (fired automatically once per distinct superclass
%% that gains its first subclass, via
%% `beamtalk_repl_loader:maybe_trigger_leaf_change_recheck/1`) is a real cost
%% concern for a bulk `:load dir` of a project with a deep class hierarchy,
%% following BT-2781's "measure, then decide" methodology
%% (`bench_recheck_fanout.escript`).
%%
%% Two parts:
%%
%%   Part A ("real hierarchy shape") — walks the full loaded stdlib class
%%   hierarchy and counts how many classes have at least one direct
%%   subclass. Every such class underwent exactly one leaf -> non-leaf
%%   transition the first time its first subclass was loaded — so this count
%%   is the exact number of `trigger_leaf_change/1` sweeps a from-scratch
%%   bulk load of this workspace already fires today. Also checks finding #2
%%   (BT-2873): whether any single source file declares more than one class,
%%   since a single-class-per-file convention means a single reload/activate
%%   event can never make *two* superclasses newly-non-leaf at once (the
%%   scenario `maybe_trigger_leaf_change_recheck/1`'s per-superclass
%%   `lists:foreach` pays N independent sweeps for).
%%
%%   Part B ("sweep cost vs. live-class count") — drives the real
%%   `beamtalk_recheck:trigger_leaf_change/1` orchestration (real compiler
%%   port, real `beamtalk_workspace_meta`) with a synthetic set of N live
%%   class sources (5/20/50/100/200), to measure the sweep's wall-clock cost
%%   as a function of workspace size (it re-checks every live class with a
%%   recorded source, so this should be linear in N per the module's own
%%   documented "at least as expensive as trigger_image/0" claim).
%%
%% Run from the `runtime/` directory after `just build`:
%%
%%   escript perf/bench_leaf_change_fanout.escript

-mode(compile).

main(_) ->
    code:add_pathsa(filelib:wildcard("_build/default/lib/*/ebin")),
    code:add_pathsa(filelib:wildcard("apps/*/ebin")),
    {ok, _} = application:ensure_all_started(compiler),
    {ok, _} = application:ensure_all_started(beamtalk_compiler),
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    timer:sleep(500),
    lists:foreach(
        fun(Beam) ->
            Mod = list_to_atom(filename:basename(Beam, ".beam")),
            code:ensure_loaded(Mod)
        end,
        filelib:wildcard("apps/beamtalk_stdlib/ebin/bt@stdlib@*.beam")
    ),
    timer:sleep(1500),

    part_a_real_hierarchy_shape(),
    part_b_sweep_cost_vs_size(),
    ok.

%%====================================================================
%% Part A: real stdlib hierarchy shape
%%====================================================================

part_a_real_hierarchy_shape() ->
    io:format("~n=== Part A: real hierarchy shape (loaded stdlib) ===~n", []),
    Classes = beamtalk_class_registry:live_class_entries(),
    ClassNames = [Name || {Name, _Mod, _Pid} <- Classes],
    io:format("Workspace: ~p loaded classes~n", [length(ClassNames)]),

    NonLeaf = [
        Name
     || Name <- ClassNames, beamtalk_class_registry:direct_subclasses(Name) =/= []
    ],
    io:format(
        "Classes with >= 1 direct subclass (each is exactly one\n"
        "trigger_leaf_change/1 sweep during a from-scratch bulk load): ~p of ~p (~.1f%)~n",
        [length(NonLeaf), length(ClassNames), 100 * length(NonLeaf) / max(1, length(ClassNames))]
    ),

    %% finding #2 (BT-2873): can one source file declare more than one class?
    %% If not, no single reload/activate_module event can make two
    %% superclasses newly-non-leaf at once, so
    %% maybe_trigger_leaf_change_recheck/1's per-superclass foreach never
    %% pays for more than one sweep per reload in practice.
    BtFiles = filelib:wildcard("../stdlib/src/**/*.bt"),
    {ok, SubclassRe} = re:compile("\\bsubclass:"),
    PerFileClassCounts = [
        {File, count_subclass_decls(File, SubclassRe)}
     || File <- BtFiles
    ],
    MultiClassFiles = [FC || {_File, N} = FC <- PerFileClassCounts, N > 1],
    io:format(
        "~nSource files declaring more than one class: ~p of ~p~n",
        [length(MultiClassFiles), length(PerFileClassCounts)]
    ),
    lists:foreach(
        fun({File, N}) -> io:format("  ~s: ~p classes~n", [File, N]) end,
        MultiClassFiles
    ),
    ok.

%% Counts real `subclass:` declarations only — stdlib doc comments (`///`
%% lines) routinely show hypothetical subclass examples (e.g. `Actor.bt`
%% documents `/// Actor subclass: Counter`) that are not actual class
%% definitions and must not be counted as such. `SubclassRe` is compiled once
%% by the caller and reused across every file, rather than recompiled per
%% call.
-spec count_subclass_decls(string(), re:mp()) -> non_neg_integer().
count_subclass_decls(File, SubclassRe) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Lines = binary:split(Bin, <<"\n">>, [global]),
            CodeLines = [L || L <- Lines, not is_doc_comment_line(L)],
            length([L || L <- CodeLines, re:run(L, SubclassRe) =/= nomatch]);
        {error, _} ->
            0
    end.

-spec is_doc_comment_line(binary()) -> boolean().
is_doc_comment_line(Line) ->
    case re:run(Line, "^\\s*///") of
        {match, _} -> true;
        nomatch -> false
    end.

%%====================================================================
%% Part B: sweep cost vs. live-class count
%%====================================================================

part_b_sweep_cost_vs_size() ->
    io:format("~n=== Part B: trigger_leaf_change/1 sweep cost vs. live-class count ===~n", []),
    restart_workspace_meta(),

    Sizes = [5, 20, 50, 100, 200],
    io:format("~-10s ~-10s ~-12s ~-14s~n", ["size", "checked", "wall_ms", "ms/checked"]),
    lists:foreach(fun run_round/1, Sizes),
    ok.

restart_workspace_meta() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            {ok, _} = beamtalk_workspace_meta:start_link(#{
                workspace_id => <<"bench_leaf_change_fanout_ws">>,
                project_path => undefined,
                created_at => erlang:system_time(second),
                repl => false
            }),
            ok;
        _Pid ->
            ok
    end.

%% One round: register N synthetic live class sources (round-scoped names, so
%% rounds accumulate rather than interfere -- trigger_leaf_change/1 sweeps
%% *every* live class with a recorded source, so this models a workspace that
%% has grown to size N through a bulk load), then time a real
%% trigger_leaf_change/1 sweep for a superclass name none of the sources
%% mention (so every diagnostic round-trip completes clean -- this measures
%% sweep cost, not finding-production cost, matching trigger_image/0's own
%% "checked" semantics).
-spec run_round(pos_integer()) -> ok.
run_round(N) ->
    Tag = integer_to_list(N),
    PrevCount = maps:size(beamtalk_workspace_meta:all_class_sources()),
    ToAdd = N - PrevCount,
    lists:foreach(
        fun(I) -> register_candidate("BenchLeafCand" ++ Tag ++ "_" ++ integer_to_list(I)) end,
        lists:seq(1, max(0, ToAdd))
    ),
    %% BT-2873: trigger_leaf_change/1 takes the whole superclass list from one
    %% reload event (a single-element list here -- the batching win itself is
    %% covered by Part A's "0 of 104 files declare more than one class"
    %% finding, not by this sweep-cost-vs-size measurement).
    SuperclassBins = [<<"BenchLeafUnrelatedSuperclass">>],
    {WallUs, Result} = timer:tc(fun() ->
        beamtalk_recheck:trigger_leaf_change(SuperclassBins)
    end),
    #{checked := Checked} = Result,
    WallMs = WallUs / 1000,
    MsPerChecked =
        case Checked of
            0 -> 0.0;
            _ -> WallMs / Checked
        end,
    io:format("~10p ~10p ~12.2f ~14.2f~n", [N, Checked, WallMs, MsPerChecked]).

-spec register_candidate(string()) -> ok.
register_candidate(NameStr) ->
    NameBin = list_to_binary(NameStr),
    Source = <<"Object subclass: ", NameBin/binary, "\n  go: -> Integer => 1\n">>,
    ok = beamtalk_workspace_meta:set_class_source(NameBin, Source),
    ok.
