%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_hot_reload_scan_tests).

-moduledoc """
Hot reload finds live instances by the process scan (ADR 0150, BT-4947):

- every instance is migrated, whether created before the reload, spawned
  through a name (`spawnAs`), or an instance of a descendant class;
- one `beamtalk_actor_scan:scan_classes/1` pass serves the whole reload (the
  reloaded class and its descendants), not one query per class;
- owner condition (a): nothing is cached between reloads, so an instance
  created after a reload is seen by the next reload and by `allInstances`.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

fixture_opts() ->
    #{
        workspace_id => <<"hot_reload_scan_test_ws">>,
        restart => [
            beamtalk_workspace_shape_store,
            beamtalk_workspace_shape_recheck_worker,
            beamtalk_workspace_findings_store
        ]
    }.

setup() -> beamtalk_test_recheck_fixtures:setup(fixture_opts()).
teardown(S) -> _ = S, beamtalk_test_recheck_fixtures:teardown(fixture_opts()).

state0() -> beamtalk_repl_state:new(undefined, 0).

write(Dir, Name, Src) ->
    Path = filename:join(Dir, Name),
    ok = file:write_file(Path, Src),
    Path.

spawn_instance(Class) ->
    {ok, #beamtalk_object{pid = Pid}, _, _, _} =
        beamtalk_repl_eval:do_eval(Class ++ " spawn", state0()),
    Pid.

parent_src(Fields) ->
    iolist_to_binary([
        "Actor subclass: HotScanParent\n",
        "  state: count :: Integer = 0\n",
        [["  state: ", F, " :: Integer = 7\n"] || F <- Fields]
    ]).

child_src() ->
    <<"HotScanParent subclass: HotScanChild\n  state: own :: Integer = 1\n">>.

migrated(Pid, Field) ->
    maps:is_key(Field, sys:get_state(Pid)).

%% The `beamtalk_actor_scan` calls made while `Fun` runs, in any process. A
%% separate tracer process collects them: the calling process is never traced
%% by `trace(all, ...)`, and the reload may not run in it.
trace_scans(Fun) ->
    {module, beamtalk_actor_scan} = code:ensure_loaded(beamtalk_actor_scan),
    Tracer = spawn(fun() -> tracer_loop([]) end),
    1 = erlang:trace_pattern({beamtalk_actor_scan, scan_classes, 1}, true, [global]),
    1 = erlang:trace_pattern({beamtalk_actor_scan, scan_class, 1}, true, [global]),
    1 = erlang:trace_pattern({beamtalk_actor_scan, scan_actors, 0}, true, [global]),
    _ = erlang:trace(all, true, [call, {tracer, Tracer}]),
    try
        Fun()
    after
        _ = erlang:trace(all, false, [call]),
        erlang:trace_pattern({beamtalk_actor_scan, '_', '_'}, false, [global])
    end,
    Ref = make_ref(),
    Tracer ! {done, self(), Ref},
    receive
        {Ref, Calls} -> Calls
    after 5000 -> error(tracer_timeout)
    end.

tracer_loop(Acc) ->
    receive
        {trace, _, call, {beamtalk_actor_scan, F, A}} ->
            tracer_loop([{F, length(A)} | Acc]);
        {done, From, Ref} ->
            From ! {Ref, lists:reverse(Acc)}
    end.

hot_reload_scan_test_() ->
    {timeout, 60,
        {setup, fun setup/0, fun teardown/1, fun(_) ->
            [
                ?_test(try begin
                    Dir = beamtalk_test_fs:base_dir(),
                    Id = beamtalk_test_unique:id(),
                    ParentPath = write(Dir, io_lib:format("hs_parent_~s.bt", [Id]), parent_src([])),
                    ChildPath = write(Dir, io_lib:format("hs_child_~s.bt", [Id]), child_src()),
                    {ok, _, S1} = beamtalk_repl_loader:handle_load(ParentPath, state0()),
                    {ok, _, S2} = beamtalk_repl_loader:handle_load(ChildPath, S1),

                    %% Instances before the reload: plain, named, and a descendant.
                    P1 = spawn_instance("HotScanParent"),
                    ParentMod = beamtalk_runtime_api:module_name(
                        beamtalk_runtime_api:whereis_class('HotScanParent')
                    ),
                    {ok, Named} = beamtalk_actor_registry:'spawnAs'(
                        hot_scan_named, ParentMod, #{}
                    ),
                    C1 = spawn_instance("HotScanChild"),
                    ?assertNot(migrated(P1, extra)),
                    ?assertNot(migrated(C1, extra)),

                    %% Reload the parent with a new field: one scan, all migrated.
                    ok = file:write_file(ParentPath, parent_src(["extra"])),
                    Calls = trace_scans(fun() ->
                        {ok, _, _} = beamtalk_repl_loader:handle_load(ParentPath, S2)
                    end),
                    ?assertEqual([{scan_classes, 1}], Calls),
                    ?assert(migrated(P1, extra)),
                    ?assert(migrated(Named, extra)),
                    ?assert(migrated(C1, extra)),

                    %% Owner condition (a): readers re-scan, no stale list. An
                    %% instance made after the reload is seen immediately...
                    P2 = spawn_instance("HotScanParent"),
                    Seen = beamtalk_runtime_api:all_instances('HotScanParent'),
                    ?assertEqual(lists:sort([P1, Named, P2]), lists:sort(Seen)),
                    ?assert(migrated(P2, extra)),

                    %% ...and by the next reload, which migrates old and new alike.
                    ok = file:write_file(ParentPath, parent_src(["extra", "more"])),
                    Calls2 = trace_scans(fun() ->
                        {ok, _, _} = beamtalk_repl_loader:handle_load(ParentPath, S2)
                    end),
                    ?assertEqual([{scan_classes, 1}], Calls2),
                    lists:foreach(
                        fun(P) -> ?assert(migrated(P, more)) end, [P1, Named, P2, C1]
                    ),
                    lists:foreach(fun beamtalk_test_proc:stop/1, [P1, Named, P2, C1])
                end after
                    %% Never leak actors into later tests, whatever failed above.
                    lists:foreach(
                        fun(C) ->
                            lists:foreach(
                                fun beamtalk_test_proc:stop/1,
                                beamtalk_runtime_api:all_instances(C)
                            )
                        end,
                        ['HotScanParent', 'HotScanChild']
                    )
                end)
            ]
        end}}.
