%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_hot_reload_scan_tests).

-moduledoc """
Hot reload finds live instances by the process scan (ADR 0150, BT-4947):

- every instance is migrated, whether created before the reload, spawned
  through a name (`spawnAs`), or an instance of a descendant class;
- one `beamtalk_actor_scan:scan_marked_classes/1` pass serves the whole reload (the
  reloaded class and its descendants), not one query per class;
- owner condition (a): nothing is cached between reloads, so an instance
  created after a reload is seen by the next reload and by `allInstances`.
""".

-export([init/1]).

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
teardown(S) ->
    _ = S,
    beamtalk_test_recheck_fixtures:teardown(fixture_opts()).

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
%% separate tracer process collects them (the calling process is never traced
%% by `trace(all, ...)`, and the reload may not run in it). Returns:
%%   * the class-scan calls naming one of `Classes` (`{Fun, 1}`), so an unrelated
%%     scan from elsewhere on the node does not affect the assertion;
%%   * every `scan_actors/0` (full-node scan) made by a process that made one
%%     of those class scans, i.e. by the reload's own processes (`{scan_actors, 0}`),
%%     so a full-node scan added to the reload path fails the test while one
%%     from an unrelated process does not.
trace_scans(Classes, Fun) ->
    {module, beamtalk_actor_scan} = code:ensure_loaded(beamtalk_actor_scan),
    Tracer = spawn(fun() -> tracer_loop([]) end),
    1 = erlang:trace_pattern({beamtalk_actor_scan, scan_marked_classes, 1}, true, [global]),
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
        {Ref, Calls} -> reload_calls(Classes, Calls)
    after 5000 -> error(tracer_timeout)
    end.

tracer_loop(Acc) ->
    receive
        {trace, Pid, call, {beamtalk_actor_scan, F, Args}} ->
            tracer_loop([{Pid, F, Args} | Acc]);
        {done, From, Ref} ->
            From ! {Ref, lists:reverse(Acc)}
    end.

reload_calls(Classes, Calls) ->
    Named = fun(Args) ->
        Flat = lists:flatten(Args),
        lists:any(fun(C) -> lists:member(C, Flat) end, Classes)
    end,
    ClassScans = [{Pid, F} || {Pid, F, [_] = Args} <- Calls, Named(Args)],
    Owners = [Pid || {Pid, _} <- ClassScans],
    [{F, 1} || {_, F} <- ClassScans] ++
        [{scan_actors, 0} || {Pid, scan_actors, []} <- Calls, lists:member(Pid, Owners)].

%% proc_lib entry point for a fake `native:` backing process.
init(_) ->
    receive
        stop -> ok
    end.

hot_reload_scan_test_() ->
    {timeout, 60,
        {setup, fun setup/0, fun teardown/1, fun(_) ->
            [
                ?_test(
                    try
                        begin
                            Dir = beamtalk_test_fs:base_dir(),
                            Id = beamtalk_test_unique:id(),
                            ParentPath = write(
                                Dir, io_lib:format("hs_parent_~s.bt", [Id]), parent_src([])
                            ),
                            ChildPath = write(
                                Dir, io_lib:format("hs_child_~s.bt", [Id]), child_src()
                            ),
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

                            %% A `native:` process on the parent's backing module is
                            %% listed by `allInstances` but a reload leaves it alone.
                            Native = proc_lib:spawn(?MODULE, init, [x]),
                            beamtalk_class_registry:ensure_backing_module_index_table(),
                            ok = beamtalk_test_wait:until(fun() ->
                                {dictionary, D} = erlang:process_info(Native, dictionary),
                                lists:keymember('$initial_call', 1, D)
                            end),
                            ets:insert(beamtalk_backing_module_index, {
                                ?MODULE,
                                'HotScanParent',
                                beamtalk_runtime_api:whereis_class('HotScanParent')
                            }),
                            ?assert(
                                lists:member(
                                    Native, beamtalk_runtime_api:all_instances('HotScanParent')
                                )
                            ),

                            %% Reload the parent with a new field: one scan, all migrated.
                            ok = file:write_file(ParentPath, parent_src(["extra"])),
                            Calls = trace_scans(['HotScanParent', 'HotScanChild'], fun() ->
                                {ok, _, _} = beamtalk_repl_loader:handle_load(ParentPath, S2)
                            end),
                            ?assertEqual([{scan_marked_classes, 1}], Calls),
                            ?assert(migrated(P1, extra)),
                            ?assert(migrated(Named, extra)),
                            ?assert(migrated(C1, extra)),
                            %% The native process was neither suspended nor migrated.
                            ?assert(is_process_alive(Native)),
                            ets:match_delete(beamtalk_backing_module_index, {?MODULE, '_', '_'}),
                            MRef = erlang:monitor(process, Native),
                            exit(Native, kill),
                            receive
                                {'DOWN', MRef, process, Native, _} -> ok
                            end,

                            %% Owner condition (a): readers re-scan, no stale list. An
                            %% instance made after the reload is seen immediately...
                            P2 = spawn_instance("HotScanParent"),
                            Seen = beamtalk_runtime_api:all_instances('HotScanParent'),
                            ?assertEqual(lists:sort([P1, Named, P2]), lists:sort(Seen)),
                            ?assert(migrated(P2, extra)),

                            %% ...and by the next reload, which migrates old and new alike.
                            ok = file:write_file(ParentPath, parent_src(["extra", "more"])),
                            Calls2 = trace_scans(['HotScanParent', 'HotScanChild'], fun() ->
                                {ok, _, _} = beamtalk_repl_loader:handle_load(ParentPath, S2)
                            end),
                            ?assertEqual([{scan_marked_classes, 1}], Calls2),
                            lists:foreach(
                                fun(P) -> ?assert(migrated(P, more)) end, [P1, Named, P2, C1]
                            ),
                            lists:foreach(fun beamtalk_test_proc:stop/1, [P1, Named, P2, C1])
                        end
                    after
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
                    end
                )
            ]
        end}}.
