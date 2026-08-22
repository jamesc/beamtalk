%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

-module(beamtalk_class_sup_tests).

-moduledoc """
EUnit tests for BT-3236: class gen_servers under `beamtalk_class_sup` with
eager crash recovery via `beamtalk_class_monitor`.
""".
-include_lib("eunit/include/eunit.hrl").

%% Custom logger handler callback (see capture_log/2 tests).
-export([log/2]).

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    setup(#{}).

setup(MonitorOpts) ->
    beamtalk_class_registry:ensure_pg_started(),
    beamtalk_class_registry:ensure_hierarchy_table(),
    beamtalk_class_registry:ensure_module_table(),
    beamtalk_class_registry:ensure_pid_table(),
    {ok, SupPid} = beamtalk_class_sup:start_link(),
    {ok, MonPid} = beamtalk_class_monitor:start_link(MonitorOpts),
    #{sup => SupPid, monitor => MonPid}.

teardown(#{sup := SupPid, monitor := MonPid}) ->
    %% Stop the monitor FIRST so class cleanup below cannot trigger eager
    %% restarts, then stop each remaining class gracefully, then the sup.
    %% Leaving either registered would change the routing behaviour of every
    %% test module that runs after this one in the same VM.
    stop_if_alive(MonPid),
    lists:foreach(
        fun(Pid) -> stop_if_alive(Pid) end,
        try
            pg:get_members(beamtalk_classes)
        catch
            _:_ -> []
        end
    ),
    stop_if_alive(SupPid),
    ok.

stop_if_alive(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            MRef = erlang:monitor(process, Pid),
            (try
                gen_server:stop(Pid)
            catch
                _:_ -> exit(Pid, kill)
            end),
            receive
                {'DOWN', MRef, process, Pid, _} -> ok
            after 2000 -> ok
            end;
        false ->
            ok
    end;
stop_if_alive(_) ->
    ok.

minimal_class_info() ->
    #{superclass => none, methods => #{}, class_methods => #{}}.

wait_until(Fun) ->
    wait_until(Fun, 100).

wait_until(_Fun, 0) ->
    timeout;
wait_until(Fun, Retries) ->
    case Fun() of
        true ->
            ok;
        false ->
            timer:sleep(50),
            wait_until(Fun, Retries - 1)
    end.

kill_and_wait(Pid) ->
    MRef = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    after 5000 -> ?assert(false)
    end.

%%====================================================================
%% Logger capture handler (log/2 is the handler callback)
%%====================================================================

log(#{msg := Msg, level := Level}, #{config := #{test_pid := TestPid}}) ->
    TestPid ! {captured_log, Level, Msg},
    ok.

add_capture_handler() ->
    %% The rebar3 eunit node's primary logger level filters warnings out, so
    %% lower it for the duration of the capture (restored in
    %% remove_capture_handler/1).
    #{level := OldLevel} = logger:get_primary_config(),
    ok = logger:set_primary_config(level, warning),
    ok = logger:add_handler(
        bt3236_capture,
        ?MODULE,
        #{config => #{test_pid => self()}, level => warning}
    ),
    OldLevel.

remove_capture_handler(OldLevel) ->
    _ = logger:remove_handler(bt3236_capture),
    _ = logger:set_primary_config(level, OldLevel),
    %% Drain any queued captures so a later test in this process starts clean.
    drain_captures().

drain_captures() ->
    receive
        {captured_log, _, _} -> drain_captures()
    after 0 -> ok
    end.

restart_warning_received() ->
    receive
        {captured_log, warning, {Fmt, _Args}} when is_list(Fmt) ->
            case string:find(Fmt, "auto-restarted") of
                nomatch -> restart_warning_received();
                _ -> true
            end;
        {captured_log, _, _} ->
            restart_warning_received()
    after 0 -> false
    end.

%%====================================================================
%% Tests
%%====================================================================

supervised_start_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"class process starts as a child of beamtalk_class_sup", fun() ->
                {ok, Pid} = beamtalk_object_class:start('SupTest3236A', minimal_class_info()),
                Children = [P || {_, P, _, _} <- supervisor:which_children(beamtalk_class_sup)],
                ?assert(lists:member(Pid, Children)),
                ?assertEqual(Pid, beamtalk_class_registry:whereis_class('SupTest3236A'))
            end},
            {"already_started contract is preserved through the supervisor", fun() ->
                {ok, Pid} = beamtalk_object_class:start('SupTest3236B', minimal_class_info()),
                ?assertEqual(
                    {error, {already_started, Pid}},
                    beamtalk_object_class:start('SupTest3236B', minimal_class_info())
                )
            end}
        ]
    end}.

eager_restart_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"killed class process is eagerly restarted without a message send",
                {timeout, 30, fun() ->
                    OldLevel = add_capture_handler(),
                    try
                        {ok, OldPid} = beamtalk_object_class:start(
                            'SupTest3236C', minimal_class_info()
                        ),
                        kill_and_wait(OldPid),
                        %% No send to the class here — the monitor's 'DOWN' handler
                        %% must bring it back on its own.
                        ?assertEqual(
                            ok,
                            wait_until(fun() ->
                                case beamtalk_class_registry:whereis_class('SupTest3236C') of
                                    undefined -> false;
                                    NewPid -> NewPid =/= OldPid
                                end
                            end)
                        ),
                        NewPid = beamtalk_class_registry:whereis_class('SupTest3236C'),
                        %% Re-registered in the pid reverse index and pg group.
                        ?assertEqual(
                            {ok, 'SupTest3236C'},
                            beamtalk_class_registry:class_name_for_pid(NewPid)
                        ),
                        ?assert(lists:member(NewPid, pg:get_members(beamtalk_classes))),
                        %% The restarted child is supervised again.
                        Children = [
                            P
                         || {_, P, _, _} <- supervisor:which_children(beamtalk_class_sup)
                        ],
                        ?assert(lists:member(NewPid, Children)),
                        %% restart_class's warning about dropped hot patches /
                        %% class-var state was logged.
                        ?assertEqual(
                            ok, wait_until(fun restart_warning_received/0)
                        )
                    after
                        remove_capture_handler(OldLevel)
                    end
                end}}
        ]
    end}.

deliberate_stop_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"gen_server:stop does not trigger a restart", fun() ->
                {ok, Pid} = beamtalk_object_class:start('SupTest3236D', minimal_class_info()),
                gen_server:stop(Pid),
                %% Give the monitor time to (wrongly) act, then assert it didn't.
                timer:sleep(300),
                ?assertEqual(undefined, beamtalk_class_registry:whereis_class('SupTest3236D'))
            end}
        ]
    end}.

restart_budget_test_() ->
    {setup, fun() -> setup(#{max_restarts => 1, window_ms => 60000}) end, fun teardown/1, fun(
        _
    ) ->
        [
            {"crash-looping class is dropped after the budget, others unaffected",
                {timeout, 30, fun() ->
                    {ok, PidA} = beamtalk_object_class:start('SupTest3236E', minimal_class_info()),
                    {ok, PidB} = beamtalk_object_class:start('SupTest3236F', minimal_class_info()),
                    %% First crash: within budget → eagerly restarted.
                    kill_and_wait(PidA),
                    ?assertEqual(
                        ok,
                        wait_until(fun() ->
                            case beamtalk_class_registry:whereis_class('SupTest3236E') of
                                undefined -> false;
                                P -> P =/= PidA
                            end
                        end)
                    ),
                    Pid2 = beamtalk_class_registry:whereis_class('SupTest3236E'),
                    %% Second crash inside the window: budget exhausted → stays down.
                    kill_and_wait(Pid2),
                    timer:sleep(300),
                    ?assertEqual(undefined, beamtalk_class_registry:whereis_class('SupTest3236E')),
                    %% The budget is per class: an unrelated class still restarts.
                    kill_and_wait(PidB),
                    ?assertEqual(
                        ok,
                        wait_until(fun() ->
                            case beamtalk_class_registry:whereis_class('SupTest3236F') of
                                undefined -> false;
                                P -> P =/= PidB
                            end
                        end)
                    )
                end}}
        ]
    end}.

fallback_without_sup_test_() ->
    {setup,
        fun() ->
            %% No supervisor / monitor here — the standalone-EUnit context.
            beamtalk_class_registry:ensure_pg_started(),
            beamtalk_class_registry:ensure_hierarchy_table(),
            beamtalk_class_registry:ensure_module_table(),
            beamtalk_class_registry:ensure_pid_table(),
            ok
        end,
        fun(_) ->
            case beamtalk_class_registry:whereis_class('SupTest3236G') of
                undefined -> ok;
                P -> stop_if_alive(P)
            end
        end,
        fun(_) ->
            [
                {"start/2 still works when beamtalk_class_sup is not running", fun() ->
                    ?assertEqual(undefined, whereis(beamtalk_class_sup)),
                    {ok, Pid} = beamtalk_object_class:start(
                        'SupTest3236G', minimal_class_info()
                    ),
                    ?assert(is_process_alive(Pid)),
                    ?assertEqual(Pid, beamtalk_class_registry:whereis_class('SupTest3236G'))
                end}
            ]
        end}.
