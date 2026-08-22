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

%% In a shared EUnit node an earlier suite may have run
%% `application:ensure_all_started(beamtalk_runtime)` and left the app
%% running — then beamtalk_class_sup and beamtalk_class_monitor are already
%% registered as beamtalk_runtime_sup children. Adopt the running sup
%% (start_child works the same either way), and swap the app's monitor out
%% via supervisor:terminate_child so each fixture gets a fresh monitor with
%% its own budget opts; teardown restores it via supervisor:restart_child.
setup(MonitorOpts) ->
    beamtalk_class_registry:ensure_pg_started(),
    beamtalk_class_registry:ensure_hierarchy_table(),
    beamtalk_class_registry:ensure_module_table(),
    beamtalk_class_registry:ensure_pid_table(),
    {SupPid, SupOwned} =
        case whereis(beamtalk_class_sup) of
            undefined ->
                {ok, P} = beamtalk_class_sup:start_link(),
                {P, true};
            P ->
                {P, false}
        end,
    RestoreMonitor = displace_registered_monitor(),
    {ok, MonPid} = beamtalk_class_monitor:start_link(MonitorOpts),
    #{sup => SupPid, sup_owned => SupOwned, monitor => MonPid, restore_monitor => RestoreMonitor}.

%% Clear the beamtalk_class_monitor registered name so the fixture can start
%% its own. Returns true when the displaced monitor was the runtime app's
%% supervised child (teardown must restart_child it).
displace_registered_monitor() ->
    case whereis(beamtalk_class_monitor) of
        undefined ->
            false;
        Pid ->
            case whereis(beamtalk_runtime_sup) of
                undefined ->
                    stop_if_alive(Pid),
                    false;
                _ ->
                    case supervisor:terminate_child(beamtalk_runtime_sup, beamtalk_class_monitor) of
                        ok ->
                            true;
                        {error, _} ->
                            %% Registered but not the app's child (leaked from
                            %% another suite) — stop it directly.
                            stop_if_alive(Pid),
                            false
                    end
            end
    end.

teardown(#{
    sup := SupPid,
    sup_owned := SupOwned,
    monitor := MonPid,
    restore_monitor := RestoreMonitor
}) ->
    %% Stop the monitor FIRST so class cleanup below cannot trigger eager
    %% restarts, then stop this module's classes gracefully, then the sup.
    %% Leaving either registered would change the routing behaviour of every
    %% test module that runs after this one in the same VM. Only classes this
    %% module created (SupTest3236* prefix) are stopped — the pg group is
    %% shared with other suites in the same EUnit node.
    stop_if_alive(MonPid),
    %% A test may have replaced the monitor (monitor_restart_readopts_test_);
    %% stop whatever currently holds the registered name too.
    stop_if_alive(whereis(beamtalk_class_monitor)),
    lists:foreach(
        fun(Pid) ->
            case beamtalk_class_registry:class_name_for_pid(Pid) of
                {ok, Name} ->
                    case string:prefix(atom_to_list(Name), "SupTest3236") of
                        nomatch -> ok;
                        _ -> stop_if_alive(Pid)
                    end;
                not_found ->
                    ok
            end
        end,
        try
            pg:get_members(beamtalk_classes)
        catch
            _:_ -> []
        end
    ),
    case SupOwned of
        true -> stop_if_alive(SupPid);
        false -> ok
    end,
    case RestoreMonitor of
        true -> catch supervisor:restart_child(beamtalk_runtime_sup, beamtalk_class_monitor);
        false -> ok
    end,
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
            %% The fixture needs beamtalk_class_sup to be ABSENT. If a prior
            %% suite left the runtime app running, carve its class_sup out via
            %% supervisor:terminate_child (restored in teardown); a leaked
            %% standalone sup is stopped directly.
            beamtalk_class_registry:ensure_pg_started(),
            beamtalk_class_registry:ensure_hierarchy_table(),
            beamtalk_class_registry:ensure_module_table(),
            beamtalk_class_registry:ensure_pid_table(),
            case whereis(beamtalk_class_sup) of
                undefined ->
                    false;
                SupPid ->
                    case whereis(beamtalk_runtime_sup) of
                        undefined ->
                            stop_if_alive(SupPid),
                            false;
                        _ ->
                            case
                                supervisor:terminate_child(
                                    beamtalk_runtime_sup, beamtalk_class_sup
                                )
                            of
                                ok ->
                                    true;
                                {error, _} ->
                                    stop_if_alive(SupPid),
                                    false
                            end
                    end
            end
        end,
        fun(RestoreSup) ->
            case beamtalk_class_registry:whereis_class('SupTest3236G') of
                undefined -> ok;
                P -> stop_if_alive(P)
            end,
            case RestoreSup of
                true ->
                    catch supervisor:restart_child(beamtalk_runtime_sup, beamtalk_class_sup);
                false ->
                    ok
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

unwatch_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"unwatched class is not restarted even on an abnormal exit", fun() ->
                {ok, Pid} = beamtalk_object_class:start('SupTest3236H', minimal_class_info()),
                %% Let the watch cast land, then unwatch (synchronous).
                ok = beamtalk_class_monitor:unwatch('SupTest3236H'),
                kill_and_wait(Pid),
                timer:sleep(300),
                ?assertEqual(undefined, beamtalk_class_registry:whereis_class('SupTest3236H'))
            end}
        ]
    end}.

monitor_restart_readopts_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(Ctx) ->
        [
            {"a restarted monitor re-adopts surviving classes from pg",
                {timeout, 30, fun() ->
                    #{monitor := MonPid} = Ctx,
                    {ok, OldPid} = beamtalk_object_class:start(
                        'SupTest3236I', minimal_class_info()
                    ),
                    %% Simulate a monitor crash/restart cycle: the replacement
                    %% must pick the surviving class back up from pg on init,
                    %% not silently lose eager recovery (adversarial finding).
                    stop_if_alive(MonPid),
                    {ok, NewMon} = beamtalk_class_monitor:start_link(),
                    ?assert(is_process_alive(NewMon)),
                    kill_and_wait(OldPid),
                    ?assertEqual(
                        ok,
                        wait_until(fun() ->
                            case beamtalk_class_registry:whereis_class('SupTest3236I') of
                                undefined -> false;
                                P -> P =/= OldPid
                            end
                        end)
                    )
                end}}
        ]
    end}.
