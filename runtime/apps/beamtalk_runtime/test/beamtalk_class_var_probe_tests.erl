%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_var_probe_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for `beamtalk_class_var_probe` (ADR 0130 Phase 0, BT-3703).

The probe must log at `domain => [beamtalk, probe]`, say whether the access
was made at home, and never raise.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-export([log/2]).

log(LogEvent, #{config := #{parent := Parent}}) ->
    Parent ! {log_event, LogEvent},
    ok.

class_self(Pid) ->
    #beamtalk_object{class = 'ProbeClass class', class_mod = bt_probe, pid = Pid}.

with_handler(Fun) ->
    %% The test sys.config caps the primary level at `error`; lower it so the
    %% notice-level probe event reaches the handler.
    OldLevel = maps:get(level, logger:get_primary_config()),
    ok = logger:set_primary_config(level, all),
    HandlerId = beamtalk_class_var_probe_test_handler,
    ok = logger:add_handler(HandlerId, ?MODULE, #{
        config => #{parent => self()},
        level => all
    }),
    try
        Fun()
    after
        logger:remove_handler(HandlerId),
        logger:set_primary_config(level, OldLevel)
    end.

next_probe_event(Timeout) ->
    receive
        {log_event, #{meta := #{domain := [beamtalk, probe]}, msg := {report, Report}}} ->
            {ok, Report};
        {log_event, _Other} ->
            next_probe_event(Timeout)
    after Timeout ->
        none
    end.

block_access_away_from_home_is_logged_test() ->
    with_handler(fun() ->
        Home = spawn(fun() ->
            receive
                stop -> ok
            end
        end),
        try
            ok = beamtalk_class_var_probe:report(
                class_self(Home), 'ProbeClass', stored, read, n, true
            ),
            {ok, Report} = next_probe_event(500),
            ?assertEqual('ProbeClass', maps:get(class, Report)),
            ?assertEqual(stored, maps:get(selector, Report)),
            ?assertEqual(read, maps:get(kind, Report)),
            ?assertEqual(n, maps:get(field, Report)),
            ?assertEqual(true, maps:get(in_block, Report)),
            ?assertEqual(false, maps:get(at_home, Report)),
            %% The home process is idle: no invocation is live.
            ?assertEqual(false, maps:get(home_live, Report)),
            ?assertEqual(abroad, maps:get(shape, Report))
        after
            Home ! stop
        end
    end).

%% ADR 0130 §2: at home means this process holds the class's key, not that its
%% pid is the one in ClassSelf.
with_home_key(Fun) ->
    Key = beamtalk_class_vars:key_for_tag('ProbeClass class'),
    erlang:put(Key, #{}),
    try
        Fun()
    after
        erlang:erase(Key)
    end.

block_access_at_home_is_logged_as_home_test() ->
    with_handler(fun() ->
        with_home_key(fun() ->
            ok = beamtalk_class_var_probe:report(
                class_self(self()), 'ProbeClass', viaCollect, read, n, true
            ),
            {ok, Report} = next_probe_event(500),
            ?assertEqual(true, maps:get(at_home, Report)),
            ?assertEqual(home, maps:get(shape, Report))
        end)
    end).

at_home_is_key_presence_not_pid_equality_test() ->
    with_handler(fun() ->
        with_home_key(fun() ->
            %% A ClassSelf carrying a stale pid (a class-process restart): the
            %% key is present in this process, so the access is still at home.
            Stale = spawn(fun() -> ok end),
            ok = beamtalk_class_var_probe:report(
                class_self(Stale), 'ProbeClass', viaCollect, read, n, true
            ),
            {ok, Report} = next_probe_event(500),
            ?assertEqual(true, maps:get(at_home, Report)),
            ?assertEqual(home, maps:get(shape, Report))
        end)
    end).

at_home_pid_without_the_key_is_not_at_home_test() ->
    with_handler(fun() ->
        %% This process is the pid in ClassSelf but holds no key (a foreign or
        %% idle process): not at home.
        ok = beamtalk_class_var_probe:report(
            class_self(self()), 'ProbeClass', viaCollect, read, n, true
        ),
        {ok, Report} = next_probe_event(500),
        ?assertEqual(false, maps:get(at_home, Report))
    end).

method_level_access_at_home_is_not_logged_test() ->
    with_handler(fun() ->
        with_home_key(fun() ->
            ok = beamtalk_class_var_probe:report(
                class_self(self()), 'ProbeClass', bump, write, n, false
            ),
            ?assertEqual(none, next_probe_event(100))
        end)
    end).

method_level_access_away_from_home_is_logged_test() ->
    with_handler(fun() ->
        %% `performLocally:` today runs the method with a nil class object.
        ok = beamtalk_class_var_probe:report(nil, 'ProbeClass', bump, write, n, false),
        {ok, Report} = next_probe_event(500),
        ?assertEqual(write, maps:get(kind, Report)),
        ?assertEqual(false, maps:get(in_block, Report)),
        ?assertEqual(<<"none">>, maps:get(home, Report)),
        ?assertEqual(abroad, maps:get(shape, Report))
    end).

probe_never_raises_on_garbage_test() ->
    ?assertEqual(ok, beamtalk_class_var_probe:report(garbage, 'C', s, read, n, true)),
    ?assertEqual(
        ok, beamtalk_class_var_probe:report(class_self(undefined), 'C', s, read, n, false)
    ).

%% BT-3723: a home pid on another node must not make the probe drop the event
%% (`process_info/2` raises badarg on a non-local pid).
remote_home_pid_is_logged_as_abroad_test() ->
    with_handler(fun() ->
        %% A pid of a fictional remote node, built from the external term format.
        Remote = binary_to_term(
            <<131, 88, 100, 0, 12, "other@nohost", 0, 0, 0, 42, 0, 0, 0, 0, 0, 0, 0, 1>>
        ),
        ?assertNotEqual(node(), node(Remote)),
        ok = beamtalk_class_var_probe:report(
            class_self(Remote), 'ProbeClass', stored, read, n, true
        ),
        {ok, Report} = next_probe_event(500),
        ?assertEqual(false, maps:get(home_live, Report)),
        ?assertEqual(abroad, maps:get(shape, Report))
    end).

%% BT-3723: the one-time probe setup must run exactly once however many
%% processes make their first report at the same instant, and no report may be
%% logged before the sink handler exists.
%%
%% The earlier version of this test was vacuous: `report/6` swallows every
%% exception and returns `ok`, the sink handler is only installed when
%% BEAMTALK_CLASS_VAR_PROBE_LOG is set (it was not, so setup was a no-op that
%% could not race), and the flag ends up `true` even when setup runs many times.
%%
%% Here the env var is set, so the real handler install runs. The workers are
%% held at a barrier and released together; each is call-traced so we count how
%% many entered the handler install (must be exactly one), and the sink file
%% must contain one line per report (a report logged before the sink existed
%% would be lost). Synchronisation is by messages only, no sleeps.
concurrent_first_reports_set_up_the_sink_exactly_once_test_() ->
    {timeout, 60, fun concurrent_first_reports_set_up_the_sink_exactly_once/0}.

concurrent_first_reports_set_up_the_sink_exactly_once() ->
    Workers = 32,
    SetupKey = {beamtalk_class_var_probe, setup},
    SinkId = beamtalk_class_var_probe,
    TmpDir = binary_to_list(beamtalk_file:'tempDirectory'()),
    SinkFile = filename:join(
        TmpDir, "bt3723_probe_sink_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".log"
    ),
    OldEnv = os:getenv("BEAMTALK_CLASS_VAR_PROBE_LOG"),
    OldDepth = erlang:system_flag(backtrace_depth, 8),
    OldPrimary = logger:get_primary_config(),
    OldHandlers = [{Id, maps:get(level, C)} || #{id := Id} = C <- logger:get_handler_config()],
    _ = persistent_term:erase(SetupKey),
    _ = logger:remove_handler(SinkId),
    true = os:putenv("BEAMTALK_CLASS_VAR_PROBE_LOG", SinkFile),
    ok = logger:set_primary_config(level, all),
    InstallMFA = {beamtalk_class_var_probe, ensure_sink_handler, 0},
    try
        1 = erlang:trace_pattern(InstallMFA, true, [local]),
        Parent = self(),
        Pids = [
            spawn_link(fun() ->
                Parent ! {ready, self()},
                receive
                    go -> ok
                end,
                R = beamtalk_class_var_probe:report(nil, 'ProbeClass', bump, write, n, true),
                Parent ! {done, self(), R}
            end)
         || _ <- lists:seq(1, Workers)
        ],
        [
            receive
                {ready, P} -> 1 = erlang:trace(P, true, [call])
            after 5000 -> error({worker_not_ready, P})
            end
         || P <- Pids
        ],
        %% Release one worker first and wait until it is inside the handler
        %% install, then release the rest while that install is still in
        %% flight: every later worker must wait for it, not run ahead of it.
        [First | Rest] = Pids,
        First ! go,
        {M, F, 0} = InstallMFA,
        receive
            {trace, First, call, {M, F, []}} -> ok
        after 10000 -> error(install_not_entered)
        end,
        [P ! go || P <- Rest],
        [
            receive
                {done, P, R} -> ?assertEqual(ok, R)
            after 30000 -> error({worker_not_done, P})
            end
         || P <- Pids
        ],
        ?assertEqual(true, persistent_term:get(SetupKey, false)),
        ?assertMatch({ok, _}, logger:get_handler_config(SinkId)),
        DeliveredRef = erlang:trace_delivered(all),
        receive
            {trace_delivered, all, DeliveredRef} -> ok
        after 5000 -> error(trace_not_delivered)
        end,
        Installs = drain_install_calls(InstallMFA, 1),
        ?assertEqual(1, Installs),
        %% Removing the handler closes the file, flushing every event.
        ok = logger:remove_handler(SinkId),
        {ok, Bin} = file:read_file(SinkFile),
        Lines = [L || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>],
        ?assertEqual(Workers, length(Lines))
    after
        erlang:trace_pattern(InstallMFA, false, [local]),
        _ = logger:remove_handler(SinkId),
        _ = file:delete(SinkFile),
        case OldEnv of
            false -> os:unsetenv("BEAMTALK_CLASS_VAR_PROBE_LOG");
            _ -> os:putenv("BEAMTALK_CLASS_VAR_PROBE_LOG", OldEnv)
        end,
        _ = erlang:system_flag(backtrace_depth, OldDepth),
        [logger:set_handler_config(Id, level, Lvl) || {Id, Lvl} <- OldHandlers, Id =/= SinkId],
        logger:set_primary_config(level, maps:get(level, OldPrimary)),
        persistent_term:erase(SetupKey)
    end.

drain_install_calls({M, F, 0} = MFA, Count) ->
    receive
        %% A call trace message carries the argument list, not the arity.
        {trace, _Pid, call, {M, F, []}} -> drain_install_calls(MFA, Count + 1)
    after 0 -> Count
    end.
