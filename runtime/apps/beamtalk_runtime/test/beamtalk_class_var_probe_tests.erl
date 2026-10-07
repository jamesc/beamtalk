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

%% BT-3723: concurrent first reports must not crash or lose the setup; every
%% caller returns ok and the setup flag ends up set.
concurrent_first_reports_all_return_ok_test() ->
    Key = {beamtalk_class_var_probe, setup},
    _ = persistent_term:erase(Key),
    Parent = self(),
    Pids = [
        spawn_link(fun() ->
            R = beamtalk_class_var_probe:report(nil, 'ProbeClass', bump, write, n, true),
            Parent ! {done, self(), R}
        end)
     || _ <- lists:seq(1, 20)
    ],
    [
        receive
            {done, P, R} -> ?assertEqual(ok, R)
        after 5000 -> ?assert(false)
        end
     || P <- Pids
    ],
    ?assertEqual(true, persistent_term:get(Key, false)).
