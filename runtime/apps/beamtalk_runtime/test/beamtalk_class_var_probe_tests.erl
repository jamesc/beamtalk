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

block_access_at_home_is_logged_as_home_test() ->
    with_handler(fun() ->
        ok = beamtalk_class_var_probe:report(
            class_self(self()), 'ProbeClass', viaCollect, read, n, true
        ),
        {ok, Report} = next_probe_event(500),
        ?assertEqual(true, maps:get(at_home, Report)),
        ?assertEqual(home, maps:get(shape, Report))
    end).

method_level_access_at_home_is_not_logged_test() ->
    with_handler(fun() ->
        ok = beamtalk_class_var_probe:report(
            class_self(self()), 'ProbeClass', bump, write, n, false
        ),
        ?assertEqual(none, next_probe_event(100))
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
