%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_ws_log_handler_tests).

-moduledoc "Tests for WebSocket log handler".
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ===========================================================================
%%% Setup / Teardown
%%% ===========================================================================

-doc "Install the handler before each test group and remove after.".
setup() ->
    %% Save and lower primary log level — test sys.config sets it to error,
    %% which drops events before they reach any handler.
    #{level := OrigLevel} = logger:get_primary_config(),
    logger:set_primary_config(level, debug),
    %% Remove if leftover from a previous test run
    _ = logger:remove_handler(beamtalk_ws_log),
    ok = logger:add_handler(beamtalk_ws_log, beamtalk_ws_log_handler, #{
        level => debug
    }),
    OrigLevel.

cleanup(OrigLevel) ->
    _ = logger:remove_handler(beamtalk_ws_log),
    logger:set_primary_config(level, OrigLevel),
    ok.

%%% ===========================================================================
%%% Test Generators
%%% ===========================================================================

subscribe_unsubscribe_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        fun subscribe_receives_log_events/0,
        fun unsubscribe_stops_events/0,
        fun level_filter_drops_below_threshold/0,
        fun level_filter_passes_at_threshold/0,
        fun subscriber_cleanup_on_death/0,
        fun multiple_subscribers/0,
        fun resubscribe_updates_level/0,
        fun format_includes_domain_metadata/0,
        fun format_includes_class_metadata/0,
        fun format_utf8_text_is_not_double_encoded/0,
        fun format_codepoint_above_255_is_encoded/0,
        fun resubscribe_does_not_leak_monitor_processes/0,
        fun unsubscribe_stops_monitor_process/0,
        fun slow_subscriber_drops_over_queue_limit/0,
        fun drop_counter_is_zero_for_unknown_subscriber/0
    ]}.

%%% ===========================================================================
%%% Tests
%%% ===========================================================================

%% `~s` on a UTF-8 binary passes its bytes through; `~ts` on a codepoint above
%% 255 must not crash `iolist_to_binary/1` (it would show as "[format error]").
format_utf8_text_is_not_double_encoded() ->
    beamtalk_ws_log_handler:subscribe(debug),
    ?LOG_INFO("caf ~s", [<<195, 169>>], #{domain => [beamtalk, runtime]}),
    ?assertMatch(#{msg := <<"caf ", 195, 169>>}, receive_log_event(500)),
    ?LOG_INFO("caf ~ts", [[233]], #{domain => [beamtalk, runtime]}),
    ?assertMatch(#{msg := <<"caf ", 195, 169>>}, receive_log_event(500)),
    beamtalk_ws_log_handler:unsubscribe().

format_codepoint_above_255_is_encoded() ->
    beamtalk_ws_log_handler:subscribe(debug),
    ?LOG_INFO("sel ~ts", [[955]], #{domain => [beamtalk, runtime]}),
    ?assertMatch(#{msg := <<"sel ", 206, 187>>}, receive_log_event(500)),
    beamtalk_ws_log_handler:unsubscribe().

subscribe_receives_log_events() ->
    beamtalk_ws_log_handler:subscribe(debug),
    ?LOG_INFO("test message", #{domain => [beamtalk, runtime]}),
    Received = receive_log_event(500),
    ?assertMatch(#{msg := <<"test message">>, level := <<"info">>}, Received),
    beamtalk_ws_log_handler:unsubscribe().

unsubscribe_stops_events() ->
    beamtalk_ws_log_handler:subscribe(debug),
    beamtalk_ws_log_handler:unsubscribe(),
    ?LOG_INFO("should not arrive", #{domain => [beamtalk, runtime]}),
    ?assertEqual(timeout, receive_log_event(100)).

level_filter_drops_below_threshold() ->
    beamtalk_ws_log_handler:subscribe(warning),
    ?LOG_INFO("below threshold", #{domain => [beamtalk, runtime]}),
    ?LOG_DEBUG("also below", #{domain => [beamtalk, runtime]}),
    ?assertEqual(timeout, receive_log_event(100)),
    beamtalk_ws_log_handler:unsubscribe().

level_filter_passes_at_threshold() ->
    beamtalk_ws_log_handler:subscribe(warning),
    ?LOG_WARNING("at threshold", #{domain => [beamtalk, runtime]}),
    Received = receive_log_event(500),
    ?assertMatch(#{level := <<"warning">>}, Received),
    %% Error is above warning — should also pass
    ?LOG_ERROR("above threshold", #{domain => [beamtalk, runtime]}),
    Received2 = receive_log_event(500),
    ?assertMatch(#{level := <<"error">>}, Received2),
    beamtalk_ws_log_handler:unsubscribe().

subscriber_cleanup_on_death() ->
    %% Spawn a process that subscribes then exits
    Parent = self(),
    Pid = spawn(fun() ->
        beamtalk_ws_log_handler:subscribe(debug),
        Parent ! subscribed,
        receive
            stop -> ok
        end
    end),
    receive
        subscribed -> ok
    end,
    %% Verify it's subscribed
    ?assertNotEqual([], ets:lookup(beamtalk_ws_log_subscribers, Pid)),
    %% Kill the subscriber
    exit(Pid, kill),
    beamtalk_test_wait:dead(Pid),
    %% The handler removes the row on its own monitor 'DOWN', which is
    %% asynchronous to the subscriber's death: wait for the observable
    %% cleanup, then verify.
    beamtalk_test_wait:until(fun() ->
        ets:lookup(beamtalk_ws_log_subscribers, Pid) =:= []
    end),
    ?assertEqual([], ets:lookup(beamtalk_ws_log_subscribers, Pid)).

multiple_subscribers() ->
    Parent = self(),
    %% Spawn two subscriber processes
    Pid1 = spawn(fun() ->
        beamtalk_ws_log_handler:subscribe(debug),
        Parent ! {subscribed, 1},
        forward_log_events(Parent, 1)
    end),
    Pid2 = spawn(fun() ->
        beamtalk_ws_log_handler:subscribe(error),
        Parent ! {subscribed, 2},
        forward_log_events(Parent, 2)
    end),
    receive
        {subscribed, 1} -> ok
    end,
    receive
        {subscribed, 2} -> ok
    end,
    %% Send info event — only Pid1 (debug) should get it
    ?LOG_INFO("info event", #{domain => [beamtalk, runtime]}),
    ?assertMatch({1, #{level := <<"info">>}}, receive_tagged_event(500)),
    %% Send error event — both should get it
    ?LOG_ERROR("error event", #{domain => [beamtalk, runtime]}),
    Events = collect_tagged_events(500, 2),
    Tags = lists:sort([Tag || {Tag, _} <- Events]),
    ?assertEqual([1, 2], Tags),
    %% Cleanup
    exit(Pid1, kill),
    exit(Pid2, kill).

resubscribe_updates_level() ->
    beamtalk_ws_log_handler:subscribe(error),
    ?LOG_INFO("should not arrive", #{domain => [beamtalk, runtime]}),
    ?assertEqual(timeout, receive_log_event(100)),
    %% Resubscribe with lower level
    beamtalk_ws_log_handler:subscribe(debug),
    ?LOG_INFO("should arrive now", #{domain => [beamtalk, runtime]}),
    Received = receive_log_event(500),
    ?assertMatch(#{level := <<"info">>}, Received),
    beamtalk_ws_log_handler:unsubscribe().

%% BT-5018: every `subscribe/1` used to spawn another unlinked monitor process
%% and `unsubscribe/0` never stopped it, so each log-level change leaked one.
%% The processes monitoring the subscriber are the observable: one, however
%% many level changes.
resubscribe_does_not_leak_monitor_processes() ->
    Before = length(monitors_of(self())),
    Levels = [debug, info, warning, error],
    lists:foreach(
        fun(N) -> beamtalk_ws_log_handler:subscribe(lists:nth(N rem 4 + 1, Levels)) end,
        lists:seq(1, 50)
    ),
    %% The monitor attaches asynchronously; wait for it, then confirm it is the
    %% only one.
    ok = beamtalk_test_wait:until(fun() -> length(monitors_of(self())) >= Before + 1 end),
    ?assertEqual(Before + 1, length(monitors_of(self()))),
    beamtalk_ws_log_handler:unsubscribe(),
    ok = beamtalk_test_wait:until(fun() -> length(monitors_of(self())) =:= Before end),
    %% Unsubscribe/subscribe cycles (what the WebSocket handler does on a level
    %% change) leave no extra monitor behind either.
    lists:foreach(
        fun(_) ->
            beamtalk_ws_log_handler:subscribe(info),
            beamtalk_ws_log_handler:unsubscribe()
        end,
        lists:seq(1, 50)
    ),
    ok = beamtalk_test_wait:until(fun() -> length(monitors_of(self())) =:= Before end).

unsubscribe_stops_monitor_process() ->
    beamtalk_ws_log_handler:subscribe(debug),
    [{_, debug, MonPid, 0}] = ets:lookup(beamtalk_ws_log_subscribers, self()),
    ?assert(is_process_alive(MonPid)),
    beamtalk_ws_log_handler:unsubscribe(),
    ok = beamtalk_test_wait:dead(MonPid),
    ?assertEqual([], ets:lookup(beamtalk_ws_log_subscribers, self())).

%% BT-5018 backpressure policy: a subscriber whose mailbox is at the limit has
%% further events dropped (not queued), and the drops are counted.
slow_subscriber_drops_over_queue_limit() ->
    Limit = beamtalk_ws_log_handler:queue_limit(),
    Parent = self(),
    %% Never reads its mailbox: a stalled socket.
    Slow = spawn(fun() ->
        beamtalk_ws_log_handler:subscribe(debug),
        Parent ! subscribed,
        receive
            stop -> ok
        end
    end),
    receive
        subscribed -> ok
    end,
    Extra = 25,
    lists:foreach(
        fun(N) -> ?LOG_INFO("flood ~p", [N], #{domain => [beamtalk, runtime]}) end,
        lists:seq(1, Limit + Extra)
    ),
    {message_queue_len, Queued} = process_info(Slow, message_queue_len),
    ?assert(Queued =< Limit),
    ?assertEqual(Extra, beamtalk_ws_log_handler:dropped_count(Slow)),
    Slow ! stop,
    ok = beamtalk_test_wait:dead(Slow).

drop_counter_is_zero_for_unknown_subscriber() ->
    ?assertEqual(0, beamtalk_ws_log_handler:dropped_count(spawn(fun() -> ok end))).

format_includes_domain_metadata() ->
    beamtalk_ws_log_handler:subscribe(debug),
    ?LOG_INFO("domain test", #{domain => [beamtalk, user]}),
    Received = receive_log_event(500),
    ?assertEqual(<<"beamtalk.user">>, maps:get(domain, Received)),
    beamtalk_ws_log_handler:unsubscribe().

format_includes_class_metadata() ->
    beamtalk_ws_log_handler:subscribe(debug),
    logger:log(info, "class test", #{
        domain => [beamtalk, user],
        beamtalk_class => 'Counter',
        beamtalk_selector => increment
    }),
    Received = receive_log_event(500),
    ?assertEqual(<<"Counter">>, maps:get(class, Received)),
    ?assertEqual(<<"increment">>, maps:get(selector, Received)),
    beamtalk_ws_log_handler:unsubscribe().

%%% ===========================================================================
%%% Helpers
%%% ===========================================================================

-doc "Receive a log_event message or return timeout.".
receive_log_event(Timeout) ->
    receive
        {log_event, Data} -> Data
    after Timeout ->
        timeout
    end.

-doc "Receive a tagged log event from a forwarding process.".
receive_tagged_event(Timeout) ->
    receive
        {forwarded_log, Tag, Data} -> {Tag, Data}
    after Timeout ->
        timeout
    end.

-doc "Collect N tagged events within Timeout ms.".
collect_tagged_events(Timeout, N) ->
    collect_tagged_events(Timeout, N, []).

collect_tagged_events(_Timeout, 0, Acc) ->
    lists:reverse(Acc);
collect_tagged_events(Timeout, N, Acc) ->
    receive
        {forwarded_log, Tag, Data} ->
            collect_tagged_events(Timeout, N - 1, [{Tag, Data} | Acc])
    after Timeout ->
        lists:reverse(Acc)
    end.

-doc "Forward log events to parent tagged with an identifier.".
forward_log_events(Parent, Tag) ->
    receive
        {log_event, Data} ->
            Parent ! {forwarded_log, Tag, Data},
            forward_log_events(Parent, Tag)
    end.

%% Pids that currently monitor `Pid`.
monitors_of(Pid) ->
    {monitored_by, By} = process_info(Pid, monitored_by),
    By.
