%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_events_tests).

-moduledoc "Tests for beamtalk_class_events pub/sub gen_server.".
-include_lib("eunit/include/eunit.hrl").

%%% ===========================================================================
%%% Lifecycle Tests
%%% ===========================================================================

server_starts_and_stops_test() ->
    {ok, Pid} = gen_server:start_link(beamtalk_class_events, [], []),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid),
    ?assertNot(is_process_alive(Pid)).

%%% ===========================================================================
%%% Subscribe / Unsubscribe Tests
%%% ===========================================================================

subscribe_receives_class_loaded_notification_test() ->
    {ok, Pid} = gen_server:start_link(
        {local, beamtalk_class_events}, beamtalk_class_events, [], []
    ),
    try
        beamtalk_class_events:subscribe(),
        %% Sync: ensure cast is processed before we fire the event
        sys:get_state(Pid),

        beamtalk_class_events:on_class_loaded('MyClass'),
        sys:get_state(Pid),

        receive
            {class_loaded, 'MyClass'} -> ok
        after 500 ->
            ?assert(false)
        end
    after
        gen_server:stop(Pid),
        flush_messages()
    end.

duplicate_subscribe_is_idempotent_test() ->
    {ok, Pid} = gen_server:start_link(
        {local, beamtalk_class_events}, beamtalk_class_events, [], []
    ),
    try
        beamtalk_class_events:subscribe(),
        beamtalk_class_events:subscribe(),
        sys:get_state(Pid),

        beamtalk_class_events:on_class_loaded('DupClass'),
        sys:get_state(Pid),

        %% Should receive exactly one notification, not two
        receive
            {class_loaded, 'DupClass'} -> ok
        after 500 ->
            ?assert(false)
        end,
        receive
            {class_loaded, 'DupClass'} ->
                ?assert(false)
        after 50 ->
            ok
        end
    after
        gen_server:stop(Pid),
        flush_messages()
    end.

unsubscribe_stops_notifications_test() ->
    {ok, Pid} = gen_server:start_link(
        {local, beamtalk_class_events}, beamtalk_class_events, [], []
    ),
    try
        beamtalk_class_events:subscribe(),
        sys:get_state(Pid),

        beamtalk_class_events:unsubscribe(),
        sys:get_state(Pid),

        beamtalk_class_events:on_class_loaded('UnsubClass'),
        sys:get_state(Pid),

        receive
            {class_loaded, 'UnsubClass'} ->
                ?assert(false)
        after 100 ->
            ok
        end
    after
        gen_server:stop(Pid),
        flush_messages()
    end.

unsubscribe_when_not_subscribed_is_safe_test() ->
    {ok, Pid} = gen_server:start_link(
        {local, beamtalk_class_events}, beamtalk_class_events, [], []
    ),
    try
        %% Should not crash — hits the `error` branch in maps:find
        beamtalk_class_events:unsubscribe(),
        sys:get_state(Pid),
        ?assert(is_process_alive(Pid))
    after
        gen_server:stop(Pid)
    end.

%%% ===========================================================================
%%% Multi-subscriber broadcast
%%% ===========================================================================

multiple_subscribers_all_notified_test() ->
    {ok, Pid} = gen_server:start_link(
        {local, beamtalk_class_events}, beamtalk_class_events, [], []
    ),
    Self = self(),
    try
        Sub1 = spawn(fun() ->
            beamtalk_class_events:subscribe(),
            Self ! sub1_subscribed,
            receive
                {class_loaded, _} = Msg -> Self ! {sub1, Msg}
            after 1000 -> Self ! sub1_timeout
            end
        end),
        Sub2 = spawn(fun() ->
            beamtalk_class_events:subscribe(),
            Self ! sub2_subscribed,
            receive
                {class_loaded, _} = Msg -> Self ! {sub2, Msg}
            after 1000 -> Self ! sub2_timeout
            end
        end),
        receive
            sub1_subscribed -> ok
        after 500 -> ok
        end,
        receive
            sub2_subscribed -> ok
        after 500 -> ok
        end,
        %% Sync: both subscribe casts processed
        sys:get_state(Pid),

        beamtalk_class_events:on_class_loaded('BroadcastClass'),
        sys:get_state(Pid),

        receive
            {sub1, {class_loaded, 'BroadcastClass'}} -> ok
        after 500 ->
            ?assert(false)
        end,
        receive
            {sub2, {class_loaded, 'BroadcastClass'}} -> ok
        after 500 ->
            ?assert(false)
        end,
        exit(Sub1, kill),
        exit(Sub2, kill)
    after
        gen_server:stop(Pid),
        flush_messages()
    end.

%%% ===========================================================================
%%% Dead Subscriber Auto-removal
%%% ===========================================================================

dead_subscriber_auto_removed_test() ->
    {ok, Pid} = gen_server:start_link(
        {local, beamtalk_class_events}, beamtalk_class_events, [], []
    ),
    Self = self(),
    try
        SubPid = spawn(fun() ->
            beamtalk_class_events:subscribe(),
            Self ! subscribed,
            receive
                stop -> ok
            end
        end),
        receive
            subscribed -> ok
        after 500 -> ok
        end,
        sys:get_state(Pid),

        exit(SubPid, kill),
        timer:sleep(50),
        %% Sync: DOWN message processed
        sys:get_state(Pid),

        %% Server should still be alive and handle events without crashing
        beamtalk_class_events:on_class_loaded('PostDeadClass'),
        sys:get_state(Pid),
        ?assert(is_process_alive(Pid))
    after
        gen_server:stop(Pid),
        flush_messages()
    end.

%%% ===========================================================================
%%% on_class_loaded when server not running
%%% ===========================================================================

on_class_loaded_when_server_down_is_safe_test() ->
    %% Ensure no registered server
    case whereis(beamtalk_class_events) of
        undefined -> ok;
        ExistingPid -> gen_server:stop(ExistingPid)
    end,
    %% Should return ok without crashing — hits the `undefined` branch
    Result = beamtalk_class_events:on_class_loaded('SomeClass'),
    ?assertEqual(ok, Result).

%%% ===========================================================================
%%% Gen_server Callback Edge Cases
%%% ===========================================================================

unknown_call_returns_error_test() ->
    {ok, Pid} = gen_server:start_link(beamtalk_class_events, [], []),
    ?assertEqual({error, unknown_request}, gen_server:call(Pid, some_unknown_request)),
    gen_server:stop(Pid).

unknown_cast_is_ignored_test() ->
    {ok, Pid} = gen_server:start_link(beamtalk_class_events, [], []),
    gen_server:cast(Pid, some_unknown_cast),
    timer:sleep(10),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid).

unknown_info_is_ignored_test() ->
    {ok, Pid} = gen_server:start_link(beamtalk_class_events, [], []),
    Pid ! {some_unknown, message},
    timer:sleep(10),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid).

%%% ===========================================================================
%%% Internal helpers
%%% ===========================================================================

flush_messages() ->
    receive
        _ -> flush_messages()
    after 0 -> ok
    end.
