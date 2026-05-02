%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_bindings_events_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc "Tests for beamtalk_bindings_events pub/sub gen_server.".
-include_lib("eunit/include/eunit.hrl").

%%% ===========================================================================
%%% Lifecycle Tests
%%% ===========================================================================

server_starts_and_stops_test() ->
    {ok, Pid} = gen_server:start_link(beamtalk_bindings_events, [], []),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid),
    ?assertNot(is_process_alive(Pid)).

%%% ===========================================================================
%%% Subscribe / Unsubscribe Tests
%%% ===========================================================================

subscribe_receives_bindings_changed_notification_test() ->
    {ok, Pid} = gen_server:start_link(
        {local, beamtalk_bindings_events}, beamtalk_bindings_events, [], []
    ),
    try
        beamtalk_bindings_events:subscribe(),
        %% Sync: ensure cast is processed before we fire the event
        sys:get_state(Pid),

        beamtalk_bindings_events:on_bindings_changed(<<"sess-1">>),
        sys:get_state(Pid),

        receive
            {bindings_changed, <<"sess-1">>} -> ok
        after 500 ->
            ?assert(false)
        end
    after
        gen_server:stop(Pid),
        flush_messages()
    end.

duplicate_subscribe_is_idempotent_test() ->
    {ok, Pid} = gen_server:start_link(
        {local, beamtalk_bindings_events}, beamtalk_bindings_events, [], []
    ),
    try
        beamtalk_bindings_events:subscribe(),
        beamtalk_bindings_events:subscribe(),
        sys:get_state(Pid),

        beamtalk_bindings_events:on_bindings_changed(<<"sess-dup">>),
        sys:get_state(Pid),

        %% Should receive exactly one notification, not two
        receive
            {bindings_changed, <<"sess-dup">>} -> ok
        after 500 ->
            ?assert(false)
        end,
        receive
            {bindings_changed, <<"sess-dup">>} ->
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
        {local, beamtalk_bindings_events}, beamtalk_bindings_events, [], []
    ),
    try
        beamtalk_bindings_events:subscribe(),
        sys:get_state(Pid),

        beamtalk_bindings_events:unsubscribe(),
        sys:get_state(Pid),

        beamtalk_bindings_events:on_bindings_changed(<<"sess-2">>),
        sys:get_state(Pid),

        receive
            {bindings_changed, <<"sess-2">>} ->
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
        {local, beamtalk_bindings_events}, beamtalk_bindings_events, [], []
    ),
    try
        %% Should not crash — hits the `error` branch in maps:find
        beamtalk_bindings_events:unsubscribe(),
        sys:get_state(Pid),
        ?assert(is_process_alive(Pid))
    after
        gen_server:stop(Pid),
        flush_messages()
    end.

%%% ===========================================================================
%%% Multi-subscriber broadcast
%%% ===========================================================================

multiple_subscribers_all_notified_test() ->
    {ok, Pid} = gen_server:start_link(
        {local, beamtalk_bindings_events}, beamtalk_bindings_events, [], []
    ),
    Self = self(),
    try
        Sub1 = spawn(fun() ->
            beamtalk_bindings_events:subscribe(),
            %% Same-sender sync: subscribe cast is processed before this returns
            sys:get_state(beamtalk_bindings_events),
            Self ! sub1_subscribed,
            receive
                {bindings_changed, _} = Msg -> Self ! {sub1, Msg}
            after 1000 -> Self ! sub1_timeout
            end
        end),
        Sub2 = spawn(fun() ->
            beamtalk_bindings_events:subscribe(),
            sys:get_state(beamtalk_bindings_events),
            Self ! sub2_subscribed,
            receive
                {bindings_changed, _} = Msg -> Self ! {sub2, Msg}
            after 1000 -> Self ! sub2_timeout
            end
        end),
        receive
            sub1_subscribed -> ok
        after 500 -> ?assert(false)
        end,
        receive
            sub2_subscribed -> ok
        after 500 -> ?assert(false)
        end,

        beamtalk_bindings_events:on_bindings_changed(<<"sess-broadcast">>),
        sys:get_state(Pid),

        receive
            {sub1, {bindings_changed, <<"sess-broadcast">>}} -> ok
        after 500 ->
            ?assert(false)
        end,
        receive
            {sub2, {bindings_changed, <<"sess-broadcast">>}} -> ok
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
        {local, beamtalk_bindings_events}, beamtalk_bindings_events, [], []
    ),
    Self = self(),
    try
        SubPid = spawn(fun() ->
            beamtalk_bindings_events:subscribe(),
            %% Same-sender sync: subscribe cast is processed before notifying parent
            sys:get_state(beamtalk_bindings_events),
            Self ! subscribed,
            receive
                stop -> ok
            end
        end),
        receive
            subscribed -> ok
        after 500 -> ?assert(false)
        end,

        %% Subscriber should be present in the map before kill
        {state, SubsBefore} = sys:get_state(Pid),
        ?assert(maps:is_key(SubPid, SubsBefore)),

        exit(SubPid, kill),
        %% Wait for DOWN to be processed; sys:get_state alone races with the
        %% DOWN message because they originate from different senders.
        wait_until_removed(Pid, SubPid, 20),

        %% Subscriber should be gone from the map
        {state, SubsAfter} = sys:get_state(Pid),
        ?assertNot(maps:is_key(SubPid, SubsAfter)),

        %% Server should still be alive and handle events without crashing
        beamtalk_bindings_events:on_bindings_changed(<<"sess-after-dead">>),
        sys:get_state(Pid),
        ?assert(is_process_alive(Pid))
    after
        gen_server:stop(Pid),
        flush_messages()
    end.

%%% ===========================================================================
%%% on_bindings_changed when server not running
%%% ===========================================================================

on_bindings_changed_when_server_down_is_safe_test() ->
    %% Ensure no registered server
    case whereis(beamtalk_bindings_events) of
        undefined -> ok;
        ExistingPid -> gen_server:stop(ExistingPid)
    end,
    %% Should return ok without crashing — hits the `undefined` branch
    Result = beamtalk_bindings_events:on_bindings_changed(<<"sess-no-server">>),
    ?assertEqual(ok, Result).

%%% ===========================================================================
%%% Gen_server Callback Edge Cases
%%% ===========================================================================

unknown_call_returns_error_test() ->
    {ok, Pid} = gen_server:start_link(beamtalk_bindings_events, [], []),
    ?assertEqual({error, unknown_request}, gen_server:call(Pid, some_unknown_request)),
    gen_server:stop(Pid).

unknown_cast_is_ignored_test() ->
    {ok, Pid} = gen_server:start_link(beamtalk_bindings_events, [], []),
    gen_server:cast(Pid, some_unknown_cast),
    sys:get_state(Pid),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid).

unknown_info_is_ignored_test() ->
    {ok, Pid} = gen_server:start_link(beamtalk_bindings_events, [], []),
    Pid ! {some_unknown, message},
    sys:get_state(Pid),
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

%% Poll the server state until Pid is no longer in the subscribers map.
%% Needed because the DOWN message and our sys:get_state come from different
%% senders, so ordering between them is not guaranteed.
wait_until_removed(_Server, _Pid, 0) ->
    ?assert(false);
wait_until_removed(Server, Pid, N) ->
    {state, Subs} = sys:get_state(Server),
    case maps:is_key(Pid, Subs) of
        false ->
            ok;
        true ->
            timer:sleep(10),
            wait_until_removed(Server, Pid, N - 1)
    end.
