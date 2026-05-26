%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_flush_events_tests).

%%% **DDD Context:** Workspace Context

-moduledoc "Tests for beamtalk_flush_events pub/sub gen_server (ADR 0082 Phase 3, BT-2289).".
-include_lib("eunit/include/eunit.hrl").

%%% ===========================================================================
%%% Lifecycle Tests
%%% ===========================================================================

server_starts_and_stops_test() ->
    {ok, Pid} = gen_server:start_link(beamtalk_flush_events, [], []),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid),
    ?assertNot(is_process_alive(Pid)).

%%% ===========================================================================
%%% Subscribe / Unsubscribe Tests
%%% ===========================================================================

subscribe_receives_flush_completed_notification_test() ->
    {ok, Pid} = gen_server:start_link(
        {local, beamtalk_flush_events}, beamtalk_flush_events, [], []
    ),
    try
        beamtalk_flush_events:subscribe(),
        sys:get_state(Pid),

        beamtalk_flush_events:on_files_flushed([<<"src/foo.bt">>, <<"src/bar.bt">>]),
        sys:get_state(Pid),

        receive
            {flush_completed, [<<"src/foo.bt">>, <<"src/bar.bt">>]} -> ok
        after 500 ->
            ?assert(false)
        end
    after
        gen_server:stop(Pid),
        flush_messages()
    end.

duplicate_subscribe_is_idempotent_test() ->
    {ok, Pid} = gen_server:start_link(
        {local, beamtalk_flush_events}, beamtalk_flush_events, [], []
    ),
    try
        beamtalk_flush_events:subscribe(),
        beamtalk_flush_events:subscribe(),
        sys:get_state(Pid),

        beamtalk_flush_events:on_files_flushed([<<"src/dup.bt">>]),
        sys:get_state(Pid),

        receive
            {flush_completed, [<<"src/dup.bt">>]} -> ok
        after 500 ->
            ?assert(false)
        end,
        receive
            {flush_completed, _} ->
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
        {local, beamtalk_flush_events}, beamtalk_flush_events, [], []
    ),
    try
        beamtalk_flush_events:subscribe(),
        sys:get_state(Pid),

        beamtalk_flush_events:unsubscribe(),
        sys:get_state(Pid),

        beamtalk_flush_events:on_files_flushed([<<"src/unsubbed.bt">>]),
        sys:get_state(Pid),

        receive
            {flush_completed, _} ->
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
        {local, beamtalk_flush_events}, beamtalk_flush_events, [], []
    ),
    try
        beamtalk_flush_events:unsubscribe(),
        sys:get_state(Pid),
        ?assert(is_process_alive(Pid))
    after
        gen_server:stop(Pid),
        flush_messages()
    end.

%%% ===========================================================================
%%% Empty-list short-circuit
%%% ===========================================================================

empty_files_list_skips_broadcast_test() ->
    {ok, Pid} = gen_server:start_link(
        {local, beamtalk_flush_events}, beamtalk_flush_events, [], []
    ),
    try
        beamtalk_flush_events:subscribe(),
        sys:get_state(Pid),

        %% Empty list short-circuits — no message should reach the subscriber.
        ok = beamtalk_flush_events:on_files_flushed([]),
        sys:get_state(Pid),

        receive
            {flush_completed, _} -> ?assert(false)
        after 50 -> ok
        end
    after
        gen_server:stop(Pid),
        flush_messages()
    end.

%%% ===========================================================================
%%% Multi-subscriber broadcast
%%% ===========================================================================

multiple_subscribers_all_notified_test() ->
    {ok, Pid} = gen_server:start_link(
        {local, beamtalk_flush_events}, beamtalk_flush_events, [], []
    ),
    Self = self(),
    try
        Sub1 = spawn(fun() ->
            beamtalk_flush_events:subscribe(),
            sys:get_state(beamtalk_flush_events),
            Self ! sub1_subscribed,
            receive
                {flush_completed, _} = Msg -> Self ! {sub1, Msg}
            after 1000 -> Self ! sub1_timeout
            end
        end),
        Sub2 = spawn(fun() ->
            beamtalk_flush_events:subscribe(),
            sys:get_state(beamtalk_flush_events),
            Self ! sub2_subscribed,
            receive
                {flush_completed, _} = Msg -> Self ! {sub2, Msg}
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

        beamtalk_flush_events:on_files_flushed([<<"src/broadcast.bt">>]),
        sys:get_state(Pid),

        receive
            {sub1, {flush_completed, [<<"src/broadcast.bt">>]}} -> ok
        after 500 -> ?assert(false)
        end,
        receive
            {sub2, {flush_completed, [<<"src/broadcast.bt">>]}} -> ok
        after 500 -> ?assert(false)
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
        {local, beamtalk_flush_events}, beamtalk_flush_events, [], []
    ),
    Self = self(),
    try
        SubPid = spawn(fun() ->
            beamtalk_flush_events:subscribe(),
            sys:get_state(beamtalk_flush_events),
            Self ! subscribed,
            receive
                stop -> ok
            end
        end),
        receive
            subscribed -> ok
        after 500 -> ?assert(false)
        end,

        {state, SubsBefore} = sys:get_state(Pid),
        ?assert(maps:is_key(SubPid, SubsBefore)),

        exit(SubPid, kill),
        wait_until_removed(Pid, SubPid, 20),

        {state, SubsAfter} = sys:get_state(Pid),
        ?assertNot(maps:is_key(SubPid, SubsAfter)),

        beamtalk_flush_events:on_files_flushed([<<"src/post-dead.bt">>]),
        sys:get_state(Pid),
        ?assert(is_process_alive(Pid))
    after
        gen_server:stop(Pid),
        flush_messages()
    end.

%%% ===========================================================================
%%% on_files_flushed when server not running
%%% ===========================================================================

on_files_flushed_when_server_down_is_safe_test() ->
    case whereis(beamtalk_flush_events) of
        undefined -> ok;
        ExistingPid -> gen_server:stop(ExistingPid)
    end,
    %% Should return ok without crashing — hits the `undefined` branch.
    Result = beamtalk_flush_events:on_files_flushed([<<"src/dropped.bt">>]),
    ?assertEqual(ok, Result).

%%% ===========================================================================
%%% Gen_server Callback Edge Cases
%%% ===========================================================================

unknown_call_returns_error_test() ->
    {ok, Pid} = gen_server:start_link(beamtalk_flush_events, [], []),
    ?assertEqual({error, unknown_request}, gen_server:call(Pid, some_unknown_request)),
    gen_server:stop(Pid).

unknown_cast_is_ignored_test() ->
    {ok, Pid} = gen_server:start_link(beamtalk_flush_events, [], []),
    gen_server:cast(Pid, some_unknown_cast),
    sys:get_state(Pid),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid).

unknown_info_is_ignored_test() ->
    {ok, Pid} = gen_server:start_link(beamtalk_flush_events, [], []),
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
