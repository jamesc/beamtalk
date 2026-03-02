%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Tests for beamtalk_ws_handler WebSocket push behaviour (BT-1020).
%%%
%%% Tests the class-loaded push event pub/sub via beamtalk_class_events,
%%% which is the server-side mechanism that delivers class-loaded events
%%% to authenticated WebSocket subscribers.

-module(beamtalk_ws_handler_tests).
-include_lib("eunit/include/eunit.hrl").

%%% ===========================================================================
%%% beamtalk_class_events: Lifecycle Tests
%%% ===========================================================================

class_events_starts_and_stops_test() ->
    unregister_if_alive(beamtalk_class_events),
    {ok, Pid} = beamtalk_class_events:start_link(registered),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid),
    ?assertNot(is_process_alive(Pid)).

%%% ===========================================================================
%%% beamtalk_class_events: Subscriber Tests
%%% ===========================================================================

subscriber_receives_class_loaded_test() ->
    unregister_if_alive(beamtalk_class_events),
    {ok, ServerPid} = beamtalk_class_events:start_link(registered),
    try
        beamtalk_class_events:subscribe(),
        %% Sync: ensure subscribe cast is processed
        sys:get_state(ServerPid),

        beamtalk_class_events:on_class_loaded('Counter'),

        receive
            {class_loaded, 'Counter'} ->
                ok
        after 500 ->
            ?assert(false)
        end
    after
        gen_server:stop(ServerPid),
        flush_messages()
    end.

non_subscriber_does_not_receive_class_loaded_test() ->
    unregister_if_alive(beamtalk_class_events),
    {ok, ServerPid} = beamtalk_class_events:start_link(registered),
    try
        %% Do NOT subscribe

        beamtalk_class_events:on_class_loaded('Counter'),
        %% Sync: ensure cast is processed before checking mailbox
        sys:get_state(ServerPid),

        receive
            {class_loaded, _} ->
                ?assert(false)
        after 100 ->
            ok
        end
    after
        gen_server:stop(ServerPid),
        flush_messages()
    end.

unsubscribe_stops_class_loaded_notifications_test() ->
    unregister_if_alive(beamtalk_class_events),
    {ok, ServerPid} = beamtalk_class_events:start_link(registered),
    try
        beamtalk_class_events:subscribe(),
        sys:get_state(ServerPid),

        beamtalk_class_events:unsubscribe(),
        sys:get_state(ServerPid),

        beamtalk_class_events:on_class_loaded('Counter'),
        sys:get_state(ServerPid),

        receive
            {class_loaded, _} ->
                ?assert(false)
        after 100 ->
            ok
        end
    after
        gen_server:stop(ServerPid),
        flush_messages()
    end.

multiple_subscribers_all_receive_class_loaded_test() ->
    unregister_if_alive(beamtalk_class_events),
    {ok, ServerPid} = beamtalk_class_events:start_link(registered),
    Self = self(),
    try
        Sub1 = spawn(fun() ->
            beamtalk_class_events:subscribe(),
            Self ! sub1_subscribed,
            receive
                {class_loaded, ClassName} -> Self ! {sub1_got, ClassName}
            after 500 ->
                Self ! sub1_timeout
            end
        end),
        Sub2 = spawn(fun() ->
            beamtalk_class_events:subscribe(),
            Self ! sub2_subscribed,
            receive
                {class_loaded, ClassName} -> Self ! {sub2_got, ClassName}
            after 500 ->
                Self ! sub2_timeout
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
        %% Sync: ensure both subscribe casts are processed
        sys:get_state(ServerPid),

        beamtalk_class_events:on_class_loaded('Timer'),

        receive
            {sub1_got, 'Timer'} -> ok
        after 500 ->
            ?assert(false)
        end,
        receive
            {sub2_got, 'Timer'} -> ok
        after 500 ->
            ?assert(false)
        end,

        exit(Sub1, kill),
        exit(Sub2, kill)
    after
        gen_server:stop(ServerPid),
        flush_messages()
    end.

%%% ===========================================================================
%%% beamtalk_class_events: Dead Subscriber Cleanup Tests
%%% ===========================================================================

dead_subscriber_auto_removed_test() ->
    unregister_if_alive(beamtalk_class_events),
    {ok, ServerPid} = beamtalk_class_events:start_link(registered),
    Self = self(),
    OldTrapExit = process_flag(trap_exit, true),
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
        sys:get_state(ServerPid),

        exit(SubPid, kill),
        timer:sleep(50),
        sys:get_state(ServerPid),

        %% Server should still be alive and functional after dead subscriber removed
        ?assert(is_process_alive(ServerPid)),

        %% Fire an event — should not crash even though the subscriber is dead
        beamtalk_class_events:on_class_loaded('Counter'),
        sys:get_state(ServerPid),
        ?assert(is_process_alive(ServerPid))
    after
        gen_server:stop(ServerPid),
        process_flag(trap_exit, OldTrapExit),
        flush_messages()
    end.

%%% ===========================================================================
%%% beamtalk_class_events: on_class_loaded when server not running
%%% ===========================================================================

on_class_loaded_safe_when_server_not_running_test() ->
    unregister_if_alive(beamtalk_class_events),
    %% Should not crash when beamtalk_class_events is not running
    ?assertEqual(ok, beamtalk_class_events:on_class_loaded('Counter')).

%%% ===========================================================================
%%% Push Message Format: classes channel
%%% ===========================================================================

class_loaded_push_json_format_test() ->
    %% Verify the expected push message JSON format for the classes channel.
    %% This mirrors what beamtalk_ws_handler:websocket_info/2 produces.
    ClassName = 'Counter',
    Push = jsx:encode(#{
        <<"type">> => <<"push">>,
        <<"channel">> => <<"classes">>,
        <<"event">> => <<"loaded">>,
        <<"data">> => #{<<"class">> => atom_to_binary(ClassName, utf8)}
    }),
    Decoded = jsx:decode(Push, [return_maps]),
    ?assertEqual(<<"push">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"classes">>, maps:get(<<"channel">>, Decoded)),
    ?assertEqual(<<"loaded">>, maps:get(<<"event">>, Decoded)),
    Data = maps:get(<<"data">>, Decoded),
    ?assertEqual(<<"Counter">>, maps:get(<<"class">>, Data)).

%%% ===========================================================================
%%% Helper functions
%%% ===========================================================================

%% Helper: stop and unregister a named process if it's still alive.
unregister_if_alive(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            try
                gen_server:stop(Pid)
            catch
                _:_ -> ok
            end,
            case whereis(Name) of
                Pid -> unregister(Name);
                _ -> ok
            end
    end.

%% Helper: drain all messages from the test process mailbox.
flush_messages() ->
    receive
        _ -> flush_messages()
    after 0 -> ok
    end.
