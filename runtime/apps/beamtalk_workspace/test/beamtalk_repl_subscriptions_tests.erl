%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_subscriptions_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Tests for the `beamtalk_repl_subscriptions` facade after the BT-2531 migration of
the actor/class/bindings/flush push streams onto the SystemAnnouncer bus
(`beamtalk_announcements`).

Focused on the explicit-pid forms (`subscribe/2`, `unsubscribe/2`,
`subscribe_all/1`) added for the Attach topology (BT-2407): a dist-attached client
(Phoenix LiveView) calls these over `rpc:call/4` and must register *its own* pid as
the subscriber. The `classes` stream (`ClassLoaded` / `ClassRemoved`) is the
representative stream; the consumer receives the native
`{beamtalk_announcement, SubRef, Class, Handler, Event}` message the inert-handler
branch of the bus delivers.
""".
-include_lib("eunit/include/eunit.hrl").

%% The inert handler term the facade registers for push-stream subscribers; it
%% rides along inert in the delivered announcement message.
-define(PUSH_HANDLER, repl_push_subscription).

%%% ===========================================================================
%%% streams/0
%%% ===========================================================================

streams_lists_all_five_streams_test() ->
    ?assertEqual(
        [transcript, actors, classes, bindings, flush],
        beamtalk_repl_subscriptions:streams()
    ).

%%% ===========================================================================
%%% subscribe/2 registers the given pid, not the caller
%%% ===========================================================================

subscribe_pid_registers_that_pid_test() ->
    ensure_bus(),
    Self = self(),
    SubPid = spawn_collector(Self),
    try
        %% Facade is called from *this* process but registers SubPid — the
        %% behaviour the Attach client depends on over RPC.
        ok = beamtalk_repl_subscriptions:subscribe(classes, SubPid),

        beamtalk_announcements:system_announce('ClassLoaded', #{className => 'FacadeClass'}),
        receive
            {got, {beamtalk_announcement, _Ref, 'ClassLoaded', ?PUSH_HANDLER, Event}} ->
                ?assertEqual('FacadeClass', maps:get(className, Event))
        after 1000 -> ?assert(false)
        end
    after
        beamtalk_repl_subscriptions:unsubscribe(classes, SubPid),
        stop_collector(SubPid)
    end.

%%% ===========================================================================
%%% classes stream delivers ClassRemoved too (newly visible on the bus, BT-2531)
%%% ===========================================================================

subscribe_classes_delivers_class_removed_test() ->
    ensure_bus(),
    Self = self(),
    SubPid = spawn_collector(Self),
    try
        ok = beamtalk_repl_subscriptions:subscribe(classes, SubPid),
        beamtalk_announcements:system_announce('ClassRemoved', #{className => 'Gone'}),
        receive
            {got, {beamtalk_announcement, _Ref, 'ClassRemoved', ?PUSH_HANDLER, Event}} ->
                ?assertEqual('Gone', maps:get(className, Event))
        after 1000 -> ?assert(false)
        end
    after
        beamtalk_repl_subscriptions:unsubscribe(classes, SubPid),
        stop_collector(SubPid)
    end.

%%% ===========================================================================
%%% unsubscribe/2 removes the given pid
%%% ===========================================================================

unsubscribe_pid_removes_that_pid_test() ->
    ensure_bus(),
    Self = self(),
    SubPid = spawn_collector(Self),
    try
        ok = beamtalk_repl_subscriptions:subscribe(classes, SubPid),
        ok = beamtalk_repl_subscriptions:unsubscribe(classes, SubPid),

        beamtalk_announcements:system_announce('ClassLoaded', #{className => 'NotDelivered'}),
        receive
            {got, _} -> ?assert(false)
        after 200 -> ok
        end
    after
        stop_collector(SubPid)
    end.

%%% ===========================================================================
%%% subscribe_all/1 covers every bus-backed stream for the given pid
%%% ===========================================================================

subscribe_all_pid_covers_bus_streams_test() ->
    ensure_bus(),
    Self = self(),
    SubPid = spawn_collector(Self),
    try
        ok = beamtalk_repl_subscriptions:subscribe_all(SubPid),

        %% A representative event from each bus-backed stream is delivered.
        beamtalk_announcements:system_announce('ClassLoaded', #{className => 'AllClass'}),
        beamtalk_announcements:system_announce('BindingChanged', #{
            name => x, value => 1, sessionId => <<"s">>
        }),
        beamtalk_announcements:system_announce('FlushCompleted', #{files => [<<"a.bt">>]}),

        ?assert(received_class('ClassLoaded')),
        ?assert(received_class('BindingChanged')),
        ?assert(received_class('FlushCompleted'))
    after
        beamtalk_repl_subscriptions:unsubscribe_all(SubPid),
        stop_collector(SubPid)
    end.

%%% ===========================================================================
%%% Internal helpers
%%% ===========================================================================

%% Start the announcements bus if it is not already running (it is a supervised
%% worker in the full runtime; standalone in EUnit).
ensure_bus() ->
    case whereis(beamtalk_announcements) of
        undefined ->
            {ok, _} = beamtalk_announcements:start_link(),
            ok;
        _ ->
            ok
    end.

%% Spawn a process that forwards every announcement message it receives to the
%% test process tagged `{got, Msg}`, until told to stop.
spawn_collector(Reporter) ->
    spawn(fun() -> collector_loop(Reporter) end).

collector_loop(Reporter) ->
    receive
        stop ->
            ok;
        Msg ->
            Reporter ! {got, Msg},
            collector_loop(Reporter)
    end.

stop_collector(SubPid) ->
    SubPid ! stop,
    ok.

%% Whether a `{got, {beamtalk_announcement, _, Class, _, _}}` for `Class` is in the
%% test process mailbox within the timeout. Drains non-matching messages.
received_class(Class) ->
    receive
        {got, {beamtalk_announcement, _Ref, Class, _Handler, _Event}} -> true;
        {got, _Other} -> received_class(Class)
    after 1000 -> false
    end.
