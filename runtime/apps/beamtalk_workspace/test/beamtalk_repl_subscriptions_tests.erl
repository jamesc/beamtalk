%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_subscriptions_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Tests for the `beamtalk_repl_subscriptions` facade after the migration of
the actor/class/bindings/flush push streams onto the SystemAnnouncer bus
(`beamtalk_announcements`).

Focused on the explicit-pid forms (`subscribe/2`, `unsubscribe/2`,
`subscribe_all/1`) added for the Attach topology: a dist-attached client
(Phoenix LiveView) calls these over `rpc:call/4` and must register *its own* pid as
the subscriber. The `classes` stream (`ClassLoaded` / `ClassRemoved`) is the
representative stream; the consumer receives the native
`{beamtalk_announcement, SubRef, Class, Handler, Event}` message the inert-handler
branch of the bus delivers.
""".
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

%% The inert handler term the facade registers for push-stream subscribers; it
%% rides along inert in the delivered announcement message.
-define(PUSH_HANDLER, repl_push_subscription).

%%% ===========================================================================
%%% streams/0
%%% ===========================================================================

streams_lists_all_six_streams_test() ->
    ?assertEqual(
        [transcript, actors, classes, bindings, flush, reload_check],
        beamtalk_repl_subscriptions:streams()
    ).

%%% ===========================================================================
%%% subscribe/2 registers the given pid, not the caller
%%% ===========================================================================

subscribe_pid_registers_that_pid() ->
    ensure_bus(),
    Self = self(),
    SubPid = spawn_collector(Self),
    try
        %% Facade is called from *this* process but registers SubPid — the
        %% behaviour the Attach client depends on over RPC.
        ok = beamtalk_repl_subscriptions:subscribe(classes, SubPid),

        beamtalk_announcements:system_announce('ClassLoaded', #{className => 'FacadeClass'}),
        ?awaitMessage(
            {got, {beamtalk_announcement, _Ref, 'ClassLoaded', ?PUSH_HANDLER, Event}}, 1000
        ),
        ?assertEqual('FacadeClass', maps:get(className, Event))
    after
        beamtalk_repl_subscriptions:unsubscribe(classes, SubPid),
        stop_collector(SubPid)
    end.

%%% ===========================================================================
%%% classes stream delivers ClassRemoved too (newly visible on the bus)
%%% ===========================================================================

subscribe_classes_delivers_class_removed() ->
    ensure_bus(),
    Self = self(),
    SubPid = spawn_collector(Self),
    try
        ok = beamtalk_repl_subscriptions:subscribe(classes, SubPid),
        beamtalk_announcements:system_announce('ClassRemoved', #{className => 'Gone'}),
        ?awaitMessage(
            {got, {beamtalk_announcement, _Ref, 'ClassRemoved', ?PUSH_HANDLER, Event}}, 1000
        ),
        ?assertEqual('Gone', maps:get(className, Event))
    after
        beamtalk_repl_subscriptions:unsubscribe(classes, SubPid),
        stop_collector(SubPid)
    end.

%%% ===========================================================================
%%% unsubscribe/2 removes the given pid
%%% ===========================================================================

unsubscribe_pid_removes_that_pid() ->
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
%%% bindings stream carries the session id (clear/put/remove
%%% refresh path uses announce_binding_changed/3 with an explicit session id)
%%% ===========================================================================

binding_changed_carries_explicit_session_id() ->
    ensure_bus(),
    Self = self(),
    SubPid = spawn_collector(Self),
    try
        ok = beamtalk_repl_subscriptions:subscribe(bindings, SubPid),
        %% The shell's clear / put / remove paths call this with an explicit session
        %% id (the shell process has no `beamtalk_session_id` in its dictionary).
        ok = beamtalk_repl_eval:announce_binding_changed(x, 42, <<"sess-xyz">>),
        ?awaitMessage(
            {got, {beamtalk_announcement, _Ref, 'BindingChanged', ?PUSH_HANDLER, Event}}, 1000
        ),
        ?assertEqual(<<"sess-xyz">>, maps:get(sessionId, Event)),
        ?assertEqual(x, maps:get(name, Event)),
        ?assertEqual(42, maps:get(value, Event))
    after
        beamtalk_repl_subscriptions:unsubscribe(bindings, SubPid),
        stop_collector(SubPid)
    end.

%%% ===========================================================================
%%% subscribe_bus is idempotent per pid+class (dedup re-mount)
%%% ===========================================================================

subscribe_twice_delivers_once() ->
    ensure_bus(),
    Self = self(),
    SubPid = spawn_collector(Self),
    try
        %% Re-subscribing the same pid (as a fast re-mount would) must not leave a
        %% duplicate subscription that double-delivers each event.
        ok = beamtalk_repl_subscriptions:subscribe(bindings, SubPid),
        ok = beamtalk_repl_subscriptions:subscribe(bindings, SubPid),

        beamtalk_announcements:system_announce('BindingChanged', #{
            name => y, value => 1, sessionId => <<"s">>
        }),
        ?awaitMessage(
            {got, {beamtalk_announcement, _Ref, 'BindingChanged', ?PUSH_HANDLER, _Event}}, 1000
        ),
        %% Exactly one delivery — no duplicate from the second subscribe.
        receive
            {got, {beamtalk_announcement, _R2, 'BindingChanged', ?PUSH_HANDLER, _E2}} ->
                ?assert(false)
        after 200 -> ok
        end
    after
        beamtalk_repl_subscriptions:unsubscribe(bindings, SubPid),
        stop_collector(SubPid)
    end.

%%% ===========================================================================
%%% subscribe_all/1 covers every bus-backed stream for the given pid
%%% ===========================================================================

subscribe_all_pid_covers_bus_streams() ->
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

%% The bus tests run under one fixture so a bus started bare here is stopped again:
%% left running it would make the next module's `ensure_all_started(beamtalk_runtime)`
%% fail with `already_started` (BT-4962).
bus_test_() ->
    {setup, fun() -> ok end,
        fun(_) -> ok = beamtalk_test_proc:stop_if_bare(beamtalk_announcements) end, [
            fun subscribe_pid_registers_that_pid/0,
            fun subscribe_classes_delivers_class_removed/0,
            fun unsubscribe_pid_removes_that_pid/0,
            fun binding_changed_carries_explicit_session_id/0,
            fun subscribe_twice_delivers_once/0,
            fun subscribe_all_pid_covers_bus_streams/0
        ]}.

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

%%% ===========================================================================
%%% BT-5206: every push stream goes through the facade
%%% ===========================================================================

%% The logs stream is named by the facade, so the WebSocket handler need not reach
%% for `beamtalk_ws_log_handler`: subscribing registers the caller, unsubscribing
%% removes it, and a repeat unsubscribe is harmless.
logs_stream_subscribes_and_unsubscribes_the_caller_test() ->
    Self = self(),
    ?assertEqual(ok, beamtalk_repl_subscriptions:subscribe_logs(warning)),
    ?assertMatch([{Self, warning, _, _}], ets:lookup(beamtalk_ws_log_subscribers, Self)),
    %% A repeat call changes the level of the one subscription.
    ?assertEqual(ok, beamtalk_repl_subscriptions:subscribe_logs(error)),
    ?assertMatch([{Self, error, _, _}], ets:lookup(beamtalk_ws_log_subscribers, Self)),
    ?assertEqual(ok, beamtalk_repl_subscriptions:unsubscribe_logs()),
    ?assertEqual([], ets:lookup(beamtalk_ws_log_subscribers, Self)),
    ?assertEqual(ok, beamtalk_repl_subscriptions:unsubscribe_logs()).

%% No client casts `{subscribe, self()}` at a gen_server, and the WebSocket handler
%% reaches every push stream through the facade: it names none of the modules the
%% facade wraps. A source scan, so a later direct call fails here.
ws_handler_reaches_push_streams_only_through_the_facade_test() ->
    {source, Path} = lists:keyfind(source, 1, beamtalk_ws_handler:module_info(compile)),
    {ok, Source} = file:read_file(Path),
    Direct = [
        <<"beamtalk_ws_log_handler:subscribe">>,
        <<"beamtalk_ws_log_handler:unsubscribe">>,
        <<"beamtalk_transcript_stream:subscribe">>,
        <<"beamtalk_transcript_stream:unsubscribe">>,
        <<"beamtalk_announcements:subscribe">>,
        <<"beamtalk_announcements:system_unsubscribe">>,
        <<"beamtalk_object_watch:subscribe">>,
        <<"{subscribe, self()}">>,
        <<"{unsubscribe, self()}">>
    ],
    Found = [P || P <- Direct, binary:match(Source, P) =/= nomatch],
    ?assertEqual([], Found).
