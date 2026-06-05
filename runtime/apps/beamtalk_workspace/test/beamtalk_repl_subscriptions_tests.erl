%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_subscriptions_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Tests for the beamtalk_repl_subscriptions facade, focused on the explicit-pid
forms (`subscribe/2`, `unsubscribe/2`, `subscribe_all/1`) added for the Attach
topology (BT-2407). A dist-attached client (Phoenix LiveView) calls these over
`rpc:call/4` and must register *its own* pid as the subscriber rather than the
short-lived RPC proxy that `self()`-based forms would register.

The `classes` stream (`beamtalk_class_events`) is used as the representative
event server because it has the simplest fire API (`on_class_loaded/1`).
""".
-include_lib("eunit/include/eunit.hrl").

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
    {ok, Server} = gen_server:start_link(
        {local, beamtalk_class_events}, beamtalk_class_events, [], []
    ),
    Self = self(),
    try
        SubPid = spawn(fun() ->
            receive
                {class_loaded, _} = Msg -> Self ! {got, Msg}
            after 1000 -> Self ! timeout
            end
        end),

        %% Facade is called from *this* process but registers SubPid — the
        %% behaviour the Attach client depends on over RPC.
        ok = beamtalk_repl_subscriptions:subscribe(classes, SubPid),
        sys:get_state(Server),

        {state, Subs} = sys:get_state(Server),
        ?assert(maps:is_key(SubPid, Subs)),
        %% The caller is NOT registered — only the explicit pid.
        ?assertNot(maps:is_key(Self, Subs)),

        beamtalk_class_events:on_class_loaded('FacadeClass'),
        receive
            {got, {class_loaded, 'FacadeClass'}} -> ok
        after 1000 -> ?assert(false)
        end
    after
        gen_server:stop(Server),
        flush_messages()
    end.

%%% ===========================================================================
%%% unsubscribe/2 removes the given pid
%%% ===========================================================================

unsubscribe_pid_removes_that_pid_test() ->
    {ok, Server} = gen_server:start_link(
        {local, beamtalk_class_events}, beamtalk_class_events, [], []
    ),
    Self = self(),
    try
        SubPid = spawn(fun() ->
            receive
                stop -> ok
            after 2000 -> ok
            end
        end),
        ok = beamtalk_repl_subscriptions:subscribe(classes, SubPid),
        sys:get_state(Server),
        {state, SubsBefore} = sys:get_state(Server),
        ?assert(maps:is_key(SubPid, SubsBefore)),

        ok = beamtalk_repl_subscriptions:unsubscribe(classes, SubPid),
        sys:get_state(Server),
        {state, SubsAfter} = sys:get_state(Server),
        ?assertNot(maps:is_key(SubPid, SubsAfter)),

        SubPid ! stop,
        Self ! done,
        receive
            done -> ok
        after 100 -> ok
        end
    after
        gen_server:stop(Server),
        flush_messages()
    end.

%%% ===========================================================================
%%% subscribe_all/1 covers every stream for the given pid
%%% ===========================================================================

subscribe_all_pid_covers_classes_stream_test() ->
    %% Only the classes server is started here; subscribe_all/1 must tolerate the
    %% other streams' servers being absent (casts to unregistered names are
    %% caught by gen_server:cast, which is a no-op when the name is unregistered).
    {ok, Server} = gen_server:start_link(
        {local, beamtalk_class_events}, beamtalk_class_events, [], []
    ),
    Self = self(),
    try
        SubPid = spawn(fun() ->
            receive
                {class_loaded, _} = Msg -> Self ! {got, Msg}
            after 1000 -> Self ! timeout
            end
        end),

        ok = beamtalk_repl_subscriptions:subscribe_all(SubPid),
        sys:get_state(Server),
        {state, Subs} = sys:get_state(Server),
        ?assert(maps:is_key(SubPid, Subs)),

        beamtalk_class_events:on_class_loaded('AllClass'),
        receive
            {got, {class_loaded, 'AllClass'}} -> ok
        after 1000 -> ?assert(false)
        end
    after
        gen_server:stop(Server),
        flush_messages()
    end.

%%% ===========================================================================
%%% Internal helpers
%%% ===========================================================================

flush_messages() ->
    receive
        _ -> flush_messages()
    after 0 -> ok
    end.
