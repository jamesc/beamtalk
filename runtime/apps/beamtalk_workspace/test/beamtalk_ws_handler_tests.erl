%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_ws_handler_tests).

-moduledoc """
Tests for beamtalk_ws_handler WebSocket push behaviour (BT-1020).

Tests the class-loaded push event pub/sub via beamtalk_class_events,
which is the server-side mechanism that delivers class-loaded events
to authenticated WebSocket subscribers.
""".
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
            sys:get_state(ServerPid),
            Self ! sub1_subscribed,
            receive
                {class_loaded, ClassName} -> Self ! {sub1_got, ClassName}
            after 500 ->
                Self ! sub1_timeout
            end
        end),
        Sub2 = spawn(fun() ->
            beamtalk_class_events:subscribe(),
            sys:get_state(ServerPid),
            Self ! sub2_subscribed,
            receive
                {class_loaded, ClassName} -> Self ! {sub2_got, ClassName}
            after 500 ->
                Self ! sub2_timeout
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
            sys:get_state(ServerPid),
            Self ! subscribed,
            receive
                stop -> ok
            end
        end),
        receive
            subscribed -> ok
        after 500 -> ?assert(false)
        end,
        sys:get_state(ServerPid),

        Ref = erlang:monitor(process, SubPid),
        exit(SubPid, kill),
        receive
            {'DOWN', Ref, process, SubPid, killed} -> ok
        after 500 ->
            ?assert(false)
        end,
        %% sys:get_state/1 serializes with the server, guaranteeing the DOWN
        %% message has been processed and the subscriber removed.
        {state, Subscribers} = sys:get_state(ServerPid),
        ?assertNot(maps:is_key(SubPid, Subscribers)),

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
%%% methods op: list_class_methods_for_ws/1
%%% ===========================================================================

methods_op_unknown_class_returns_empty_list_test() ->
    %% An atom that has never been registered returns []
    Result = beamtalk_repl_ops_dev:list_class_methods_for_ws(<<"NonExistentClass12345">>),
    ?assertEqual([], Result).

methods_op_empty_class_name_returns_empty_list_test() ->
    Result = beamtalk_repl_ops_dev:list_class_methods_for_ws(<<"">>),
    ?assertEqual([], Result).

methods_op_known_class_returns_instance_and_class_methods_test() ->
    beamtalk_class_registry:ensure_pg_started(),
    ClassName = 'BT1026TestClass',
    RegName = beamtalk_class_registry:registry_name(ClassName),
    %% Clean up any leftover registration from a previous test run
    case whereis(RegName) of
        undefined -> ok;
        Pid0 -> gen_server:stop(Pid0)
    end,
    InstanceFun = fun(_Self) -> ok end,
    ClassFun = fun(_Self) -> ok end,
    ClassInfo = #{
        name => ClassName,
        module => bt_test_module,
        superclass => none,
        instance_methods => #{
            increment => #{block => InstanceFun, arity => 1},
            value => #{block => InstanceFun, arity => 1}
        },
        class_methods => #{
            new => #{block => ClassFun, arity => 1}
        }
    },
    {ok, Pid} = beamtalk_object_class:start(ClassName, ClassInfo),
    try
        Result = beamtalk_repl_ops_dev:list_class_methods_for_ws(<<"BT1026TestClass">>),
        %% Check total count: 2 instance + 1 class
        ?assertEqual(3, length(Result)),
        %% Check all entries have required keys
        lists:foreach(
            fun(Entry) ->
                ?assert(maps:is_key(<<"name">>, Entry)),
                ?assert(maps:is_key(<<"selector">>, Entry)),
                ?assert(maps:is_key(<<"side">>, Entry)),
                Side = maps:get(<<"side">>, Entry),
                ?assert(Side =:= <<"instance">> orelse Side =:= <<"class">>)
            end,
            Result
        ),
        %% Check instance methods are present
        InstanceEntries = [E || E <- Result, maps:get(<<"side">>, E) =:= <<"instance">>],
        ?assertEqual(2, length(InstanceEntries)),
        InstanceSelectors = [maps:get(<<"selector">>, E) || E <- InstanceEntries],
        ?assert(lists:member(<<"increment">>, InstanceSelectors)),
        ?assert(lists:member(<<"value">>, InstanceSelectors)),
        %% Check class-side method is present
        ClassEntries = [E || E <- Result, maps:get(<<"side">>, E) =:= <<"class">>],
        ?assertEqual(1, length(ClassEntries)),
        ?assertEqual(<<"new">>, maps:get(<<"selector">>, hd(ClassEntries)))
    after
        gen_server:stop(Pid)
    end.

methods_op_protocol_json_shape_test() ->
    %% Exercise the full "methods" op handler and assert on JSON response shape.
    beamtalk_class_registry:ensure_pg_started(),
    ClassName = 'BT1026ProtoTestClass',
    RegName = beamtalk_class_registry:registry_name(ClassName),
    case whereis(RegName) of
        undefined -> ok;
        Pid0 -> gen_server:stop(Pid0)
    end,
    Fun = fun(_Self) -> ok end,
    ClassInfo = #{
        name => ClassName,
        module => bt_test_module_proto,
        superclass => none,
        instance_methods => #{
            value => #{block => Fun, arity => 1}
        },
        class_methods => #{
            new => #{block => Fun, arity => 1}
        }
    },
    {ok, Pid} = beamtalk_object_class:start(ClassName, ClassInfo),
    try
        %% Build a protocol message via decode, then call handle/4
        Json = iolist_to_binary(
            json:encode(#{
                <<"op">> => <<"methods">>,
                <<"id">> => <<"test-1">>,
                <<"class">> => <<"BT1026ProtoTestClass">>
            })
        ),
        {ok, Msg} = beamtalk_repl_protocol:decode(Json),
        Params = beamtalk_repl_protocol:get_params(Msg),
        %% SessionPid unused for methods op, pass self()
        Reply = beamtalk_repl_ops_dev:handle(<<"methods">>, Params, Msg, self()),
        Decoded = json:decode(Reply),
        %% Must have status, methods, and id fields
        ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
        ?assert(maps:is_key(<<"methods">>, Decoded)),
        ?assertEqual(<<"test-1">>, maps:get(<<"id">>, Decoded)),
        %% 1 instance + 1 class method
        Methods = maps:get(<<"methods">>, Decoded),
        ?assertEqual(2, length(Methods))
    after
        gen_server:stop(Pid)
    end.

methods_op_name_equals_selector_test() ->
    beamtalk_class_registry:ensure_pg_started(),
    ClassName = 'BT1026NameSelectorClass',
    RegName = beamtalk_class_registry:registry_name(ClassName),
    case whereis(RegName) of
        undefined -> ok;
        Pid0 -> gen_server:stop(Pid0)
    end,
    Fun = fun(_Self) -> ok end,
    ClassInfo = #{
        name => ClassName,
        module => bt_test_module2,
        superclass => none,
        instance_methods => #{
            'printString' => #{block => Fun, arity => 1}
        },
        class_methods => #{}
    },
    {ok, Pid} = beamtalk_object_class:start(ClassName, ClassInfo),
    try
        [Entry] = beamtalk_repl_ops_dev:list_class_methods_for_ws(<<"BT1026NameSelectorClass">>),
        ?assertEqual(maps:get(<<"name">>, Entry), maps:get(<<"selector">>, Entry)),
        ?assertEqual(<<"printString">>, maps:get(<<"selector">>, Entry))
    after
        gen_server:stop(Pid)
    end.

%%% ===========================================================================
%%% Push Message Format: classes channel
%%% ===========================================================================

class_loaded_push_json_format_test() ->
    %% Verify the expected push message JSON format for the classes channel.
    %% This mirrors what beamtalk_ws_handler:websocket_info/2 produces.
    ClassName = 'Counter',
    Push = iolist_to_binary(
        json:encode(#{
            <<"type">> => <<"push">>,
            <<"channel">> => <<"classes">>,
            <<"event">> => <<"loaded">>,
            <<"data">> => #{<<"class">> => atom_to_binary(ClassName, utf8)}
        })
    ),
    Decoded = json:decode(Push),
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
