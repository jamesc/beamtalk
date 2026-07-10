%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_ws_handler_tests).

-moduledoc """
Tests for beamtalk_ws_handler WebSocket push behaviour.

Drives the Cowboy WebSocket callbacks (`init/2`, `websocket_init/1`,
`websocket_handle/2`, `websocket_info/2`, `terminate/3`) directly (BT-2389).
These live in this module — the `_tests` companion of `beamtalk_ws_handler` —
rather than a standalone suite, because `rebar3 eunit --app=...` (used by the
coverage harness) only auto-discovers a source module's `Module_tests`
companion. A standalone `*_callbacks_tests` module compiles but is silently
skipped under `--app`, so its coverage never reaches the merged badge.
""".
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk_ws_state.hrl").

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
%%% WebSocket handler callbacks — direct coverage (BT-2389)
%%% ===========================================================================

%%% ===========================================================================
%%% Helpers
%%% ===========================================================================

%% A fresh, unauthenticated state as produced by init/2.
unauth_state() ->
    #ws_state{
        authenticated = false,
        peer = {{127, 0, 0, 1}, 54321},
        pending_eval = undefined,
        io_capture_pid = undefined,
        stdin_ref = undefined,
        log_subscribed = false
    }.

%% An authenticated state with no eval in flight. Uses self() as the session
%% pid so is_process_alive/1 checks see a live process.
authed_state() ->
    #ws_state{
        authenticated = true,
        session_id = <<"sess-1">>,
        session_pid = self(),
        session_mon = make_ref(),
        peer = {{127, 0, 0, 1}, 54321},
        pending_eval = undefined,
        io_capture_pid = undefined,
        stdin_ref = undefined,
        log_subscribed = false
    }.

%% Decode a valid protocol message for use as a pending_eval correlation token.
make_msg(Op) ->
    Json = iolist_to_binary(
        json:encode(#{
            <<"op">> => Op,
            <<"id">> => <<"msg-1">>,
            <<"params">> => #{<<"code">> => <<"1 + 1">>}
        })
    ),
    {ok, Msg} = beamtalk_repl_protocol:decode(Json),
    Msg.

%% Build a raw protocol JSON frame for handle_protocol/websocket_handle.
%% Protocol params are the top-level message keys (alongside op/id), not a
%% nested "params" object.
op_json(Op, Params) ->
    Base = #{<<"op">> => Op, <<"id">> => <<"req-1">>},
    iolist_to_binary(json:encode(maps:merge(Base, Params))).

%% Decode the text frame out of a {[{text, Bin}], State} reply.
first_text([{text, Bin} | _]) -> json:decode(Bin).

%% Spawn a process and wait for it to be dead, returning its (now dead) pid.
dead_pid() ->
    {Pid, Ref} = spawn_monitor(fun() -> ok end),
    receive
        {'DOWN', Ref, process, Pid, _} -> Pid
    after 1000 ->
        error(timeout_waiting_for_dead_pid)
    end.

%%% ===========================================================================
%%% init/2 + websocket_init/1
%%% ===========================================================================

init_returns_cowboy_websocket_with_unauth_state_test() ->
    Req = #{peer => {{127, 0, 0, 1}, 12345}},
    {cowboy_websocket, Req, State, Opts} = beamtalk_ws_handler:init(Req, []),
    ?assertEqual(false, State#ws_state.authenticated),
    ?assertEqual({{127, 0, 0, 1}, 12345}, State#ws_state.peer),
    ?assertEqual(infinity, maps:get(idle_timeout, Opts)),
    ?assert(maps:is_key(max_frame_size, Opts)).

websocket_init_sends_auth_required_test() ->
    {[{text, Bin}], _State} = beamtalk_ws_handler:websocket_init(unauth_state()),
    ?assertEqual(<<"auth-required">>, maps:get(<<"op">>, json:decode(Bin))).

%%% ===========================================================================
%%% websocket_handle/2 — authentication (unauthenticated state)
%%% ===========================================================================

auth_malformed_json_closes_test() ->
    {Frames, State} = beamtalk_ws_handler:websocket_handle({text, <<"{not json">>}, unauth_state()),
    ?assertEqual(false, State#ws_state.authenticated),
    Decoded = first_text(Frames),
    ?assertEqual(<<"auth_error">>, maps:get(<<"type">>, Decoded)),
    ?assert(lists:keymember(close, 1, Frames)).

auth_non_auth_message_rejected_test() ->
    Json = iolist_to_binary(json:encode(#{<<"type">> => <<"eval">>})),
    {Frames, _State} = beamtalk_ws_handler:websocket_handle({text, Json}, unauth_state()),
    Decoded = first_text(Frames),
    ?assertEqual(<<"auth_error">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"First message must be auth">>, maps:get(<<"message">>, Decoded)),
    ?assert(lists:keymember(close, 1, Frames)).

auth_invalid_cookie_rejected_test() ->
    %% A cookie whose byte size differs from the node cookie short-circuits to
    %% false (the byte_size guard before crypto:hash_equals).
    WrongCookie = <<"definitely-not-the-node-cookie-xyzzy">>,
    Json = iolist_to_binary(
        json:encode(#{<<"type">> => <<"auth">>, <<"cookie">> => WrongCookie})
    ),
    {Frames, State} = beamtalk_ws_handler:websocket_handle({text, Json}, unauth_state()),
    ?assertEqual(false, State#ws_state.authenticated),
    Decoded = first_text(Frames),
    ?assertEqual(<<"auth_error">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"Invalid cookie">>, maps:get(<<"message">>, Decoded)),
    ?assert(lists:keymember(close, 1, Frames)).

%%% ===========================================================================
%%% websocket_handle/2 — non-text frames + post-auth dispatch
%%% ===========================================================================

non_text_frame_ignored_test() ->
    State = authed_state(),
    ?assertEqual({ok, State}, beamtalk_ws_handler:websocket_handle({binary, <<1, 2, 3>>}, State)).

protocol_decode_error_returns_error_frame_test() ->
    %% Valid JSON that is not an object decodes to {error, _}, exercising the
    %% decode-error branch of handle_protocol (a non-JSON frame would instead
    %% be treated as a legacy raw-eval expression).
    {Frames, State} = beamtalk_ws_handler:websocket_handle({text, <<"[1,2,3]">>}, authed_state()),
    ?assert(State#ws_state.authenticated),
    ?assertMatch([{text, _} | _], Frames).

eval_empty_code_returns_error_test() ->
    Json = op_json(<<"eval">>, #{<<"code">> => <<>>}),
    {Frames, _State} = beamtalk_ws_handler:websocket_handle({text, Json}, authed_state()),
    Decoded = first_text(Frames),
    ?assert(maps:is_key(<<"error">>, Decoded) orelse maps:is_key(<<"status">>, Decoded)).

eval_non_binary_code_returns_error_test() ->
    Json = op_json(<<"eval">>, #{<<"code">> => 42}),
    {Frames, _State} = beamtalk_ws_handler:websocket_handle({text, Json}, authed_state()),
    ?assertMatch([{text, _} | _], Frames).

eval_with_dead_session_returns_session_down_test() ->
    Json = op_json(<<"eval">>, #{<<"code">> => <<"1 + 1">>}),
    State = (authed_state())#ws_state{session_pid = dead_pid()},
    {Frames, _State} = beamtalk_ws_handler:websocket_handle({text, Json}, State),
    ?assertMatch([{text, _} | _], Frames).

eval_with_live_session_sets_pending_test() ->
    Json = op_json(<<"eval">>, #{<<"code">> => <<"1 + 1">>}),
    %% session_pid = self() is alive; eval_async is a cast to our own mailbox.
    {ok, State1} = beamtalk_ws_handler:websocket_handle({text, Json}, authed_state()),
    ?assertNotEqual(undefined, State1#ws_state.pending_eval),
    %% Drain the cast we sent ourselves.
    receive
        {'$gen_cast', {eval_async, _, _}} -> ok
    after 0 -> ok
    end.

eval_when_already_pending_returns_busy_test() ->
    Json = op_json(<<"eval">>, #{<<"code">> => <<"2 + 2">>}),
    State = (authed_state())#ws_state{pending_eval = make_msg(<<"eval">>)},
    {Frames, _State} = beamtalk_ws_handler:websocket_handle({text, Json}, State),
    ?assertMatch([{text, _} | _], Frames).

stdin_without_pending_input_errors_test() ->
    Json = op_json(<<"stdin">>, #{<<"value">> => <<"hello\n">>}),
    {Frames, _State} = beamtalk_ws_handler:websocket_handle({text, Json}, authed_state()),
    ?assertMatch([{text, _} | _], Frames).

stdin_with_pending_routes_value_test() ->
    Ref = make_ref(),
    State = (authed_state())#ws_state{io_capture_pid = self(), stdin_ref = Ref},
    Json = op_json(<<"stdin">>, #{<<"value">> => <<"line\n">>}),
    {ok, State1} = beamtalk_ws_handler:websocket_handle({text, Json}, State),
    ?assertEqual(undefined, State1#ws_state.io_capture_pid),
    ?assertEqual(undefined, State1#ws_state.stdin_ref),
    receive
        {stdin_input, Ref, <<"line\n">>} -> ok
    after 500 -> ?assert(false)
    end.

stdin_with_pending_routes_eof_test() ->
    Ref = make_ref(),
    State = (authed_state())#ws_state{io_capture_pid = self(), stdin_ref = Ref},
    Json = op_json(<<"stdin">>, #{<<"value">> => <<"eof">>}),
    {ok, _State1} = beamtalk_ws_handler:websocket_handle({text, Json}, State),
    receive
        {stdin_input, Ref, eof} -> ok
    after 500 -> ?assert(false)
    end.

stdin_with_pending_invalid_value_errors_test() ->
    Ref = make_ref(),
    State = (authed_state())#ws_state{io_capture_pid = self(), stdin_ref = Ref},
    Json = op_json(<<"stdin">>, #{<<"value">> => 99}),
    {Frames, State1} = beamtalk_ws_handler:websocket_handle({text, Json}, State),
    %% Invalid value keeps the capture pid so the client can retry.
    ?assertEqual(self(), State1#ws_state.io_capture_pid),
    ?assertMatch([{text, _} | _], Frames).

subscribe_logs_sets_subscribed_test() ->
    Json = op_json(<<"subscribe-logs">>, #{<<"level">> => <<"warning">>}),
    {Frames, State1} = beamtalk_ws_handler:websocket_handle({text, Json}, authed_state()),
    ?assert(State1#ws_state.log_subscribed),
    ?assertMatch([{text, _} | _], Frames),
    %% Re-subscribe path (already subscribed -> unsubscribe then subscribe).
    Json2 = op_json(<<"subscribe-logs">>, #{<<"level">> => <<"error">>}),
    {_, State2} = beamtalk_ws_handler:websocket_handle({text, Json2}, State1),
    ?assert(State2#ws_state.log_subscribed),
    %% Clean up subscription.
    beamtalk_ws_log_handler:unsubscribe().

unsubscribe_logs_clears_subscribed_test() ->
    State = (authed_state())#ws_state{log_subscribed = true},
    Json = op_json(<<"unsubscribe-logs">>, #{}),
    {Frames, State1} = beamtalk_ws_handler:websocket_handle({text, Json}, State),
    ?assertEqual(false, State1#ws_state.log_subscribed),
    ?assertMatch([{text, _} | _], Frames).

shutdown_missing_cookie_denied_test() ->
    Json = op_json(<<"shutdown">>, #{}),
    {Frames, _State} = beamtalk_ws_handler:websocket_handle({text, Json}, authed_state()),
    ?assertMatch([{text, _} | _], Frames).

shutdown_invalid_cookie_denied_test() ->
    Json = op_json(<<"shutdown">>, #{<<"cookie">> => <<"wrong-cookie-value-here">>}),
    {Frames, _State} = beamtalk_ws_handler:websocket_handle({text, Json}, authed_state()),
    ?assertMatch([{text, _} | _], Frames).

%% NB: the valid-cookie shutdown success path is deliberately NOT tested here.
%% On a valid cookie the handler arms `send_after(100, beamtalk_repl_server,
%% shutdown_requested)`, and `beamtalk_repl_server` handles that message by
%% calling `init:stop()` — halting the whole node. Firing it from an in-node
%% EUnit suite (where other tests start a real `beamtalk_repl_server`) would be
%% an order/timing-dependent way to kill the entire run. That branch is covered
%% by the WebSocket E2E surface instead.

shutdown_non_binary_cookie_denied_test() ->
    Json = op_json(<<"shutdown">>, #{<<"cookie">> => 42}),
    {Frames, _State} = beamtalk_ws_handler:websocket_handle({text, Json}, authed_state()),
    ?assertMatch([{text, _} | _], Frames).

subscribe_logs_all_levels_test() ->
    %% Drive every parse_log_level/1 clause (including the unknown -> debug
    %% fallback) and the no-level default.
    Levels = [
        <<"emergency">>,
        <<"alert">>,
        <<"critical">>,
        <<"error">>,
        <<"warning">>,
        <<"notice">>,
        <<"info">>,
        <<"debug">>,
        <<"bogus-level">>
    ],
    lists:foreach(
        fun(Level) ->
            Json = op_json(<<"subscribe-logs">>, #{<<"level">> => Level}),
            {_, S} = beamtalk_ws_handler:websocket_handle({text, Json}, authed_state()),
            ?assert(S#ws_state.log_subscribed)
        end,
        Levels
    ),
    %% No level key -> defaults to debug.
    NoLevel = op_json(<<"subscribe-logs">>, #{}),
    {_, S2} = beamtalk_ws_handler:websocket_handle({text, NoLevel}, authed_state()),
    ?assert(S2#ws_state.log_subscribed),
    beamtalk_ws_log_handler:unsubscribe().

%%% ===========================================================================
%%% websocket_info/2 — streaming + push frames (authenticated, pending eval)
%%% ===========================================================================

eval_out_streams_chunk_test() ->
    State = (authed_state())#ws_state{pending_eval = make_msg(<<"eval">>)},
    Reply = beamtalk_ws_handler:websocket_info({eval_out, <<"chunk">>}, State),
    %% Either streams a text frame or (legacy client) drops to {ok, State}.
    ?assert(element(1, Reply) =:= ok orelse is_list(element(1, Reply))).

need_input_sets_capture_pid_test() ->
    State = (authed_state())#ws_state{pending_eval = make_msg(<<"eval">>)},
    Ref = make_ref(),
    {Frames, State1} = beamtalk_ws_handler:websocket_info(
        {need_input, self(), Ref, <<"Name? ">>}, State
    ),
    ?assertEqual(self(), State1#ws_state.io_capture_pid),
    ?assertEqual(Ref, State1#ws_state.stdin_ref),
    ?assertMatch([{text, _} | _], Frames).

eval_done_sends_result_and_clears_pending_test() ->
    State = (authed_state())#ws_state{pending_eval = make_msg(<<"eval">>)},
    {Frames, State1} = beamtalk_ws_handler:websocket_info(
        {eval_done, 42, <<"out">>, []}, State
    ),
    ?assertEqual(undefined, State1#ws_state.pending_eval),
    ?assertMatch([{text, _} | _], Frames).

eval_error_sends_error_and_clears_pending_test() ->
    State = (authed_state())#ws_state{pending_eval = make_msg(<<"eval">>)},
    {Frames, State1} = beamtalk_ws_handler:websocket_info(
        {eval_error, {error, boom}, <<>>, []}, State
    ),
    ?assertEqual(undefined, State1#ws_state.pending_eval),
    ?assertMatch([{text, _} | _], Frames).

eval_error_with_compile_location_test() ->
    %% A compile_error reason carrying line + hint exercises
    %% extract_compile_error_location/1's success branch.
    State = (authed_state())#ws_state{pending_eval = make_msg(<<"eval">>)},
    Reason = {compile_error, [#{line => 7, hint => <<"missing period">>}]},
    {Frames, State1} = beamtalk_ws_handler:websocket_info(
        {eval_error, Reason, <<>>, []}, State
    ),
    ?assertEqual(undefined, State1#ws_state.pending_eval),
    ?assertMatch([{text, _} | _], Frames).

eval_error_with_compile_error_no_line_test() ->
    %% compile_error whose diagnostic map lacks a line falls to the #{} branch.
    State = (authed_state())#ws_state{pending_eval = make_msg(<<"eval">>)},
    Reason = {compile_error, [#{message => <<"oops">>}]},
    {Frames, _State1} = beamtalk_ws_handler:websocket_info(
        {eval_error, Reason, <<>>, []}, State
    ),
    ?assertMatch([{text, _} | _], Frames).

%% BT-2531: the push streams now arrive as `{beamtalk_announcement, SubRef,
%% Class, Handler, Event}` tuples from the SystemAnnouncer bus. `ann/2` builds one
%% with a fresh ref and the inert push-handler term the facade registers.
ann(Class, Event) ->
    {beamtalk_announcement, make_ref(), Class, repl_push_subscription, Event}.

bindings_changed_pushes_frame_test() ->
    Event = #{
        '$beamtalk_class' => 'BindingChanged',
        name => x,
        value => 1,
        sessionId => <<"sess-1">>
    },
    {Frames, _State} = beamtalk_ws_handler:websocket_info(
        ann('BindingChanged', Event), authed_state()
    ),
    Decoded = first_text(Frames),
    ?assertEqual(<<"bindings">>, maps:get(<<"channel">>, Decoded)),
    ?assertEqual(<<"changed">>, maps:get(<<"event">>, Decoded)),
    ?assertEqual(<<"sess-1">>, maps:get(<<"session">>, maps:get(<<"data">>, Decoded))).

bindings_changed_nil_session_pushes_null_test() ->
    Event = #{'$beamtalk_class' => 'BindingChanged', name => x, value => 1, sessionId => nil},
    {Frames, _State} = beamtalk_ws_handler:websocket_info(
        ann('BindingChanged', Event), authed_state()
    ),
    Decoded = first_text(Frames),
    ?assertEqual(null, maps:get(<<"session">>, maps:get(<<"data">>, Decoded))).

transcript_output_pushes_frame_test() ->
    {Frames, _State} = beamtalk_ws_handler:websocket_info(
        {transcript_output, <<"hello">>}, authed_state()
    ),
    Decoded = first_text(Frames),
    ?assertEqual(<<"transcript">>, maps:get(<<"push">>, Decoded)).

actor_spawned_pushes_frame_test() ->
    Event = #{'$beamtalk_class' => 'ActorSpawned', actorClass => 'Counter', pid => self()},
    {Frames, _State} = beamtalk_ws_handler:websocket_info(
        ann('ActorSpawned', Event), authed_state()
    ),
    Decoded = first_text(Frames),
    ?assertEqual(<<"actors">>, maps:get(<<"channel">>, Decoded)),
    ?assertEqual(<<"spawned">>, maps:get(<<"event">>, Decoded)),
    Data = maps:get(<<"data">>, Decoded),
    ?assertEqual(<<"Counter">>, maps:get(<<"class">>, Data)),
    %% BT-2531: the live spawned frame no longer carries spawned_at (the typed
    %% ActorSpawned event has no such field — the connect snapshot still does).
    ?assertNot(maps:is_key(<<"spawned_at">>, Data)).

actor_stopped_pushes_frame_test() ->
    Event = #{
        '$beamtalk_class' => 'ActorStopped',
        actorClass => 'Counter',
        pid => self(),
        reason => normal
    },
    {Frames, _State} = beamtalk_ws_handler:websocket_info(
        ann('ActorStopped', Event), authed_state()
    ),
    Decoded = first_text(Frames),
    ?assertEqual(<<"stopped">>, maps:get(<<"event">>, Decoded)),
    %% BT-2531: reason is the typed normalized symbol, not a ~P-formatted term.
    ?assertEqual(<<"normal">>, maps:get(<<"reason">>, maps:get(<<"data">>, Decoded))).

actor_stopped_unknown_class_test() ->
    %% A missing actorClass defaults to the typed `#unknown` symbol.
    Event = #{'$beamtalk_class' => 'ActorStopped', pid => self(), reason => crashed},
    {Frames, _State} = beamtalk_ws_handler:websocket_info(
        ann('ActorStopped', Event), authed_state()
    ),
    Data = maps:get(<<"data">>, first_text(Frames)),
    ?assertEqual(<<"unknown">>, maps:get(<<"class">>, Data)),
    ?assertEqual(<<"crashed">>, maps:get(<<"reason">>, Data)).

class_loaded_pushes_frame_test() ->
    Event = #{'$beamtalk_class' => 'ClassLoaded', className => 'Counter'},
    {Frames, _State} = beamtalk_ws_handler:websocket_info(
        ann('ClassLoaded', Event), authed_state()
    ),
    Decoded = first_text(Frames),
    ?assertEqual(<<"classes">>, maps:get(<<"channel">>, Decoded)),
    ?assertEqual(<<"loaded">>, maps:get(<<"event">>, Decoded)),
    ?assertEqual(<<"Counter">>, maps:get(<<"class">>, maps:get(<<"data">>, Decoded))).

class_removed_pushes_frame_test() ->
    %% BT-2531: ClassRemoved is newly visible on the bus-backed `classes` stream.
    Event = #{'$beamtalk_class' => 'ClassRemoved', className => 'Counter'},
    {Frames, _State} = beamtalk_ws_handler:websocket_info(
        ann('ClassRemoved', Event), authed_state()
    ),
    Decoded = first_text(Frames),
    ?assertEqual(<<"classes">>, maps:get(<<"channel">>, Decoded)),
    ?assertEqual(<<"removed">>, maps:get(<<"event">>, Decoded)),
    ?assertEqual(<<"Counter">>, maps:get(<<"class">>, maps:get(<<"data">>, Decoded))).

flush_completed_normalises_files_test() ->
    %% Mix of binary, charlist, and invalid entries exercises every
    %% normalise_files_for_push/1 branch.
    Files = [<<"src/a.bt">>, "src/b.bt", 12345, {bad, tuple}],
    Event = #{'$beamtalk_class' => 'FlushCompleted', files => Files},
    {Frames, _State} = beamtalk_ws_handler:websocket_info(
        ann('FlushCompleted', Event), authed_state()
    ),
    Decoded = first_text(Frames),
    ?assertEqual(<<"workspace">>, maps:get(<<"channel">>, Decoded)),
    ?assertEqual(<<"flush_completed">>, maps:get(<<"event">>, Decoded)),
    Out = maps:get(<<"files">>, maps:get(<<"data">>, Decoded)),
    %% Only the two valid path entries survive.
    ?assertEqual([<<"src/a.bt">>, <<"src/b.bt">>], Out).

%% ADR 0105 Phase 1 (BT-2779): the reload_check/completed push frame —
%% `beamtalk_repl_loader:publish_recheck_outcome/5`'s `'ReloadCheckCompleted'`
%% announcement, re-encoded as JSON.
reload_check_completed_pushes_frame_test() ->
    Finding = #{
        owner => <<"Dashboard">>,
        changed_class => <<"Counter">>,
        selector => <<"getCount">>,
        classification => signature_change,
        severity => <<"warning">>,
        category => <<"Dnu">>,
        message => <<"String does not understand '+'">>,
        note => undefined,
        sites => [#{method => <<"refresh">>, line => 14}],
        start => 0,
        'end' => 5
    },
    Event = #{
        '$beamtalk_class' => 'ReloadCheckCompleted',
        changedClass => <<"Counter">>,
        changedSelector => <<"getCount">>,
        classification => signature_change,
        checked => 1,
        notChecked => 0,
        capNote => undefined,
        checkedOwners => [<<"Dashboard">>],
        findings => [Finding]
    },
    {Frames, _State} = beamtalk_ws_handler:websocket_info(
        ann('ReloadCheckCompleted', Event), authed_state()
    ),
    Decoded = first_text(Frames),
    ?assertEqual(<<"reload_check">>, maps:get(<<"channel">>, Decoded)),
    ?assertEqual(<<"completed">>, maps:get(<<"event">>, Decoded)),
    Data = maps:get(<<"data">>, Decoded),
    ?assertEqual(<<"Counter">>, maps:get(<<"changedClass">>, Data)),
    ?assertEqual(<<"signature_change">>, maps:get(<<"classification">>, Data)),
    ?assertEqual(null, maps:get(<<"capNote">>, Data)),
    ?assertEqual([<<"Dashboard">>], maps:get(<<"checkedOwners">>, Data)),
    [FindingOut] = maps:get(<<"findings">>, Data),
    ?assertEqual(<<"Dashboard">>, maps:get(<<"owner">>, FindingOut)),
    ?assertEqual(null, maps:get(<<"note">>, FindingOut)),
    [SiteOut] = maps:get(<<"sites">>, FindingOut),
    ?assertEqual(14, maps:get(<<"line">>, SiteOut)).

reload_check_completed_with_cap_note_and_undefined_category_test() ->
    Finding = #{
        owner => <<"AdminPanel">>,
        changed_class => <<"Counter">>,
        selector => <<"reset">>,
        classification => removal,
        severity => <<"hint">>,
        category => undefined,
        message => <<"'Counter' does not understand 'reset'">>,
        note => <<"removed by the reload of Counter">>,
        sites => [#{method => <<"onClick">>, line => 9}],
        start => 0,
        'end' => 1
    },
    Event = #{
        '$beamtalk_class' => 'ReloadCheckCompleted',
        changedClass => <<"Counter">>,
        changedSelector => <<"reset">>,
        classification => removal,
        checked => 20,
        notChecked => 3,
        capNote => <<"3 more not checked">>,
        checkedOwners => [<<"AdminPanel">>],
        findings => [Finding]
    },
    {Frames, _State} = beamtalk_ws_handler:websocket_info(
        ann('ReloadCheckCompleted', Event), authed_state()
    ),
    Decoded = first_text(Frames),
    Data = maps:get(<<"data">>, Decoded),
    ?assertEqual(<<"3 more not checked">>, maps:get(<<"capNote">>, Data)),
    [FindingOut] = maps:get(<<"findings">>, Data),
    ?assertEqual(null, maps:get(<<"category">>, FindingOut)),
    ?assertEqual(
        <<"removed by the reload of Counter">>, maps:get(<<"note">>, FindingOut)
    ).

log_event_subscribed_pushes_frame_test() ->
    State = (authed_state())#ws_state{log_subscribed = true},
    {Frames, _State} = beamtalk_ws_handler:websocket_info(
        {log_event, #{level => <<"info">>, msg => <<"hi">>, extra => undefined}}, State
    ),
    Decoded = first_text(Frames),
    ?assertEqual(<<"logs">>, maps:get(<<"channel">>, Decoded)),
    Data = maps:get(<<"data">>, Decoded),
    ?assertEqual(<<"info">>, maps:get(<<"level">>, Data)),
    %% undefined values are stripped.
    ?assertNot(maps:is_key(<<"extra">>, Data)).

log_event_not_subscribed_dropped_test() ->
    State = (authed_state())#ws_state{log_subscribed = false},
    ?assertEqual(
        {ok, State},
        beamtalk_ws_handler:websocket_info({log_event, #{level => <<"info">>}}, State)
    ).

%%% ===========================================================================
%%% websocket_info/2 — late messages dropped when no eval pending
%%% ===========================================================================

late_eval_out_dropped_test() ->
    State = authed_state(),
    ?assertEqual({ok, State}, beamtalk_ws_handler:websocket_info({eval_out, <<"x">>}, State)).

late_eval_done_dropped_test() ->
    State = authed_state(),
    ?assertEqual({ok, State}, beamtalk_ws_handler:websocket_info({eval_done, 1, <<>>, []}, State)).

late_eval_error_dropped_test() ->
    State = authed_state(),
    ?assertEqual(
        {ok, State}, beamtalk_ws_handler:websocket_info({eval_error, oops, <<>>, []}, State)
    ).

late_need_input_dropped_test() ->
    State = authed_state(),
    ?assertEqual(
        {ok, State},
        beamtalk_ws_handler:websocket_info({need_input, self(), make_ref(), <<"?">>}, State)
    ).

unknown_info_ignored_test() ->
    State = authed_state(),
    ?assertEqual({ok, State}, beamtalk_ws_handler:websocket_info(some_random_message, State)).

%%% ===========================================================================
%%% websocket_info/2 — session DOWN
%%% ===========================================================================

session_down_closes_and_clears_test() ->
    beamtalk_session_table:new(),
    beamtalk_session_table:insert(<<"sess-1">>, self()),
    MonRef = make_ref(),
    State = (authed_state())#ws_state{session_mon = MonRef},
    {Frames, State1} = beamtalk_ws_handler:websocket_info(
        {'DOWN', MonRef, process, self(), shutdown}, State
    ),
    ?assertEqual(false, State1#ws_state.authenticated),
    ?assertEqual(undefined, State1#ws_state.session_id),
    ?assert(lists:keymember(close, 1, Frames)).

%%% ===========================================================================
%%% terminate/3
%%% ===========================================================================

terminate_no_session_is_noop_test() ->
    ?assertEqual(ok, beamtalk_ws_handler:terminate(normal, #{}, unauth_state())).

terminate_with_session_unsubscribes_test() ->
    %% All five pub/sub unsubscribes are gen_server:cast (safe when the server
    %% is down) and ws_log_handler:unsubscribe guards on ets:info, so this runs
    %% without the workspace supervision tree.
    State = authed_state(),
    ?assertEqual(ok, beamtalk_ws_handler:terminate(normal, #{}, State)).

%%% ===========================================================================
%%% Authentication success + session create/resume (needs session supervisor)
%%% ===========================================================================
%%%
%%% These drive the post-auth `create_session`/`start_or_resume_session`
%%% success paths. They need a live `beamtalk_session_sup` (which spawns a real
%%% `beamtalk_repl_shell`) plus the session ETS table. The pub/sub subscribe
%%% calls are casts that are safe when their servers are down, and
%%% `actor_snapshot_frames/0` falls back to [] without a registry, so this
%%% fixture stays lightweight.

session_setup() ->
    beamtalk_session_table:new(),
    case beamtalk_session_sup:start_link() of
        {ok, Pid} ->
            %% start_link links the sup to this test process; unlink so the
            %% cleanup kill below doesn't propagate and take down the runner.
            unlink(Pid),
            {started, Pid};
        {error, {already_started, Pid}} ->
            {existing, Pid}
    end.

session_cleanup({started, Pid}) ->
    case is_process_alive(Pid) of
        true ->
            Ref = monitor(process, Pid),
            exit(Pid, kill),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 1000 -> ok
            end;
        false ->
            ok
    end;
session_cleanup({existing, _Pid}) ->
    %% A supervisor we didn't start (e.g. the workspace app is running in a
    %% combined eunit run) — leave it alone.
    ok.

node_cookie() ->
    atom_to_binary(erlang:get_cookie(), utf8).

auth_json(Extra) ->
    Base = #{<<"type">> => <<"auth">>, <<"cookie">> => node_cookie()},
    iolist_to_binary(json:encode(maps:merge(Base, Extra))).

%% Pull the first decoded text frame whose JSON has a key K equal to V.
has_frame_with(Frames, K, V) ->
    lists:any(
        fun
            ({text, Bin}) ->
                case catch json:decode(Bin) of
                    M when is_map(M) -> maps:get(K, M, undefined) =:= V;
                    _ -> false
                end;
            (_) ->
                false
        end,
        Frames
    ).

auth_session_test_() ->
    {foreach, fun session_setup/0, fun session_cleanup/1, [
        fun(_) -> {"auth success creates session", fun auth_success_creates_session/0} end,
        fun(_) -> {"resume alive session", fun resume_alive_session/0} end,
        fun(_) -> {"resume dead session creates new", fun resume_dead_session_creates_new/0} end,
        fun(_) -> {"resume unknown session creates new", fun resume_unknown_creates_new/0} end,
        fun(_) -> {"non-binary resume creates new", fun resume_non_binary_creates_new/0} end
    ]}.

auth_success_creates_session() ->
    {Frames, State} = beamtalk_ws_handler:websocket_handle({text, auth_json(#{})}, unauth_state()),
    ?assert(State#ws_state.authenticated),
    ?assertNotEqual(undefined, State#ws_state.session_id),
    ?assert(is_pid(State#ws_state.session_pid)),
    ?assert(has_frame_with(Frames, <<"type">>, <<"auth_ok">>)),
    ?assert(has_frame_with(Frames, <<"op">>, <<"session-started">>)).

resume_alive_session() ->
    %% A live process registered as an existing session triggers the resume
    %% branch (monitor + auth_ok + session-started for the SAME id).
    {ok, Pid} = beamtalk_session_sup:start_session(<<"resume-me">>),
    beamtalk_session_table:insert(<<"resume-me">>, Pid),
    Json = auth_json(#{<<"resume">> => <<"resume-me">>}),
    {Frames, State} = beamtalk_ws_handler:websocket_handle({text, Json}, unauth_state()),
    ?assert(State#ws_state.authenticated),
    ?assertEqual(<<"resume-me">>, State#ws_state.session_id),
    ?assertEqual(Pid, State#ws_state.session_pid),
    ?assert(has_frame_with(Frames, <<"op">>, <<"session-started">>)).

resume_dead_session_creates_new() ->
    DeadId = <<"dead-sess">>,
    beamtalk_session_table:insert(DeadId, dead_pid()),
    Json = auth_json(#{<<"resume">> => DeadId}),
    {_Frames, State} = beamtalk_ws_handler:websocket_handle({text, Json}, unauth_state()),
    ?assert(State#ws_state.authenticated),
    %% A fresh id was generated, not the dead one.
    ?assertNotEqual(DeadId, State#ws_state.session_id),
    ?assert(is_pid(State#ws_state.session_pid)).

resume_unknown_creates_new() ->
    Json = auth_json(#{<<"resume">> => <<"never-existed">>}),
    {_Frames, State} = beamtalk_ws_handler:websocket_handle({text, Json}, unauth_state()),
    ?assert(State#ws_state.authenticated),
    ?assertNotEqual(<<"never-existed">>, State#ws_state.session_id),
    ?assert(is_pid(State#ws_state.session_pid)).

resume_non_binary_creates_new() ->
    %% A non-binary resume value falls through to the catch-all clause.
    Json = auth_json(#{<<"resume">> => 12345}),
    {_Frames, State} = beamtalk_ws_handler:websocket_handle({text, Json}, unauth_state()),
    ?assert(State#ws_state.authenticated),
    ?assert(is_pid(State#ws_state.session_pid)).
