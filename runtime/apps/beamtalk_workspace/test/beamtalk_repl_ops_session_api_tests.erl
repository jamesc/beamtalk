%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_session_api_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
The shared session API on the operation layer (BT-5202).

`beamtalk_repl_ops:start_session/2`, `attach_session/2` and `session_bindings/1`,
plus the `interrupt`, `sessions` and `close` ops, are the one implementation the
WebSocket handler and LiveView both call. These tests drive the whole
start → eval → interrupt → bindings → close path through that layer, the way
LiveView does (`request/3` with the session pid), and check the busy and
timeout semantics both transports share.
""".
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

session_api_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun start_normalises_kind/1,
        fun two_sessions_are_independent_shells/1,
        fun lifecycle_start_eval_interrupt_bindings_close/1,
        fun attach_reports_ok_gone_and_timeout/1,
        fun interrupt_frees_a_busy_session/1,
        fun interrupt_of_a_dead_session_is_ok/1,
        fun bindings_of_a_dead_session_is_a_structured_error/1,
        fun alive_and_count_do_not_depend_on_other_sessions/1,
        fun interrupt_of_a_wedged_session_is_a_structured_error/1,
        fun liveview_sessions_are_not_registered_by_id/1
    ]}.

setup() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    ok = beamtalk_session_table:new(),
    {ok, SupPid} = beamtalk_session_sup:start_link(),
    unlink(SupPid),
    SupPid.

cleanup(SupPid) ->
    exit(SupPid, shutdown),
    beamtalk_test_wait:dead(SupPid),
    ets:delete_all_objects(beamtalk_sessions),
    ok.

%% A stand-in for the owner process (the LiveView, or the WebSocket handler).
new_owner() ->
    spawn(fun() ->
        receive
            stop -> ok
        end
    end).

%% Park a fake eval worker in the shell: it is busy until interrupted.
park_busy(Session) ->
    Worker = spawn(fun() ->
        receive
            release -> ok
        end
    end),
    sys:replace_state(Session, fun({SId, State, undefined}) ->
        {SId, State, {Worker, make_ref(), {async, self()}}}
    end),
    Worker.

start_normalises_kind(_) ->
    {"start_session starts a supervised shell and normalises `kind`", fun() ->
        {ok, Session} = beamtalk_repl_ops:start_session(<<"api-start">>, #{kind => <<"LiveView">>}),
        %% Not registered by id: other clients cannot resume or close it by guessing.
        ?assertEqual(error, beamtalk_session_table:lookup(<<"api-start">>)),
        ?assertMatch(
            {ok, <<"api-start">>, #{kind := <<"liveview">>}},
            beamtalk_repl_shell:get_session_meta(Session)
        )
    end}.

two_sessions_are_independent_shells(_) ->
    {"two sessions can be started; each is its own shell", fun() ->
        {ok, A} = beamtalk_repl_ops:start_session(<<"api-a">>, #{}),
        {ok, B} = beamtalk_repl_ops:start_session(<<"api-b">>, #{}),
        ?assertNotEqual(A, B)
    end}.

lifecycle_start_eval_interrupt_bindings_close(_) ->
    {"start -> eval -> interrupt -> bindings -> close, all through the operation layer",
        {timeout, 60, fun() ->
            ok = beamtalk_test_boot:compiler(),
            Owner = new_owner(),
            {ok, Session} = beamtalk_repl_ops:start_session(<<"api-life">>, #{
                kind => <<"liveview">>
            }),
            ok = beamtalk_repl_ops:attach_session(Session, Owner),
            %% eval: a binding is created in the session
            ?assertMatch(
                {ok, 42, _, _},
                beamtalk_repl_ops:request(<<"eval">>, #{<<"code">> => <<"x := 42">>}, Session)
            ),
            %% interrupt with nothing running is idempotent
            ?assertEqual(
                {status, ok}, beamtalk_repl_ops:request(<<"interrupt">>, #{}, Session)
            ),
            %% bindings: the live value, not a printed string
            ?assertMatch({ok, #{x := 42}}, beamtalk_repl_ops:session_bindings(Session)),
            %% the session probes see it
            ?assert(beamtalk_repl_ops:session_alive(Session)),
            ?assertEqual(1, beamtalk_repl_ops:session_count()),
            %% close stops it and removes the row
            ?assertEqual(
                {status, ok}, beamtalk_repl_ops:request(<<"close">>, #{}, Session)
            ),
            beamtalk_test_wait:dead(Session),
            ?assertNot(beamtalk_repl_ops:session_alive(Session)),
            ?assertEqual(0, beamtalk_repl_ops:session_count()),
            Owner ! stop
        end}}.

attach_reports_ok_gone_and_timeout(_) ->
    {"attach answers ok, gone (stopped shell) and timeout (busy shell), as on the WebSocket",
        {timeout, 30, fun() ->
            Owner = new_owner(),
            {ok, Live} = beamtalk_repl_ops:start_session(<<"api-attach-live">>, #{}),
            ?assertEqual(ok, beamtalk_repl_ops:attach_session(Live, Owner)),
            {ok, Gone} = beamtalk_repl_ops:start_session(<<"api-attach-gone">>, #{}),
            ok = beamtalk_repl_shell:stop(Gone),
            ?assertEqual(gone, beamtalk_repl_ops:attach_session(Gone, Owner)),
            {ok, Busy} = beamtalk_repl_ops:start_session(<<"api-attach-busy">>, #{}),
            ok = sys:suspend(Busy),
            ?assertEqual(timeout, beamtalk_repl_ops:attach_session(Busy, Owner)),
            ok = sys:resume(Busy),
            Owner ! stop
        end}}.

interrupt_frees_a_busy_session(_) ->
    {"interrupt through the op layer stops the running eval and frees the shell", fun() ->
        {ok, Session} = beamtalk_repl_ops:start_session(<<"api-busy">>, #{}),
        Worker = park_busy(Session),
        ?assertEqual({status, ok}, beamtalk_repl_ops:request(<<"interrupt">>, #{}, Session)),
        beamtalk_test_wait:dead(Worker),
        ?assertMatch({_, _, undefined}, sys:get_state(Session))
    end}.

interrupt_of_a_dead_session_is_ok(_) ->
    {"interrupt of a session that has ended is idempotent", fun() ->
        {ok, Session} = beamtalk_repl_ops:start_session(<<"api-dead-int">>, #{}),
        ok = beamtalk_repl_shell:stop(Session),
        beamtalk_test_wait:dead(Session),
        ?assertEqual({status, ok}, beamtalk_repl_ops:request(<<"interrupt">>, #{}, Session))
    end}.

bindings_of_a_dead_session_is_a_structured_error(_) ->
    {"session_bindings of an ended session is a structured session_not_found", fun() ->
        {ok, Session} = beamtalk_repl_ops:start_session(<<"api-dead-b">>, #{}),
        ok = beamtalk_repl_shell:stop(Session),
        beamtalk_test_wait:dead(Session),
        ?assertMatch(
            {error, #beamtalk_error{kind = session_not_found}},
            beamtalk_repl_ops:session_bindings(Session)
        )
    end}.

alive_and_count_do_not_depend_on_other_sessions(_) ->
    {"session_alive/session_count stay exact while another shell is wedged",
        {timeout, 30, fun() ->
            {ok, Live} = beamtalk_repl_ops:start_session(<<"api-alive-1">>, #{}),
            {ok, Wedged} = beamtalk_repl_ops:start_session(<<"api-alive-2">>, #{}),
            ok = sys:suspend(Wedged),
            %% A pid that is not a session of this workspace is not alive as one.
            Stranger = new_owner(),
            ?assert(beamtalk_repl_ops:session_alive(Live)),
            ?assert(beamtalk_repl_ops:session_alive(Wedged)),
            ?assertNot(beamtalk_repl_ops:session_alive(Stranger)),
            ?assertEqual(2, beamtalk_repl_ops:session_count()),
            ok = sys:resume(Wedged),
            Stranger ! stop
        end}}.

interrupt_of_a_wedged_session_is_a_structured_error(_) ->
    {"interrupt of a shell that does not answer is session_busy, not a crash",
        {timeout, 30, fun() ->
            {ok, Session} = beamtalk_repl_ops:start_session(<<"api-wedged">>, #{}),
            ok = sys:suspend(Session),
            ?assertMatch(
                {error, #beamtalk_error{kind = session_busy}},
                beamtalk_repl_ops:request(<<"interrupt">>, #{}, Session)
            ),
            ok = sys:resume(Session)
        end}}.

liveview_sessions_are_not_registered_by_id(_) ->
    {"a session started for LiveView cannot be closed by id from another client", fun() ->
        {ok, Session} = beamtalk_repl_ops:start_session(<<"phoenix-1">>, #{kind => <<"liveview">>}),
        Msg = beamtalk_test_repl:make_msg(<<"close">>, <<"c-1">>, <<"phoenix-1">>),
        ?assertEqual(
            {status, ok}, beamtalk_repl_ops:dispatch(<<"close">>, #{}, Msg, self())
        ),
        ?assert(is_process_alive(Session))
    end}.
