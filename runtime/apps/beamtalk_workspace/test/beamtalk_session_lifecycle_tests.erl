%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_session_lifecycle_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Session lifecycle tests (BT-5012): a session whose client disconnected stays
resumable for its idle period and is then stopped (process and table row gone);
`close` stops a session; `sessions` lists resumable ids; the idle monitor
treats detached sessions as idle but never an attached, busy or unflushed one.

No fixed waits: the idle period is set to 0 where a stop is expected, and the
tests wait on process death / state, never a sleep.
""".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Fixture
%%====================================================================

lifecycle_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun detached_session_is_stopped_and_row_removed/1,
        fun detached_session_is_resumable_within_idle_period/1,
        fun reattach_cancels_idle_countdown/1,
        fun attached_session_never_idles_out/1,
        fun busy_detached_session_is_not_stopped/1,
        fun never_attached_session_idles_out/1,
        fun never_attached_session_within_period_stays/1,
        fun close_aborts_running_eval/1,
        fun attach_to_stopped_session_answers_gone/1,
        fun attach_after_queued_idle_timeout_answers_gone/1,
        fun attach_to_busy_shell_answers_timeout_not_gone/1,
        fun attach_twice_keeps_one_monitor/1,
        fun idle_period_is_settable_from_environment/1,
        fun close_stops_session_and_removes_row/1,
        fun close_with_session_stops_that_session/1,
        fun close_with_unknown_session_stops_nothing/1,
        fun sessions_op_lists_ids_that_resume_accepts/1,
        fun idle_monitor_sees_detached_only_workspace_as_idle/1,
        fun idle_monitor_keeps_attached_session_alive/1,
        fun idle_monitor_keeps_unattached_and_busy_sessions_alive/1,
        fun idle_monitor_waits_for_unflushed_edits/1
    ]}.

setup() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    ok = beamtalk_session_table:new(),
    {ok, SupPid} = beamtalk_session_sup:start_link(),
    unlink(SupPid),
    Prev = application:get_env(beamtalk_workspace, session_idle_seconds),
    {SupPid, Prev}.

cleanup({SupPid, Prev}) ->
    case Prev of
        undefined -> application:unset_env(beamtalk_workspace, session_idle_seconds);
        {ok, V} -> application:set_env(beamtalk_workspace, session_idle_seconds, V)
    end,
    exit(SupPid, shutdown),
    beamtalk_test_wait:dead(SupPid),
    ets:delete_all_objects(beamtalk_sessions),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

new_session(Id) ->
    {ok, Pid} = beamtalk_session_sup:start_session(Id, #{kind => <<"repl">>}),
    true = beamtalk_session_table:insert(Id, Pid),
    Pid.

%% A stand-in for the WebSocket handler process.
new_client() ->
    spawn(fun() ->
        receive
            stop -> ok
        end
    end).

disconnect(Client) ->
    Client ! stop,
    beamtalk_test_wait:dead(Client).

attach(Session, Client) ->
    ok = beamtalk_repl_shell:attach(Session, Client),
    attached = beamtalk_repl_shell:lifecycle_state(Session),
    ok.

await_state(Session, Expected) ->
    beamtalk_test_wait:until(fun() ->
        beamtalk_repl_shell:lifecycle_state(Session) =:= Expected
    end).

set_idle_seconds(N) ->
    application:set_env(beamtalk_workspace, session_idle_seconds, N).

%% Put a fake running eval worker in the shell, as the shell tests do.
fake_worker(Session) ->
    Worker = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    sys:replace_state(Session, fun({SId, State, _}) ->
        {SId, State, {Worker, make_ref(), {async, self()}}}
    end),
    Worker.

%%====================================================================
%% Shell lifecycle
%%====================================================================

detached_session_is_stopped_and_row_removed(_) ->
    {"connect, disconnect without resuming: process and row are gone", fun() ->
        %% The period is read at detach time; setting it after attach keeps the
        %% never-attached bound (BT-5084) from racing the attach.
        Session = new_session(<<"lc-1">>),
        Mon = erlang:monitor(process, Session),
        Client = new_client(),
        attach(Session, Client),
        set_idle_seconds(0),
        disconnect(Client),
        receive
            {'DOWN', Mon, process, Session, normal} -> ok
        after 5000 -> ?assert(false)
        end,
        ?assertEqual(error, beamtalk_session_table:lookup(<<"lc-1">>))
    end}.

detached_session_is_resumable_within_idle_period(_) ->
    {"a detached session stays alive and registered until its period ends", fun() ->
        Session = new_session(<<"lc-2">>),
        Client = new_client(),
        attach(Session, Client),
        disconnect(Client),
        ok = await_state(Session, detached),
        %% An early timeout message (period not elapsed) is ignored.
        Session ! session_idle_timeout,
        ok = beamtalk_test_wait:sync(Session),
        ?assert(is_process_alive(Session)),
        ?assertEqual({ok, Session}, beamtalk_session_table:lookup_alive(<<"lc-2">>))
    end}.

reattach_cancels_idle_countdown(_) ->
    {"resume within the period cancels the countdown", fun() ->
        Session = new_session(<<"lc-3">>),
        C1 = new_client(),
        attach(Session, C1),
        disconnect(C1),
        ok = await_state(Session, detached),
        C2 = new_client(),
        attach(Session, C2),
        %% Even if the period were over, a late timeout finds a client attached.
        set_idle_seconds(0),
        Session ! session_idle_timeout,
        ok = beamtalk_test_wait:sync(Session),
        ?assert(is_process_alive(Session)),
        ?assertEqual(attached, beamtalk_repl_shell:lifecycle_state(Session)),
        disconnect(C2)
    end}.

attached_session_never_idles_out(_) ->
    {"a session with a client is not stopped by a timeout", fun() ->
        Session = new_session(<<"lc-4">>),
        Client = new_client(),
        attach(Session, Client),
        set_idle_seconds(0),
        Session ! session_idle_timeout,
        ok = beamtalk_test_wait:sync(Session),
        ?assert(is_process_alive(Session)),
        disconnect(Client)
    end}.

busy_detached_session_is_not_stopped(_) ->
    {"a detached session with a running eval is not stopped", fun() ->
        Session = new_session(<<"lc-5">>),
        Client = new_client(),
        attach(Session, Client),
        set_idle_seconds(0),
        Worker = fake_worker(Session),
        disconnect(Client),
        Session ! session_idle_timeout,
        ok = beamtalk_test_wait:sync(Session),
        ?assert(is_process_alive(Session)),
        ?assertEqual(busy, beamtalk_repl_shell:lifecycle_state(Session)),
        Worker ! stop
    end}.

never_attached_session_idles_out(_) ->
    {"a session no client ever attached to idles out (BT-5084)", fun() ->
        set_idle_seconds(0),
        Session = new_session(<<"lc-6">>),
        beamtalk_test_wait:dead(Session),
        ?assertEqual(error, beamtalk_session_table:lookup(<<"lc-6">>))
    end}.

never_attached_session_within_period_stays(_) ->
    {"a never-attached session inside its idle period is kept", fun() ->
        set_idle_seconds(3600),
        Session = new_session(<<"lc-6b">>),
        Session ! session_idle_timeout,
        ok = beamtalk_test_wait:sync(Session),
        ?assert(is_process_alive(Session)),
        ?assertEqual(unattached, beamtalk_repl_shell:lifecycle_state(Session))
    end}.

%%====================================================================
%% close / sessions ops
%%====================================================================

close_stops_session_and_removes_row(_) ->
    {"close stops the session and removes its table row", fun() ->
        Session = new_session(<<"lc-7">>),
        Msg = beamtalk_test_repl:make_msg(<<"close">>, <<"c-1">>, undefined),
        ?assertEqual({status, ok}, beamtalk_repl_ops:dispatch(<<"close">>, #{}, Msg, Session)),
        beamtalk_test_wait:dead(Session),
        ?assertEqual(error, beamtalk_session_table:lookup(<<"lc-7">>))
    end}.

close_with_session_stops_that_session(_) ->
    {"close naming another session stops that one, not the caller (BT-5199)", fun() ->
        Caller = new_session(<<"lc-7-caller">>),
        Target = new_session(<<"lc-7-target">>),
        Msg = beamtalk_test_repl:make_msg(<<"close">>, <<"c-2">>, <<"lc-7-target">>),
        ?assertEqual({status, ok}, beamtalk_repl_ops:dispatch(<<"close">>, #{}, Msg, Caller)),
        beamtalk_test_wait:dead(Target),
        ?assertEqual(error, beamtalk_session_table:lookup(<<"lc-7-target">>)),
        ?assert(is_process_alive(Caller))
    end}.

close_with_unknown_session_stops_nothing(_) ->
    {"close naming an unknown session never falls back to the caller (BT-5199)", fun() ->
        Caller = new_session(<<"lc-7-alone">>),
        Msg = beamtalk_test_repl:make_msg(<<"close">>, <<"c-3">>, <<"lc-no-such">>),
        ?assertEqual({status, ok}, beamtalk_repl_ops:dispatch(<<"close">>, #{}, Msg, Caller)),
        ok = beamtalk_test_wait:sync(Caller),
        ?assert(is_process_alive(Caller))
    end}.

close_aborts_running_eval(_) ->
    {"close mid-eval kills the eval worker instead of orphaning it (BT-5084)", fun() ->
        Session = new_session(<<"lc-close-eval">>),
        Worker = fake_worker(Session),
        ?assert(is_process_alive(Worker)),
        ?assertEqual(ok, beamtalk_session_sup:stop_session(Session)),
        beamtalk_test_wait:dead(Session),
        beamtalk_test_wait:dead(Worker)
    end}.

attach_to_stopped_session_answers_gone(_) ->
    {"attach to a session that is gone answers gone, never ok", fun() ->
        Session = new_session(<<"lc-gone">>),
        ok = beamtalk_repl_shell:stop(Session),
        Client = new_client(),
        ?assertEqual(gone, beamtalk_repl_shell:attach(Session, Client)),
        disconnect(Client)
    end}.

attach_to_busy_shell_answers_timeout_not_gone(_) ->
    {"BT-5129: a live shell that does not answer in time is `timeout`, not `gone`",
        {timeout, 30, fun() ->
            Session = new_session(<<"lc-busy">>),
            Client = new_client(),
            %% Suspended: alive but unable to answer the attach call.
            ok = sys:suspend(Session),
            ?assertEqual(timeout, beamtalk_repl_shell:attach(Session, Client)),
            ok = sys:resume(Session),
            disconnect(Client)
        end}}.

attach_twice_keeps_one_monitor(_) ->
    {"BT-5129: attaching the same client twice monitors it once", fun() ->
        Session = new_session(<<"lc-twice">>),
        Client = new_client(),
        ok = beamtalk_repl_shell:attach(Session, Client),
        ok = beamtalk_repl_shell:attach(Session, Client),
        {monitored_by, Monitors} = process_info(Client, monitored_by),
        ?assertEqual(1, length([M || M <- Monitors, M =:= Session])),
        disconnect(Client)
    end}.

attach_after_queued_idle_timeout_answers_gone(_) ->
    {"resume race: an idle timeout queued ahead of the attach wins; the caller is told",
        {timeout, 30, fun() ->
            %% A detached session, so the order of messages is under the test's control.
            set_idle_seconds(3600),
            Session2 = new_session(<<"lc-race2">>),
            C1 = new_client(),
            attach(Session2, C1),
            ok = sys:suspend(Session2),
            set_idle_seconds(0),
            C1 ! stop,
            beamtalk_test_wait:dead(C1),
            beamtalk_test_wait:until(fun() ->
                {message_queue_len, N} = erlang:process_info(Session2, message_queue_len),
                N >= 1
            end),
            Session2 ! session_idle_timeout,
            Self = self(),
            C2 = new_client(),
            spawn(fun() -> Self ! {attached, beamtalk_repl_shell:attach(Session2, C2)} end),
            beamtalk_test_wait:until(fun() ->
                {message_queue_len, N} = erlang:process_info(Session2, message_queue_len),
                N >= 3
            end),
            ok = sys:resume(Session2),
            receive
                {attached, Result} -> ?assertEqual(gone, Result)
            after 5000 -> ?assert(false)
            end,
            beamtalk_test_wait:dead(Session2),
            disconnect(C2)
        end}}.

idle_period_is_settable_from_environment(_) ->
    {"BEAMTALK_SESSION_IDLE_SECONDS sets the idle period; app env wins", fun() ->
        application:unset_env(beamtalk_workspace, session_idle_seconds),
        os:putenv("BEAMTALK_SESSION_IDLE_SECONDS", "7"),
        try
            ?assertEqual(7, beamtalk_repl_shell:session_idle_seconds()),
            set_idle_seconds(9),
            ?assertEqual(9, beamtalk_repl_shell:session_idle_seconds()),
            application:unset_env(beamtalk_workspace, session_idle_seconds),
            os:putenv("BEAMTALK_SESSION_IDLE_SECONDS", "nope"),
            ?assertEqual(14400, beamtalk_repl_shell:session_idle_seconds())
        after
            os:unsetenv("BEAMTALK_SESSION_IDLE_SECONDS")
        end
    end}.

sessions_op_lists_ids_that_resume_accepts(_) ->
    {"sessions returns session ids, not pids", fun() ->
        Session = new_session(<<"lc-8">>),
        Msg = beamtalk_test_repl:make_msg(<<"sessions">>, <<"s-1">>, undefined),
        {sessions, List} = beamtalk_repl_ops:dispatch(<<"sessions">>, #{}, Msg, self()),
        ?assertEqual([#{id => <<"lc-8">>}], List),
        %% The listed id resolves through the same table `resume` uses.
        ?assertEqual({ok, Session}, beamtalk_session_table:lookup_alive(<<"lc-8">>))
    end}.

%%====================================================================
%% Idle monitor
%%====================================================================

with_meta(Fun) ->
    {ok, Meta} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"lc-idle">>,
        project_path => iolist_to_binary([element(2, beamtalk_file:'tempDirectory'()), "/bt-lc"]),
        created_at => erlang:system_time(second)
    }),
    try
        backdate_activity(Meta, 100000),
        Fun()
    after
        gen_server:stop(Meta)
    end.

%% Move workspace_meta's last-activity stamp into the past (test-only state edit,
%% as in beamtalk_activity_tracking_tests). #state.last_activity is element 6.
backdate_activity(MetaPid, Seconds) ->
    {ok, Now} = beamtalk_workspace_meta:get_last_activity(),
    _ = sys:replace_state(MetaPid, fun(State) ->
        Now = element(6, State),
        setelement(6, State, Now - Seconds)
    end),
    ok.

idle_monitor_sees_detached_only_workspace_as_idle(_) ->
    {"only detached sessions: workspace is idle past the limit", fun() ->
        with_meta(fun() ->
            Session = new_session(<<"lc-9">>),
            Client = new_client(),
            attach(Session, Client),
            disconnect(Client),
            ok = await_state(Session, detached),
            ?assertNot(beamtalk_idle_monitor:has_active_sessions()),
            ?assert(beamtalk_idle_monitor:should_terminate(3600))
        end)
    end}.

idle_monitor_keeps_attached_session_alive(_) ->
    {"an attached client blocks auto-shutdown", fun() ->
        with_meta(fun() ->
            Session = new_session(<<"lc-10">>),
            Client = new_client(),
            attach(Session, Client),
            ?assert(beamtalk_idle_monitor:has_active_sessions()),
            ?assertNot(beamtalk_idle_monitor:should_terminate(3600)),
            disconnect(Client)
        end)
    end}.

idle_monitor_keeps_unattached_and_busy_sessions_alive(_) ->
    {"never-attached and mid-eval sessions block auto-shutdown", fun() ->
        with_meta(fun() ->
            Session = new_session(<<"lc-11">>),
            ?assertNot(beamtalk_idle_monitor:should_terminate(3600)),
            Client = new_client(),
            attach(Session, Client),
            Worker = fake_worker(Session),
            disconnect(Client),
            ok = await_state(Session, busy),
            ?assertNot(beamtalk_idle_monitor:should_terminate(3600)),
            Worker ! stop
        end)
    end}.

idle_monitor_waits_for_unflushed_edits(_) ->
    {"flushable edits pending in the ChangeLog block auto-shutdown", fun() ->
        with_meta(fun() ->
            ?assert(beamtalk_idle_monitor:should_terminate(3600)),
            {WorkspaceId, TmpHome, OldHome} = beamtalk_test_fs:changelog_workspace(),
            {ok, Clog} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
            try
                {ok, _} = beamtalk_workspace_changelog:append(#{
                    class => <<"Counter">>,
                    selector => <<"inc">>,
                    kind => instance,
                    source => <<"^ self value + 1">>,
                    prev_source => <<"^ self value">>,
                    intent => durable,
                    flushable => true,
                    author => <<"sess-1">>,
                    author_kind => human,
                    source_file => <<"/proj/src/counter.bt">>,
                    span => #{start => 0, 'end' => 10}
                }),
                ?assertNot(beamtalk_idle_monitor:should_terminate(3600))
            after
                beamtalk_test_proc:shutdown(Clog),
                beamtalk_test_fs:restore_home(OldHome),
                beamtalk_test_fs:rm_rf(TmpHome)
            end
        end)
    end}.
