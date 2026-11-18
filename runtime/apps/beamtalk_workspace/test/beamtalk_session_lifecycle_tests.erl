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
        fun never_attached_session_has_no_idle_timer/1,
        fun close_stops_session_and_removes_row/1,
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
        set_idle_seconds(0),
        Session = new_session(<<"lc-1">>),
        Mon = erlang:monitor(process, Session),
        Client = new_client(),
        attach(Session, Client),
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
        set_idle_seconds(0),
        Session = new_session(<<"lc-4">>),
        Client = new_client(),
        attach(Session, Client),
        Session ! session_idle_timeout,
        ok = beamtalk_test_wait:sync(Session),
        ?assert(is_process_alive(Session)),
        disconnect(Client)
    end}.

busy_detached_session_is_not_stopped(_) ->
    {"a detached session with a running eval is not stopped", fun() ->
        set_idle_seconds(0),
        Session = new_session(<<"lc-5">>),
        Client = new_client(),
        attach(Session, Client),
        Worker = fake_worker(Session),
        disconnect(Client),
        Session ! session_idle_timeout,
        ok = beamtalk_test_wait:sync(Session),
        ?assert(is_process_alive(Session)),
        ?assertEqual(busy, beamtalk_repl_shell:lifecycle_state(Session)),
        Worker ! stop
    end}.

never_attached_session_has_no_idle_timer(_) ->
    {"a session no client ever attached to is not reaped by a timeout", fun() ->
        set_idle_seconds(0),
        Session = new_session(<<"lc-6">>),
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
