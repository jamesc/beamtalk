%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_session_primitives_tests).

-moduledoc """
Unit tests for beamtalk_session_primitives (BT-2366, ADR 0081 Phase 3).

Covers the factory primitives (current/0, withId/1, idOf/1), the session
operations (bindingsViewFor/1, resolveFor/2, clearFor/1), the workspace-globals
view, the BindingsView read/write primitives, cross-session read + write
rejection, and the dead-session liveness error.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Fixtures
%%====================================================================

setup() ->
    application:ensure_all_started(beamtalk_runtime),
    beamtalk_session_table:new(),
    %% Clear any leftover process-dict session context from other tests.
    erase(beamtalk_session_pid),
    erase(beamtalk_session_id),
    ok.

teardown(_) ->
    erase(beamtalk_session_pid),
    erase(beamtalk_session_id),
    ok.

%% Start a shell and register it in the session table under SessionId.
start_session(SessionId) ->
    {ok, Pid} = beamtalk_repl_shell:start_link(SessionId),
    beamtalk_session_table:insert(SessionId, Pid),
    Pid.

%% Simulate "this process is the eval worker for SessionId" by seeding the
%% process dict the way beamtalk_repl_shell:seed_session_context/2 does.
seed_context(Pid, SessionId) ->
    put(beamtalk_session_pid, Pid),
    put(beamtalk_session_id, SessionId),
    ok.

%%====================================================================
%% current/0
%%====================================================================

current_outside_eval_returns_nil_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_assertEqual(nil, beamtalk_session_primitives:current())
        ]
    end}.

current_in_eval_returns_session_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Pid = start_session(<<"prim-current-1">>),
                seed_context(Pid, <<"prim-current-1">>),
                Session = beamtalk_session_primitives:current(),
                ?assertMatch(#{'$beamtalk_class' := 'Session'}, Session),
                ?assertEqual(<<"prim-current-1">>, beamtalk_session_primitives:idOf(Session)),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% withId/1
%%====================================================================

with_id_live_returns_session_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Pid = start_session(<<"prim-withid-1">>),
                Session = beamtalk_session_primitives:withId(<<"prim-withid-1">>),
                ?assertMatch(#{'$beamtalk_class' := 'Session'}, Session),
                ?assertEqual(<<"prim-withid-1">>, beamtalk_session_primitives:idOf(Session)),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

with_id_missing_returns_nil_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_assertEqual(nil, beamtalk_session_primitives:withId(<<"prim-no-such">>))
        ]
    end}.

with_id_dead_returns_nil_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Pid = start_session(<<"prim-withid-dead">>),
                beamtalk_repl_shell:stop(Pid),
                wait_dead(Pid),
                %% Entry still in table but PID dead → liveness check returns nil.
                ?assertEqual(nil, beamtalk_session_primitives:withId(<<"prim-withid-dead">>))
            end)
        ]
    end}.

with_id_accepts_string_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Pid = start_session(<<"prim-withid-str">>),
                %% A string (list) id resolves the same as the binary id.
                Session = beamtalk_session_primitives:withId("prim-withid-str"),
                ?assertMatch(#{'$beamtalk_class' := 'Session'}, Session),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% bindingsViewFor/1 + cross-session read
%%====================================================================

bindings_view_reads_own_locals_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Pid = start_session(<<"prim-bv-own">>),
                set_locals(Pid, #{x => 1, y => 2}),
                seed_context(Pid, <<"prim-bv-own">>),
                Session = beamtalk_session_primitives:current(),
                View = beamtalk_session_primitives:bindingsViewFor(Session),
                ?assertMatch(#{'$beamtalk_class' := 'BindingsView', scope := session}, View),
                ?assertEqual(1, beamtalk_session_primitives:view_at(View, x)),
                ?assertEqual(2, beamtalk_session_primitives:view_size(View)),
                ?assertEqual(
                    lists:sort([x, y]), lists:sort(beamtalk_session_primitives:view_keys(View))
                ),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

bindings_view_cross_session_read_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Caller = start_session(<<"prim-bv-caller">>),
                Other = start_session(<<"prim-bv-other">>),
                set_locals(Other, #{remote => 99}),
                seed_context(Caller, <<"prim-bv-caller">>),
                OtherSession = beamtalk_session_primitives:withId(<<"prim-bv-other">>),
                View = beamtalk_session_primitives:bindingsViewFor(OtherSession),
                %% Cross-session READ is supported.
                ?assertEqual(99, beamtalk_session_primitives:view_at(View, remote)),
                beamtalk_repl_shell:stop(Caller),
                beamtalk_repl_shell:stop(Other)
            end)
        ]
    end}.

%% Reading a never-interned name returns nil without minting a fresh atom
%% (atom-exhaustion safety for introspection over arbitrary user strings).
bindings_view_read_unknown_name_is_safe_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Pid = start_session(<<"prim-bv-unknown">>),
                seed_context(Pid, <<"prim-bv-unknown">>),
                Session = beamtalk_session_primitives:current(),
                View = beamtalk_session_primitives:bindingsViewFor(Session),
                Before = erlang:system_info(atom_count),
                %% A binary that has never been an atom must not become one.
                ?assertEqual(
                    nil,
                    beamtalk_session_primitives:view_at(
                        View, <<"definitely_not_an_atom_bt2366_xyz">>
                    )
                ),
                %% Removing it is a no-op (returns nil).
                ?assertEqual(
                    nil,
                    beamtalk_session_primitives:view_remove(
                        View, <<"another_unknown_bt2366_abc">>
                    )
                ),
                After = erlang:system_info(atom_count),
                ?assertEqual(Before, After),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% view_includes_key/2 — O(1) membership, atom + String key parity (BT-2380)
%%====================================================================

%% Session scope: present/absent for both atom and String keys. The String key
%% must match the same binding `at:` resolves (parity with the lenient read).
bindings_view_includes_key_session_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Pid = start_session(<<"prim-bv-inc-session">>),
                set_locals(Pid, #{x => 1}),
                seed_context(Pid, <<"prim-bv-inc-session">>),
                Session = beamtalk_session_primitives:current(),
                View = beamtalk_session_primitives:bindingsViewFor(Session),
                %% Atom key: present / absent.
                ?assertEqual(true, beamtalk_session_primitives:view_includes_key(View, x)),
                ?assertEqual(false, beamtalk_session_primitives:view_includes_key(View, y)),
                %% String key: parity with view_at/2.
                ?assertEqual(1, beamtalk_session_primitives:view_at(View, "x")),
                ?assertEqual(true, beamtalk_session_primitives:view_includes_key(View, "x")),
                %% Binary key parity too.
                ?assertEqual(true, beamtalk_session_primitives:view_includes_key(View, <<"x">>)),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

%% Workspace scope: present/absent against the bind:as: ETS.
bindings_view_includes_key_workspace_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                beamtalk_workspace_interface_primitives:create_bindings_table(),
                maybe_register_meta(),
                try
                    View = beamtalk_session_primitives:globalsView(),
                    ?assertMatch(
                        #{'$beamtalk_class' := 'BindingsView', scope := workspace}, View
                    ),
                    ?assertEqual(
                        false, beamtalk_session_primitives:view_includes_key(View, inc_ws_name)
                    ),
                    ?assertEqual(
                        7, beamtalk_session_primitives:view_at_put(View, inc_ws_name, 7)
                    ),
                    %% Atom key present.
                    ?assertEqual(
                        true, beamtalk_session_primitives:view_includes_key(View, inc_ws_name)
                    ),
                    %% String key parity.
                    ?assertEqual(
                        true, beamtalk_session_primitives:view_includes_key(View, "inc_ws_name")
                    )
                after
                    maybe_unregister_meta()
                end
            end)
        ]
    end}.

%% A never-interned String must return false without minting a fresh atom
%% (atom-exhaustion safety, matching the view_at/2 read-path guard).
bindings_view_includes_key_unknown_name_is_safe_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Pid = start_session(<<"prim-bv-inc-unknown">>),
                seed_context(Pid, <<"prim-bv-inc-unknown">>),
                Session = beamtalk_session_primitives:current(),
                View = beamtalk_session_primitives:bindingsViewFor(Session),
                Before = erlang:system_info(atom_count),
                ?assertEqual(
                    false,
                    beamtalk_session_primitives:view_includes_key(
                        View, "definitely_not_an_atom_bt2380_xyz"
                    )
                ),
                ?assertEqual(
                    false,
                    beamtalk_session_primitives:view_includes_key(
                        View, <<"another_unknown_bt2380_abc">>
                    )
                ),
                After = erlang:system_info(atom_count),
                ?assertEqual(Before, After),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% Cross-session write rejection
%%====================================================================

cross_session_write_rejected_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Caller = start_session(<<"prim-xw-caller">>),
                Other = start_session(<<"prim-xw-other">>),
                seed_context(Caller, <<"prim-xw-caller">>),
                OtherSession = beamtalk_session_primitives:withId(<<"prim-xw-other">>),
                View = beamtalk_session_primitives:bindingsViewFor(OtherSession),
                ?assertError(
                    #{error := #beamtalk_error{kind = cross_session_mutation_unsupported}},
                    beamtalk_session_primitives:view_at_put(View, x, 1)
                ),
                ?assertError(
                    #{error := #beamtalk_error{kind = cross_session_mutation_unsupported}},
                    beamtalk_session_primitives:view_remove(View, x)
                ),
                ?assertError(
                    #{error := #beamtalk_error{kind = cross_session_mutation_unsupported}},
                    beamtalk_session_primitives:clearFor(OtherSession)
                ),
                beamtalk_repl_shell:stop(Caller),
                beamtalk_repl_shell:stop(Other)
            end)
        ]
    end}.

%%====================================================================
%% Own-session writes enqueue
%%====================================================================

own_session_put_enqueues_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Pid = start_session(<<"prim-put-own">>),
                seed_context(Pid, <<"prim-put-own">>),
                Session = beamtalk_session_primitives:current(),
                View = beamtalk_session_primitives:bindingsViewFor(Session),
                %% at:put: returns the value put and enqueues a put mutation.
                ?assertEqual(42, beamtalk_session_primitives:view_at_put(View, foo, 42)),
                %% removeKey: returns nil and enqueues a remove.
                ?assertEqual(nil, beamtalk_session_primitives:view_remove(View, bar)),
                %% clear enqueues a clear and returns nil.
                ?assertEqual(nil, beamtalk_session_primitives:clearFor(Session)),
                {_SId, State, _W} = sys:get_state(Pid),
                ?assertEqual(
                    [{put, foo, 42}, {remove, bar, undefined}, {clear, undefined, undefined}],
                    beamtalk_repl_state:get_pending_mutations(State)
                ),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% resolveFor/2
%%====================================================================

resolve_for_finds_local_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Pid = start_session(<<"prim-resolve-1">>),
                set_locals(Pid, #{myvar => 7}),
                seed_context(Pid, <<"prim-resolve-1">>),
                Session = beamtalk_session_primitives:current(),
                ?assertEqual(7, beamtalk_session_primitives:resolveFor(Session, myvar)),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% globalsView/0 — workspace scope read + write-through
%%====================================================================

globals_view_write_through_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                beamtalk_workspace_interface_primitives:create_bindings_table(),
                %% get_session_bindings/0 gates reads on the workspace-meta
                %% sentinel; register this process as that sentinel so reads see
                %% the bind:as: ETS (the production path always has meta up).
                maybe_register_meta(),
                try
                    View = beamtalk_session_primitives:globalsView(),
                    ?assertMatch(
                        #{'$beamtalk_class' := 'BindingsView', scope := workspace}, View
                    ),
                    %% Write through to bind:as: ETS.
                    ?assertEqual(
                        123, beamtalk_session_primitives:view_at_put(View, gv_test_name, 123)
                    ),
                    ?assertEqual(123, beamtalk_session_primitives:view_at(View, gv_test_name)),
                    %% Remove through unbind.
                    ?assertEqual(nil, beamtalk_session_primitives:view_remove(View, gv_test_name)),
                    ?assertEqual(nil, beamtalk_session_primitives:view_at(View, gv_test_name))
                after
                    maybe_unregister_meta()
                end
            end)
        ]
    end}.

%%====================================================================
%% Dead-session liveness error
%%====================================================================

dead_session_raises_session_not_found_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Pid = start_session(<<"prim-dead-send">>),
                Session = beamtalk_session_primitives:withId(<<"prim-dead-send">>),
                %% Build a view while alive, then kill the shell.
                View = beamtalk_session_primitives:bindingsViewFor(Session),
                beamtalk_repl_shell:stop(Pid),
                wait_dead(Pid),
                %% A read against the dead session raises rather than blocking.
                ?assertError(
                    #{error := #beamtalk_error{kind = session_not_found}},
                    beamtalk_session_primitives:view_at(View, x)
                )
            end)
        ]
    end}.

%%====================================================================
%% Type errors
%%====================================================================

session_id_type_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_assertError(
                #{error := #beamtalk_error{kind = type_error}},
                beamtalk_session_primitives:withId(42)
            )
        ]
    end}.

%%====================================================================
%% Class-method dispatch propagates the caller's session context
%% (ADR 0081 / BT-2367): `Session current` is a class method, so dispatch
%% hops from the eval worker to the Session class gen_server. The worker's
%% seeded session context must be mirrored there for the duration of the call.
%%====================================================================

class_current_sees_caller_session_context_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Pid = start_session(<<"prim-class-current">>),
                ClassPid = wait_for_class('Session'),
                %% A caller with seeded context, dispatching `current` through the
                %% real class gen_server, gets a Session value back.
                Session = dispatch_class_method_with_context(
                    ClassPid, current, [], Pid, <<"prim-class-current">>
                ),
                ?assertMatch(#{'$beamtalk_class' := 'Session'}, Session),
                ?assertEqual(<<"prim-class-current">>, maps:get(id, Session)),
                %% The class process dict must NOT leak the mirrored context to a
                %% subsequent unseeded caller — it returns nil, not the prior id.
                NilSession = dispatch_class_method_no_context(ClassPid, current, []),
                ?assertEqual(nil, NilSession),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% liveSessions/0 (Workspace sessions, ADR 0081 Phase 7 / BT-2368)
%%====================================================================

%% No session supervisor running → empty list, never a crash.
live_sessions_no_supervisor_returns_empty_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                undefined = whereis(beamtalk_session_sup),
                ?assertEqual([], beamtalk_session_primitives:liveSessions())
            end)
        ]
    end}.

%% One Session value per live supervised shell; ids and pids match the shells,
%% and the values are usable with the instance reads (idOf/1, bindingsViewFor/1).
live_sessions_lists_live_shells_test_() ->
    {setup, fun setup_with_sup/0, fun teardown_with_sup/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid1} = beamtalk_session_sup:start_session(<<"live-a">>),
                {ok, Pid2} = beamtalk_session_sup:start_session(<<"live-b">>),
                Sessions = beamtalk_session_primitives:liveSessions(),
                ?assertEqual(2, length(Sessions)),
                lists:foreach(
                    fun(S) -> ?assertMatch(#{'$beamtalk_class' := 'Session'}, S) end, Sessions
                ),
                Ids = lists:sort([beamtalk_session_primitives:idOf(S) || S <- Sessions]),
                ?assertEqual([<<"live-a">>, <<"live-b">>], Ids),
                %% PIDs carried by the minted values match the live shells.
                Pids = lists:sort([maps:get(pid, S) || S <- Sessions]),
                ?assertEqual(lists:sort([Pid1, Pid2]), Pids),
                %% A minted value works with the bindings read (cross-session read).
                [S1 | _] = Sessions,
                View = beamtalk_session_primitives:bindingsViewFor(S1),
                ?assertMatch(#{'$beamtalk_class' := 'BindingsView'}, View),
                ?assertEqual([], beamtalk_session_primitives:view_keys(View))
            end)
        ]
    end}.

%% A minted value rejects cross-session writes when the caller is a different
%% session (the cross-session-mutation guard, same as withId/1 values).
live_sessions_value_rejects_cross_session_write_test_() ->
    {setup, fun setup_with_sup/0, fun teardown_with_sup/1, fun(_) ->
        [
            ?_test(begin
                {ok, _Pid} = beamtalk_session_sup:start_session(<<"live-target">>),
                [Session | _] = beamtalk_session_primitives:liveSessions(),
                View = beamtalk_session_primitives:bindingsViewFor(Session),
                %% Caller is a *different* session than the target → rejected.
                seed_context(self(), <<"live-caller">>),
                ?assertError(
                    #{error := #beamtalk_error{kind = cross_session_mutation_unsupported}},
                    beamtalk_session_primitives:view_at_put(View, x, 1)
                )
            end)
        ]
    end}.

%% A shell that dies between which_children/1 and the id query is skipped, not
%% minted into a Session value carrying a dead PID.
live_sessions_skips_dead_shell_test_() ->
    {setup, fun setup_with_sup/0, fun teardown_with_sup/1, fun(_) ->
        [
            ?_test(begin
                {ok, AlivePid} = beamtalk_session_sup:start_session(<<"live-alive">>),
                {ok, DeadPid} = beamtalk_session_sup:start_session(<<"live-dead">>),
                exit(DeadPid, kill),
                wait_dead(DeadPid),
                Sessions = beamtalk_session_primitives:liveSessions(),
                Pids = [maps:get(pid, S) || S <- Sessions],
                ?assert(lists:member(AlivePid, Pids)),
                ?assertNot(lists:member(DeadPid, Pids))
            end)
        ]
    end}.

%%====================================================================
%% Helpers
%%====================================================================

%% Setup variant that also starts the session supervisor so supervised shells
%% (beamtalk_session_sup:start_session/1) appear in which_children/1.
%% Defensively stops any sup left registered by a prior fixture, and unlinks the
%% fresh one so teardown's shutdown does not propagate an exit to the test runner.
setup_with_sup() ->
    setup(),
    stop_session_sup(),
    {ok, SupPid} = beamtalk_session_sup:start_link(),
    unlink(SupPid),
    SupPid.

teardown_with_sup(_SupPid) ->
    stop_session_sup(),
    teardown(undefined).

%% Stop the locally-registered session supervisor (and all supervised shells)
%% if one is running, guaranteeing the registered name is clear before returning
%% so a later fixture's start_link/0 never hits {already_started, _}.
stop_session_sup() ->
    case whereis(beamtalk_session_sup) of
        undefined ->
            ok;
        SupPid ->
            Ref = erlang:monitor(process, SupPid),
            exit(SupPid, shutdown),
            receive
                {'DOWN', Ref, process, SupPid, _} -> ok
            after 5000 ->
                %% Graceful shutdown stalled — force-kill so the name actually
                %% clears (otherwise the next fixture's start_link/0 fails) and
                %% block until the DOWN confirms it is gone.
                erlang:demonitor(Ref, [flush]),
                Ref2 = erlang:monitor(process, SupPid),
                exit(SupPid, kill),
                receive
                    {'DOWN', Ref2, process, SupPid, _} -> ok
                after 5000 -> erlang:demonitor(Ref2, [flush])
                end
            end
    end.

set_locals(Pid, Locals) ->
    sys:replace_state(Pid, fun({SId, State, Worker}) ->
        {SId, beamtalk_repl_state:set_bindings(Locals, State), Worker}
    end).

%% Wait for a class gen_server to be registered (the stdlib classes start
%% asynchronously when beamtalk_runtime boots).
wait_for_class(ClassName) ->
    wait_for_class(ClassName, 50).

wait_for_class(ClassName, 0) ->
    erlang:error({class_not_registered, ClassName});
wait_for_class(ClassName, N) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            timer:sleep(20),
            wait_for_class(ClassName, N - 1);
        Pid when is_pid(Pid) ->
            Pid
    end.

%% Dispatch a class method through the real class gen_server FROM a worker
%% process that has the session context seeded — exactly the eval-worker path.
%% Runs in a spawned process so the gen_server `From` carries that process's
%% (seeded) dictionary.
dispatch_class_method_with_context(ClassPid, Selector, Args, SessionPid, SessionId) ->
    run_in_worker(fun() ->
        seed_context(SessionPid, SessionId),
        unwrap_class_reply(gen_server:call(ClassPid, {class_method_call, Selector, Args}))
    end).

%% Same dispatch but from a worker with NO seeded session context.
dispatch_class_method_no_context(ClassPid, Selector, Args) ->
    run_in_worker(fun() ->
        unwrap_class_reply(gen_server:call(ClassPid, {class_method_call, Selector, Args}))
    end).

unwrap_class_reply({ok, Result}) -> Result;
unwrap_class_reply(Other) -> Other.

%% Run Fun in a fresh process and return its result (so the gen_server `From`
%% is that process, carrying its own process dictionary).
run_in_worker(Fun) ->
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() -> Parent ! {Ref, Fun()} end),
    receive
        {Ref, Result} -> Result
    after 5000 ->
        erlang:error(worker_timeout)
    end.

%% Register/unregister this process as the workspace-meta sentinel, but only if
%% no real meta process is already registered (so we don't clobber it).
maybe_register_meta() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            register(beamtalk_workspace_meta, self()),
            ok;
        _ ->
            ok
    end.

maybe_unregister_meta() ->
    case whereis(beamtalk_workspace_meta) of
        Self when Self =:= self() ->
            unregister(beamtalk_workspace_meta),
            ok;
        _ ->
            ok
    end.

wait_dead(Pid) ->
    %% Bounded wait via monitor so an unterminated shell fails the test fast
    %% instead of hanging the whole EUnit run on infinite recursion.
    case is_process_alive(Pid) of
        false ->
            ok;
        true ->
            Ref = erlang:monitor(process, Pid),
            receive
                {'DOWN', Ref, process, Pid, _Reason} ->
                    ok
            after 5000 ->
                erlang:demonitor(Ref, [flush]),
                ?assert(false)
            end
    end.
