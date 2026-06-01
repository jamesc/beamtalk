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
%% Helpers
%%====================================================================

set_locals(Pid, Locals) ->
    sys:replace_state(Pid, fun({SId, State, Worker}) ->
        {SId, beamtalk_repl_state:set_bindings(Locals, State), Worker}
    end).

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
    case is_process_alive(Pid) of
        false ->
            ok;
        true ->
            timer:sleep(10),
            wait_dead(Pid)
    end.
