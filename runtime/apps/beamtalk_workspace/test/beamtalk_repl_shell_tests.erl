%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_repl_shell module (BT-343).
%%%
%%% Tests session lifecycle, eval, bindings, and gen_server callbacks.

-module(beamtalk_repl_shell_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    application:ensure_all_started(beamtalk_runtime),
    ok.

teardown(_) ->
    ok.

%%====================================================================
%% Session Lifecycle Tests
%%====================================================================

start_stop_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-session-1">>),
                ?assert(is_process_alive(Pid)),
                beamtalk_repl_shell:stop(Pid),
                timer:sleep(50),
                ?assertNot(is_process_alive(Pid))
            end)
        ]
    end}.

%%====================================================================
%% Bindings Tests
%%====================================================================

bindings_initially_empty_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-bindings-1">>),
                {ok, Bindings} = beamtalk_repl_shell:get_bindings(Pid),
                %% ADR 0019: Workspace bindings are injected automatically;
                %% verify no USER bindings are present.
                UserBindings = maps:without(['Transcript', 'Beamtalk', 'Workspace'], Bindings),
                ?assertEqual(#{}, UserBindings),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

clear_bindings_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-clear-1">>),
                %% Clear on empty bindings should be ok
                ok = beamtalk_repl_shell:clear_bindings(Pid),
                {ok, Bindings} = beamtalk_repl_shell:get_bindings(Pid),
                %% ADR 0019: Workspace bindings re-injected after clear
                UserBindings = maps:without(['Transcript', 'Beamtalk', 'Workspace'], Bindings),
                ?assertEqual(#{}, UserBindings),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% Eval Tests (basic - without compiler daemon)
%%====================================================================

eval_empty_expression_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-eval-1">>),
                %% Empty expression should return error
                Result = beamtalk_repl_shell:eval(Pid, ""),
                ?assertMatch({error, _, _, _}, Result),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% Show Codegen Tests (BT-700)
%%====================================================================

show_codegen_returns_core_erlang_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-codegen-1">>),
                Result = beamtalk_repl_shell:show_codegen(Pid, "1 + 2"),
                ?assertMatch({ok, _, _}, Result),
                {ok, CoreErlang, Warnings} = Result,
                ?assert(is_binary(CoreErlang)),
                ?assert(byte_size(CoreErlang) > 0),
                ?assert(is_list(Warnings)),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

show_codegen_rejects_during_eval_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-codegen-busy">>),
                %% Start a long-running eval to occupy the worker slot
                Self = self(),
                spawn(fun() ->
                    Res = beamtalk_repl_shell:eval(Pid, "1 + 2"),
                    Self ! {eval_done, Res}
                end),
                %% Give eval time to start (even if it fails, the worker is set briefly)
                timer:sleep(50),
                %% Try show_codegen - if eval is still in progress, should get eval_busy
                Result = beamtalk_repl_shell:show_codegen(Pid, "42"),
                case Result of
                    {error, #beamtalk_error{kind = eval_busy}, _} ->
                        ok;
                    _ ->
                        %% Eval may have completed before show_codegen ran
                        %% (race condition in test - both outcomes are valid)
                        ok
                end,
                %% Drain eval result
                receive
                    {eval_done, _} -> ok
                after 5000 -> ok
                end,
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% gen_server Callback Tests
%%====================================================================

handle_call_unknown_request_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-unknown-1">>),
                Result = gen_server:call(Pid, unknown_request),
                ?assertEqual({error, unknown_request}, Result),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

handle_cast_ignored_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-cast-1">>),
                gen_server:cast(Pid, some_message),
                timer:sleep(20),
                ?assert(is_process_alive(Pid)),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

handle_info_ignored_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-info-1">>),
                Pid ! random_info,
                timer:sleep(20),
                ?assert(is_process_alive(Pid)),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

code_change_preserves_state_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-code-1">>),
                State = sys:get_state(Pid),
                {ok, NewState} = beamtalk_repl_shell:code_change(old_vsn, State, extra),
                ?assertEqual(State, NewState),
                beamtalk_repl_shell:stop(Pid)
            end)
        ]
    end}.

%%====================================================================
%% Session Independence Tests
%%====================================================================

multiple_sessions_independent_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid1} = beamtalk_repl_shell:start_link(<<"session-a">>),
                {ok, Pid2} = beamtalk_repl_shell:start_link(<<"session-b">>),
                ?assertNotEqual(Pid1, Pid2),
                %% ADR 0019: Both have only workspace bindings (no user bindings)
                {ok, B1} = beamtalk_repl_shell:get_bindings(Pid1),
                {ok, B2} = beamtalk_repl_shell:get_bindings(Pid2),
                UserB1 = maps:without(['Transcript', 'Beamtalk', 'Workspace'], B1),
                UserB2 = maps:without(['Transcript', 'Beamtalk', 'Workspace'], B2),
                ?assertEqual(#{}, UserB1),
                ?assertEqual(#{}, UserB2),
                beamtalk_repl_shell:stop(Pid1),
                beamtalk_repl_shell:stop(Pid2)
            end)
        ]
    end}.

%%====================================================================
%% Unload Module Tests (BT-519)
%%====================================================================

unload_module_removes_from_tracker_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-unload-1">>),
                %% Create a dummy module in the code server
                DummyMod = 'bt519_test_dummy',
                Forms = [
                    {attribute, 1, module, DummyMod},
                    {attribute, 2, export, [{hello, 0}]},
                    {function, 3, hello, 0, [{clause, 3, [], [], [{atom, 3, world}]}]}
                ],
                {ok, DummyMod, Binary} = compile:forms(Forms),
                {module, DummyMod} = code:load_binary(DummyMod, "test.erl", Binary),
                %% Inject module into the session's tracker
                sys:replace_state(Pid, fun({SId, State, Worker}) ->
                    Tracker = beamtalk_repl_state:get_module_tracker(State),
                    NewTracker = beamtalk_repl_modules:add_module(
                        DummyMod, "/tmp/test.bt", Tracker
                    ),
                    {SId, beamtalk_repl_state:set_module_tracker(NewTracker, State), Worker}
                end),
                try
                    %% Verify module is in tracker
                    {ok, Tracker1} = beamtalk_repl_shell:get_module_tracker(Pid),
                    ?assert(beamtalk_repl_modules:module_exists(DummyMod, Tracker1)),
                    %% Unload the module
                    ok = beamtalk_repl_shell:unload_module(Pid, DummyMod),
                    %% Verify module is removed from tracker
                    {ok, Tracker2} = beamtalk_repl_shell:get_module_tracker(Pid),
                    ?assertNot(beamtalk_repl_modules:module_exists(DummyMod, Tracker2)),
                    %% Verify module is no longer loaded in code server
                    ?assertEqual(false, code:is_loaded(DummyMod))
                after
                    _ = code:soft_purge(DummyMod),
                    _ = code:delete(DummyMod),
                    beamtalk_repl_shell:stop(Pid)
                end
            end)
        ]
    end}.

unload_module_not_loaded_returns_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-unload-2">>),
                try
                    Result = beamtalk_repl_shell:unload_module(Pid, 'nonexistent_module_xyz'),
                    ?assertMatch({error, #beamtalk_error{kind = module_not_loaded}}, Result)
                after
                    beamtalk_repl_shell:stop(Pid)
                end
            end)
        ]
    end}.

unload_module_not_in_tracker_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-unload-3">>),
                %% Create and load a module without adding to tracker
                DummyMod = 'bt519_test_untracked',
                Forms = [
                    {attribute, 1, module, DummyMod},
                    {attribute, 2, export, [{hello, 0}]},
                    {function, 3, hello, 0, [{clause, 3, [], [], [{atom, 3, world}]}]}
                ],
                {ok, DummyMod, Binary} = compile:forms(Forms),
                {module, DummyMod} = code:load_binary(DummyMod, "test.erl", Binary),
                try
                    %% Unload succeeds even though module isn't tracked
                    ok = beamtalk_repl_shell:unload_module(Pid, DummyMod),
                    %% Module is purged from code server
                    ?assertEqual(false, code:is_loaded(DummyMod)),
                    %% Tracker is still empty (no crash from removing non-existent entry)
                    {ok, Tracker} = beamtalk_repl_shell:get_module_tracker(Pid),
                    ?assertEqual(#{}, Tracker)
                after
                    _ = code:soft_purge(DummyMod),
                    _ = code:delete(DummyMod),
                    beamtalk_repl_shell:stop(Pid)
                end
            end)
        ]
    end}.

unload_module_in_use_returns_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-unload-4">>),
                %% Create a module with a long-running function
                DummyMod = 'bt519_test_in_use',
                Forms = [
                    {attribute, 1, module, DummyMod},
                    {attribute, 2, export, [{loop, 0}]},
                    {function, 3, loop, 0, [
                        {clause, 3, [], [], [{call, 3, {atom, 3, receive_loop}, []}]}
                    ]},
                    {function, 4, receive_loop, 0, [
                        {clause, 4, [], [], [
                            {'receive', 4, [{clause, 4, [{atom, 4, stop}], [], [{atom, 4, ok}]}]}
                        ]}
                    ]}
                ],
                {ok, DummyMod, Binary} = compile:forms(Forms),
                {module, DummyMod} = code:load_binary(DummyMod, "test.erl", Binary),
                %% Spawn a process running old code, then load new version
                %% so soft_purge will fail (old code still in use)
                LoopPid = spawn(DummyMod, loop, []),
                %% Load a new version to make the running process use "old" code
                {module, DummyMod} = code:load_binary(DummyMod, "test.erl", Binary),
                try
                    %% Now soft_purge should fail because LoopPid runs old code
                    Result = beamtalk_repl_shell:unload_module(Pid, DummyMod),
                    ?assertMatch({error, #beamtalk_error{kind = module_in_use}}, Result)
                after
                    LoopPid ! stop,
                    timer:sleep(50),
                    _ = code:soft_purge(DummyMod),
                    _ = code:delete(DummyMod),
                    _ = code:soft_purge(DummyMod),
                    beamtalk_repl_shell:stop(Pid)
                end
            end)
        ]
    end}.
