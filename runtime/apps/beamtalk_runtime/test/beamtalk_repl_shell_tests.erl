%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_repl_shell module (BT-343).
%%%
%%% Tests session lifecycle, eval, bindings, and gen_server callbacks.

-module(beamtalk_repl_shell_tests).
-include_lib("eunit/include/eunit.hrl").

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
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-session-1">>),
              ?assert(is_process_alive(Pid)),
              beamtalk_repl_shell:stop(Pid),
              timer:sleep(50),
              ?assertNot(is_process_alive(Pid))
          end)]
     end}.

%%====================================================================
%% Bindings Tests
%%====================================================================

bindings_initially_empty_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-bindings-1">>),
              {ok, Bindings} = beamtalk_repl_shell:get_bindings(Pid),
              ?assertEqual(#{}, Bindings),
              beamtalk_repl_shell:stop(Pid)
          end)]
     end}.

clear_bindings_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-clear-1">>),
              %% Clear on empty bindings should be ok
              ok = beamtalk_repl_shell:clear_bindings(Pid),
              {ok, Bindings} = beamtalk_repl_shell:get_bindings(Pid),
              ?assertEqual(#{}, Bindings),
              beamtalk_repl_shell:stop(Pid)
          end)]
     end}.

%%====================================================================
%% Eval Tests (basic - without compiler daemon)
%%====================================================================

eval_empty_expression_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-eval-1">>),
              %% Empty expression should return error
              Result = beamtalk_repl_shell:eval(Pid, ""),
              ?assertMatch({error, _, _}, Result),
              beamtalk_repl_shell:stop(Pid)
          end)]
     end}.

%%====================================================================
%% gen_server Callback Tests
%%====================================================================

handle_call_unknown_request_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-unknown-1">>),
              Result = gen_server:call(Pid, unknown_request),
              ?assertEqual({error, unknown_request}, Result),
              beamtalk_repl_shell:stop(Pid)
          end)]
     end}.

handle_cast_ignored_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-cast-1">>),
              gen_server:cast(Pid, some_message),
              timer:sleep(20),
              ?assert(is_process_alive(Pid)),
              beamtalk_repl_shell:stop(Pid)
          end)]
     end}.

handle_info_ignored_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-info-1">>),
              Pid ! random_info,
              timer:sleep(20),
              ?assert(is_process_alive(Pid)),
              beamtalk_repl_shell:stop(Pid)
          end)]
     end}.

code_change_preserves_state_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              {ok, Pid} = beamtalk_repl_shell:start_link(<<"test-code-1">>),
              State = sys:get_state(Pid),
              {ok, NewState} = beamtalk_repl_shell:code_change(old_vsn, State, extra),
              ?assertEqual(State, NewState),
              beamtalk_repl_shell:stop(Pid)
          end)]
     end}.

%%====================================================================
%% Session Independence Tests
%%====================================================================

multiple_sessions_independent_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              {ok, Pid1} = beamtalk_repl_shell:start_link(<<"session-a">>),
              {ok, Pid2} = beamtalk_repl_shell:start_link(<<"session-b">>),
              ?assertNotEqual(Pid1, Pid2),
              %% Both have empty bindings
              {ok, B1} = beamtalk_repl_shell:get_bindings(Pid1),
              {ok, B2} = beamtalk_repl_shell:get_bindings(Pid2),
              ?assertEqual(#{}, B1),
              ?assertEqual(#{}, B2),
              beamtalk_repl_shell:stop(Pid1),
              beamtalk_repl_shell:stop(Pid2)
          end)]
     end}.
