%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Integration tests for REPL sessions
%%%
%%% Tests the session-per-connection architecture implemented in BT-262.

-module(beamtalk_session_tests).

-include_lib("eunit/include/eunit.hrl").

%%% Test Fixture

%% Setup: Start supervisor for each test
session_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun session_creation/1,
        fun start_session/1,
        fun multiple_sessions/1,
        fun independent_bindings/1,
        fun clear_bindings/1,
        fun session_cleanup/1,
        fun invalid_request/1,
        fun supervisor_flags/1,
        fun supervisor_child_count/1
    ]}.

setup() ->
    %% Start supervisor
    {ok, SupPid} = beamtalk_session_sup:start_link(),
    SupPid.

cleanup(SupPid) ->
    %% Unlink and kill supervisor
    unlink(SupPid),
    exit(SupPid, kill),
    %% Wait for cleanup
    timer:sleep(50),
    ok.

%%% Session Creation Tests

session_creation(SupPid) ->
    %% Session supervisor should be running
    [
        ?_assert(is_pid(SupPid)),
        ?_assert(is_process_alive(SupPid))
    ].

start_session(_SupPid) ->
    %% Start a session
    SessionId = <<"test_session">>,
    {ok, SessionPid} = beamtalk_session_sup:start_session(SessionId),

    [
        ?_assert(is_pid(SessionPid)),
        ?_assert(is_process_alive(SessionPid))
    ].

multiple_sessions(_SupPid) ->
    %% Start two sessions
    {ok, Session1} = beamtalk_session_sup:start_session(<<"session_1">>),
    {ok, Session2} = beamtalk_session_sup:start_session(<<"session_2">>),

    [
        ?_assert(is_pid(Session1)),
        ?_assert(is_pid(Session2)),
        ?_assertNot(Session1 =:= Session2)
    ].

%%% Session Isolation Tests

independent_bindings(_SupPid) ->
    %% Start two sessions
    {ok, Session1} = beamtalk_session_sup:start_session(<<"session_1">>),
    {ok, Session2} = beamtalk_session_sup:start_session(<<"session_2">>),

    %% Get bindings from both sessions
    {ok, Bindings1} = beamtalk_repl_shell:get_bindings(Session1),
    {ok, Bindings2} = beamtalk_repl_shell:get_bindings(Session2),

    %% Both should start with empty bindings
    [
        ?_assertEqual(#{}, Bindings1),
        ?_assertEqual(#{}, Bindings2)
    ].

clear_bindings(_SupPid) ->
    %% Start session
    {ok, SessionPid} = beamtalk_session_sup:start_session(<<"session_1">>),

    %% Clear bindings (should succeed even if empty)
    ok = beamtalk_repl_shell:clear_bindings(SessionPid),

    %% Get bindings (should still be empty)
    {ok, Bindings} = beamtalk_repl_shell:get_bindings(SessionPid),
    [?_assertEqual(#{}, Bindings)].

%%% Session Cleanup Tests

session_cleanup(_SupPid) ->
    %% Start session
    {ok, SessionPid} = beamtalk_session_sup:start_session(<<"session_1">>),
    ?assert(is_process_alive(SessionPid)),

    %% Monitor session to detect when it dies
    MRef = erlang:monitor(process, SessionPid),

    %% Kill session
    exit(SessionPid, kill),

    %% Wait for session to die
    receive
        {'DOWN', MRef, process, SessionPid, _Reason} ->
            ok
    after 1000 ->
        error(session_did_not_die)
    end,

    %% Session should be gone
    [?_assertNot(is_process_alive(SessionPid))].

%%% Error Handling Tests

invalid_request(_SupPid) ->
    %% Start session
    {ok, SessionPid} = beamtalk_session_sup:start_session(<<"session_1">>),

    %% Send invalid request (gen_server should respond with error)
    Response = gen_server:call(SessionPid, {invalid_request}),

    [
        ?_assertMatch({error, _}, Response),
        ?_assert(is_process_alive(SessionPid))
    ].

%%% Supervisor Tests

supervisor_flags(SupPid) ->
    %% Get supervisor children
    Children = supervisor:which_children(SupPid),

    %% Should start with no children (simple_one_for_one)
    [?_assertEqual([], Children)].

supervisor_child_count(SupPid) ->
    %% Start sessions
    {ok, _Session1} = beamtalk_session_sup:start_session(<<"s1">>),
    {ok, _Session2} = beamtalk_session_sup:start_session(<<"s2">>),

    %% Count children
    ChildCount = supervisor:count_children(SupPid),
    Active = proplists:get_value(active, ChildCount),

    %% Should have 2 active sessions
    [?_assertEqual(2, Active)].
