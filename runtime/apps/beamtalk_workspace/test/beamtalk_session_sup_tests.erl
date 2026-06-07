%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_session_sup_tests).

-moduledoc """
Unit tests for beamtalk_session_sup module

Tests session supervisor behavior.
""".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

%%% Supervisor initialization tests

init_returns_correct_strategy_test() ->
    {ok, {SupFlags, _ChildSpecs}} = beamtalk_session_sup:init([]),

    %% Should use simple_one_for_one strategy
    ?assertEqual(simple_one_for_one, maps:get(strategy, SupFlags)).

init_returns_correct_intensity_test() ->
    {ok, {SupFlags, _ChildSpecs}} = beamtalk_session_sup:init([]),

    %% Should allow 10 restarts in 60 seconds
    ?assertEqual(10, maps:get(intensity, SupFlags)),
    ?assertEqual(60, maps:get(period, SupFlags)).

init_returns_single_child_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_session_sup:init([]),

    %% simple_one_for_one should have exactly 1 child spec (the template)
    ?assertEqual(1, length(ChildSpecs)).

init_child_spec_is_temporary_test() ->
    {ok, {_SupFlags, [ChildSpec]}} = beamtalk_session_sup:init([]),

    %% Sessions should be temporary (not restarted on disconnect)
    ?assertEqual(temporary, maps:get(restart, ChildSpec)),
    ?assertEqual(worker, maps:get(type, ChildSpec)).

%%% Child spec structure tests

child_spec_uses_repl_shell_test() ->
    {ok, {_SupFlags, [ChildSpec]}} = beamtalk_session_sup:init([]),

    %% Should use beamtalk_repl_shell:start_link/1
    ?assertEqual({beamtalk_repl_shell, start_link, []}, maps:get(start, ChildSpec)).

%%% Client-kind normalisation tests (session origin metadata)

normalize_kind_accepts_known_binaries_test() ->
    [
        ?assertEqual(K, beamtalk_session_sup:normalize_kind(K))
     || K <- beamtalk_session_sup:known_kinds()
    ].

normalize_kind_lowercases_test() ->
    ?assertEqual(<<"repl">>, beamtalk_session_sup:normalize_kind(<<"REPL">>)),
    ?assertEqual(<<"liveview">>, beamtalk_session_sup:normalize_kind(<<"LiveView">>)).

normalize_kind_accepts_atoms_test() ->
    ?assertEqual(<<"mcp">>, beamtalk_session_sup:normalize_kind(mcp)),
    ?assertEqual(<<"lsp">>, beamtalk_session_sup:normalize_kind(lsp)).

normalize_kind_accepts_strings_test() ->
    ?assertEqual(<<"ide">>, beamtalk_session_sup:normalize_kind("ide")).

normalize_kind_unknown_string_falls_back_test() ->
    ?assertEqual(<<"unknown">>, beamtalk_session_sup:normalize_kind(<<"bogus">>)),
    ?assertEqual(<<"unknown">>, beamtalk_session_sup:normalize_kind(<<"">>)).

normalize_kind_undefined_and_garbage_fall_back_test() ->
    ?assertEqual(<<"unknown">>, beamtalk_session_sup:normalize_kind(undefined)),
    ?assertEqual(<<"unknown">>, beamtalk_session_sup:normalize_kind(42)),
    ?assertEqual(<<"unknown">>, beamtalk_session_sup:normalize_kind({tuple})).

known_kinds_includes_user_visible_surfaces_test() ->
    Known = beamtalk_session_sup:known_kinds(),
    [
        ?assert(lists:member(K, Known))
     || K <- [<<"repl">>, <<"mcp">>, <<"lsp">>, <<"liveview">>, <<"unknown">>]
    ].

%%% Integration test

supervisor_can_start_test() ->
    %% Start the supervisor
    {ok, Pid} = beamtalk_session_sup:start_link(),

    %% Verify it's running
    ?assert(is_process_alive(Pid)),

    %% Get supervisor info
    Children = supervisor:which_children(Pid),

    %% Should have no children initially
    ?assertEqual([], Children),

    %% Cleanup
    exit(Pid, normal).

supervisor_count_children_test() ->
    %% Start the supervisor
    {ok, Pid} = beamtalk_session_sup:start_link(),

    %% Count children
    Counts = supervisor:count_children(Pid),

    %% Initially should have 0 active children
    ?assertEqual(0, proplists:get_value(active, Counts)),
    ?assertEqual(0, proplists:get_value(workers, Counts)),

    %% Cleanup
    exit(Pid, normal).

%%% stop_session/1 tests

stop_session_unknown_pid_returns_not_found_test() ->
    {ok, Sup} = beamtalk_session_sup:start_link(),

    %% A pid that is not a child of this supervisor cannot be terminated by it.
    Dead = spawn(fun() -> ok end),
    ?assertEqual({error, not_found}, beamtalk_session_sup:stop_session(Dead)),

    exit(Sup, normal).

stop_session_terminates_live_session_test() ->
    {ok, Sup} = beamtalk_session_sup:start_link(),

    %% Start a real session under supervision.
    {ok, SessionPid} = beamtalk_session_sup:start_session(<<"stop-session-test">>),
    ?assert(is_process_alive(SessionPid)),
    ?assertEqual(1, proplists:get_value(active, supervisor:count_children(Sup))),

    %% Stopping it terminates the child and drops the active count.
    ?assertEqual(ok, beamtalk_session_sup:stop_session(SessionPid)),
    ?assertNot(is_process_alive(SessionPid)),
    ?assertEqual(0, proplists:get_value(active, supervisor:count_children(Sup))),

    %% Idempotent: a second stop on the just-terminated child is a non-fatal
    %% no-op. `supervisor:terminate_child/2` returns `ok` for a dynamic child it
    %% has recently terminated (vs `{error, not_found}` for a pid that was never
    %% a child of this supervisor — covered above). Either way the call site
    %% treats it as best-effort.
    ?assertEqual(ok, beamtalk_session_sup:stop_session(SessionPid)),

    exit(Sup, normal).
